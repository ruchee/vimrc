from subprocess import Popen, PIPE
from os import path
import sys
import string
import tempfile
import unittest
import json
import threading
import hidewin
import vim


class G:
    fsac = None
    fsi = None
    paths = {}
    locations = []
    projects = {}


class Interaction:
    def __init__(self, proc, timeOut, logfile = None):
        self.data = None
        self.event = threading.Event()
        self.proc = proc
        self._timeOut = timeOut
        self.logfile = logfile
        self.debug = not logfile is None

    def _write(self, txt):
        self.proc.stdin.write(txt)
        self.proc.stdin.flush()

        if self.debug:
            self.logfile.write("> " + txt)
            self.logfile.flush()

    def read(self):
        self.event.wait(self._timeOut)
        if self.debug:
            self.logfile.write('msg received %s\n' % self.data)
        return self.data

    def send(self, command):
        self.data = None
        self.event.clear()
        self._write(command)
        return self.read()

    def send_async(self, command):
        self.data = None
        self.event.clear()
        self._write(command)

    # only on worker thread
    def update(self, data):
        self.data = data
        self.event.set()


class FSAutoComplete:
    def __init__(self, dir, debug = False):
        if debug:
            self.logfiledir = tempfile.gettempdir() + "/log.txt"
            self.logfile = open(self.logfiledir, "w")
        else:
            self.logfile = None

        command = ['mono', dir + '/bin/fsautocomplete.exe']
        opts = { 'stdin': PIPE, 'stdout': PIPE, 'stderr': PIPE, 'universal_newlines': True }
        hidewin.addopt(opts)
        try:
            self.p = Popen(command, **opts)
        except OSError:
            try:
                self.p = Popen(command[1:], **opts)
            except OSError as error:
                msg = "Error encountered while trying to execute %s: %s" % (command[1:], error)
                raise OSError(msg)

        self.debug = debug
        self.switch_to_json()

        self.completion = Interaction(self.p, 3, self.logfile)
        self._finddecl = Interaction(self.p, 1, self.logfile)
        self._tooltip = Interaction(self.p, 1, self.logfile)
        self._helptext = Interaction(self.p, 1, self.logfile)
        self._errors = Interaction(self.p, 3, self.logfile)
        self._project = Interaction(self.p, 3, self.logfile)
        self._getpaths = Interaction(self.p, 1, self.logfile)

        self.worker = threading.Thread(target=self.work, args=(self,))
        self.worker.daemon = True
        self.worker.start()

    def __log(self, msg):
        if self.debug:
            self.logfile.write(msg)
            self.logfile.flush()

    def send(self, txt):
        if self.debug:
            self.logfile.write("> " + txt)
            self.logfile.flush()

        self.p.stdin.write(txt)

    def work(self,_):
        if self.debug:
            self.logfile2 = open(tempfile.gettempdir() + "/log2.txt", "w")

        while True:
            data = self.p.stdout.readline()

            if self.debug:
                self.logfile2.write("::work read: %s" % data)
                self.logfile2.flush()

            # Temporary fix for unwanted stdout messages from Mono V5.0.1.1
            # To be removed for the next Mono release
            if (len(data) == 0 or data[0] != '{'):
                continue

            parsed = json.loads(data)
            if parsed['Kind'] == "completion":
                self.completion.update(parsed['Data'])
            elif parsed['Kind'] == "tooltip":
                self._tooltip.update(parsed['Data'])
            elif parsed['Kind'] == "helptext":
                self._helptext.update(parsed['Data'])
            elif parsed['Kind'] == "errors":
                self._errors.update(parsed['Data'])
            elif parsed['Kind'] == "project":
                data = parsed['Data']
                G.projects[data['Project']] = data
                self._project.update(data)
            elif parsed['Kind'] == "finddecl":
                self._finddecl.update(parsed['Data'])
            elif parsed['Kind'] == "compilerlocation":
                self._getpaths.update(parsed['Data'])

    def help(self):
        self.send("help\n")

    def switch_to_json(self):
        self.send("outputmode json\n")

    def project(self, fn):
        self.send("project \"%s\" verbose\n" % path.abspath(fn))

    def parse(self, fn, full, lines):
        self.send("parse \"%s\"\n" % (fn))
        for line in lines:
            self.send(line + "\n")
        self.send("<<EOF>>\n")

    def quit(self):
        self.send("quit\n")
        self.p.wait()

        if self.debug:
            self.logfile.close()

    def get_paths(self):
        return self._getpaths.send("compilerlocation\n")

    def complete(self, fn, line, column, base):
        self.__log('complete: base = %s\n' % base)

        msg = self.completion.send('completion "%s" \"%s\" %d %d filter=%s\n' % (fn, self._current_line(), line, column, base))

        self.__log('msg received %s\n' % msg)

        if msg is None:
            return []

        if base != '':
            msg = list(filter(lambda line: line['Name'].lower().find(base.lower()) != -1,
                              msg))
        msg.sort(key=lambda x: x['Name'].startswith(base), reverse=True)

        return msg

    def finddecl(self, fn, line, column):
        msg = self._finddecl.send('finddecl "%s" \"%s\" %d %d\n' % (fn, self._current_line(), line, column))
        if msg != None:
            return str(msg['File']), (int(str(msg['Line'])), int(str(msg['Column'])))
        else:
            return None

    def errors(self, fn, full, lines):
        self.__log('errors: fn = %s\n' % fn)

        fulltext = "full" if full else ""
        self.send("parse \"%s\" %s\n" % (fn, fulltext))

        for line in lines:
            self.send(line + "\n")

        msg = self._errors.send("<<EOF>>\n")
        self.__log('msg received: %s\n' % msg)

        return msg

    def _current_line(self):
        return vim.current.line.replace("\"", "\\\"")

    def errors_current(self):
        msg = self._errors.read()
        if msg == None:
            return []
        else:
            return msg

    def _vim_encode(self, s):
        """Encode string so vim can properly display it"""
        if (sys.version_info > (3, 0)):
            return s
        return s.encode(vim.eval("&encoding"))

    def _format_comment(self, comment):
        """Clean comment so it displays nicely"""
        return self._vim_encode(comment.replace("\"", "\\\"").replace("\n ", "\n").strip())

    def tooltip(self, fn, line, column, include_comments):
        """Get the tooltip information for an expression"""
        msg = self._tooltip.send('tooltip "%s" \"%s\" %d %d 500\n' % (fn, self._current_line(), line, column))
        if msg == None:
            return ""

        output_signature = ""
        for ols in msg:
            for ol in ols:
                output_signature = output_signature + self._vim_encode(ol['Signature']) + "\n"

        output_comments = ""
        if include_comments:
            for ols in msg:
                for ol in ols:
                    output_comments = output_comments + self._format_comment(ol['Comment']) + "\n"
                
        if include_comments and output_comments.strip() != "":
            output = 'HasComments%s\n%s' % (output_signature, output_comments)
        else:
            output = output_signature

        return output

    def helptext(self, candidate, include_comments):
        """Get the helptext for an expression (used for omni completion)"""
        msg = self._helptext.send('helptext %s\n' % candidate)
        if msg == None:
            return ""

        output_signature = ""
        for ols in msg['Overloads']:
            for ol in ols:
                output_signature = output_signature + self._vim_encode(ol['Signature']) + "\n"

        output_comments = ""
        if include_comments:
            for ols in msg['Overloads']:
                for ol in ols:
                    output_comments = output_comments + self._format_comment(ol['Comment']) + "\n"
        
        if include_comments and output_comments.strip() != "":
            msg = '%s\n%s' % (output_signature, output_comments)
        else:
            msg = output_signature

        if "\'" in msg and "\\\"" in msg:
            msg = msg.replace("\\\"", "\'") #HACK: dictionary parsing in vim gets weird if both ' and " get printed in the same string, so replace " with '
        elif "\n" in msg:
            msg = msg + "\n\n'" #HACK: - the ' is inserted to ensure that newlines are interpreted properly in the preview window
        return msg

    def shutdown(self):
        """Shutdown fsautocomplete process"""
        try:
            self.send("quit\n")
            self.p.kill()
        except:
            pass

class FSharpVimFixture(unittest.TestCase):
    def setUp(self):
        self.fsac = FSAutoComplete('.')
        self.testscript = 'test/TestScript.fsx'
        with open(self.testscript, 'r') as content_file:
            content = map(lambda line: line.strip('\n'), list(content_file))

        self.fsac.parse(self.testscript, True, content)

    def tearDown(self):
        self.fsac.quit()

    def test_completion(self):
        completions = self.fsac.complete(self.testscript, 8, 16, '')

if __name__ == '__main__':
    unittest.main()
