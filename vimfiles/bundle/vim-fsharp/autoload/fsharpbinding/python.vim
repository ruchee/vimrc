" Vim autoload functions

if exists('g:loaded_autoload_fsharpbinding_python')
    finish
endif
let g:loaded_autoload_fsharpbinding_python = 1

let s:cpo_save = &cpo
set cpo&vim

if has('python3')
    let s:py_env = 'python3 << EOF'
else
    let s:py_env = 'python << EOF'
endif

" taken from: http://stackoverflow.com/questions/1533565/how-to-get-visually-selected-text-in-vimscript
function! s:get_visual_selection()
  " Why is this not a built-in Vim script function?!
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  "return join(lines, "\n")
  return lines
endfunction

function! s:get_complete_buffer()
    return join(getline(1, '$'), "\n")
endfunction

if has('win32') || has('win32unix')
    function! s:platform_exec(cmd)
        execute '!' . a:cmd
    endfunction
else
    function! s:platform_exec(cmd)
        execute '!mono' a:cmd
    endfunction
endif

" Vim73-compatible version of pyeval
" taken from: http://stackoverflow.com/questions/13219111/how-to-embed-python-expression-into-s-command-in-vim
function s:pyeval(expr)
    if version > 703
        if has('python3')
            return py3eval(a:expr)
        elseif has('python')
            return pyeval(a:expr)
        endif
    endif
exe s:py_env
import json
arg = vim.eval('a:expr')
result = json.dumps(eval(arg))
vim.command('return ' + result)
EOF
endfunction

function! fsharpbinding#python#LoadLogFile()
exe s:py_env
print(G.fsac.logfiledir)
EOF
endfunction

function! fsharpbinding#python#ParseProject(...)
    execute 'wa'
    if a:0 > 0
    exe s:py_env
G.fsac.project(vim.eval("a:1"))
EOF
    elseif exists('b:proj_file')
    exe s:py_env
G.fsac.project(vim.eval("b:proj_file"))
EOF
    endif
endfunction

function! fsharpbinding#python#BuildProject(...)
    try
        execute 'wa'
        if a:0 > 0
            execute '!' . shellescape(g:fsharp_xbuild_path) . ' ' . fnameescape(a:1)
        elseif exists('b:proj_file')
            execute '!' . shellescape(g:fsharp_xbuild_path) . ' ' . fnameescape(b:proj_file) "/verbosity:quiet /nologo"
            call fsharpbinding#python#ParseProject()
            let b:fsharp_buffer_changed = 1
        else
            echoe "no project file could be found"
        endif
    catch
        echoe "failed to execute build. ex: " v:exception
    endtry
endfunction

function! fsharpbinding#python#RunProject(...)
    try
        execute 'wa'
        if a:0 > 0
            call s:platform_exec(fnameescape(a:1))
        elseif exists('b:proj_file')
            let cmd = 'G.projects[vim.eval("b:proj_file")]["Output"]'
            "echom "runproj pre s:pyeval " cmd
            let target = s:pyeval(cmd)
            call s:platform_exec(fnameescape(target))
        else
            echoe "no project file could be found" > 0
        endif
    catch
        echoe "failed to execute build. ex: " v:exception
    endtry
endfunction

function! fsharpbinding#python#RunTests(...)
    try
        execute 'wa'
        call fsharpbinding#python#BuildProject()
        if a:0 > 0 && exists('g:fsharp_test_runner')
            call s:platform_exec(shellescape(g:fsharp_test_runner) . ' ' . fnameescape(a:1))
        elseif exists('b:proj_file') && exists('g:fsharp_test_runner')
            let cmd = 'G.projects[vim.eval("b:proj_file")]["Output"]'
            let target = s:pyeval(cmd)
            call s:platform_exec(shellescape(g:fsharp_test_runner) . ' ' . fnameescape(target))
        else
            echoe "no project file or test runner could be found"
        endif
    catch
        echoe "failed to execute tests. ex: " v:exception
    endtry
endfunction

" Get type information for the expression at the cursor
" includeComments [0|1]
function! fsharpbinding#python#TypeInfo(includeComments)
    exe s:py_env
b = vim.current.buffer
G.fsac.parse(b.name, True, b)
row, col = vim.current.window.cursor
res = G.fsac.tooltip(b.name, row, col + 1, vim.eval('a:includeComments') != '0')
lines = res.splitlines()
first = ""
if len(lines):
    first = lines[0]
if first.startswith('Multiple') or first.startswith('type'):
    vim.command('echo "%s"' % res)
elif first.startswith('HasComments'):
    vim.command('echo "%s"' % res.replace("HasComments", "", 1))
else:
    vim.command('echo "%s"' % first)
EOF
    let b:fsharp_buffer_changed = 0
endfunction

" Get a type definition for an expression
function! fsharpbinding#python#TypeCheck()
    call fsharpbinding#python#TypeInfo(0)
endfunction

" Get a type definition and available comment block for an expression
function! fsharpbinding#python#TypeHelp()
    call fsharpbinding#python#TypeInfo(1)
endfunction

" probable loclist format
" {'lnum': 2, 'bufnr': 1, 'col': 1, 'valid': 1, 'vcol': 1, 'nr': -1, 'type': 'W', 'pattern': '', 'text': 'Expected an assignment or function call and instead saw an expression.'}

" G.fsac format
" {"StartLine":4,"StartLine":5,"EndLine":4,"EndLine":5,"StartColumn":0,"EndColumn":4,"Severity":"Error","Message":"The value or constructor 'asdf' is not defined","Subcategory":"typecheck","FileName":"/Users/karlnilsson/code/kjnilsson/fsharp-vim/test.fsx"}
function! fsharpbinding#python#CurrentErrors()
    let result = []
    let buf = bufnr('%')
    try
        if version > 703
            let errs = s:pyeval('G.fsac.errors_current()')
        else
            " Send a sync parse request if Vim 7.3, otherwise misses response for large files
            let errs = s:pyeval("G.fsac.errors(vim.current.buffer.name, True, vim.current.buffer)")
        endif

        if !empty(errs)
            for e in errs['Errors']
                call add(result,
                    \{'lnum': e['StartLine'],
                    \ 'col': e['StartColumn'] - 1,
                    \ 'type': e['Severity'][0],
                    \ 'text': e['Message'],
                    \ 'hl': '\%' . e['StartLine'] . 'l\%>' . (e['StartColumn'] - 1) .  'c\%<' . e['EndColumn'] . 'c',
                    \ 'bufnr': buf,
                    \ 'valid': 1 })
            endfor
        endif
    catch
        echoe "failed to parse file. ex: " v:exception
    endtry
    return result
endfunction

function! fsharpbinding#python#ToggleHelptext()
    if g:fsharp_completion_helptext
        let g:fsharp_completion_helptext = 0
    else
        let g:fsharp_completion_helptext = 1
    endif
endfunction

function! fsharpbinding#python#Complete(findstart, base)
    let line = getline('.')
    let idx = col('.') - 1 "1-indexed

    " if there are trailing characters move one further back
    if len(line) >= idx
        let idx -= 1
    endif

    while idx > 0
        let c = line[idx]
        if c == ' ' || c == '.' || c == '('
            let idx += 1
            break
        endif
        let idx -= 1
    endwhile

    if a:findstart == 1
        return idx
    else

    exe s:py_env
b = vim.current.buffer
row, col = vim.current.window.cursor
line = b[row - 1]
if col > len(line):
    col = len(line)
G.fsac.parse(b.name, True, b)
for line in G.fsac.complete(b.name, row, col + 1, vim.eval('a:base')):
    name = str(line['Name'])
    abbr = name
    if " " in name:
        name = "``%s``" % name
    glyph = str(line['Glyph'])
    if int(vim.eval('g:fsharp_completion_helptext')) > 0:
        include_comments = vim.eval('g:fsharp_helptext_comments') != '0'
        ht = G.fsac.helptext(name, include_comments)
        x = {'word': name,
             'abbr': abbr,
             'info': ht,
             'menu': glyph}
        vim.eval('complete_add(%s)' % x)
    else:
        x = {'word': name,
             'menu': glyph}
        vim.eval('complete_add(%s)' % x)

EOF
        return []
    endif
    let b:fsharp_buffer_changed = 0
endfunction

function! fsharpbinding#python#GoBackFromDecl()
    exe s:py_env
b = vim.current.buffer
w = vim.current.window
try:
    f, cur = G.locations.pop()
    # declared within same file
    if b.name == f:
        w.cursor = cur
    else:
        pyvim.jump(f, cur)
except:
    print("no more locations")
EOF
endfunction

function! fsharpbinding#python#GotoDecl()
    exe s:py_env
b = vim.current.buffer
w = vim.current.window
G.fsac.parse(b.name, True, b)
row, col = vim.current.window.cursor
res = G.fsac.finddecl(b.name, row, col + 1)
G.locations.append((b.name, w.cursor))
if res == None:
    vim.command('echo "declaration not found"')
else:
    f, cur = res
    # declared within same file
    if b.name == f:
        w.cursor = cur
    else:
        pyvim.jump(f, cur)
EOF
endfunction

function! fsharpbinding#python#OnBufWritePre()
    "ensure a parse has been requested before BufWritePost is called
    exe s:py_env
G.fsac.parse(vim.current.buffer.name, True, vim.current.buffer)
EOF
    let b:fsharp_buffer_changed = 0
endfunction

function! fsharpbinding#python#OnInsertLeave()
    if exists ("b:fsharp_buffer_changed") != 0
        if b:fsharp_buffer_changed == 1
    exe s:py_env
G.fsac.parse(vim.current.buffer.name, True, vim.current.buffer)
EOF
        endif
    endif
endfunction

function! fsharpbinding#python#OnCursorHold()
    if exists ("g:fsharp_only_check_errors_on_write") != 0 &&
    \  exists (':SyntasticCheck') == 2
        if g:fsharp_only_check_errors_on_write != 1 && b:fsharp_buffer_changed == 1
            exec "SyntasticCheck"
        endif
    endif
    let b:fsharp_buffer_changed = 0
endfunction

function! fsharpbinding#python#OnTextChanged()
    let b:fsharp_buffer_changed = 1
    "TODO: make an parse_async that writes to the server on a background thread
    exe s:py_env
G.fsac.parse(vim.current.buffer.name, True, vim.current.buffer)
EOF
endfunction

function! fsharpbinding#python#OnTextChangedI()
    let b:fsharp_buffer_changed = 1
endfunction

" Ensure that python processes close on exit
function! fsharpbinding#python#OnVimLeave()
exe s:py_env
G.fsac.shutdown()
G.fsi.shutdown()
EOF
endfunction

function! fsharpbinding#python#OnBufEnter()
    let b:fsharp_buffer_changed = 1
    set updatetime=500
exe s:py_env
G.fsac.parse(vim.current.buffer.name, True, vim.current.buffer)

file_dir = vim.eval("expand('%:p:h')")
G.fsi.cd(file_dir)
if vim.eval("exists('b:proj_file')") == 1:
    G.fsac.project(vim.eval("b:proj_file"))
EOF
    "set makeprg
    if !filereadable(expand("%:p:h")."/Makefile")
        if exists('b:proj_file')
            let &l:makeprg=shellescape(g:fsharp_xbuild_path) . ' ' . b:proj_file . ' /verbosity:quiet /nologo /p:Configuration=Debug'
            setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
        endif
    endif
endfunction

function! fsharpbinding#python#FsiReset(fsi_path)
    exe s:py_env
G.fsi.shutdown()
G.fsi = FSharpInteractive(vim.eval('a:fsi_path'))
G.fsi.cd(vim.eval("expand('%:p:h')"))
EOF
    exec 'bw fsi-out'
    echo "fsi reset"
endfunction

function! fsharpbinding#python#FsiInput()
    let text = input('> ')
    call fsharpbinding#python#FsiEval(text)
endfunction

function! fsharpbinding#python#FsiSend(text)
    exe s:py_env
path = vim.current.buffer.name
(row, col) = vim.current.window.cursor
G.fsi.set_loc(path, row)
G.fsi.send(vim.eval('a:text'))
EOF
endfunction

function! fsharpbinding#python#FsiShow()
    try
        if bufnr('fsi-out') == -1
            exec 'badd fsi-out'
        else
            exec 'vsplit fsi-out'
            setlocal buftype=nofile
            setlocal bufhidden=hide
            setlocal noswapfile
            exec 'wincmd p'
        endif
    catch
        echoe "failed to display fsi output. ex" v:exception
    endtry
endfunction

function! fsharpbinding#python#FsiPurge()
exe s:py_env
lines = G.fsi.purge()
for b in vim.buffers:
    if 'fsi-out' in b.name:
        b.append(lines)
        break
EOF
endfunction

function! fsharpbinding#python#FsiClear()
exe s:py_env
lines = G.fsi.purge()
for b in vim.buffers:
    if 'fsi-out' in b.name:
        del b[:]
        break
EOF
endfunction
function! fsharpbinding#python#FsiRead(time_out)
exe s:py_env
lines = G.fsi.read_until_prompt(float(vim.eval('a:time_out')))
for b in vim.buffers:
    if 'fsi-out' in b.name:
        b.append(lines)
        for w in vim.current.tabpage.windows:
            if b.name in w.buffer.name:
                w.cursor = len(b) - 1, 0
                vim.command('exe %s"wincmd w"' % w.number)
                vim.command('exe "normal! G"')
                vim.command('exe "wincmd p"')
                break
        break
#echo first nonempty line
for l in lines:
    if l != "":
        vim.command("redraw | echo '%s'" % l.replace("'", "''"))
        break
EOF
endfunction

function! fsharpbinding#python#FsiEval(text)
    try
    "clear anything in the buffer
        call fsharpbinding#python#FsiPurge()
        call fsharpbinding#python#FsiSend(a:text)
        if bufnr('fsi-out') == -1
            exec 'badd fsi-out'

            " auto-open the fsi-out buffer
            if exists('g:fsharp_fsi_show_auto_open')
                if g:fsharp_fsi_show_auto_open == 1
                    call fsharpbinding#python#FsiShow()
                endif
            endif
        endif
        call fsharpbinding#python#FsiRead(5)
    catch
        echoe "fsi eval failure. ex" v:exception
    endtry
endfunction

function! fsharpbinding#python#FsiSendLine()
    let text = getline('.')
    "need to do this before FsiEval else we'll force a refresh
    exec 'normal j'
    call fsharpbinding#python#FsiEval(text)
endfunction

function! fsharpbinding#python#FsiSendSel()
    let lines = s:get_visual_selection()
    exec 'normal' len(lines) . 'j'
    let text = join(lines, "\n")
    call fsharpbinding#python#FsiEval(text)
endfunction

function! fsharpbinding#python#FsiSendAll()
    let text = s:get_complete_buffer()
    call fsharpbinding#python#FsiEval(text)
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=4 et sts=4
