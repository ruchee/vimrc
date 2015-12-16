" Vim filetype plugin
" Language:     F#
" Last Change:  Fri 16 Oct 2015
" Maintainer:   Gregor Uhlenheuer <kongo2002@googlemail.com>

if exists('b:did_ftplugin')
    finish
endif
let b:did_ftplugin = 1

"set some defaults
if !exists('g:fsharp_only_check_errors_on_write')
    let g:fsharp_only_check_errors_on_write = 0
endif
if !exists('g:fsharp_completion_helptext')
    let g:fsharp_completion_helptext = 1
endif

let s:cpo_save = &cpo
set cpo&vim

" check for python support
if has('python')
    python <<EOF
import vim
import os
import re
fsharp_dir = vim.eval("expand('<sfile>:p:h')")
file_dir = vim.eval("expand('%:p:h')")
sys.path.append(fsharp_dir)

from fsharpvim import FSAutoComplete, G
from fsi import FSharpInteractive
import pyvim

debug = vim.eval("get(g:, 'fsharpbinding_debug', 0)") != '0'
if G.fsac is None:
   G.fsac = FSAutoComplete(fsharp_dir, debug)
   G.fsac.get_paths()
vim_var_exists = lambda var_name: vim.eval("exists('%s')" % var_name) != '0'
# retrieve path to a compiler tool (fsi, msbuild/xbuild) with fsautocomplete unless set by the user
def get_path(var_name, path_obj):
    if vim_var_exists(var_name):
        return
    if path_obj not in G.paths:
        G.paths = G.fsac.get_paths()
    if path_obj in G.paths:
        vim.command('let %s = "%s"' % (var_name, re.escape(G.paths[path_obj])))
get_path('g:fsharp_interactive_bin', 'Fsi')
get_path('g:fsharp_xbuild_path', 'MSBuild')
if G.fsi is None and vim_var_exists('g:fsharp_interactive_bin'):
    G.fsi = FSharpInteractive(vim.eval('g:fsharp_interactive_bin'), debug)

#find project file if any - assumes fsproj file will be in the same directory as the fs or fsi file
b = vim.current.buffer
x,ext = os.path.splitext(b.name)
if '.fs' == ext or '.fsi' == ext:
    dir = os.path.dirname(os.path.realpath(b.name))
    projs = filter(lambda f: '.fsproj' == os.path.splitext(f)[1], os.listdir(dir))
    if len(projs):
        proj_file = os.path.join(dir, projs[0])
        vim.command("let b:proj_file = '%s'" % proj_file)
        G.fsac.project(proj_file)
G.fsac.parse(b.name, True, b)
EOF

    nnoremap <buffer> <leader>t :call fsharpbinding#python#TypeCheck()<cr>
    nnoremap <buffer> <leader>d :call fsharpbinding#python#GotoDecl()<cr>
    nnoremap <buffer> <leader>s :call fsharpbinding#python#GoBackFromDecl()<cr>
    nnoremap <buffer> <leader>e :call fsharpbinding#python#FsiInput()<cr>

    com! -buffer FSharpLogFile call fsharpbinding#python#LoadLogFile()
    com! -buffer FSharpToggleHelptext call fsharpbinding#python#ToggleHelptext()
    com! -buffer -nargs=* -complete=file FSharpParseProject call fsharpbinding#python#ParseProject(<f-args>)
    com! -buffer -nargs=* -complete=file FSharpBuildProject call fsharpbinding#python#BuildProject(<f-args>)
    com! -buffer -nargs=* -complete=file FSharpRunTests call fsharpbinding#python#RunTests(<f-args>)
    com! -buffer -nargs=* -complete=file FSharpRunProject call fsharpbinding#python#RunProject(<f-args>)

    "fsi
    com! -buffer FsiShow call fsharpbinding#python#FsiShow()
    com! -buffer FsiClear call fsharpbinding#python#FsiClear()
    com! -buffer FsiRead call fsharpbinding#python#FsiRead(0.5) "short timeout as there may not be anything to read
    com! -buffer FsiReset call fsharpbinding#python#FsiReset(g:fsharp_interactive_bin)
    com! -buffer -nargs=1 FsiEval call fsharpbinding#python#FsiEval(<q-args>)
    com! -buffer FsiEvalBuffer call fsharpbinding#python#FsiSendAll()

    nnoremap  :<C-u>call fsharpbinding#python#FsiSendLine()<cr>
    vnoremap  :<C-u>call fsharpbinding#python#FsiSendSel()<cr>
    nnoremap <leader>i :<C-u>call fsharpbinding#python#FsiSendLine()<cr>
    vnoremap <leader>i :<C-u>call fsharpbinding#python#FsiSendSel()<cr>

    augroup fsharpbindings_au
        au!
        " closing the scratch window after leaving insert mode
        " is common practice
        au BufWritePre  *.fs,*.fsi,*fsx call fsharpbinding#python#OnBufWritePre()
        if version > 703
            " these events new in Vim 7.4
            au TextChanged  *.fs,*.fsi,*fsx call fsharpbinding#python#OnTextChanged()
            au TextChangedI *.fs,*.fsi,*fsx call fsharpbinding#python#OnTextChangedI()
            au CursorHold   *.fs,*.fsi,*fsx call fsharpbinding#python#OnCursorHold()
            au InsertLeave  *.fs,*.fsi,*fsx call fsharpbinding#python#OnInsertLeave()
        endif
        au BufEnter     *.fs,*.fsi,*fsx call fsharpbinding#python#OnBufEnter()
        au InsertLeave  *.fs,*.fsi,*fsx  if pumvisible() == 0|silent! pclose|endif
    augroup END

    " omnicomplete
    setlocal omnifunc=fsharpbinding#python#Complete
endif

" enable syntax based folding
setl fdm=syntax

" comment settings
setl formatoptions=croql
setl commentstring=(*%s*)
setl comments=s0:*\ -,m0:*\ \ ,ex0:*),s1:(*,mb:*,ex:*),:\/\/\/,:\/\/

" make ftplugin undo-able
let b:undo_ftplugin = 'setl fo< cms< com< fdm<'

let &cpo = s:cpo_save

" vim: sw=4 et sts=4
