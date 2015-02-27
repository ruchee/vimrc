" File: ftplugin/swift.vim
" Author: Kevin Ballard
" Description: Filetype plugin for Swift
" Last Change: Jul 25, 2014

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

let s:save_cpo = &cpo
set cpo&vim

" Settings {{{1

" Match Xcode default indentation settings
setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

" Use 80 as the text width but only format comments
setlocal textwidth=80
setlocal formatoptions-=t formatoptions+=croqnl
silent! setlocal formatoptions+=j

" cc=+1 is common, but showing it for the comment width kind of sucks.
" Let's pick 120 characters instead, that's a good length.
setlocal colorcolumn=121

setlocal suffixesadd=.swift

setlocal comments=s1:/**,mb:*,ex:*/,s1:/*,mb:*,ex:*/,:///,://
setlocal commentstring=//%s

let s:fold = has("folding") ? get(b:, 'swift_no_fold', get(g:, 'swift_no_fold')) : 0
if s:fold != 2
    if s:fold == 1
        setlocal foldmethod=syntax
        setlocal foldlevel<
    else
        setlocal foldmethod=syntax
        setlocal foldlevel=999
    endif
    let b:swift__did_set_fold=1
endif
unlet s:fold

let s:conceal = has('conceal') ? get(b:, 'swift_no_conceal', get(g:, 'swift_no_conceal')) : 0
if !s:conceal
    setlocal conceallevel=2
    let b:swift__did_set_conceallevel=1
endif
unlet s:conceal


" Commands {{{1

" See |:SwiftRun| for docs
command! -nargs=* -complete=file -buffer -bang SwiftRun call swift#Run(<bang>0, <q-args>)

" See |:SwiftEmitIr| for docs
command! -nargs=* -buffer SwiftEmitIr call swift#Emit(0, "ir", 0, <q-args>)

" See |:SwiftEmitSil| for docs
command! -nargs=* -buffer -bang SwiftEmitSil call swift#Emit(0, "sil", <bang>0, <q-args>)

" See |:SwiftEmitAsm| for docs
command! -nargs=* -buffer SwiftEmitAsm call swift#Emit(0, "assembly", 0, <q-args>)

" See |:SwiftEmitObjcHeader| for docs
command! -nargs=* -buffer SwiftEmitHeader call swift#Emit(0, "objc-header", 0, <q-args>)

" See |:SwiftRunTests| for docs
command! -nargs=* -buffer SwiftRunTests call swift#RunTests(<q-args>)

" See |:SwiftVersion| for docs
command! -buffer -bar SwiftVersion call swift#PrintVersion()

" Tab command variants {{{2

if has("windows")
    command! -nargs=* -buffer TabSwiftEmitIr call swift#Emit(1, "ir", 0, <q-args>)
    command! -nargs=* -buffer -bang TabSwiftEmitSil call swift#Emit(1, "sil", <bang>0, <q-args>)
    command! -nargs=* -buffer TabSwiftEmitAsm call swift#Emit(1, "assembly", 0, <q-args>)
    command! -nargs=* -buffer TabSwiftEmitHeader call swift#Emit(1, "objc-header", 0, <q-args>)
endif

" Mappings {{{1

" Map ⌘R in MacVim to :SwiftRun
nnoremap <buffer> <silent> <D-r> :SwiftRun<CR>

" Map ⌘⇧R in MacVim to :SwiftRun! pre-filled with the last args
nnoremap <buffer> <D-R> :SwiftRun! <C-r>=join(b:swift_last_swift_args)<CR><C-\>eswift#AppendCmdLine(' -- ' . join(b:swift_last_args))<CR>

if !exists("b:swift_last_swift_args") || !exists("b:swift_last_args")
    let b:swift_last_swift_args = []
    let b:swift_last_args = []
endif

" Miscellaneous {{{1

" Add support to NERDCommenter
if !exists('g:swift_setup_NERDCommenter')
    let g:swift_setup_NERDCommenter = 1

    let s:delimiter_map = { 'swift': { 'left': '//', 'leftAlt': '/*', 'rightAlt': '*/' } }

    if exists('g:NERDDelimiterMap')
        call extend(g:NERDDelimiterMap, s:delimiter_map)
    elseif exists('g:NERDCustomDelimiters')
        call extend(g:NERDCustomDelimiters, s:delimiter_map)
    else
        let g:NERDCustomDelimiters = s:delimiter_map
    endif
    unlet s:delimiter_map
endif

" Check for 'showmatch' because it doesn't work right with \()
if &showmatch
    echohl WarningMsg
    echomsg "Swift string interpolations do not work well with 'showmatch'"
    echohl None
    echomsg "It is recommended that you turn it off and use matchparen instead"
endif

" Cleanup {{{1

let b:undo_ftplugin = "
            \ setlocal expandtab< shiftwidth< tabstop< softtabstop< textwidth<
            \|setlocal colorcolumn<
            \|setlocal formatoptions< suffixesadd< comments< commentstring<
            \|setlocal showmatch<
            \|if exists('b:swift__did_set_fold')
                \|setlocal foldmethod< foldlevel<
                \|unlet b:swift__did_set_fold
            \|endif
            \|if exists('b:swift__did_set_conceallevel')
                \|setlocal conceallevel<
                \|unlet b:swift__did_set_conceallevel
            \|endif
            \|delcommand SwiftRun
            \|delcommand SwiftEmitIr
            \|delcommand SwiftEmitSil
            \|delcommand SwiftEmitAsm
            \|delcommand SwiftEmitHeader
            \|if has('windows')
                \|delcommand TabSwiftEmitIr
                \|delcommand TabSwiftEmitSil
                \|delcommand TabSwiftEmitAsm
                \|delcommand TabSwiftEmitHeader
            \|endif
            \|nunmap <buffer> <D-r>
            \|nunmap <buffer> <D-R>
            \|unlet! b:swift_last_args b:swift_last_swift_args
            \"

" }}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sw=4 ts=4:
