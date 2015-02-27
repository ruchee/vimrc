" File: plugin/swift.vim
" Author: Kevin Ballard
" Description: Plugin for Swift
" Last Change: Feb 16, 2015

if exists("g:loaded_syntastic_swift_filetype")
    finish
endif
let g:loaded_syntastic_swift_filetype = 1

let s:save_cpo = &cpo
set cpo&vim

if exists("g:syntastic_extra_filetypes")
    let g:syntastic_extra_filetypes += ['swift']
else
    let g:syntastic_extra_filetypes = ['swift']
endif

" Syntastic added a built-in Swift checker that's dangerous.
" It actually runs the file under the swift JIT instead of merely compiling
" it.
" Let's disable it for our users by setting g:syntastic_swift_checkers if the
" user hasn't already set it.
if !exists('g:syntastic_swift_checkers')
    let g:syntastic_swift_checkers = ['swiftc']
endif

let &cpo = s:save_cpo
unlet s:save_cpo
