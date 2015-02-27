" File: swift.vim
" Author: Kevin Ballard
" Description: Indentation file for Swift
" Last Modified: June 05, 2014

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setl autoindent smartindent nocindent

let b:undo_indent = "
            \ setlocal autoindent< smartindent< cindent<
            \"

" vim: set et sw=4 ts=4:
