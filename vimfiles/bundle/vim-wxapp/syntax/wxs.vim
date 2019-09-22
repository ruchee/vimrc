if exists("b:current_syntax")
  finish
endif

runtime! syntax/javascript.vim

unlet! b:current_syntax

let b:current_syntax = "wxs"

" vim:set sw=2:
