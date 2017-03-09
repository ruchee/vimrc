if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'wxml'
endif

runtime! syntax/xml.vim
unlet! b:current_syntax

let b:current_syntax = "wxml"
if main_syntax == "wxml"
  unlet main_syntax
endif
" vim:set sw=2:
