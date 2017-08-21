scriptencoding utf-8

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax keyword nimTypedef         task
syntax keyword nimBuiltinFunction exec
syntax keyword nimStorage         build tests bench

so <sfile>:p:h/nim.vim

let b:current_syntax = "nims"
