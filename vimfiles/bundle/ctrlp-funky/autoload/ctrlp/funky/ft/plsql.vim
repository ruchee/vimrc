" Language: PL/SQL
" Author: somini
" License: The MIT License
" Description: This is just a copy of sql.vim

let s:pattern = '\V\c\<function\>\s\+\(\k\|\.\)\+\((\_.\{-})\)\?'
let s:function_name = '\V\cfunction\s\+\(\(\k\|\.\)\+\)'

function! ctrlp#funky#ft#plsql#filters()
  " By default, remove whitespace and arguments
  return [{
        \ 'pattern': s:pattern,
        \ 'formatter': ['\V\c\^\s\+\|\%((\.\*\$\)', '', 'g']
        \ }]
endfunction

function! ctrlp#funky#ft#plsql#strippers()
  return [{
        \ 'pattern': s:function_name,
        \ 'position': 1,
        \ }]
endfunction
