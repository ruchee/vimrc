" Language: SQL
" Author: somini
" License: The MIT License

let s:pattern = '\V\c\<function\>\s\+\(\k\|.\)\+\((\_.\{-})\)\?'
let s:function_name = '\V\cfunction\s\+\(\(\k\|\.\)\+\)'

function! ctrlp#funky#ft#sql#filters()
  " By default, remove whitespace and arguments
  return [{
        \ 'pattern': s:pattern,
        \ 'formatter': ['\V\c\^\s\+\|\%((\.\*\$\)', '', 'g']
        \ }]
endfunction

function! ctrlp#funky#ft#sql#strippers()
  return [{
        \ 'pattern': s:function_name,
        \ 'position': 1,
        \ }]
endfunction
