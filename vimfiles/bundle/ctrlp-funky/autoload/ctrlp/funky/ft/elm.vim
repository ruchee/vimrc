" Language: Elm
" Author: lorenzo
" License: The MIT License

let s:pat = {}

function! ctrlp#funky#ft#elm#filters()
  let s:pat.func = '\v^([a-zA-Z0-9\(].*)\s*:'

  let filters = [
        \ { 'pattern': s:pat.func,
        \   'formatter': ['\m\C^[\t ]*', '', ''] }
  \ ]

  return filters
endfunction

function! ctrlp#funky#ft#elm#strippers()
  return [ { 'pattern': s:pat.func, 'position': 1 } ]
endfunction
