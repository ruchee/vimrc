" Language: HTML (html)
" Author: mmazer 
" License: The MIT License

function! ctrlp#funky#ft#html#filters()
  let filters = [
        \ { 'pattern': '\v<id>\=',
        \   'formatter': ['\m\C^[\t ]*', '', ''] }
  \ ]
  return filters
endfunction
