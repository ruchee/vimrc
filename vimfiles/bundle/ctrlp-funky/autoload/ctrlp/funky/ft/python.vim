" Language: Python (python)
" Author: pydave
" Contributor: dusans
" License: The MIT License

function! ctrlp#funky#ft#python#filters()
  let filters = [
        \ { 'pattern': '\v\C^\s*(class\s+\w+\s*(\([^\)]+\))?|def\s+\w+\s*(\_.{-})):',
        \   'formatter': ['\v\C^\s*', '', ''] }
  \ ]
  return filters
endfunction
