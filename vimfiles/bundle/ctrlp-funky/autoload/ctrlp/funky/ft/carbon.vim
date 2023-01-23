" Language: Carbon
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License

function! ctrlp#funky#ft#carbon#filters()
  let filters = [
        \ { 'pattern': '\v^[\t ]*(fn|class)[\t ]+',
        \   'formatter': [] 
        \ }
  \ ]
  return filters
endfunction
