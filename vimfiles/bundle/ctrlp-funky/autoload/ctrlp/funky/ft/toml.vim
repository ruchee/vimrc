" Language: TOML
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#toml#filters()
  let filters = [
        \ { 'pattern': '\v\C^\[\S+\]$',
        \   'formatter': [] }
  \ ]
  return filters
endfunction
