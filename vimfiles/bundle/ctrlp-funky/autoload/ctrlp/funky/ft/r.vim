" Language: R
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#r#filters()
  let filters = [
        \ { 'pattern': '\v\C^\S+\s*\<-\s*',
        \   'formatter': ['<-.*$', '', ''] }
  \ ]
  return filters
endfunction
