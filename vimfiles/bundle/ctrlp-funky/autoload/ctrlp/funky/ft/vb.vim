" Language: Visual Basic (vb)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#vb#filters()
  let filters = [
        \ { 'pattern': '\v\c^[\t ]*%((Private|Friend|Public)[\t ]+)?(Function|Sub)[\t ]+',
        \   'formatter': [] },
        \ { 'pattern': '\v\c^[\t ]*%((Private|Friend|Public)[\t ]+)?(Property)[\t ]',
        \   'formatter': [] }
  \ ]
  return filters
endfunction
