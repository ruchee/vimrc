" Language: LaTeX (tex)
" Author: Julian Wulfheide
" License: The MIT License
      " \ { 'pattern': '\\\\(\(sub\)\{0,2}section\|chapter\|paragraph\){.*}',

function! ctrlp#funky#ft#tex#filters()
  let filters = [
        \ { 'pattern': '\\\(\(sub\)\{0,2}section\|chapter\|\(sub\)\=paragraph\|part\)\(*\)\={.*}',
        \   'formatter': [] }
  \ ]
  return filters
endfunction
