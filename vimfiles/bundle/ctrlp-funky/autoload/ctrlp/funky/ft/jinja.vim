" Language: Jinja
" Author: Vital Kudzelka
" License: MIT

function! ctrlp#funky#ft#jinja#filters()
  let filters = [
        \ { 'pattern': '\m\C{%-\?\s*block\s\+\w\+\s*-\?%}',
        \   'formatter': ['\m\C^\s*', '', ''] },
        \
        \ { 'pattern': '\m\C{%-\?\s*macro\s\+\w\+\s*(.*)\s*-\?%}',
        \   'formatter': ['\m\C^\s*', '', ''] }
        \ ]

  call extend(filters, ctrlp#funky#ft#html#filters())

  return filters
endfunction
