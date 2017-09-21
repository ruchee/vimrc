" Language: Moonscript
" Author: ggVGc
" License: The MIT License

function! ctrlp#funky#ft#moon#filters()
  let filters = [
  \ { 'pattern': '^[ \t]*[A-Za-z][A-Za-z0-9_.]*[ \t]*=.*[-=]>','formatter': [] },
  \ { 'pattern': '^[ \t]*[A-Za-z][A-Za-z0-9_]*[ \t]*:[ \t]\w*[ \t]*[-=]>','formatter': [] }
  \ ]
  return filters
endfunction
