" Language: ColdFusion
" Author: Arash Mousavi
" License: The MIT License

function! ctrlp#funky#ft#cf#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*<cffunction',
        \   'formatter': ['\m\C^[\t ]*', '', ''] }
  \ ]
  return filters
endfunction
