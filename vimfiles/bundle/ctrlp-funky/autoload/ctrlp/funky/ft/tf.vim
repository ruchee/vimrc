" Language: tf (Terraform)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#tf#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*\(variable\|resource\|provider\|data\|output\) ',
        \   'formatter': ['\m\C^[\t ]\+', '', ''] },
  \ ]
  return filters
endfunction
