" Language: Ansible (yaml)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#ansible#filters()
  return [
    \ { 'pattern': '\m\C^[\t ]*- \S\+:',
    \   'formatter': [] }
    \ ]
endfunction
