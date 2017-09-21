" Language: Debian Almquist shell (dash)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#dash#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*[_a-zA-Z][_a-zA-Z0-9]\+[\t ]*\(([\t ]*)\)\?[\t ]*\n*{',
        \   'formatter': [] }
  \ ]
  return filters
endfunction

function! ctrlp#funky#ft#dash#is_kinda_sh()
  return 1
endfunction
