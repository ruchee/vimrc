" Language: Bourne-Again SHell script (bash)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#bash#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*\(function[\t ]\+\)\?[_a-zA-Z][_a-zA-Z0-9-]\+[\t ]*\(([\t ]*)\)\?[\t ]*\n*{',
        \   'formatter': [] }
  \ ]
  return filters
endfunction

function! ctrlp#funky#ft#bash#is_kinda_sh()
  return 1
endfunction
