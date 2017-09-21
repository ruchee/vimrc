" Language: Dart
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License

function! ctrlp#funky#ft#dart#filters()
  let filters = [
        \ { 'pattern': '\v[\t ]*\w+[\t ]+(%(%(get|set)[\t ]+)|\w+[\t ]*\(.*\{)',
        \   'formatter': [] },
  \ ]
  return filters
endfunction
