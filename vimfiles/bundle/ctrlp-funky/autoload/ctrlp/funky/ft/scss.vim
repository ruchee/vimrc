" Language: SASS/SCSS
" Author: Ansel Santosa <anstosa@gmail.com>
" License: The MIT License

function! ctrlp#funky#ft#scss#filters()
  let filters = [
        \ { 'pattern': '\v^\s*[^{]+\s*\{',
        \   'formatter': ['\v^\s*|\s*\{.*', '', 'g'] }
  \ ]
  return filters
endfunction
