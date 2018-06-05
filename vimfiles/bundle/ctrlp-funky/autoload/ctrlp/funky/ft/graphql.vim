" Language: GraphQL
" Author: timfeirg
" License: The MIT License

function! ctrlp#funky#ft#graphql#filters()
  let filters = [
        \ { 'pattern': '\m\C^\s*\(type\|schema\|enum\|interface\|input\)\s\+',
        \   'formatter': [] }
  \ ]
  return filters
endfunction
