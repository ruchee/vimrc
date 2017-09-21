" Language: TypeScript (typescript)
" Author: prabirshrestha
" License: The MIT License

function! ctrlp#funky#ft#typescript#filters()
  let filters = [
      \ { 'pattern': '\v^[ \t]*(export)?[ \t]*((module)|(class)|(interface)|(enum)|(function))[ \t]+([a-zA-Z0-9_]+)',
      \   'formatter': []},
      \ { 'pattern': '\v^[ \t]*export[ \t]+var[ \t]+([a-zA-Z0-9_]+)',
      \   'formatter': []},
      \ { 'pattern': '\v^[ \t]*(export)?[ \t]*(public|private|protected)[ \t]+(static)?[ \t]*([a-zA-Z0-9_]+)',
      \   'formatter': []},
      \ { 'pattern': '\v^[ \t]*(constructor)[ \t]*',
      \   'formatter': []},
  \ ]
  return filters
endfunction
