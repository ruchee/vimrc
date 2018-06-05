" Language: Go
" Author: Takahiro Yoshihara
" Contributors: Sina Siadat
" License: The MIT License

function! ctrlp#funky#ft#go#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*func[\t ]\+',
        \   'formatter': [] },
        \ { 'pattern': '\m\C^\s*\w\+\s\+\(struct\|interface\)[\t {]\+',
        \   'formatter': [] },
  \ ]

  if get(g:, 'ctrlp_funky_go_types', 1)
    call extend(filters, [
          \ { 'pattern': '\m\C^[\t ]*type[\t ]\+\w',
          \   'formatter': [] }]
    \ )
  endif

  return filters
endfunction
