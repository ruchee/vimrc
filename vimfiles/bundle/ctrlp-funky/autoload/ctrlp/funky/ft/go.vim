" Language: Go
" Author: Takahiro Yoshihara
" Contributors: Sina Siadat
" License: The MIT License

function! ctrlp#funky#ft#go#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*func[\t ]\+',
        \   'formatter': [] },
  \ ]

  if get(g:, 'ctrlp_funky_go_types', 1)
    call extend(filters, [
          \ { 'pattern': '\m\C^[\t ]*type[\t ]\+',
          \   'formatter': [] }]
    \ )
  endif

  return filters
endfunction
