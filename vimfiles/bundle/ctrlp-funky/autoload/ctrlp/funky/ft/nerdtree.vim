" Language: The NERD tree (nerdtree)
" Author: Takahiro Yoshihara
" License: The MIT License

let s:is_files = get(g:, 'ctrlp_funky_nerdtree_include_files', 0)

function! ctrlp#funky#ft#nerdtree#filters()
  let filters = [
        \ { 'pattern': '\m\C^\s*[▸▾|~+].*\/$',
        \   'formatter': [] }
  \ ]

  " useful?
  if s:is_files
    call add(filters,
          \ { 'pattern': '\m\C^\s\+.\+$',
          \   'formatter': [] }
    \ )
  endif

  return filters
endfunction
