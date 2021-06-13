if !get(g:, 'jsx_improve_javascriptreact', 1)
  finish
endif

let s:path = expand('<sfile>:p:h')
exec 'so ' . s:path . '/javascript.vim'
