" Language:     Racket 5.1
" Maintainer:   Will Langstroth <will@langstroth.com>
" URL:          http://github.com/wlangstroth/vim-racket
" Last Change:  2012-02-18

if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

setlocal ai nosi

let b:undo_indent = "setl ai< si<"
