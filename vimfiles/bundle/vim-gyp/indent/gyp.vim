" Vim indent file
" Language:     Gyp
" Maintainer:   Kelan Champagne  (http://yeahrightkeller.com)
" URL:          https://github.com/kelan/gyp.vim

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

" C indenting is not too bad.
setlocal cindent
setlocal cinoptions+=j1,J1

let b:undo_indent = "setl cin<"

