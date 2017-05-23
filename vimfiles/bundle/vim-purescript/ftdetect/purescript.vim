au BufNewFile,BufRead *.purs setf purescript
au FileType purescript let &l:commentstring='{--%s--}'
