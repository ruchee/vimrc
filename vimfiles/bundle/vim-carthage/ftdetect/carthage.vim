if has("autocmd")
  au BufRead,BufNewFile Cartfile set filetype=carthage
  au BufRead,BufNewFile Cartfile.private set filetype=carthage
  au BufRead,BufNewFile Cartfile.resolved set filetype=carthage
endif
