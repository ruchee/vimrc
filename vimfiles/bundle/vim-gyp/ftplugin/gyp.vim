" Vim filetype plugin file
" Language:     Gyp
" Maintainer:   Kelan Champagne  (http://yeahrightkeller.com)
" URL:          https://github.com/kelan/gyp.vim

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

set ts=2
set sw=2
set tw=0
set wm=0

" Set 'formatoptions' to break comment lines but not other lines,
" and insert the comment leader when hitting <CR> or using "o".
setlocal formatoptions-=t formatoptions+=croql

" Set 'comments' to format dashed lists in comments.
setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://

setlocal commentstring=#%s

let b:undo_ftplugin = "setl fo< ofu< com< cms<" 

