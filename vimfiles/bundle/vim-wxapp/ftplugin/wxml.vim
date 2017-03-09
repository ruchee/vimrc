" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/xml.vim ftplugin/xml_*.vim ftplugin/xml/*.vim
unlet! b:did_ftplugin

syn sync fromstart
setl foldmethod=syntax
syn region XMLFold start=+^<\([^/?!><]*[^/]>\)\&.*\(<\1\|[[:alnum:]]\)$+ end=+^</.*[^-?]>$+ fold transparent keepend extend

syn match XMLCData "<!\[CDATA\[\_.\{-}\]\]>" fold transparent extend

syn match XMLCommentFold "<!--\_.\{-}-->" fold transparent extend

setl foldtext=XMLFoldLabel()
setl omnifunc=wxmlcomplete#Complete

function! XMLFoldLabel()
  let getcontent = substitute(getline(v:foldstart), "^[[:space:]]*", "", 'g')
  let linestart = substitute(v:folddashes, ".", '»', 'g')
  return linestart . " " . getcontent
endfunction

" vim:set sw=2:
