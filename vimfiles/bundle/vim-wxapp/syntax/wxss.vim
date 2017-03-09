if exists("b:current_syntax")
  finish
endif

runtime! syntax/css.vim

syn keyword cssTagName scroll-view page view text swiper icon text progress button checkbox form input label picker radio slider switch action-sheet modal toast loading audio image video map canvas

unlet! b:current_syntax

let b:current_syntax = "wxss"

" vim:set sw=2:
