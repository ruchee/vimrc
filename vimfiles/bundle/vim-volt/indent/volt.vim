" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif

" Use PHP formatting rules.
runtime! indent/php.vim
