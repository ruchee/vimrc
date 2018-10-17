" Vim indent file
" Language:	Jinja HTML template
" Maintainer:	Evan Hammer <evan@evanhammer.com>
" Last Change:	2013 Jan 26

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif

" Use HTML formatting rules for filetypes in `ftdetect/jinja.vim`
if expand('%:e') =~ 'htm\|nunj|jinja\|j2'
  runtime! indent/html.vim
endif
