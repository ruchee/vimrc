scriptencoding utf-8


if has('nvim')
  runtime! plugin/python_setup.vim
endif

call plug#begin('$HOME/.config/nvim/plugged')
Plug 'baabelfish/nvim-nim'
call plug#end()

filetype plugin indent on

if has("multi_byte") && has("starting")
  if &termencoding == ""
    let &termencoding = &encoding
  endif
  setglobal fileencoding=utf-8
  scriptencoding utf-8
endif
