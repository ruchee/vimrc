smarty3.vim
===========

## ~/.vimrc

```
NeoBundle 'iakio/smarty3.vim'

" Jump to {if}/{else}/{/if}, {foreach},{foreachelse},{/foreach} pair.
runtime macros/matchit.vim

" change delimiter
let g:smarty_left_delimiter = '<{'
let g:smarty_right_delimiter = '}>'

```

## ~/.vim/ftdetect/smarty3.vim

```
au BufRead,BufNewFile *.html set filetype=smarty3
```
