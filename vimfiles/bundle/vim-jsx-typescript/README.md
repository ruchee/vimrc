vim-jsx-typescript
=======

Syntax highlighting for JSX in Typescript.

vim-jsx-typescript is _not_ a JavaScript syntax package, so in order to use it, you will
also need to choose a base JS highlighter. [leafgarland/typescript-vim][1] is the
recommended package.


![alt tag](./screenshot.jpg)
![alt tag](./screenshot2.png)


## Installation

You can also add vim-jsx using [Vundle] or junegunn/vim-plug---just add the following lines to
your `~/.vimrc`:

### Vundle:

```
Plugin 'pangloss/vim-javascript'
Plugin 'peitalin/vim-jsx-typescript'
```

### Vim-plug:

```
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
```

To install from within vim, use the commands below.
```
    :so ~/.vimrc
    :PluginInstall

OR for vim-plug:
    :PlugInstall

```

You will also need to set .tsx files as filetype=typescript.jsx, since leafgarland/typescript-vim
sets .tsx files as "typescript".

```
" set filetypes as typescript.jsx
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.jsx
```

Note you can include .jsx and .js files as typescript.jsx files for syntax highlighting.


Set jsx-tag colors in vimrc:
```
" light blues
hi xmlTagName guifg=#59ACE5
hi xmlTag guifg=#59ACE5

" dark blues
hi xmlEndTag guifg=#2974a1
```



