vim-jsx-typescript
=======

Syntax highlighting for JSX in Typescript.

`vim-jsx-typescript` works with a typescript syntax highlighter for TSX highlighting. The recommended TypeScript syntax highlighter is `leafgarland/typescript-vim`[1].


![alt tag](./screen1.jpg)
![alt tag](./screen2c.jpg)


## Installation

You need to install [Vundle] or [vim-plug]: `https://github.com/junegunn/vim-plug` --- just add the following lines to
your `~/.vimrc`:

### Vundle:

```
Plugin 'leafgarland/typescript-vim'
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

" OR for vim-plug:
:so ~/.vimrc
:PlugInstall

```

Note you can include .jsx files as typescript.tsx files for syntax highlighting.
```
" set filetypes as typescript.tsx
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.tsx
```


Set jsx-tag colors in vimrc, for example:
```
" dark red
hi tsxTagName guifg=#E06C75

" orange
hi tsxCloseString guifg=#F99575
hi tsxCloseTag guifg=#F99575
hi tsxAttributeBraces guifg=#F99575
hi tsxEqual guifg=#F99575

" yellow
hi tsxAttrib guifg=#F8BD7F cterm=italic
```


![alt tag](./screen4.jpg)

Or use the blue-green [Original] colorscheme
```
" light blue
hi tsxTagName guifg=#59ACE5
" dark blue
hi tsxCloseString guifg=#2974a1
hi tsxCloseTag guifg=#2974a1
hi tsxAttributeBraces guifg=#2974a1
hi tsxEqual guifg=#2974a1
" green
hi tsxAttrib guifg=#1BD1C1

```


![alt tag](./screen3.jpg)


Other keywords you can change coloring:
```
hi ReactState guifg=#C176A7
hi ReactProps guifg=#D19A66
hi Events ctermfg=204 guifg=#56B6C2
hi ReduxKeywords ctermfg=204 guifg=#C678DD
hi WebBrowser ctermfg=204 guifg=#56B6C2
hi ReactLifeCycleMethods ctermfg=204 guifg=#D19A66
```

