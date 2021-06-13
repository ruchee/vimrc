# vim-jsx-typescript

Syntax highlighting and indentation for JSX in Typescript (`typescriptreact` filetypes).

`vim-jsx-typescript` works with the built-in typescript syntax highlighter and indentation engine for recent versions of Vim/Neovim.

**Changelog: filetypes were updated from typescript.tsx to typescriptreact**
**Please set filetypes as typescriptreact, not typescript.tsx as in prior versions in your .vimrc if you have any issues**
```
" set filetypes as typescriptreact
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescriptreact
```

![alt tag](./screen1.jpg)
![alt tag](./screen2.jpg)

## Installation

You need to install [Vundle] or [vim-plug]: `https://github.com/junegunn/vim-plug` --- just add the following lines to your `~/.vimrc`:

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

Note you can include .jsx files as typescriptreact files for syntax highlighting.

```
" set filetypes as typescriptreact
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescriptreact
```

Set jsx-tag colors in vimrc, for example:

```
" dark red
hi tsxTagName guifg=#E06C75
hi tsxComponentName guifg=#E06C75
hi tsxCloseComponentName guifg=#E06C75

" orange
hi tsxCloseString guifg=#F99575
hi tsxCloseTag guifg=#F99575
hi tsxCloseTagName guifg=#F99575
hi tsxAttributeBraces guifg=#F99575
hi tsxEqual guifg=#F99575

" yellow
hi tsxAttrib guifg=#F8BD7F cterm=italic
```

![alt tag](./screen4.jpg)

There is support for JSX Generics (Typescript 2.9). You can set the colors by adding this to your .vimrc settings

```
" light-grey
hi tsxTypeBraces guifg=#999999
" dark-grey
hi tsxTypes guifg=#666666

```

![alt tag](./screen5.jpg)
![alt tag](./screen9.jpg)

Other keywords you can change coloring:

```
hi ReactState guifg=#C176A7
hi ReactProps guifg=#D19A66
hi ApolloGraphQL guifg=#CB886B
hi Events ctermfg=204 guifg=#56B6C2
hi ReduxKeywords ctermfg=204 guifg=#C678DD
hi ReduxHooksKeywords ctermfg=204 guifg=#C176A7
hi WebBrowser ctermfg=204 guifg=#56B6C2
hi ReactLifeCycleMethods ctermfg=204 guifg=#D19A66
```
