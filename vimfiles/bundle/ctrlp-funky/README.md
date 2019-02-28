ctrlp-funky
============
A super simple function navigator for ctrlp.vim.  

SYNOPSIS
----------
This is a ctrlp.vim extension. It simply navigates and jumps to function definitions from the current file without ctags. It just searches for function definitions or equivalent lines using regular expressions, therefore some languages' abstractions aren't accurate because of them being hard to parse.

One of advantages of this plugin is that no configuration is required in most cases, so it starts working right after installation with no ctags required.
*If you want to have a more accurate list of function defs, you should use other ctags-based tools, etc.*

![ctrlp-funky][1]

### Supported filetypes:
See [ctrlp-funky.txt](https://github.com/tacahiroy/ctrlp-funky/blob/master/doc/ctrlp-funky.txt#L22)


PREMISE
----------
First of all, I believe you're a user of a great Vim plugin called [ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim).
Otherwise, you need to install ctrlp.vim before you start using this plugin.


INSTALLATION
----------

### Plugin managers
It is recommended to install the plugin using plugin managers such as minpac, vim-plug, pathogen, Vundle, Dein.vim etc.
You can copy/paste a line below if you use vim-plug:
`Plug 'tacahiroy/ctrlp-funky'`

### Manual installation
If you use neither of the plugin management systems, copy _autoload_ and _plugin_ directories to _.vim_ directory.
On Windows, basically, _vimfiles_ directory is used instead of _.vim_ directory.


CONFIGURATION
--------------
It should be useful to define key mappings like this:
```vim
nnoremap <Leader>fu :CtrlPFunky<Cr>
" narrow the list down with a word under cursor
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
```


EXPERIMENTAL
------------
### MATCHED CHARS HIGHLIGHTING
If you want to have this highlight feature, you may configure like this:
```vim
let g:ctrlp_funky_matchtype = 'path'
```
See `:h g:ctrlp_funky_matchtype` for more details and notes.

![funky-matched-chars-highlighting][3]


### SYNTAX HIGHLIGHTING
I'd like to introduce one of ctrlp-funky options.
Do you want to make ctrlp-funky funkier? Okay - you can do it with just 1 line of config:
```vim
let g:ctrlp_funky_syntax_highlight = 1
```
![funky-syntax][2]

Note that this feature doesn't work perfectly, because ctrlp-funky just sets
filetype to the funky buffer and the buffer contains '>' in the first column.
In some filetypes, this sign has special meaning such as HTML tag, so it breaks
syntax highlighting.


LINK
-------

* [ctrlpvim/ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim)
* [vim.org/ctrlp-funky](http://www.vim.org/scripts/script.php?script_id=4592)


LICENSE
-------

Copyright (C) 2012-2017 Takahiro Yoshihara. Distributed under the MIT License.

[1]: http://i.imgur.com/yO4PWAF.png
[2]: http://i.imgur.com/CnKui5H.png
[3]: http://i.imgur.com/B3hBycd.png
