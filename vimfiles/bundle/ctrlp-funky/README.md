ctrlp-funky
============

A super simple function navigator for ctrlp.vim.

SYNOPSIS
----------

This is an extension of ctrlp.vim. It simply navigates to a function definition from the current file without ctags. It just searches for function definitions or equivalent lines using regular expressions, therefore some languages' abstractions aren't accurate because of them being hard to parse.

One of advantages of this plugin is that no configuration is required in most cases, so it starts working right after installation with no ctags required.
*If you want to have a more accurate list of function definitions, you should use other ctags-based tools, etc.*

![ctrlp-funky][1]

### Supported file types:

* c
* cf (ColdFusion)
* clojure
* cmm (TRACE32)
* coffee-script
* coldfusion
* cpp (C++)
* cs (C#)
* css (css, scss)
* dart
* elixir
* elm
* go (Golang)
* graphql
* groovy
* haskell
* html/xhtml
* java
* javascript
* Jenkinsfile (Jenkins pipeline script)
* jinja (template engine for Python)
* lua
* make (Makefile)
* markdown
* moon (MoonScript)
* nerdtree
* objc (Objective-C)
* perl
* php
* plsql (PL/SQL)
* proto (Protocol Buffers)
* python
* r
* rmd (rmarkdown)
* ruby (ruby, rake, rspec and chef recipe)
* rust
* scala
* sh (bash, dash and zsh)
* sql
* tex (LaTeX)
* tf (terraform)
* thrift
* typescript
* vb (Visual Basic)
* vim
* vue (Vue.js)
* yaml


PREMISE
----------

First of all, I believe you have already installed a great Vim plugin, [ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim).
Otherwise, you need to install ctrlp.vim before you start using this plugin.


INSTALLATION
----------

### Plugin managers
It is recommended to install the plugin using your favourite plugin manager or use Vim's built-in package manager if you use Vim 8+.

#### Vim 8+ built-in package manager

```sh
mkdir -p ~/.vim/pack/plugins/start
git clone --depth=1 https://github.com/tacahiroy/ctrlp-funky.git ~/.vim/pack/plugins/start/ctrlp-funky
```

#### vim-plug (https://github.com/junegunn/vim-plug)

```vim
Plug 'tacahiroy/ctrlp-funky'
```

#### Vundle (https://github.com/VundleVim/Vundle.vim)

```vim
Plugin 'tacahiroy/ctrlp-funky'
```

### dein.vim (https://github.com/Shougo/dein.vim)

```vim
call dein#add('tacahiroy/ctrlp-funky')
```


### Manual installation
If you do not use any plugin management system, simply copy _autoload_ and _plugin_ directories to the _.vim_ directory.
On Windows, basically, _vimfiles_ directory is used instead of the _.vim_ directory.


CONFIGURATION
--------------

It is useful to define key mappings for the funky commands as below:
```vim
nnoremap <Leader>fu :CtrlPFunky<Cr>
" narrow the list down with a word under cursor
nnoremap <Leader>uu :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
```


EXPERIMENTAL
------------

### MATCHED CHARACTER HIGHLIGHTINGS

If you want to have this highlight feature, you need to have a configuration
below in your .vimrc:
```vim
let g:ctrlp_funky_matchtype = 'path'
```
See `:h g:ctrlp_funky_matchtype` for more details and notes.

![funky-matched-chars-highlighting][3]


### SYNTAX HIGHLIGHTING

Do you want to make ctrlp-funky funkier? Okay - you can do it with just a single line of config:
```vim
let g:ctrlp_funky_syntax_highlight = 1
```
![funky-syntax][2]

Note that this feature doesn't work perfectly, because ctrlp-funky just sets
the filetype to the funky buffer.
CtrlP's indicator `>` which appears at the begining of each line in the funky
buffer has special meaning for some filetypes such as HTML, XML, etc., so it
breaks syntax highlighting.


LINK
-------

* [ctrlpvim/ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim)
* [vim.org/ctrlp-funky](http://www.vim.org/scripts/script.php?script_id=4592)


LICENSE
-------

Copyright (C) 2012-2021 Takahiro Yoshihara. Distributed under the MIT License.

[1]: http://i.imgur.com/yO4PWAF.png
[2]: http://i.imgur.com/CnKui5H.png
[3]: http://i.imgur.com/B3hBycd.png
