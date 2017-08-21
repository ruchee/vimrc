# nvim-nim [![Build Status](https://travis-ci.org/baabelfish/nvim-nim.svg?branch=master)](https://travis-ci.org/baabelfish/nvim-nim)
Nim support for vim and advanced support for neovim. Still in heavy development.


## Features

- Asynchronous
- Up to date syntax highlighting
    - Normal vim highlight
    - More intelligent highlight with nimsuggest (**experimental**)
    - Semantic highlighting for specified nim symbol kinds (**experimental**)
- Indentation
- Error checking
    - Using ``:make``
    - [Neomake](https://github.com/benekastah/neomake)
- Project navigation with nimsuggest
    - Jump to definition
    - Get symbol information (type, module, file, signature, etc...)
    - Find usages (file and/or project)
- Autocompletion
    - Nimsuggest omnicompletion (still *sync*, deoplete/ycm incoming...)
    - Autocomplete module names (**experimental**)
- IDE like stuff
    - Jump to documentation in web
    - Refactoring
        - Rename symbol in file or project
    - Outline listing all symbols in the module (like tagbar)
        - Jump to outline symbol with unite (**experimental**)
- REPL (**experimental**)
    - Open repl
    - Send current buffer
    - Send selection

## Installation

This plugin doesn't work if [zah's nim plugin](https://github.com/zah/nim.vim) is installed.

### Dependencies
- Linux (OS X and Windows untested at the moment)
- [Nim](http://nim-lang.org/) (0.13.0)
- [Nimsuggest](https://github.com/nim-lang/nimsuggest) (0.13.0)
- [Neovim](https://neovim.io/) (1.2) or [Vim](http://www.vim.org/) (7.4)
    - NOTE: Vim is not currently supporting async features

- Optional: [vim-operator-user](https://github.com/kana/vim-operator-user) for defining routine text object
- Optional: [unite-outline](https://github.com/h1mesuke/unite-outline) for jumping to symbols inside module
- Optional: [YouCompleteMe](https://github.com/Valloric/YouCompleteMe) for jumping to symbols inside module
- Optional: [Neomake](https://github.com/benekastah/neomake) for compilation errors

### Configuration

Easiest way to install this plugin is with a plugin manager:

[vim-plug](https://github.com/junegunn/vim-plug)
```viml
Plug "baabelfish/nvim-nim"
```

[neobundle](https://github.com/Shougo/neobundle.vim)
```viml
NeoBundle "baabelfish/nvim-nim"
```

[pathogen](https://github.com/tpope/vim-pathogen)
```sh
git clone https://github.com/baabelfish/nvim-nim ~/.vim/bundle/nvim-nim
```

## Screenshots

#### Syntax highlighting
![Syntax highlighting](https://raw.githubusercontent.com/baabelfish/nvim-nim/master/misc/screenshots/syntaxhl.png)

#### Autocompletion
![Autocomplete](https://raw.githubusercontent.com/baabelfish/nvim-nim/master/misc/screenshots/autocomplete.png)

#### Symbol information and module outline
![Definition](https://raw.githubusercontent.com/baabelfish/nvim-nim/master/misc/screenshots/definition.png)

#### Symbol usage listing
![Usages](https://raw.githubusercontent.com/baabelfish/nvim-nim/master/misc/screenshots/usages.png)

## Caveats (Will be fixed asap)
- No support for main/project file yet
- Launching new nimsuggest for every completion
- Using stdio to communicate with nimsuggest
- Docstring is still missing
- Completion is a bit wonky with plain vim. Works well with [YouCompleteMe](https://github.com/Valloric/YouCompleteMe)

## Planned features

- Debugger support
- Async autocomplete with deoplete (maybe a seperate plugin)
