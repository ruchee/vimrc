Purescript Syntax/Indentation
=============================

Syntax highlighting and indentation for [Purescript][] based on [idris-vim][] and [haskell-vim][].

I hope you find this useful.

## Installation

I recommend using [Pathogen][] for installation. Simply clone
this repo into your `~/.vim/bundle` directory and you are ready to go.

    cd ~/.vim/bundle
    git clone https://github.com/raichoo/purescript-vim.git

### Manual Installation

Copy content into your `~/.vim` directory (or `%HOME%\vimfiles` on Windows).

Be sure that the following lines are in your
`.vimrc`


    syntax on
    filetype on
    filetype plugin indent on
    
## Configuration

### Indentation

To configure indentation in `purescript-vim` you can use the following variables:

* `let g:purescript_indent_if = 3`

        if bool
        >>>then ...
        >>>else ...
  
* `let g:purescript_indent_case = 5`

        case xs of
        >>>>>[]     -> ...
        >>>>>(y:ys) -> ...
    
* `let g:purescript_indent_let = 4`

        let x = 0 in
        >>>>x
  
* `let g:purescript_indent_where = 6`

        where f :: Int -> Int
        >>>>>>f x = x
  
* `let g:purescript_indent_do = 3`
        
        do x <- a
        >>>y <- b


[Purescript]: http://www.purescript.org
[Pathogen]: https://github.com/tpope/vim-pathogen
[idris-vim]: https://github.com/idris-hackers/idris-vim
[haskell-vim]: https://github.com/raichoo/haskell-vim
