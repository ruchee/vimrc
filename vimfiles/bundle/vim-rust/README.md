# rust.vim

## Description

This is a vim plugin provides [Rust][r] file detection and syntax highlighting.

It is synchronized daily to the vim support code in [mozilla/rust][mr]'s
master branch via cronjob.

## Installation

### Using [Vundle][v]

1. Add `Bundle 'wting/rust.vim'` to `~/.vimrc`
2. `vim +BundleInstall +qall`

*Note:* Vundle will not automatically detect Rust files properly if `filetype
on` is executed before Vundle. Please check the [quickstart][vqs] for more
details.

### Using [Pathogen][p]

1. `cd ~/.vim/bundle`
2. `git clone git://github.com/wting/rust.vim.git`

[mr]: https://github.com/mozilla/rust
[p]: https://github.com/tpope/vim-pathogen
[r]: https://en.wikipedia.org/wiki/Rust_language
[v]: https://github.com/gmarik/vundle
[vqs]: https://github.com/gmarik/vundle#quick-start
