# vim-cucumber

This is the development version of Vim's included runtime files for the Ruby
acceptance testing framework [Cucumber](http://cukes.info/). It provides
syntax highlighting, indenting, and some editing commands.

## Commands

vim-cucumber provides commands to jump from steps to step definitions in
feature files.

In normal mode, pressing `[<C-d>` or `]<C-d>` on a step jumps to the
corresponding step definition and replaces the current buffer. `<C-W>d` or
`<C-w><C-d>` on a step jumps to its definition in a new split buffer and moves
the cursor there. `[d` or `]d` on a step opens the step definition in a new
split buffer while maintaining the current cursor position.

## Installation

If you don't have a preferred installation method, I recommend installing
[pathogen.vim](https://github.com/tpope/vim-pathogen), and then simply copy
and paste:

    cd ~/.vim/bundle
    git clone git://github.com/tpope/vim-cucumber.git
