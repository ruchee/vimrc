# bundler.vim

This is a lightweight bag of Vim goodies for
[Bundler](http://gembundler.com), best accompanied by
[rake.vim](https://github.com/tpope/vim-rake) and/or
[rails.vim](https://github.com/tpope/vim-rails).  Features:

* `:Bundle`, which wraps `bundle`.
* An internalized version of `bundle open`: `:Bopen` (and `:Bsplit`,
  `:Btabedit`, etc.).
* `'path'` and `'tags'` are automatically altered to include all gems
  from your bundle.  (Generate those tags with
  [gem-ctags](https://github.com/tpope/gem-ctags)!)
* Highlight Bundler keywords in `Gemfile`.
* Support for `gf` in `Gemfile.lock`, plus syntax highlighting that
  distinguishes between installed and missing gems.
* Support for [projectionist.vim](https://github.com/tpope/vim-projectionist),
  including projections based on which gems are bundled.

## Installation

Install using your favorite package manager, or use Vim's built-in package
support:

    mkdir -p ~/.vim/pack/tpope/start
    cd ~/.vim/pack/tpope/start
    git clone https://tpope.io/vim/bundler.git
    vim -u NONE -c "helptags bundler/doc" -c q

## Self-Promotion

Like bundler.vim? Follow the repository on
[GitHub](https://github.com/tpope/vim-bundler) and vote for it on
[vim.org](http://www.vim.org/scripts/script.php?script_id=4280).  And if
you're feeling especially charitable, follow [tpope](http://tpo.pe/) on
[Twitter](http://twitter.com/tpope) and
[GitHub](https://github.com/tpope).

## License

Copyright (c) Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.
