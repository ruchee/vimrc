# rvm.vim

Want to use [RVM](http://rvm.io) with Vim?  You don't
need a plugin to do that:  Just start Vim from your RVM enabled shell
and it will work.  But say you started MacVim from Launchpad, or you
started Vim with one version of Ruby and now you want another.  That's
where rvm.vim comes in.

    :Rvm 1.9.2

If you want to see the version that was chosen, use `use`:

    :Rvm use default

If you leave off the version, it goes `.rvmrc` hunting relative to the
current buffer.

    :Rvm
    :Rvm use

If you really want to get crazy, you can make this happen automatically
as you switch from buffer to buffer.

    :autocmd BufEnter * Rvm

You can also invoke any old `rvm` command.

    :Rvm install 1.9.3

Add `%{rvm#statusline()}` to `'statusline'` (or `'titlestring'`) to see
the current Ruby version at all times.

Last but not least, rvm.vim tells recent versions of [vim-ruby][] where your
Ruby installs are located, so that it can set `'path'` in your Ruby buffers to
reflect the nearest `.ruby-version` file.

[vim-ruby]: https://github.com/vim-ruby/vim-ruby

## FAQ

> RVM doesn't work in my Vim.

You're using zsh on OS X Yosemite or earlier, aren't you?  Move that stupid
`/etc/zshenv` to `/etc/zprofile`, which is where it correctly lives on El
Capitan.

## Self-Promotion

Like rvm.vim? Follow the repository on
[GitHub](https://github.com/tpope/vim-rvm) and vote for it on
[vim.org](http://www.vim.org/scripts/script.php?script_id=4269).  And if
you're feeling especially charitable, follow [tpope](http://tpo.pe/) on
[Twitter](http://twitter.com/tpope) and
[GitHub](https://github.com/tpope).

## License

Copyright (c) Tim Pope.  Distributed under the same terms as Vim itself.
See `:help license`.
