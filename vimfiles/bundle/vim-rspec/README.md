# rspec.vim

This provides improved syntax highlighting for rspec. This is pulled
from [Specky](http://www.vim.org/scripts/script.php?script_id=2286)
which is a great plugin which just does a little too much for me.

If you don't have a preferred installation method check out
[vim-plug](https://github.com/junegunn/vim-plug)

## Usage

By default this syntax is used with all `*_spec.rb` files. If you don't
follow this pattern use something like:

```vim
autocmd BufNewFile,BufRead *_foo.rb set syntax=rspec
```

In your `~/.vimrc`
