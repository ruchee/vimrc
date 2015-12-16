# Erlang indentation and syntax for Vim

This is the Erlang indentation and syntax that is shipped with Vim (from Vim 7.4).

## Installation

### Method 1

- Clone this repository:

        $ mkdir -p ~/.vim/bundle
        $ cd ~/.vim/bundle
        $ git clone https://github.com/vim-erlang/vim-erlang-runtime

- Add the repository path to `runtimepath` in your `.vimrc`:

        :set runtimepath^=~/.vim/bundle/vim-erlang-runtime/

### Method 2

- Copy `syntax/erlang.vim` into `~/.vim/syntax/`.
- Copy `indent/erlang.vim` into `~/.vim/indent/`.

### Installing using vim-plug

1. Install vim-plug using the [instructions][vim-plug]
2. Add vim-erlang-runtime to your plugin list in `.vimrc` and re-source it:

    insert vim-erlang-runtime
    ```
    '' Erlang Runtime
    Plug 'vim-erlang/vim-erlang-runtime'
    ```
    between
    `call plug#begin('~/.vim/plugged')`

    and
    `call plug#end()`
3. Run `:PlugInstall`

[vim-plug]:https://github.com/junegunn/vim-plug

## Development and testing

This section is relevant only if you want to be involved in the development of
the script.

The indentation script can be tested in the following way:

- Copy `syntax/erlang.vim` into `~/syntax`.
- Open `test_indent.erl` in Vim from this directory. (`test_indent.erl` always
  shows how the Erlang code is indented by the script – not how it should be.)
- Source `helper.vim` (`:source helper.vim`)
- Press F1 to load the new indentation (`indent/erlang.vim`).
- Press F3 to reindent the current line. Press shift-F3 to print a log.
- Press F4 to reindent the current buffer.
- Press F5 to show the tokens of the current line.

*Note:*

- When the indentation scripts detects a syntax error in test mode (i.e. when it
  was loaded with `F1` from `helper.vim`), it indents the line to column 40
  instead of leaving it as it is. This behavior is useful for testing.

## Tip: indentation from the command line

The following snippet re-indents all `src/*.?rl` files using the indentation
shipped with Vim:

```bash
vim -ENn -u NONE \
    -c 'filetype plugin indent on' \
    -c 'set expandtab shiftwidth=4' \
    -c 'args src/*.?rl' \
    -c 'argdo silent execute "normal gg=G" | update' \
    -c q
```

Notes:

- This can be for example added to a Makefile as a "re-indent rule".
- You can use the `expandtab`, `shiftwidth` and `tabstop` options to customize
  how to use space and tab characters. The command above uses only spaces, and
  one level of indentation is 4 spaces.
- If you would like to use a different version of the indentation script from
  that one shipped in Vim (e.g. because you have Vim 7.3), then also add the
  following as the first command parameter:

  ```bash
  -c ':set runtimepath^=~/.vim/bundle/vim-erlang-runtime/'
  ```
