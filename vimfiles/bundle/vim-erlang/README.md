# Erlang runtime files for Vim

This repository contains the indentation, syntax and ftplugin scripts which are
shipped with Vim for the Erlang programming language. Here you can download the
newest version and contribute.

## Table of Contents

* [Installation](#installation)
* [Tips](#tips)
    * [Indentation from the command line](#indentation-from-the-command-line)
* [Development](#development)
    * [File layout](#file-layout)
    * [Erlang-related files in Vim](#erlang-related-files-in-vim)
    * [Developing and testing the indentation script](#developing-and-testing-the-indentation-script)
    * [Running Vader tests](#running-vader-tests)
* [Contributing](#contributing)

## Installation

<details>
<summary>Vim's built-in package manager</summary>

This is the recommended installation method if you use at least Vim 8 and you
don't use another package manager.

Information about Vim's built-in package manager: [`:help packages`].

Installation steps:

1.  Clone this repository (you can replace `foo` with the directory name of your
    choice):

    ```sh
    $ git clone https://github.com/vim-erlang/vim-erlang-runtime.git \
          ~/.vim/pack/foo/start/vim-erlang-runtime
    ```

2.  Restart Vim.
</details>

<details>
<summary>Pathogen</summary>

Information about Pathogen: [Pathogen repository].

Installation steps:

1.  Clone this repository:

    ```
    $ git clone https://github.com/vim-erlang/vim-erlang-runtime.git \
          ~/.vim/bundle/vim-erlang-runtime
    ```

2.  Restart Vim.
</details>

<details>
<summary>Vundle</summary>

Information about Vundle: [Vundle repository].

Installation steps:

1.  Add `vim-erlang-runtime` to your plugin list in `.vimrc` by inserting
    the line that starts with `Plugin`:

    ```
    call vundle#begin()
      [...]
      Plugin 'vim-erlang/vim-erlang-runtime'
      [...]
    call vundle#end()
    ```

2.  Restart Vim.

3.  Run `:PluginInstall`.
</details>

<details>
  <summary>Vim-Plug</summary>

Information about Vim-Plug: [vim-plug repository].

Installation steps:

1.  Add `vim-erlang-runtime` to your plugin list in `.vimrc` by inserting the
    line that starts with `Plug`:

    ```
    call plug#begin()
      [...]
      Plug 'vim-erlang/vim-erlang-runtime'
      [...]
    call plug#end()
    ```

2.  Restart Vim.

3.  Run `:PlugInstall`.
</details>

## Tips

### Indentation from the command line

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

*   This can be for example added to a Makefile as a "re-indent rule".

*   You can use the `expandtab`, `shiftwidth` and `tabstop` options to customize
    how to use space and tab characters. The command above uses only spaces, and
    one level of indentation is 4 spaces.

*   If you would like to use a different version of the indentation script from
    that one shipped in Vim, then also add the following as the first command
    parameter (replace the `/path/to` part):

    ```bash
    -c ':set runtimepath^=/path/to/vim-erlang-runtime/'
    ```

## Development

### File layout

This repository contains the following files and directories:

<!-- If you edit the list, please maintain the alphabetical order. -->

*   [`ftdetect/erlang.vim`]: Script for detecting Erlang files based on file
    extension. See [`:help ftdetect`].

    File type detection based on content (e.g., when the first line
    is `#!/usr/bin/escript`) is not in this file. See
    [`:help new-filetype-scripts`].

*   [`ftplugin/erlang.vim`] File type plugin for Erlang files. See
    [`:help ftplugin`].

    This file is also distributed with Vim as
    [`runtime/ftplugin/erlang.vim`][vim-src/runtime/ftplugin/erlang.vim].

*   [`indent/erlang.vim`]: Indentation plugin for Erlang files. See
    [`:help indent-expression`].

    This file is also distributed with Vim as
    [`runtime/indent/erlang.vim`][vim-src/runtime/indent/erlang.vim].

*   [`syntax/erlang.vim`]: Syntax highlight plugin for Erlang files. See
    [`:help syntax`].

    This file is also distributed with Vim as
    [`runtime/syntax/erlang.vim`][vim-src/runtime/syntax/erlang.vim].

*   [`test`]: Manual and automatic test that help the development and testing of
    vim-erlang-runtime.

### Erlang-related files in Vim

The Vim repository contains the following Erlang-related files:

<!-- If you edit the list, please maintain the alphabetical order. -->

*   [`runtime/compiler/erlang.vim`][vim-src/runtime/compiler/erlang.vim]:
    Allows simple Erlang files to be compiled after calling `:compiler erlang`.
    `vim-erlang-compiler` has a similar but broader scope.

*   [`runtime/doc/syntax.txt`][vim-src/runtime/doc/syntax.txt]:
    Contains documentation about configuring `runtime/syntax/erlang.vim`.

*   [`runtime/filetype.vim`][vim-src/runtime/filetype.vim]:
    Sets the file type to `erlang` if the file name matches either `*.erl`,
    `*.hrl` or `*.yaws`. The list of patterns is a subset of the patterns in
    `ftdetect/erlang.vim` in this repository.

*   [`runtime/ftplugin/erlang.vim`][vim-src/runtime/ftplugin/erlang.vim]:
    Same as [`ftplugin/erlang.vim`] in this repository.

*   [`runtime/indent/erlang.vim`][vim-src/runtime/indent/erlang.vim]:
    Same as [`indent/erlang.vim`] in this repository.

*   [`runtime/makemenu.vim`][vim-src/runtime/makemenu.vim]:
    Allows Erlang to be selected in the syntax menu. See also
    `runtime/synmenu.vim`.

*   [`runtime/scripts.vim`][vim-src/runtime/scripts.vim]:
    Sets the file type to `erlang` if the first line of the file matches
    `^#! [...]escript`. (It is not trivial what is accepted in the `[...]`
    part.)

*   [`runtime/synmenu.vim`][vim-src/runtime/synmenu.vim]:
    Allows Erlang to be selected in the syntax menu. See also
    `runtime/makemenu.vim`.

*   [`runtime/syntax/erlang.vim`][vim-src/runtime/syntax/erlang.vim]:
    Same as [`syntax/erlang.vim`] in this repository.

*   [`src/testdir/test_filetype.vim`][vim-src/src/testdir/test_filetype.vim]:
    An automatic test for setting file types.

### Developing and testing the indentation script

The indentation script can be tested in the following way:

1.  Copy `syntax/erlang.vim` into `~/syntax`.

2.  Change to the `test` directory and open `test/test_indent.erl`.

    Note: `test_indent.erl` always shows how the Erlang code is indented by the
    script – not how it should be.

3.  Source `helper.vim` (`:source helper.vim`)

4.  Press F1 to load the new indentation (`indent/erlang.vim`).

5.  Press F3 to re-indent the current line. Press shift-F3 to print a log.

6.  Press F4 to re-indent the current buffer.

7.  Press F5 to show the tokens of the current line.

Note: When the indentation scripts detects a syntax error in test mode (i.e.
when it was loaded with `F1` from `helper.vim`), it indents the line to column
40 instead of leaving it as it is. This behavior is useful for testing.

### Running Vader tests

The tests for the `include` and `define` options in `test_include_search.vader`
are run using the [Vader] Vim plugin.

A common pattern to use for test cases is to do the following:

```vim
Given:
  text to test

Do:
  daw

Expect:
  to text
```

The text that should be tested is placed in the `Given` block. A normal command
is placed in the `Do` block and the expected output in the `Expect` block. The
cursor is by default on the first column in the first line, and doing `daw`
should therefore delete around the first word.

The simplest way to run a Vader test file is to open the test file in Vim and
run `:Vader`. To run it from the command line, do `vim '+Vader!*' && echo
Success || echo Failure`. If the environment variable `VADER_OUTPUT_FILE` is
set, the results are written to this file.

To test the code with only the wanted plugins loaded and without a vimrc, you
can go to the `test` directory and execute the following command:

```bash
vim -N -u NONE \
    -c 'set runtimepath=..,$VIMRUNTIME,~/.vim/plugged/vader.vim' \
    -c 'runtime plugin/vader.vim' \
    -c 'filetype plugin indent on' \
    -c 'Vader!*' \
    && echo Success || echo Failure
```

The command does the following:

1.  Starts Vim with `nocompatible` set and without sourcing any vimrc.

2.  Puts the directory above the current one, i.e. the root directory of this
    repository, first in the `runtimepath`, such that the ftplugin, indent etc.
    from this repository are sourced first. Then the regular runtime path is
    added and finally the path to where Vader is installed is added (this will
    be different depending on which plugin manager you use, the path below is
    where vim-plug puts it).

3.  Sources the Vader plugin file so that the `Vader` command can be used.

4.  Enables using filetype specific settings and indentation.

5.  Runs all Vader test files found in the current directory and then exits Vim.

6.  Echoes `Success` if all test cases pass, else `Failure`.

For more details, see the [Vader] repository.

## Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    [`vim-erlang`] README.

<!-- If you modify the list below, please maintain the order with `:sort i`. -->

[`:help ftdetect`]: https://vimhelp.org/filetype.txt.html#ftdetect
[`:help ftplugin`]: https://vimhelp.org/usr_41.txt.html#ftplugin
[`:help indent-expression`]: https://vimhelp.org/indent.txt.html#indent-expression
[`:help new-filetype-scripts`]: https://vimhelp.org/filetype.txt.html#new-filetype-scripts
[`:help packages`]: https://vimhelp.org/repeat.txt.html#packages
[`:help syntax`]: https://vimhelp.org/syntax.txt.html#syntax
[`ftdetect/erlang.vim`]: ftdetect/erlang.vim
[`ftplugin/erlang.vim`]: ftplugin/erlang.vim
[`indent/erlang.vim`]: indent/erlang.vim
[`syntax/erlang.vim`]: syntax/erlang.vim
[`test`]: test
[`vim-erlang`]: https://github.com/vim-erlang/vim-erlang
[Pathogen repository]: https://github.com/tpope/vim-pathogen
[Vader]: https://github.com/junegunn/vader.vim
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
[vim-plug repository]: https://github.com/junegunn/vim-plug
[vim-src/runtime/compiler/erlang.vim]: https://github.com/vim/vim/blob/master/runtime/compiler/erlang.vim
[vim-src/runtime/doc/syntax.txt]: https://github.com/vim/vim/blob/master/runtime/doc/syntax.txt
[vim-src/runtime/filetype.vim]: https://github.com/vim/vim/blob/master/runtime/filetype.vim
[vim-src/runtime/ftplugin/erlang.vim]: https://github.com/vim/vim/blob/master/runtime/ftplugin/erlang.vim
[vim-src/runtime/indent/erlang.vim]: https://github.com/vim/vim/blob/master/runtime/indent/erlang.vim
[vim-src/runtime/makemenu.vim]: https://github.com/vim/vim/blob/master/runtime/makemenu.vim
[vim-src/runtime/scripts.vim]: https://github.com/vim/vim/blob/master/runtime/scripts.vim
[vim-src/runtime/synmenu.vim]: https://github.com/vim/vim/blob/master/runtime/synmenu.vim
[vim-src/runtime/syntax/erlang.vim]: https://github.com/vim/vim/blob/master/runtime/syntax/erlang.vim
[vim-src/src/testdir/test_filetype.vim]: https://github.com/vim/vim/blob/master/src/testdir/test_filetype.vim
[Vundle repository]: https://github.com/VundleVim/Vundle.vim
