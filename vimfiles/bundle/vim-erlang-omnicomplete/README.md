# vim-erlang-omnicomplete

`vim-erlang-omnicomplete` is an Erlang **autocompletion plugin** for Vim.

## Table of Contents

* [Installation](#installation)
* [Quick start](#quick-start)
* [Documentation](#documentation)
* [Development](#development)
    * [File layout](#file-layout)
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
    $ git clone https://github.com/vim-erlang/vim-erlang-omnicomplete.git \
          ~/.vim/pack/foo/start/vim-erlang-omnicomplete
    ```

2.  Restart Vim.

3.  Generate help page (replace `foo` with the same directory name as above):

    ```
    :helptags ~/.vim/pack/foo/start/vim-erlang-omnicomplete/doc
    ```

</details>

<details>
<summary>Pathogen</summary>

Information about Pathogen: [Pathogen repository].

Installation steps:

1.  Clone this repository:

    ```
    $ git clone https://github.com/vim-erlang/vim-erlang-omnicomplete.git \
          ~/.vim/bundle/vim-erlang-omnicomplete
    ```

2.  Restart Vim.

3.  Generate help page:

    ```
    :Helptags
    ```
</details>

<details>
<summary>Vundle</summary>

Information about Vundle: [Vundle repository].

Installation steps:

1.  Add `vim-erlang-omnicomplete` to your plugin list in `.vimrc` by inserting
    the line that starts with `Plugin`:

    ```
    call vundle#begin()
      [...]
      Plugin 'vim-erlang/vim-erlang-omnicomplete'
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

1.  Add `vim-erlang-omnicomplete` to your plugin list in `.vimrc` by inserting the
    line that starts with `Plug`:

    ```
    call plug#begin()
      [...]
      Plug 'vim-erlang/vim-erlang-omnicomplete'
      [...]
    call plug#end()
    ```

2.  Restart Vim.

3.  Run `:PlugInstall`.

</details>

## Quick start

1.  Open an Erlang source file.

2.  Start typing something (e.g. `li` or `io:fo`).

3.  Without leaving insert mode, hit CTRL-X and then CTRL-O.

4.  After at most a few seconds, you should see a list of completions.

## Documentation

*   Vim's omni completion (i.e., autocomplete) functionality:
    [`:help compl-omni`].

*   Vim's `completeopt` option to configure omni completion
    [`:help completeopt`].

*   `vim-erlang-omnicomplete` plugin: [`:help vim-erlang-omnicomplete`].

## Development

### File layout

This repository contains the following files and directories:

<!-- If you edit the list, please keep the alphabetical order. -->

*   [`autoload/erlang_complete.erl`]: Erlang script which can analyse the code
    base and calculate the list of modules and the list of functions in
    a module.

*   [`autoload/erlang_complete.vim`]: This script contains most of
    `vim-erlang-omnicomplete`'s functionality on the Vim side.

    This functionality is here (instead of [`plugin/erlang_omnicomplete.vim`])
    so that Vim's autoload functionality ([`:help autoload`]) can make sure that
    this script is executed only when needed the first time.

*   [`doc/vim-erlang-omnicomplete.txt`]: This file contains the user
    documentation of `vim-erlang-omnicomplete`.

*   [`ftplugin/erlang.vim`]: This script sets up `vim-erlang-omnicomplete` when
    an Erlang source file is opened.

*   [`plugin/erlang_omnicomplete.vim`]: This script sets up
    `vim-erlang-omnicomplete` when Vim is started. It is kept small so that the
    effect on Vim's startup time is minimal.

## Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    [`vim-erlang`] README.

*   If you modify [`autoload/erlang_complete.erl`], please:

    -   update the tests in the [`vim-erlang`] repository

    -   also modify `vim-erlang-compiler` (if you modify the
        ["load build information" code block][common-code-block])

<!-- If you modify the list below, please keep the order with `:sort i`. -->

[`:help autoload`]: https://vimhelp.org/eval.txt.html#autoload
[`:help compl-omni`]: https://vimhelp.org/insert.txt.html#compl-omni
[`:help completeopt`]: https://vimhelp.org/options.txt.html#%27completeopt%27
[`:help packages`]: https://vimhelp.org/repeat.txt.html#packages
[`:help vim-erlang-omnicomplete`]: doc/vim-erlang-omnicomplete.txt
[`autoload/erlang_complete.erl`]: autoload/erlang_complete.erl
[`autoload/erlang_complete.vim`]: autoload/erlang_complete.vim
[`doc/vim-erlang-omnicomplete.txt`]: doc/vim-erlang-omnicomplete.txt
[`ftplugin/erlang.vim`]: ftplugin/erlang.vim
[`plugin/erlang_omnicomplete.vim`]: plugin/erlang_omnicomplete.vim
[`vim-erlang`]: https://github.com/vim-erlang/vim-erlang
[common-code-block]: https://github.com/vim-erlang/vim-erlang-omnicomplete/blob/448a9feae5a284bf36748e611111d183cfa52ab5/autoload/erlang_complete.erl#L160-L700
[Pathogen repository]: https://github.com/tpope/vim-pathogen
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
[vim-plug repository]: https://github.com/junegunn/vim-plug
[Vundle repository]: https://github.com/VundleVim/Vundle.vim
