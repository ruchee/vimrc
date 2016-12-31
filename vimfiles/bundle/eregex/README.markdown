# eregex.vim

## Installation

It is recommended to install the script using [Vundle][] or [pathogen][].

[Vundle]:https://github.com/gmarik/vundle
[pathogen]:https://github.com/tpope/vim-pathogen

## Quick Start

After installation, just press <kbd>/</kbd> or <kbd>?</kbd> as usual.
This will map to `:M/` command, which is used to perform the PCRE search.

You can call `eregex#toggle` funtion to toggle the keymapping. For example, 
add the following line into your `.vimrc` file:

    nnoremap <leader>/ :call eregex#toggle()<CR>

Then you can use <kbd><leader>/</kbd> to toggle the eregex.vim.

For replacement, use `:%S//` (uppercase S) to trigger perl style regexp.

See `:help eregex` for more information.

## Config

To disable the script by default, put this line in your `.vimrc` file:

    let g:eregex_default_enable = 0

To change the search delimiter to something else than the default `/` and `?`,
following options can be used:

    let g:eregex_forward_delim = '/'
    let g:eregex_backward_delim = '?'

To force case sensitive like perl re. Add the following config:

    let g:eregex_force_case = 1

Then you will have case sensitive match by default. You can always change it by adding `/i` modifier.

## Changes

### 2.62

* Support ignorecase, smartcase. Add force case sensitive mode.

### 2.61

* Support for ignorecase

### 2.60

* Support for the backword search.
* Support for the count argument.
* Use function to auto map keys.
* Support for custom search delimeters.
* hlsearch works fine.

## License

Author     : 安久津  
Origin     : [eregex.vim][origin]  
Maintainer : othree  

See `:help eregex-license-to-use` for license information.

[origin]:http://www.vector.co.jp/soft/unix/writing/se265654.html
