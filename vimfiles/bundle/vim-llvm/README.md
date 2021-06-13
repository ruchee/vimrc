Vim files for Low Level Virtual Machine (LLVM)
==============================================

This repository and its subdirectories contain source code for Vim files for the Low Level Virtual
Machine, a toolkit for the construction of highly optimized compilers, optimizers, and runtime
environments. LLVM is open source software. You may freely distribute it under the terms of the license
agreement found in LICENSE.txt.

This repository aims to make Vim plugin package managers deal with a Vim plugin bundled in the LLVM
official repository and provides some extended features.

If no license is specified in the header of a file (it means that it came from
[LLVM official repository][llvm]), the file is distributed under [LLVM's LICENSE](LICENSE.TXT).

## Imported from upstream ([LLVM official repository][llvm])

Following files are imported from `llvm/utils/vim` and `mlir/utils/vim`. They are updated at LLVM
version bump.

- `ftdetect/*.vim`
- `ftplugin/*.vim`
- `indent/*.vim`
- `syntax/*.vim`

Imported from LLVM 12.0.0

## Extended features

This repository provides some advanced features which are not supported in LLVM official repository.

- `after/**/*.vim`: Extended filetype support
- `scripts.vim`: Improved `llvm` filetype detection

If you want to disable these features, write the following config in your `vimrc`:

```vim
let g:llvm_extends_official = 0
```

### Mappings

Some useful mappings to jump a cursor are provided.

- `K`: Jump to the definition of an identifier under the cursor. Or if an instruction (like `getelementptr`)
  is under the cursor, the explanation of the instruction will be opened in a browser.
- `]]`, `][`: Move the cursor to the next basic block (Please see `:help ]]` for more details).
- `b]`: Jump to a basic block which follows the current basic block.
- `b[`: Jump to a basic block which the current basic block is following.

More mappings might be supported in the future.

When `g:llvm_ext_no_mapping` is set to `1`, these mappings won't be defined. Instead, please map `<Plug>`
mappings to your favorite key sequences.

```vim
" e.g. Map 'go to definition' to gd
autocmd FileType llvm nmap <buffer><silent>gd <Plug>(llvm-goto-definition)
```

### Commands

Some useful commands are defined in `llvm` filetype buffers.

#### `:LLI [file]`

Runs the given `file` using `lli` command. If `file` is omitted, it runs current buffer instead.
This command uses Neovim/Vim8 terminal feature. The LLVM IR code is run in job asynchronously and
the result is output in a temporary terminal buffer.

The default command to run is `lli`. You can change it by setting `g:llvm_ext_lli_executable`.

## Installation

Three options. First one or second one are recommended.

- Use your favorite plugin manager such as [vim-plug][], [dein.vim][], [minpac][]
- Use `:packadd` (Please see `:help packadd` for more details)
- Manually copy all directories and `scripts.vim` in this repository to your `~/.vim` (or `~/vimfiles` on Windows)

[llvm]: https://github.com/llvm/llvm-project
[vim-plug]: https://github.com/junegunn/vim-plug
[dein.vim]: https://github.com/Shougo/dein.vim
[minpac]: https://github.com/k-takata/minpac
