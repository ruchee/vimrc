# vim-rescript

**This is the official vim plugin for ReScript.**

> If you are experiencing any troubles, open an issue or visit our [Forum](https://forum.rescript-lang.org) and ask for guidance.

## Features

**Supported Workflows:**
- Plain VIM (without any third party plugins)
- LSP + [coc-vim](https://github.com/neoclide/coc.nvim)

**Basics:**
- Syntax highlighting for ReSript files
- Filetype detection for `.res`, `.resi`
- Basic automatic indentation
- Includes LSP for coc-vim usage
- Proper tooling detection for monorepo like setups (yarn workspaces)

**Provided by vim-rescript commands:**
- Formatting `.res` files w/ syntax error diagnostics in VIM quickfix
- Convert existing `.re` /`.rei` files to `.res` /`.resi`
- Type hint for current cursor position
- Jump to definition for current cursor position
- Building the current projec w/ build diagnostics in VIM quickfix
- Autocompletion w/ Vim's omnicomplete

**Monorepo support:**

The vim-rescript plugin automatically updates its project environment on each file open separately.
- Tested for yarn workspaces (see [./examples/monorepo-yarn-workspaces])
- **Note for non-LSP usage:** Always make sure to switch to a `.res` file **within the project you want to compile** before running `:RescriptBuild` etc.

See `:h rescript` for the detailed [helpfile](./doc/rescript.txt).

## Requirements

The plugin works with projects based on `bs-platform@8.3` or later

## Installation

`vim-rescript` can be installed either manually or by using your favourite plugin manager.

```viml
" vim-plug
Plug 'rescript-lang/vim-rescript'

" Vundle
Plugin 'rescript-lang/vim-rescript'

" NeoBundle
NeoBundle 'rescript-lang/vim-rescript'
```

You can also pin your installation to specific tags (check our releases [here](https://github.com/rescript-lang/vim-rescript/releases)):

```
Plug 'rescript-lang/vim-rescript', {'tag': 'v2.1.0'}
```

## Using vim-rescript with COC

(`:h rescript-coc`)

Our plugin comes with all the necessary tools (LSP + editor-support binaries for Windows, Mac, Linux) to set up coc-vim.
After the installation, open your coc config (`:CocConfig`) and add the following configuration:

```json
"languageserver": {
  "rescript": {
    "enable": true,
    "module": "~/.config/nvim/plugged/vim-rescript/server/out/server.js",
    "args": ["--node-ipc"],
    "filetypes": ["rescript"],
    "rootPatterns": ["bsconfig.json"]
  }
}
```

- The config above assumes that you were using `vim-plug` for plugin installations.
- Adapt your `module` path according to the install location of your vim-rescript plugin.
- Save the configuration, run `:CocRestart` and open a (built) ReScript project and check your code for type-hints.

**Note:** Even if you are using COC, we recommend checking out the builtin commands that come with `vim-rescript` (`:h rescript-commands).

## Using vim-rescript's functionality (no vim-coc, no vim-ale, etc)

(`:h rescript-config`)

Vim comes with a set of useful functions that are completely self contained and work with any neovim setup without any plugins:

```
:h :RescriptFormat
:h :RescriptUpgradeFromReason
:h :RescriptBuild
:h :RescriptTypeHint
:h :RescriptJumpToDefinition
:h :RescriptInfo
```
Please refer to the [doc](./doc/rescript.txt) file for more details!

We don't come with any predefined keybindings, but provide a basic set of keymappings down below.

### Basic Key Bindings

These bindings won't collide with any other mappings in your vimrc setup, since they are scoped to `rescript` buffers only:

```vim
" Note that <buffer> allows us to use different commands with the same keybindings depending
" on the filetype. This is useful if to override your e.g. ALE bindings while working on
" ReScript projects.
autocmd FileType rescript nnoremap <silent> <buffer> <localleader>r :RescriptFormat<CR>
autocmd FileType rescript nnoremap <silent> <buffer> <localleader>t :RescriptTypeHint<CR>
autocmd FileType rescript nnoremap <silent> <buffer> <localleader>b :RescriptBuild<CR>
autocmd FileType rescript nnoremap <silent> <buffer> gd :RescriptJumpToDefinition<CR>
```

### Configure omnicomplete Support

(`:h rescript-omnicomplete`)

We support auto-completion with Vim's builtin `omnifunc`, which is triggered with `C-x C-o` in insert mode to look for autocomplete candidates.

> While omnicomplete's dialog is open, use `C-n` / `C-p` to navigate to the next / previous item

```vim
" Hooking up the ReScript autocomplete function
set omnifunc=rescript#Complete

" When preview is enabled, omnicomplete will display additional
" information for a selected item
set completeopt+=preview
```

## Credits

- [amirales](https://github.com/amiralies): Started the plugin w/ syntax & indent functionality
