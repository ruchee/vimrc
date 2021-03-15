# vim-svelte-plugin [![Build Status][12]](https://travis-ci.com/leafOfTree/vim-svelte-plugin)

<p align="center">
<a href="https://github.com/altercation/vim-colors-solarized">
<img alt="screenshot" src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/vim-svelte-solarized.png" width="220" />
</a>
<a href="https://github.com/leafOfTree/vim-svelte-theme">
<img alt="screenshot" src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/vim-svelte-theme.png" width="220" />
</a>
</p>

Vim syntax and indent plugin for `.svelte` files. Forked from [vim-vue-plugin][3]. 

## Installation

<details>

<summary><a>How to install</a></summary>

- [VundleVim][2]

        Plugin 'leafOfTree/vim-svelte-plugin'

- [vim-pathogen][5]

        cd ~/.vim/bundle && \
        git clone https://github.com/leafOfTree/vim-svelte-plugin --depth 1

- [vim-plug][7]

        Plug 'leafOfTree/vim-svelte-plugin'
        :PlugInstall

- Or manually, clone this plugin to `path/to/this_plugin`, and add it to `rtp` in vimrc

        set rtp+=path/to/this_plugin

<br />
</details>

This plugin works if it has set `filetype` to `svelte`. Please stay up to date. Feel free to open an issue or pull request.

## How it works

It combines HTML, CSS and JavaScript syntax and indent in one file.

Supports

- Svelte directives.
- Less/Sass/Scss, Pug with [vim-pug][4], Coffee with [vim-coffee-script][6], TypeScript with [typescript-vim][14] or [yats.vim][15].^

    Relative plugins need to be installed.

- A builtin `foldexpr` foldmethod.^
- [emmet-vim][10] HTML/CSS/JavaScript filetype detection.

^: see Configuration for details.

## Configuration

Set global variable to `1` to enable or `0` to disable. Ex:

    let g:vim_svelte_plugin_load_full_syntax = 1

| variable                              | description                                                                                            | default                    |
|---------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|----------------------------|
| `g:vim_svelte_plugin_load_full_syntax`\* | Enable: load all syntax files in `runtimepath` to enable related syntax plugins.<br> Disable: only in `$VIMRUNTIME/syntax`, `~/.vim/syntax` and `$VIM/vimfiles/syntax` | 0 |
| `g:vim_svelte_plugin_use_pug`\*             | Enable pug syntax for `<template lang="pug">`.                                                         | 0 |
| `g:vim_svelte_plugin_use_coffee`            | Enable coffee syntax for `<script lang="coffee">`.                                                     | 0 |
| `g:vim_svelte_plugin_use_typescript`        | Enable typescript syntax for `<script lang="ts">`.                                                     | 0 |
| `g:vim_svelte_plugin_use_less`              | Enable less syntax for `<style lang="less">`.                                                          | 0 |
| `g:vim_svelte_plugin_use_sass`              | Enable scss syntax for `<style lang="scss">`(or sass for `lang="sass"`).                               | 0 |
| `g:vim_svelte_plugin_has_init_indent`       | Initially indent one tab inside `style/script` tags.                                                   | 1 |
| `g:vim_svelte_plugin_use_foldexpr`          | Enable builtin `foldexpr` foldmethod.                                                                  | 0 |
| `g:vim_svelte_plugin_debug`                 | Echo debug messages in `messages` list. Useful to debug if unexpected indents occur.                   | 0 |

\*: Vim may be slow if the feature is enabled. Find a balance between syntax highlight and speed. By the way, custom syntax can be added in `~/.vim/syntax` or `$VIM/vimfiles/syntax`. 

**Note**

- `filetype` is set to `svelte` so autocmds and other custom settings for `javascript` have to be manually enabled for `svelte`.
- `g:vim_svelte_plugin_load_full_syntax` applies to `HTML/Sass/Less`.
- See <https://svelte.dev/docs#svelte_preprocess> for how to use `Less/Sass/Pug`... in svelte.

## Context based behavior

As there are more than one language in `.svelte` file, the different behaviors like mapping, completion, and local options may be expected according to tags or subtypes(current language type).

This plugin provides functions to get the tag/subtype where the cursor is in.

- `GetSvelteTag() => String` Return value is one of `'template', 'script', 'style'`.

  ```vim
  " Example
  autocmd FileType svelte inoremap <buffer><expr> : InsertColon()

  function! InsertColon()
    let tag = GetSvelteTag()
    return tag == 'template' ? ':' : ': '
  endfunction
  ```

- `GetSvelteSubtype() => String` Return value is one of `'html', 'javascript', 'css', 'scss', ...`.

- `OnChangeSvelteSubtype(subtype)` An event listener that is called when subtype changes.

    You can also define an event listener function `OnChangeSvelteSubtype(subtype)` in your `vimrc` to get the subtype and set its local options whenever it changes.

    ```vim
    " Example: set local options based on subtype
    function! OnChangeSvelteSubtype(subtype)
      echom 'Subtype is '.a:subtype
      if empty(a:subtype) || a:subtype == 'html'
        setlocal commentstring=<!--%s-->
        setlocal comments=s:<!--,m:\ \ \ \ ,e:-->
      elseif a:subtype =~ 'css'
        setlocal comments=s1:/*,mb:*,ex:*/ commentstring&
      else
        setlocal commentstring=//%s
        setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://
      endif
    endfunction
    ```

### emmet-vim

Currently emmet-vim works regarding your HTML/CSS/JavaScript emmet settings, but it depends on how emmet-vim gets `filetype` and may change in the future. Feel free to report an issue if any problem appears.

## Avoid overload

Since there are many sub languages included, most delays come from syntax files overload. A variable named `b:current_loading_main_syntax` is set to `svelte` which can be used as loading condition if you'd like to manually find and modify the syntax files causing overload.

For example, the builtin syntax `sass.vim` and `less.vim` in vim8.1 runtime always load `css.vim` which this plugin already loads. It can be optimized like

```diff
- runtime! syntax/css.vim
+ if !exists("b:current_loading_main_syntax")
+   runtime! syntax/css.vim
+ endif
```

## See also

- [vim-svelte-theme][11] svelte syntax color
- [vim-vue-plugin][3]
- [mxw/vim-jsx][1]

## License

This plugin is under [The Unlicense][8]. Other than this, `lib/indent/*` files are extracted from vim runtime.

[1]: https://github.com/mxw/vim-jsx "mxw: vim-jsx"
[2]: https://github.com/VundleVim/Vundle.vim
[3]: https://github.com/leafOfTree/vim-vue-plugin
[4]: https://github.com/digitaltoad/vim-pug
[5]: https://github.com/tpope/vim-pathogen
[6]: https://github.com/kchmck/vim-coffee-script
[7]: https://github.com/junegunn/vim-plug
[8]: https://choosealicense.com/licenses/unlicense/
[10]: https://github.com/mattn/emmet-vim
[11]: https://github.com/leafOfTree/vim-svelte-theme
[12]: https://travis-ci.com/leafOfTree/vim-svelte-plugin.svg?branch=master
[14]: https://github.com/leafgarland/typescript-vim
[15]: https://github.com/HerringtonDarkholme/yats.vim
