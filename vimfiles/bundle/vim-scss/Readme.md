# Vim Syntax File for SCSS (Sassy CSS)

## Installation

I recommend to use a plugin manager like [Vundle](https://github.com/gmarik/vundle) for the installation.

### Vundle

Open your `~/.vimrc` file and add the following line(s):

```
Bundle 'JulesWang/css.vim' // only necessary if your Vim version < 7.4
Bundle 'cakebaker/scss-syntax.vim'
```

Afterwards, run `:BundleInstall` in Vim.

### Manual

* Download [scss.vim](https://github.com/cakebaker/scss-syntax.vim/archive/master.zip)
* Download [css.vim](https://github.com/JulesWang/css.vim/archive/master.zip) (only necessary if your Vim version < 7.4)
* Copy the content of the folders to the respective folders in `~/.vim/`

## Configuration

Usually no configuration is necessary.

### Filetype

In some cases you might want to change the filetype from `scss` to `scss.css`, for example, if you want to use [SnipMate](https://github.com/garbas/vim-snipmate)'s CSS snippets within your SCSS files. In this case, add the following line to your `~/.vimrc` file:

```
au BufRead,BufNewFile *.scss set filetype=scss.css
```

Please be aware that this setting can cause problems with other plugins as mentioned in [#41](https://github.com/cakebaker/scss-syntax.vim/pull/41).
