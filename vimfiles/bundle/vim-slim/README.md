vim-slim
===

slim syntax highlighting for vim.

Install with pathogen
---------------------

If you are already using pathogen, you can skip to step 3.

1. Install pathogen (if you haven't already)

        mkdir -p ~/.vim/autoload ~/.vim/bundle && \
        curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

2. Edit `~/.vimrc` to run pathogen as the first line of the file (if you haven't already)

    ```vim
    execute pathogen#infect()
    syntax on
    filetype plugin indent on
    ```

3. Install slim-vim

        pushd ~/.vim/bundle; \
        git clone git://github.com/slim-template/vim-slim.git; \
        popd


Install with Vundle
--------------------

1. [Install Vundle] into `~/.vim/bundle/`.

[Install Vundle]: https://github.com/gmarik/Vundle.vim#quick-start

        mkdir -p ~/.vim/bundle; pushd ~/.vim/bundle; \
        git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
        popd

2. Configure your vimrc for Vundle. Here's a bare-minimum vimrc that enables vim-slim :


    ```vim
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

    Plugin 'gmarik/Vundle.vim'
    Plugin 'slim-template/vim-slim.git'

    call vundle#end()
    syntax enable
    filetype plugin indent on
    ```

If you're adding Vundle to a built-up vimrc, just make sure all these calls
   are in there and that they occur in this order.

3. Open vim and run `:PluginInstall`.

To update, open vim and run `:PluginInstall!` (notice the bang!)


Known Issues
------------

We use `setfiletype` upon autodetect, which does not overrides filetype once it
was set. That leads into an issue when filetype is set to `html` before we took
our chance (happens when slim file has `doctype html` header):

> Vim's `filetype.vim` has an `autocmd` that tries to detect *html* files based
> on *doctype* and this is triggered **before** scripts in `ftdetect/*`
> are sourced.
>
> -- https://github.com/slim-template/vim-slim/issues/38#issuecomment-23760100

To avoid that you have two options. Either using `doctype 5` instead of
`doctype html` or adding your own enforced version of `autocmd` to your
`.vimrc`:

```vim
autocmd BufNewFile,BufRead *.slim setlocal filetype=slim
```

See Also:

- https://github.com/slim-template/vim-slim/issues/38
- https://github.com/slim-template/vim-slim/commit/1ac9ebd51467ddbe0f12f01cdac57a76483a00af
