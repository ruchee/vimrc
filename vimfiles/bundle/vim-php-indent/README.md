## About


This is the official PHP indentation plug-in for VIm (version 1.39 is bundled with VIm 7.4).

Please, visit [the homepage](https://www.2072productions.com/to/phpindent.txt) of the PHP indentation script to see all the details about its features and capabilities.

### Options

Just type `:help php-indent` inside VIm.

### Optional updated Syntax script

Feel free to use [my updated version](https://github.com/2072/vim-syntax-for-PHP) of the official VIm syntax
script for PHP which fixes a few issues and add support for some of the newest PHP
features (see the [change log](https://github.com/2072/vim-syntax-for-PHP/commits/master) for the details).

## Install


### Vundle
 1. Install and configure the [Vundle](https://github.com/gmarik/vundle) plug-in manager, [follow the instructions here](https://github.com/gmarik/vundle#quick-start)
 2. Add the following line to your `.vimrc`:

         Plugin '2072/PHP-Indenting-for-VIm'
 3. Source your `.vimrc` with `:so %` or otherwise reload your VIm
 4. Run the `:BundleInstall` command

### Pathogen
 1. Install the [pathogen.vim](https://github.com/tpope/vim-pathogen) plug-in, [follow the instructions here](https://github.com/tpope/vim-pathogen#installation)
 2. Clone the repository under your `~/.vim/bundle/` directory:

         cd ~/.vim/bundle
         git clone git@github.com:2072/PHP-Indenting-for-VIm.git


