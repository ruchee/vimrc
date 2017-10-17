# phpcd - PHP Completion Daemon Plugin for Vim/NeoVim

[![asciicast](https://asciinema.org/a/4dzyyjymrguylqt21igxlhhqx.png)](https://asciinema.org/a/4dzyyjymrguylqt21igxlhhqx)

## Introduction

PHPCD (PHP Completion Daemon) is another PHP completion engine for Vim/NeoVim.

PHPCD is based on [phpcomplete.vim](https://github.com/shawncplus/phpcomplete.vim) but is faster and smarter.

While phpcomplete.vim uses the tags file to fetch the context info, PHPCD uses PHP's Reflection mechanism to fetch the context info, and this is why PHPCD is faster. All the phpcomplete VimL code related the tags file has been droped and reimplemented.

PHPCD consists of two parts. On part is written in VimL (mainly based on phpcomplete.vim), and the other in PHP. ~~The communication between the VimL part and the PHP part relies on NeoVim's MsgPack-RPC mechanism. This is why NeoVim is needed.~~ Both NeoVim and Vim 8.0+ are supported now. Thanks to NoeVims's MsgPack-RPC and Vim's Channel/Job mechanism.

##  Feature
 * Fast, Lightweight, Powerful, Smart
 * Correct restriction of static or standard methods based on context (show only static methods with `::` and only standard with `->`)
 * Real support for `self::`, `static::`, `parent::` and `$this->` with the aforementioned context restriction
 * Better class detection
     - Recognize `/* @var $yourvar YourClass */`、 `/* @var YourClass $yourvar */` type mark comments
     - Recognize `$instance = new Class;` class instantiations
     - Recognize `(new Class)->` class instantiations
     - ~~Recognize `$date = DateTime::createFromFormat(...)` built-in class return types~~
     - Recognize both parameter type hinting and return hinting in function prototypes
     - Recognize types in `@param` lines in function docblocks
     - Recognize types in `@return` lines in function docblocks
     - Recognize `$instance = Class::foo()->bar();` method call chain return type
     - Recognize array of objects via docblock like `$foo[42]->` or for variables created in `foreach`
 * Displays docblock info in the preview for methods and properties
 * Support built-in class support with constants, methods and properties
 * Enhanced jump-to-definition on <kbd>ctrl</kbd>+<kbd>]</kbd>

## Installation & Usage

### System requirement

 1. [PHP 5.4+](http://php.net/)
 2. [PCNTL](http://php.net/manual/en/book.pcntl.php) Extension
 3. [Msgpack 0.5.7+(for NeoVim)](https://github.com/msgpack/msgpack-php) Extension or [JSON(for Vim 7.4+)](http://php.net/manual/en/intro.json.php) Extension
 4. ~~[Composer](https://getcomposer.org/) Project~~


### Install PHPCD

We recommend you use [Vim-Plug](https://github.com/junegunn/vim-plug/blob/master/README.md) to manage your vim plugins.

With Vim-Plug installed, put the following lines in your vimrc:

```
Plug 'lvht/phpcd.vim', { 'for': 'php', 'do': 'composer install' }
```

And then execute `:PlugInstall` in the command mode.

**If you install phpcd manually, you need run `composer install` in the phpcd.vim root directory.**

### deoplete
If you using the [deoplete](https://github.com/Shougo/deoplete.nvim), you can add the following lines to you init.vim

```viml
let g:deoplete#ignore_sources = get(g:, 'deoplete#ignore_sources', {})
let g:deoplete#ignore_sources.php = ['omni']
```
The phpcd will work with deoplete happily.

However if you are experiencing problems with deoplete you can disable the phpcd source.

```viml
let g:deoplete#ignore_sources = get(g:, 'deoplete#ignore_sources', {})
let g:deoplete#ignore_sources.php = ['phpcd', 'omni']
```

## Usage

~~First, in the project directory, run `composer install` to install all the dependent packages and generate the autoload file.~~
The **composer** is not required any more. However, if you want to let phpcd work with your project, you must to make a **autoload** file for your project.

If you use composer, composer will make the `vendor/autoload.php`. phpcd will use `vendor/autoload.php` automatically.

If your project does not use composer but have a `path/to/autoload_file.php`, you need to create a **.phpcd.vim** file in your project root path, and let it reads
```viml
let g:phpcd_autoload_path = 'path/to/autoload_file.php'
```
And phpcd will use the `g:phpcd_autoload_path` to load your class.

The default PHP command used to run PHP parts of daemon is simply `php`. You may override it by assigning `g:phpcd_php_cli_executable` another value in your `vimrc`, for example:
```
let g:phpcd_php_cli_executable = 'php7.0'
```

Use <kbd>Ctrl</kbd>+<kbd>x</kbd><kbd>Ctrl</kbd>+<kbd>o</kbd> to complete and use <kbd>ctrl</kbd>+<kbd>]</kbd> to go to the defination.

If you use neosnippet, you may like to set `g:phpcd_disable_modifier` to `0`.

Good luck :)
