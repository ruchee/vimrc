php.vim
=======

This project is a fork of [php.vim--Garvin][garvin] which in turn is an update of the [php.vim][php-vim] script which in turn is an updated version of the php.vim syntax file distributed with Vim. Whew!

:point_right: Accepting [pull requests](https://github.com/StanAngeloff/php.vim/issues/15) for [PHP 5.6 new features](http://docs.php.net/manual/en/migration56.new-features.php). :point_left:

  [garvin]:  https://github.com/vim-scripts/php.vim--Garvin
  [php-vim]: http://www.vim.org/scripts/script.php?script_id=2874

Configuration
-------------

- `g:php_syntax_extensions_enabled`, `g:php_syntax_extensions_disabled`  
  `b:php_syntax_extensions_enabled`, `b:php_syntax_extensions_disabled`

  A list of extension names (lowercase) for which built-in functions, constants, classes and interfaces is enabled / disabled.

Updating
--------

The project comes with a Dockerfile which can be used to rebuild the syntax file.

```bash
docker build -t stanangeloff/php.vim .
docker run --rm -i -v "$PWD":/var/php -t stanangeloff/php.vim > /tmp/php.vim && cat /tmp/php.vim | sed 's/\x0D$//' > syntax/php.vim
docker rmi stanangeloff/php.vim
```
