php.vim
=======

This project is a fork of [php.vim--Garvin][garvin] which in turn is an update of the [php.vim][php-vim] script which in turn is an updated version of the php.vim syntax file distributed with Vim. Whew!

  [garvin]:  https://github.com/vim-scripts/php.vim--Garvin
  [php-vim]: http://www.vim.org/scripts/script.php?script_id=2874


Updating
--------

The project comes with a Dockerfile which can be used to rebuild the syntax file.

```bash
docker build -t StanAngeloff/php.vim .
docker run -i -t StanAngeloff/php.vim /build/update_syntax.php | sed 's/\x0D$//' > syntax/php.vim
```
