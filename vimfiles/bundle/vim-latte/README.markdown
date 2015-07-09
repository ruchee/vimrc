Latte syntax highlighter
========================

Latte is an [amazing template engine](http://doc.nette.org/en/templating) for [Nette Framework](http://nette.org/en/) (but usable standalone too).

Naturally it brings some new syntax elements to HTML & Co., and this is a Vim syntax plugin that encorporates them.

Things to change (maybe)
------------------------

For now, the plugin doesn't check for errors like wrong number of arguments to macros like {link}. It probably should, but that's for future me to add.

If you want it and I seem to do nothing for a long time, poke me! Either on a [forum thread](http://forum.nette.org/cs/10222-latte-vim-syntax-highlighter) (Czech dominant, but what the hell ;) ) or here, or [send me a mail](mailto:martin@janiczek.cz).

In case of feature requests the same things apply. Just give me a shout somewhere and that should be enough for things to start moving (again :) ).

Installation
------------

If you don't have a preferred installation method, I recommend installing [pathogen.vim](https://github.com/tpope/vim-pathogen), and then simply copy and paste:

    cd ~/.vim/bundle
    git clone https://github.com/janiczek/vim-latte.git
