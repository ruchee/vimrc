PHP Vim Syntax
==============

[This repo](https://github.com/TysonAndre/php-vim-syntax) contains syntax highlighting files for vim at [syntax/php.vim](syntax/php.vim).

Installation
------------

An older version of [syntax/php.vim](syntax/php.vim) may already be distributed with your editor.
Older versions may be missing highlighting for new syntax/keywords/functions/classes.

In vim, [syntax/php.vim](syntax/php.vim) can be placed in `~/.vim/syntax/php.vim` to use the newer version instead.
(`~/vimfiles/syntax/php.vim` on Windows)

In neovim, [syntax/php.vim](syntax/php.vim) can be placed in `~/.config/nvim/syntax/php.vim` to use the newer version instead.
(`~/AppData/Local/nvim/syntax/php.vim` on Windows)

Alternately, php vim syntax files can be installed and updated using a plugin manager such as [vim-plug](https://github.com/junegunn/vim-plug), [vundle](https://github.com/VundleVim/Vundle.vim), or [pathogen.vim](https://github.com/tpope/vim-pathogen) by using this repo as a plugin ([TysonAndre/php-vim-syntax](https://github.com/TysonAndre/php-vim-syntax) from github).

Documentation
-------------

See `:help ft-php-syntax` and [`syntax/php.vim`](syntax/php.vim)


```
Note: If you are using a colour terminal with dark background, you will
      probably find the 'elflord' colorscheme is much better for PHP's syntax
      than the default colourscheme, because elflord's colours will better
      highlight the break-points (Statements) in your code.

Options:
  Set to anything to enable:
    php_sql_query           SQL syntax highlighting inside strings
    php_htmlInStrings       HTML syntax highlighting inside strings

                            By setting this to 2 instead, this will use a local copy of
                            HTML syntax highlighting instead of the official
                            HTML syntax highlighting, and properly highlight
                            `<?php $startTag = '<?php';`.
                            This may become the new default in the future.

                            By setting this to 3, this will use the
                            official installed top level html syntax highlighting rules.
    php_baselib             highlighting baselib functions
    php_asp_tags            highlighting ASP-style short tags
    php_parent_error_close  highlighting parent error ] or )
    php_parent_error_open   skipping an php end tag, if there exists
                              an open ( or [ without a closing one
    php_oldStyle            use old colorstyle
    php_noShortTags         don't sync <? ?> as php
  Set to a specific value:
    php_folding = 1         fold classes and functions
    php_folding = 2         fold all { } regions
    php_sync_method = x  where x is an integer:
                      -1  sync by search ( default )
                      >0  sync at least x lines backwards
                      0   sync from start
  Set to 0 to _disable_:      (Added by Peter Hodge On June 9, 2006)
    php_special_functions = 0      highlight functions with abnormal behaviour
    php_alt_comparisons = 0        comparison operators in an alternate colour
    php_alt_assignByReference = 0  '= &' in an alternate colour


Note:
Setting php_folding=1 will match a closing } by comparing the indent
before the class or function keyword with the indent of a matching }.
Setting php_folding=2 will match all of pairs of {,} ( see known
bugs ii )

Known Bugs:
 - setting  php_parent_error_close  on  and  php_parent_error_open  off
   has these two leaks:
    i) A closing ) or ] inside a string match to the last open ( or [
       before the string, when the the closing ) or ] is on the same line
       where the string started. In this case a following ) or ] after
       the string would be highlighted as an error, what is incorrect.
   ii) Same problem if you are setting php_folding = 2 with a closing
       } inside an string on the first line of this string.
```
