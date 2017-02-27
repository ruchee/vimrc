# vim-blade #

Vim syntax highlighting for Blade templates (Laravel 4+).

This plugin contributes to [vim-polyglot](https://github.com/sheerun/vim-polyglot) language pack.

Installation
------------

Using vim-plug
[vim-plug](https://github.com/junegunn/vim-plug).

    Plug 'jwalton512/vim-blade'

Using pathogen 
[pathogen.vim](https://github.com/tpope/vim-pathogen).  

    cd ~/.vim/bundle
    git clone git://github.com/jwalton512/vim-blade.git

Configuration
-------------

Because Blade allows you to define your own directives, you can let the plugin
know about them through some variables. Examples:

```vim
" Define some single Blade directives. This variable is used for highlighting only.
let g:blade_custom_directives = ['datetime', 'javascript']

" Define pairs of Blade directives. This variable is used for highlighting and indentation.
let g:blade_custom_directives_pairs = {
      \   'markdown': 'endmarkdown',
      \   'cache': 'endcache',
      \ }
```

Contributing
------------

Pull requests are greatly appreciated. Please be certain to include a test where applicable (`test.blade.php`). You may test locally by using `vim -u vimrc`.

Want to buy me a coffee?
------------------------
[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=jwalton512&url=https://github.com/jwalton512/vim-blade&title=vim-blade&language=vimscript&tags=github&category=software)
