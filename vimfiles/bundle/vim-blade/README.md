# vim-blade #

Vim syntax highlighting for Blade templates (Laravel 4).

Installation
------------

Using pathogen 
[pathogen.vim](https://github.com/tpope/vim-pathogen).  

    cd ~/.vim/bundle
    git clone git://github.com/xsbeats/vim-blade.git

Without vim-blade | With vim-blade
------------------|---------------
![before](screenshots/without.png) | ![after](screenshots/with.png)

Development
-----------

### Testing

First install the [synchk](http://www.drchip.org/astronaut/vim/#SYNCHK) plugin
by Dr Chip, which requires his
[cecutil](http://www.drchip.org/astronaut/vim/#CECUTIL) plugin. They are
available [packaged together for Pathogen](https://github.com/tremby/synchk).

To run all tests, run the `test.sh` script.

To test just the currently open and focused test file, run `:SynChk`. If there
is no obvious message run `:messages`.

When expected output changes (new test, incorrect test output, change in desired
behaviour), ensure visually that the highlighting is correct and then, with the
test input file open and focused, run `:MakeSynChk`. This will create or
overwrite the expected output file.

Todo
----
- Add blade specific indentation (if, endif, etc)
