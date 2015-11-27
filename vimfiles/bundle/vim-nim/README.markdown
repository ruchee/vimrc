#Nim language support for Vim

This provides [Nim](http://nimrod-code.org) language support for Vim:

* Syntax highlighting
* Auto-indent
* Build/jump to errors within Vim
* Project navigation and Jump to Definition (cgats or compiler-assisted
  idetools).

The source of this script comes mainly from
http://www.vim.org/scripts/script.php?script_id=2632, which comes from a
modified python.vim (http://www.vim.org/scripts/script.php?script_id=790).

#Installation

Installing `nimrod.vim` is easy but first you need to have either pathogen plugin or vundle
installed.  If you already have one working then skip to the [final step](README.markdown#final-step).
It is also recommened that you use the [syntastic](https://github.com/scrooloose/syntastic) plugin for best results.

##Pathogen

###Step 1: Install pathogen.vim

First I'll show you how to install tpope's
[pathogen.vim](https://github.com/tpope/vim-pathogen) so that it's easy to
install `nimrod.vim`.  Do this in your Terminal so that you get the
`pathogen.vim` file and the directories it needs:

    mkdir -p ~/.vim/autoload ~/.vim/bundle; \
    curl -so ~/.vim/autoload/pathogen.vim \
        https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

Next you *need to add this* to your `~/.vimrc`:

    call pathogen#infect()

###Step 2: Install nimrod.vim as a pathogen bundle

You now have pathogen installed and can put `nimrod.vim` into `~/.vim/bundle`
like this:

    cd ~/.vim/bundle
    git clone git://github.com/zah/nimrod.vim.git
    
You may also want to install synastic by calling 

    git clone https://github.com/scrooloose/syntastic.git

##Vundle
  
Vundle is a more automatic way to install vim plugins that works by cloning 
the git reposotory.
  
###Step 1: Install Vundle
  
Add the vundle script to your vim:
  
    git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    
Modify your ~/.vimrc to get vundle running, lightly adapted from [Vundle's readme](https://github.com/gmarik/Vundle.vim/blob/master/README.md)

    set nocompatible              " be iMproved, required
    filetype off                  " required
    
    " set the runtime path to include Vundle and initialize
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()
    " alternatively, pass a path where Vundle should install bundles
    "let path = '~/some/path/here'
    "call vundle#rc(path)
    
    " let Vundle manage Vundle, required
    Bundle 'gmarik/vundle'
    
    filetype plugin indent on     " required
    
###Step 2: Install nimrod.vim

On the line after `Bundle 'gmarik/vundle'`, add `Bundle 'zah/nimrod.vim'`. You may also want
to add `Bundle 'scrooloose/syntastic'`. Save `~/.vimrc` and restart vim. Execute `:BundleInstall`
and wait for nimrod.vim to be installed.

##Final Step
Next you *need to add this* to your `~/.vimrc`:

    fun! JumpToDef()
      if exists("*GotoDefinition_" . &filetype)
        call GotoDefinition_{&filetype}()
      else
        exe "norm! \<C-]>"
      endif
    endf
    
    " Jump to tag
    nn <M-g> :call JumpToDef()<cr>
    ino <M-g> <esc>:call JumpToDef()<cr>i

The `JumpToDef` function hooks the `nimrod.vim` plugin to invoke the nimrod
compiler with the appropriate idetools command. Pressing meta+g will then jump
to the definition of the word your cursor is on. This uses the nimrod compiler
instead of ctags, so it works on any nimrod file which is compilable without
requiring you to maintain a database file.
  
#Other recomended Vim plugins

* https://github.com/scrooloose/syntastic (copied bits from its readme)
* https://github.com/Shougo/neocomplcache

#If something goes wrong

Since you are using vim, on source code which might have syntax problems,
invoking an external tool which may have its own share of bugs, sometimes stuff
just doesn't work as expected. In these situations if you want to debug the
issue you can type ``:e log://nimrod`` and a buffer will open with the log of
the plugin's invocations and nimrod's idetool answers.

This can give you a hint of where the problem is and allow you to easily
reproduce on the commandline the idetool parameters the vim plugin is
generating so you can prepare a test case for either this plugin or the nimrod
compiler.
