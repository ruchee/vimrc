### LiveScript
For more information about LiveScript see [gkz.github.com/LiveScript](http://gkz.github.com/LiveScript/).

### Installing and Using

1. Install [tpope's][tpope] [pathogen] into `~/.vim/autoload/` and add the
   following line to your `~/.vimrc`:

        call pathogen#infect()

     Be aware that it must be added before any `filetype plugin indent on`
     lines according to the install page:

     > Note that you need to invoke the pathogen functions before invoking
     > "filetype plugin indent on" if you want it to load ftdetect files. On
     > Debian (and probably other distros), the system vimrc does this early on,
     > so you actually need to "filetype off" before "filetype plugin indent on"
     > to force reloading.

[pathogen]: http://www.vim.org/scripts/script.php?script_id=2332
[tpope]: http://github.com/tpope/vim-pathogen

2. Create, and change into, the `~/.vim/bundle/` directory:

        $ mkdir -p ~/.vim/bundle
        $ cd ~/.vim/bundle

3. Make a clone of the `vim-ls` repository:

        $ git clone git://github.com/gkz/vim-ls.git
        [...]
        $ ls
        vim-ls/

That's it. Pathogen should handle the rest. Opening a file with a `.ls`
extension or a `Slakefile` will load everything.

### Updating

1. Change into the `~/.vim/bundle/vim-ls/` directory:

        $ cd ~/.vim/bundle/vim-ls

2. Pull in the latest changes:

        $ git pull

Everything will then be brought up to date.

### LiveScriptMake: Compile the Current File

The `LiveScriptMake` command compiles the current file and parses any errors.

The full signature of the command is:

    :[silent] LiveScriptMake[!] [ls-OPTIONS]...

By default, `LiveScriptMake` shows all compiler output and jumps to the first line
reported as an error by `livescript`:

    :LiveScriptMake

Compiler output can be hidden with `silent`:

    :silent LiveScriptMake

Line-jumping can be turned off by adding a bang:

    :LiveScriptMake!

Options given to `LiveScriptMake` are passed along to `livescript`:

    :LiveScriptMake --bare

`LiveScriptMake` can be manually loaded for a file with:

    :compiler ls

#### Recompile on write

To recompile a file when it's written, add an `autocmd` like this to your
`vimrc`:

    au BufWritePost *.ls silent LiveScriptMake!

All of the customizations above can be used, too. This one compiles silently
and with the `-b` option, but shows any errors:

    au BufWritePost *.ls silent LiveScriptMake! -b | cwindow | redraw!

The `redraw!` command is needed to fix a redrawing quirk in terminal vim, but
can removed for gVim.

#### Default compiler options

The `LiveScriptMake` command passes any options in the `livescript_make_options`
variable along to the compiler. You can use this to set default options:

    let livescript_make_options = '--bare'

#### Path to compiler

To change the compiler used by `LiveScriptMake` and `LiveScriptCompile`, set
`livescript_compiler` to the full path of an executable or the filename of one
in your `$PATH`:

    let livescript_compiler = '/usr/bin/livescript'

This option is set to `livescript` by default.

### LiveScriptCompile: Compile Snippets of LiveScript

The `LiveScriptCompile` command shows how the current file or a snippet of
LiveScript is compiled to JavaScript. The full signature of the command is:

    :[RANGE] LiveScriptCompile [watch|unwatch] [vert[ical]] [WINDOW-SIZE]

Calling `LiveScriptCompile` without a range compiles the whole file.

Calling `LiveScriptCompile` with a range, like in visual mode, compiles the selected
snippet of LiveScript.

The scratch buffer can be quickly closed by hitting the `q` key.

Using `vert` splits the LiveScriptCompile buffer vertically instead of horizontally:

    :LiveScriptCompile vert

Set the `ls_compile_vert` variable to split the buffer vertically by
default:

    let ls_compile_vert = 1

The initial size of the LiveScriptCompile buffer can be given as a number:

    :LiveScriptCompile 4

#### Watch (live preview) mode

Writing some code and then exiting insert mode automatically updates the
compiled JavaScript buffer.

Use `watch` to start watching a buffer (`vert` is also recommended):

    :LiveScriptCompile watch vert

After making some changes in insert mode, hit escape and your code will
be recompiled. Changes made outside of insert mode don't trigger this recompile,
but calling `LiveScriptCompile` will compile these changes without any bad effects.

To get synchronized scrolling of a LiveScript and LiveScriptCompile buffer, set
`scrollbind` on each:

    :setl scrollbind

Use `unwatch` to stop watching a buffer:

    :LiveScriptCompile unwatch

### Configure Syntax Highlighting

Add these lines to your `vimrc` to disable the relevant syntax group.

#### Disable trailing whitespace error

Trailing whitespace is highlighted as an error by default. This can be disabled
with:

    hi link lsSpaceError NONE

#### Disable reserved words error

Reserved words like `function` and `var` are highlighted as an error where
they're not allowed in LiveScript. This can be disabled with:

    hi link lsReservedError NONE

### Tune Vim for LiveScript

Changing these core settings can make vim more LiveScript friendly.

#### Fold by indentation

Folding by indentation works well for LiveScript functions and classes.
To fold by indentation in LiveScript files, add this line to your `vimrc`:

    au BufNewFile,BufReadPost *.ls setl foldmethod=indent nofoldenable

With this, folding is disabled by default but can be quickly toggled per-file
by hitting `zi`. To enable folding by default, remove `nofoldenable`:

    au BufNewFile,BufReadPost *.ls setl foldmethod=indent

#### Two-space indentation

To get standard two-space indentation in LiveScript files, add this line to
your `vimrc`:

    au BufNewFile,BufReadPost *.ls setl shiftwidth=2 expandtab
