This project adds [eco] support to the vim editor. Most of the code is
originally from Mick Koch's [vim-coffee-script].

Included features:
  - eco filetype detection
  - automatic indenting
  - syntax highlighting
  - surround.vim support (see [ftplugin/eco.vim] for details)
  - `=` text object for code blocks (see [ftplugin/eco.vim] for details)

Some of the code here depends on [vim-coffee-script], so it's required that you
install that before installing this plugin. The master branch should always be
compatible with [vim-coffee-script]'s master branch, so if you have any
problems, please ensure you've updated both plugins to their latest version.

Collaborators are welcome. If you feel you're fairly comfortable with the code
and use eco on a regular basis, please consider becoming a maintainer of a part
of the project -- just open up a github issue requesting collaborator access.
Naturally, pull request with fixes or interesting new features are always
welcome.

[eco]: https://github.com/sstephenson/eco
[vim-coffee-script]: https://github.com/kchmck/vim-coffee-script
[ftplugin/eco.vim]: https://github.com/AndrewRadev/vim-eco/blob/master/ftplugin/eco.vim

### Install from a Zipball

This is the quickest way to get things running.

1. Download the latest zipball from [vim.org][zipball-vim] or
   [github][zipball-github]. The latest version on github is under Download
   Packages (don't use the Download buttons.)

2. Extract the archive into `~/.vim/`:

        unzip -od ~/.vim vim-eco-HASH.zip

These steps are also used to update the plugin.

[zipball-vim]: http://www.vim.org/scripts/script.php?script_id=4270
[zipball-github]: https://github.com/AndrewRadev/vim-eco/downloads

### Install with Pathogen

Since this plugin has rolling versions based on git commits, using pathogen and
git is the preferred way to install. The plugin ends up contained in its own
directory and updates are just a `git pull` away.

1. Install tpope's [pathogen] into `~/.vim/autoload/` and add this line to your
   `vimrc`:

        call pathogen#infect()

    To get the all the features of this plugin, make sure you also have a
    `filetype plugin indent on` line in there.

[pathogen]: http://www.vim.org/scripts/script.php?script_id=2332

2. Create and change into `~/.vim/bundle/`:

        $ mkdir ~/.vim/bundle
        $ cd ~/.vim/bundle

3. Make a clone of the `vim-eco` repository:

        $ git clone https://github.com/AndrewRadev/vim-eco.git

#### Updating

1. Change into `~/.vim/bundle/vim-eco/`:

        $ cd ~/.vim/bundle/vim-eco

2. Pull in the latest changes:

        $ git pull

### Configure Syntax Highlighting

Add these lines to your `vimrc` to disable the relevant syntax group.

#### Disable reserved words error

Reserved words like `function` and `var` are highlighted as an error where
they're not allowed in CoffeeScript. This can be disabled with:

``` vim
hi link coffeeReservedError NONE
```

Note: this affects coffeescript highlighting as well

#### Disable missing colon error

A few coffeescript control structures -- if-clauses, else-clauses and
for-cycles -- need to end with a colon, like so:

``` eco
<% if foo: %>
  bar
<% else: %>
  baz
<% end %>
```

If there's no colon at the end of these, they will be highlighted as errors.
This can be disabled with:

``` vim
hi link ecoMissingColonError NONE
```
