# vala.vim

- [Description](#description)
- [File detection](#file-detection)
- [Syntax highlighting](#syntax-highlighting)
- [Indentation](#indentation)
- [Syntastic](#syntastic)
- [Snippets](#snippets)
- [Additional functionality](#additional-functionality)

## Description

This is a [Vim][vim] plugin that provides file detection, syntax highlighting, proper indentation, better [Syntastic][syntastic] integration, code snippets and more for the [Vala programming language][vala].

The base version has been imported directly from the [official site][vala-vim].

![vala.vim - solarized dark](https://i.imgur.com/FW2vpKj.png)
![vala.vim - solarized light](https://i.imgur.com/mFMA3Bt.png)

Some of the features displayed in the above images are listed below.

## File detection

Automatic detection of `.vala`, `.vapi` and `.valadoc` files.

## Syntax highlighting

* Methods: any word followed by `(`
* Lambda expressions: `(...) =>`
* Arrays, lists and hash tables as in `Array<int>`, `List<string>` and `HashTable<string, int>`
* Operators and Delimiters: `+`, `-`, `*`, `/`, `=`, `( )`, `[ ]`, `{ }`...
* String formatting in `printf`-like methods: `%d`, `%f`, `%s`, `%c`, `%u`, `%%`...
* String templates: `@"$var1 = $(var2 * var3)"`

## Indentation

The indentation file is largely based on the [rust.vim][rust-vim] plugin, which is mainly a fix on top of `cindent`. It improves the indentation of:

* Method arguments spanning multiple lines.
* Code Attributes such as `CCode`, `DBus`, etc.
* Lambda expressions, like those used inside a `foreach` method.

## Syntastic

The amazing [Syntastic][syntastic] plugin already comes with support for [Vala][vala].
One can make use of the following magic comments to specify particular packages and vapi directories, for example:

``` vala
// modules: gio-2.0 gtk+-3.0
// vapidirs: vapi
```

However, I thought it would be useful to be able to specify which files should be compiled with the current one, as well as additional compiler flags, which will be passed to the `valac` compiler:

``` vala
// sources: neededfile.vala
// flags: --enable-deprecated
```

Note that passing files like this, while convenient, is suboptimal, since their location is relative to the current working path.

## Snippets

Useful snippets with [UltiSnips][ultisnips]:

* `try catch` statements.
* `for`, `foreach`, `while` loops.
* `if else` statements.
* `switch case` statements.
* `class`, `property`, `signal` definitions.
* Documentation using [Valadoc][valadoc] taglets.
* Many more!

## Additional functionality

This plugin also comes with helper functions to:

* Adhere to the [Vala Coding Style][vcs].
* Insert `CCode` attributes for the symbol below the cursor, useful when creating [Vala Legacy Bindings][vlb].

You can bind them by adding these lines to your `.vimrc`:

``` vim
if has("autocmd")
	autocmd FileType vala ValaCodingStyle
	autocmd FileType vala noremap <F8> :CCode<CR>
end
```

[rust-vim]:https://github.com/rust-lang/rust.vim
[syntastic]:https://github.com/vim-syntastic/syntastic
[vala]:https://wiki.gnome.org/Projects/Vala
[vala-vim]:https://wiki.gnome.org/Projects/Vala/Vim
[valadoc]:https://valadoc.org
[vcs]:https://wiki.gnome.org/Projects/Vala/Hacking#Coding_Style
[vlb]:https://wiki.gnome.org/Projects/Vala/LegacyBindings
[vim]:http://www.vim.org/
[ultisnips]:https://github.com/sirver/UltiSnips

