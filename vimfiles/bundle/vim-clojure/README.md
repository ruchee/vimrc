# Clojure.vim

[Vim][] runtime files for [Clojure][].  This is a fork of [vim-clojure-static][].


## Installation

These files ship with Vim version 7.3.803 and later and are periodically
merged into the official Vim repository.

If you are running an old version of Vim or if you would like to keep up with
the latest changes, you can install this repository as you would any other Vim
plugin.

Make sure that the following options are set in your vimrc so that all
features are enabled:

```vim
syntax on
filetype plugin indent on
```

## Features

* [Augmentable](#syntax-options) syntax highlighting for Clojure and
  ClojureScript buffers.

* [Configurable](#indent-options) Clojure-specific indentation.

* Basic insert mode completion for special forms and public vars in
  `clojure.core`.

  This is bound to both the `'omnifunc'` and `'completefunc'` options, which
  can be invoked with the insert mode mappings `<C-X><C-O>` and `<C-X><C-U>`
  respectively.


## Configuration

### Syntax Options

Syntax highlighting for public vars from `clojure.core` is provided by
default, but any symbol can be matched and highlighted by adding it to the
`g:clojure_syntax_keywords` or `b:clojure_syntax_keywords` variables:

```vim
let g:clojure_syntax_keywords = {
    \ 'clojureMacro': ["defproject", "defcustom"],
    \ 'clojureFunc': ["string/join", "string/replace"]
    \ }
```

See `s:clojure_syntax_keywords` in the [syntax script](syntax/clojure.vim) for
a complete example.

The global version of this variable is intended for users that always wish
to have a certain set of symbols highlighted in a certain way, while the
buffer-local version is intended for plugin authors who wish to highlight
symbols dynamically.

If the buffer flag `b:clojure_syntax_without_core_keywords` is set, vars from
`clojure.core` are not highlighted by default. This is useful for highlighting
namespaces that have set `(:refer-clojure :only [])`.

[`vim-clojure-highlight`](https://github.com/guns/vim-clojure-highlight) uses
these variables to highlight extra vars when connected to a REPL.

### Indent Options

Clojure indentation differs somewhat from traditional Lisps, due in part to
the use of square and curly brackets, and otherwise by community convention.
These conventions are not universally followed, so the Clojure indent script
offers a few configurable options, listed below.

If the current vim does not include searchpairpos(), the indent script falls
back to normal `'lisp'` indenting, and the following options are ignored.

#### `g:clojure_maxlines`

Set maximum scan distance of searchpairpos(). Larger values trade performance
for correctness when dealing with very long forms. A value of 0 will scan
without limits.

```vim
" Default
let g:clojure_maxlines = 300
```

#### `g:clojure_fuzzy_indent`, `g:clojure_fuzzy_indent_patterns`, `g:clojure_fuzzy_indent_blacklist`

The `'lispwords'` option is a list of comma-separated words that mark special
forms whose subforms must be indented with two spaces.

For example:

```clojure
(defn bad []
      "Incorrect indentation")

(defn good []
  "Correct indentation")
```

If you would like to specify `'lispwords'` with a pattern instead, you can use
the fuzzy indent feature:

```vim
" Default
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

" Legacy comma-delimited string version; the list format above is
" recommended. Note that patterns are implicitly anchored with ^ and $.
let g:clojure_fuzzy_indent_patterns = 'with.*,def.*,let.*'
```

`g:clojure_fuzzy_indent_patterns` and `g:clojure_fuzzy_indent_blacklist` are
lists of patterns that will be matched against the unqualified symbol at the
head of a list. This means that a pattern like `"^foo"` will match all these
candidates: `foobar`, `my.ns/foobar`, and `#'foobar`.

Each candidate word is tested for special treatment in this order:

1. Return true if word is literally in `'lispwords'`
2. Return false if word matches a pattern in `g:clojure_fuzzy_indent_blacklist`
3. Return true if word matches a pattern in `g:clojure_fuzzy_indent_patterns`
4. Return false and indent normally otherwise

#### `g:clojure_special_indent_words`

Some forms in Clojure are indented so that every subform is indented only
two spaces, regardless of `'lispwords'`. If you have a custom construct that
should be indented in this idiosyncratic fashion, you can add your symbols to
the default list below.

```vim
" Default
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn'
```

#### `g:clojure_align_multiline_strings`

Align subsequent lines in multiline strings to the column after the opening
quote, instead of the same column.

For example:

```clojure
(def default
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
  eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
  enim ad minim veniam, quis nostrud exercitation ullamco laboris
  nisi ut aliquip ex ea commodo consequat.")

(def aligned
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
   eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
   enim ad minim veniam, quis nostrud exercitation ullamco laboris
   nisi ut aliquip ex ea commodo consequat.")
```

This option is off by default.

```vim
" Default
let g:clojure_align_multiline_strings = 0
```

#### `g:clojure_align_subforms`

By default, parenthesized compound forms that look like function calls and
whose head subform is on its own line have subsequent subforms indented by
two spaces relative to the opening paren:

```clojure
(foo
  bar
  baz)
```

Setting this option changes this behavior so that all subforms are aligned to
the same column, emulating the default behavior of clojure-mode.el:

```clojure
(foo
 bar
 baz)
```

This option is off by default.

```vim
" Default
let g:clojure_align_subforms = 0
```


## Want to improve your Clojure development set up?

Be sure to check out our list of [suggested Vim plugins in the
Wiki](https://github.com/clojure-vim/clojure.vim/wiki/Plugins).


## Contribute!

Pull requests are welcome!  Make sure to read the
[`CONTRIBUTING.md`](CONTRIBUTING.md) for useful information.


## Acknowledgements

[Clojure.vim][] is a continuation of [vim-clojure-static][].
_Vim-clojure-static_ was created by [Sung Pae](https://github.com/guns).  The
original copies of the packaged runtime files came from [Meikel
Brandmeyer](http://kotka.de/)'s [VimClojure][] project with permission.

Thanks to [Tim Pope](https://github.com/tpope/) for advice in
[#vim](https://www.vi-improved.org/).


## License

Clojure.vim is licensed under the [Vim
License](http://vimdoc.sourceforge.net/htmldoc/uganda.html#license) for
distribution with Vim.

- Copyright © 2013–2017, Sung Pae. <self@sungpae.com>
- Copyright © 2008–2012, Meikel Brandmeyer. <mb@kotka.de>
- Copyright © 2007–2008, Toralf Wittner. <toralf.wittner@gmail.com>

See [LICENSE](https://github.com/clojure-vim/clojure.vim/blob/master/LICENSE)
for more details.


<!-- Links -->

[vim]: https://www.vim.org
[clojure.vim]: https://github.com/clojure-vim/clojure.vim
[vim-clojure-static]: https://github.com/guns/vim-clojure-static
[vimclojure]: https://www.vim.org/scripts/script.php?script_id=2501
[clojure]: https://clojure.org

<!-- vim: set tw=79 : -->
