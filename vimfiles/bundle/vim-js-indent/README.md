vim-js-indent
=============

Vim indenter for standalone and embedded JavaScript and TypeScript.

Introduction
------------

This is an indenter for standalone and HTML-embedded JavaScript. It's based on
Preston Koprivica's
[JavaScript Indent plugin](http://www.vim.org/scripts/script.php?script_id=1840),
but it uses the current HTML indenter from Vim 7.4 and has a few other minor
modifications.

Installation
------------

### Vundle

1. Add `Bundle 'jason0x43/vim-js-indent'` to your `.vimrc`
1. Restart vim
1. Run `:BundleInstall`

### Pathogen

Clone `https://github.com/jason0x43/vim-js-indent.git` into your bundles
directory (`~/.vim/bundle`).

Configuration
-------------

<dl>
<dt><code>js_indent_flat_switch</code></dt>
<dd>Boolean, default=0<br>
Set to 1 to make `case` statements align with their containing `switch`.</dd>
<dt><code>js_indent_logging</code></dt>
<dd>Boolean, default=0<br>
Set to 1 to enable logging comments (useful for debugging).</dd>
<dt><code>js_indent_typescript</code></dt>
<dd>Boolean, default=1<br>
Set to 0 to disable use of the JavaScript indenter for TypeScript buffers.</dd>
</dl>

License
------
Copyright © 2014 Jason Cheatham. Distributed under the same terms as Vim
itself. See `:help license`.
