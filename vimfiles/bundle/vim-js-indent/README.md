vim-js-indent
=============

Vim indenter for standalone and embedded JavaScript and TypeScript.

Installation
------------

### vim-plug

1. Add `Plug 'jason0x43/vim-js-indent'` to your `.vimrc`
1. Restart vim
1. Run `:PlugInstall`

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
Copyright © 2014-2016 Jason Cheatham. Distributed under the same terms as Vim
itself. See `:help license`.
