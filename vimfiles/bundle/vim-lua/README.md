vim-lua
=======

Improved Lua 5.3 syntax and indentation support for Vim.

Highlighting: stock Vim (left) vs. this plugin (right):
![](http://tbastos.github.io/i/vim-lua-syntax.jpg)
_(Color scheme: solarized dark; Font: Anonymous Pro)_

Configuration
-------------

vim-lua supports a few options. All are boolean and off by default: set an
option to 1 with `let g:lua_syntax_someoption = 1` to enable it (setting to `0`
for disabling is currently not supported -- just don't set the option at all to
keep it disabled).

- `g:lua_syntax_nosymboloperator` disables highlighting of the Lua symbol
  operators (that is, all operators except for the keyword operators `and` and
  `or`).
- `g:lua_syntax_fancynotequal` enables displaying Lua's `~=` operator with
  Unicode character `≠`.
- `g:lua_syntax_nofold` disables code folding. If this option is set, you can
  selectively reenable some folds with `g:lua_syntax_fold_<group>` where
  `<group>` can be table, comment, function, control or string. You can also
  have even more fine-grained control by enabling specific syntax regions
  instead of groups. Look in the source code for the second argument of `call
  s:FoldableRegion` for all options, but note that the names are subject to
  change in later versions.
- `g:lua_syntax_nostdlib` disables highlighting of all standard library names
  like, `io`, `print` and even `require`, `error` etc. If that is too much you
  can use `g:lua_syntax_noextendedstdlib` instead to keep `_G`, `module`,
  `require`, `assert`, `error`, `pcall` and `xpcall` highlighted.


Contributing
------------

Contributions are welcome! Please follow the existing code style and in your
pull request explain the reason for the proposed change and how it is valuable.

Credits
-------

The indent code was based off [this gist](https://gist.github.com/bonsaiviking/8845871).

Various configuration options were contributed by Christian Neumüller
(@Oberon00).
