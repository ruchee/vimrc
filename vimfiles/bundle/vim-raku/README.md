# vim-raku
Improved support for Raku in Vim.

## Installation

Once installed, all files ending in `.raku`, `.rakumod`, `.rakudoc`,
and `.rakutest` (and also `.pl6`, `.pm6`, `.p6`, and `.t6` for legacy
purposes) will make use of the plugin's features.

### With a plugin manager

Installation of this plugin can be done via any of the available plugin
managers, such as [Pathogen][pathogen], [vim-plug][vim-plug], [vundle][vundle]
or any other one.

### Using Vim's built-in package management

Clone this repository in vim autoload packages directory:
```bash
$ # installation
$ git clone https://github.com/Raku/vim-raku.git ~/.vim/pack/vim-raku-pack/start/vim-raku

$ # testing
$ vim /tmp/example.raku +'syntax' +'q'
```

For more details follow those instructions:
```bash
$ vim +'help packages' +'only'
```

## Configuring optional features
Not all features are enabled by default. This is in part due to them still
being considered in testing, or because they may influence your regular
workflow too much. These can be enabled by setting a certain variable to a
truthy value.

### Unicode ops
`vim-raku` can use Vim's abbreviation feature to convert ASCII based operators
to their Unicode equivalents on the fly. To enable this feature, add the
following line to your `vimrc` file:

```vim
let g:raku_unicode_abbrevs = 1
```

[pathogen]: https://github.com/tpope/vim-pathogen
[vim-plug]: https://github.com/junegunn/vim-plug
[vundle]: https://github.com/gmarik/Vundle.vim

## License

This project is available under the [MIT License](LICENSE.md).
