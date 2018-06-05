# vim-ocaml

Vim runtime files for OCaml. These are the default runtime files plus:

* Markus Mottl's improvments from his vim files repo
* `:Opam` command to switch compilers from inside of vim
* Syntax highlighting for `_oasis`, `_tags`, and `opam` files.

## Installation

Depending on your plugin manager:

```
Plugin 'rgrinberg/vim-ocaml'
```

```vim
NeoBundleInstall 'rgrinberg/vim-ocaml'

" or use NeoBundleLazy
NeoBundleLazy 'rgrinberg/vim-ocaml', {'autoload' : {'filetypes' :
    \ ['ocaml', 'jbuild', 'opam', 'oasis', 'omake', 'ocamlbuild_tags', 'sexplib']}}
```

