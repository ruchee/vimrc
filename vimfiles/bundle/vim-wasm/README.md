WebAssembly Text Format Syntax Support for Vim
==============================================

[vim-wasm][] provides a [WebAssembly][] filetype support; syntax, filetype detection, and indentation.
This filetype plugin defines highlights based on [WebAssembly text format spec][].

Now this plugin is included in [official Vim release][vim runtime dir]. This repository is for development.
Unless you want to use HEAD version, you don't need to install this plugin.

[vim-wasm]: https://github.com/rhysd/vim-wasm
[WebAssembly]: https://webassembly.org/
[WebAssembly text format spec]: https://webassembly.github.io/spec/core/text/index.html
[vim runtime dir]: https://github.com/vim/vim/tree/master/runtime


## Installation and Usage

Copy `ftdetect`, `ftplugin`, `indent` and `syntax` directories into your `~/.vim` directory or use your
favorite plugin manager to install.
This plugin automatically detects `*.wast` files as WebAssembly text format.

| `set filetype=wast` | `set filetype=lisp` |
|---------------------|---------------------|
| <img width="515" alt="wast" src="https://raw.githubusercontent.com/rhysd/ss/master/vim-wast/wast.png"> | <img width="515" alt="wast" src="https://raw.githubusercontent.com/rhysd/ss/master/vim-wast/lisp.png"> |


## License

MIT License.

    Copyright (c) 2016 rhysd

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
    PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
    THE USE OR OTHER DEALINGS IN THE SOFTWARE.

