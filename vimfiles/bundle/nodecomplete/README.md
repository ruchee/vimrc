vim-nodejs-complete
===================

Nodejs `'omnifunc'` function of vim

Support node's builtin module's method&property completion(`ctrl-x ctrl-o`) in `js` file with preview.


## Install
Download the [tarball](https://github.com/myhere/vim-nodejs-complete/zipball/master) and extract to your vimfiles(`~/.vim` or `~/vimfiles`) folder.

Completion require `:filetype plugin on`, please make sure it's on.

## Settings
All the settings is optional.

```viml
let g:nodejs_complete_config = {
\  'js_compl_fn': 'jscomplete#CompleteJS',
\  'max_node_compl_len': 15
\}
```

### js_compl_fn
* `String` function name, called to complete javascript
* default to `javascriptcomplete#CompleteJS`
* if you want to integrate with [jscomplete](https://github.com/teramako/jscomplete-vim), set it to `jscomplete#CompleteJS`, make sure it's installed.

### max_node_compl_len
* `Number`, length of nodejs complete items. set to 0 if unlimited


## Example

```js
var fs = req
// then hit ctrl-x_ctrl-o you'll get:
var fs = require(
// and then hit ctrl-x_ctrl-o again you'll get module name completion

var fs = require('f
// then hit ctrl-x_ctrl-o

fs.
// then hit ctrl-x_ctrl-o

proc
// then hit ctrl-x_ctrl-o

process.ex
// then hit ctrl-x_ctrl-o
```


## Tip
1. Close the method preview window

     `ctrl-w_ctrl-z` or `:pc`.

     If you want close it automatically, put the code(from [spf13-vim](https://github.com/spf13/spf13-vim/blob/3.0/.vimrc)) below in your `.vimrc` file.

     ```vim
     " automatically open and close the popup menu / preview window
     au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
     ```

2. About vim's complete

     Vim supports several kinds of completion, `:h ins-completion` for help.

3. Completion of module in `node_modules` will cache result of every `js` file

     If you modified module in `node_modules` directory, use code below to clear the cache.

     ```vim
     :unlet b:npm_module_names
     ```


## Feedback
[feedback or advice or feature-request](https://github.com/myhere/vim-nodejs-complete/issues)

