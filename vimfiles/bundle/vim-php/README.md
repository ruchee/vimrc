php.vim
=======

An up-to-date Vim syntax for PHP.

_This project is a fork of [php.vim--Garvin][php.vim-garvin] which in turn is an update of the [php.vim][php.vim-original] script which in turn is an updated version of the php.vim syntax file distributed with Vim. **Whew!**_

Installation
------------

If you don't have a preferred installation method, [vim-plug] is quick and simple. With [vim-plug installed], add the following to your `vimrc` (if you are a NeoVim user, see the [FAQ page][neovim-faq] for help):

```vim
Plug 'StanAngeloff/php.vim'
```

If you are using Git, please be aware [the original repository this project was forked from][php.vim-garvin] contains bad timezone in some of the commits. You'll need to fetch with `fsckObjects` disabled:

```sh
git clone -c fetch.fsckObjects=false git@github.com:StanAngeloff/php.vim.git
```

Unless you specify `-c fetch.fsckObjects=false` as an option to `git clone`, you may see `badTimezone: invalid author/committer line - bad time zone` in the output and the cloning will [fail](https://github.com/StanAngeloff/php.vim/issues/96).

Configuration
-------------

`php.vim` comes with sensible defaults for most use cases. Below is a list of some interesting configuration options you may tweak. Refer to the [source code][php.vim-source] for additional options.

- `g:php_version_id`,
  `b:php_version_id`
  
  Default: `g:php_version_id = 70300`
  
  The PHP version the Vim syntax should adhere to. This currently determines how strict [Heredoc/Nowdoc syntax](https://www.php.net/manual/en/migration73.new-features.php#migration73.new-features.core.heredoc) should be. The format of the Vim variable follows the [PHP predefined constant `PHP_VERSION_ID`](https://www.php.net/manual/en/reserved.constants.php#constant.php-version-id).

- `g:php_syntax_extensions_enabled`, `g:php_syntax_extensions_disabled`  
  `b:php_syntax_extensions_enabled`, `b:php_syntax_extensions_disabled`

  Default: `g:php_syntax_extensions_enabled = ["bcmath", "bz2", "core", "curl", "date", "dom", "ereg", "gd", "gettext", "hash", "iconv", "json", "libxml", "mbstring", "mcrypt", "mhash", "mysql", "mysqli", "openssl", "pcre", "pdo", "pgsql", "phar", "reflection", "session", "simplexml", "soap", "sockets", "spl", "sqlite3", "standard", "tokenizer", "wddx", "xml", "xmlreader", "xmlwriter", "zip", "zlib"]`

  A list of PHP extension names (lowercase) for which highlighting of built-in functions, constants, classes and interfaces is enabled / disabled.

  If you are **not** interested in highlighting **any** built-in functions/constants/etc., set `g:php_syntax_extensions_enabled` to an empty list `[]`.

  If you are **not** interested in highlighting built-in functions/constants/etc. for a subset of PHP extensions, set `g:php_syntax_extensions_enabled` to a list of extensions you wish to disable, e.g., `["mcrypt"]`.

- `php_var_selector_is_identifier`

  Default: `0`

  Set this to a truthy value (e.g., `1`) to include the dollar sign `$` as part of the highlighting group for a PHP variable.

- `php_html_load`, `php_html_in_heredoc`, `php_html_in_nowdoc`

  Default: `1` (NOTE: setting `php_html_load` to a truthy value takes precedence and overrides both `php_html_in_heredoc` & `php_html_in_nowdoc`)

  Set to a falsy value (e.g., `0`) to disable embedding HTML in PHP. Doing so may yield significant speed-ups of syntax highlighting.

  This should not affect HTML highlighting in templating languages, such as [Blade].

- `php_sql_query`, `php_sql_heredoc`, `php_sql_nowdoc`

  Default: `1`

  Set to a falsy value (e.g., `0`) to disable SQL syntax in PHP. Doing so may yield moderate speed-ups of syntax highlighting.

### Projects of interest

`php.vim` pairs nicely with:

- [`phpfolding.vim`](https://github.com/rayburgemeestre/phpfolding.vim): _Automatic folding of PHP functions, classes, … (also folds related PhpDoc)_
- [`PHP-Indenting-for-VIm`](https://github.com/2072/PHP-Indenting-for-VIm): _The official VIm indent script for PHP_

### Overriding highlighting

Syntax highlighting can be controlled at a fine-grained level. For example, all text in PHP comments is highlighted as `phpComment` by default, however there are smaller syntax groups you can tweak, e.g., how PHPDoc `@tags` appear. There are [several syntax groups you can choose from](syntax-groups).

Example: Overriding PHP `@tags` and `$parameters` in comments to appear as a different highlighting group, giving them distinct colouring:

```vim
" Put this function at the very end of your vimrc file.

function! PhpSyntaxOverride()
  " Put snippet overrides in this function.
  hi! link phpDocTags phpDefine
  hi! link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END
```

#### Snippets

You may add the code snippets below to your `PhpSyntaxOverride` function (see above for instructions on how to create this function).

##### Colourising namespace separator in `use`, `extends` and `implements`

If you wish to highlight the namespace separator `\` differently ([original request](https://github.com/StanAngeloff/php.vim/issues/63)):

```vim
hi phpUseNamespaceSeparator guifg=#808080 guibg=NONE gui=NONE
hi phpClassNamespaceSeparator guifg=#808080 guibg=NONE gui=NONE
```

##### Colourising parentheses

If you wish to highlight `(` and `)` differently ([original request](https://github.com/StanAngeloff/php.vim/issues/31#issuecomment-52879108)):

```vim
syn match phpParentOnly "[()]" contained containedin=phpParent
hi phpParentOnly guifg=#f08080 guibg=NONE gui=NONE
```

Developing
----------

When you install `php.vim` using your preferred installation method, all the needed files are already in place.

If you wish to rebuild the syntax file with a more recent version of PHP available on the [Docker Hub], you should use the provided `Dockerfile` to do so:

```bash
docker build --no-cache --force-rm -f attic/Dockerfile -t php.vim .
cat syntax/php.vim | docker run --rm -i php.vim > syntax/php.vim.new
docker rmi php.vim
mv syntax/php.vim.new syntax/php.vim
```

NOTE: If the updated syntax file fails to load and is corrupted, try loading `syntax/php.vim` in your favourite editor and ensure line endings are set to Unix `\n`.


  [php.vim-garvin]:  https://github.com/vim-scripts/php.vim--Garvin
  [php.vim-original]: http://www.vim.org/scripts/script.php?script_id=2874
  [vim-plug]: https://github.com/junegunn/vim-plug
  [vim-plug installed]: https://github.com/junegunn/vim-plug#installation
  [neovim-faq]: https://github.com/neovim/neovim/wiki/FAQ#where-should-i-put-my-config-vimrc
  [php.vim-source]: https://github.com/StanAngeloff/php.vim/blob/master/syntax/php.vim#L35
  [Blade]: https://github.com/jwalton512/vim-blade
  [syntax-groups]: https://github.com/StanAngeloff/php.vim/blob/41c36f7f/syntax/php.vim#L804
  [Docker Hub]: https://docs.docker.com/samples/library/php/
