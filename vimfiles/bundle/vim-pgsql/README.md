Vim Postgresql syntax file
===============

* Language:     pgsql
* Maintainer:   Devrim GUNDUZ <devrim@PostgreSQL.org>
* Contributors: Jacek Wysocki, Ryan Delaney <ryan.delaney@gmail.com>
* Last Change:  $Fri May 23 09:55:21 PDT 2014$
* Filenames:    *.pgsql *.plpgsql
* URL:			https://github.com/exu/pgsql.vim

Installation
---

If you use [Pathogen](https://github.com/tpope/vim-pathogen) for plugin
management, simply clone this repository into `~/.vim/bundle` to make the
`pgsql` syntax type available:

    git clone https://github.com/exu/pgsql.vim ~/.vim/bundle/pgsql.vim

If you prefer to install manually, copy `syntax/pgsql.vim`  into your
`~/.vim/syntax/` directory, creating it if necessary.

Usage
---

Files with the suffix `.pgsql` will use the pgsql highlighting automatically.

To enable `pgsql` syntax for any open buffer, use:

    :set syntax=pgsql

Enabling for all .sql files
---

You can make `pgsql.vim` the default for SQL syntax by adding this line to your
`.vimrc`:

    let g:sql_type_default = 'pgsql'

This tells the `sql.vim` module to use the `pgsql` dialect.

Alternately, you can use an autocmd in your `~/.vim/filetype.vim` to enable it
for all `.sql` files or some finer pattern:

    autocmd BufNewFile,BufRead *.sql setf pgsql

You do not need both. If in doubt, use the first method.
