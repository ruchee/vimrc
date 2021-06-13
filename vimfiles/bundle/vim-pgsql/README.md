# Vim PostgreSQL syntax plugin

![pgsql syntax highlighting](https://raw.github.com/lifepillar/Resources/master/pgsql/pgsql.png)

This plugin provides syntax highlighting and auto-completion support for
PostgreSQL version 9.6 or above and for some of its extensions, including:

-   PL/pgSQL;
-   [PostGIS](http://postgis.net) (including PostGIS Topology);
-   [pgRouting](http://pgrouting.org);
-   [pgTap](http://pgtap.org).

Besides, syntax highlighting for any language may be activated inside functions
(see below).

# Installation

If your Vim supports packages (`echo has('packages')` prints `1`), I strongly
recommend that you use them. Just clone this repo inside `pack/*/start`, e.g.,

```bash
mkdir -p ~/.vim/pack/plugins/start
git clone https://github.com/lifepillar/pgsql.vim.git ~/.vim/pack/plugins/start/pgsql
```

Otherwise, if you don't have a preferred installation method, I recommend
installing [Pathogen](https://github.com/tpope/vim-pathogen), and then simply
copy and paste:

```bash
cd ~/.vim/bundle
git clone https://github.com/lifepillar/pgsql.vim.git
```

# Usage

**For thorough documentation, see `:h pgsql.txt`.**

Files with a `.pgsql` suffix are highlighted out of the box. If you want to
highlight `.sql` files using this plugin by default, add this to your `.vimrc`
(see `:h ft_sql.txt`):

```vim
let g:sql_type_default = 'pgsql'
```

Alternatively, after loading a `.sql` file use this command:

```vim
:SQLSetType pgsql.vim
```

To set the file type in new buffers use:

```vim
:let b:sql_type_override='pgsql' | set ft=sql
```

Code between `$pgsql$` or `$$` pairs is treated as PL/pgSQL and highlighted
accordingly:

![PL/pgSQL snippet](https://raw.github.com/lifepillar/Resources/master/pgsql/plpgsql.png)

You may set `g:pgsql_pl` to a list of file types to be used in user-defined
functions. For example, after setting:

```vim
let g:pgsql_pl = ['python']
```

code between `$python$` pairs will be highlighted as Python:

![PL/Pythonu snippet](https://raw.github.com/lifepillar/Resources/master/pgsql/plpython.png)

# Hacking

The syntax file is generated automatically. If you want to hack it, edit
`src/pgsql.sql`, then execute:

```sh
cd ./src
DBUSER=<user> DBHOST=<hostname> make install
```

When `DBUSER` is omitted, `postgres` is assumed. When `DBHOST` is omitted,
`127.0.0.1` is assumed.

The specified user must be a superuser, because the script will create
a database called `vim_pgsql_syntax` and a few extensions that require admin
privileges. It will then proceed to extract all the keywords. Feel free to
browse the source scripts to see exactly what they do.

The above command will update `syntax/pgsql.vim`. Use `make distclean` to drop
the database (or drop it manually).

The script has been tested in macOS, but it should work on any \*nix system.

# Acknowledgments

This plugin was originally based on code from
[space::tekk](https://github.com/spacetekk/pgsql.vim) (and completely
rewritten).
