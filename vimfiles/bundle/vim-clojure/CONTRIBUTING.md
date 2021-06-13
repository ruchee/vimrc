# Contributing

A large portion of the syntax file is generated using Clojure code in the
`clj/` directory.  Generation of Vim code in this fashion is preferred over
hand crafting of the same.

There is an incomplete syntax test suite in `clj/test/`.  Any additions and
improvements to these tests are highly appreciated.

To contribute to Clojure.vim you will need [Leiningen][].


## Update syntax files

When a new Clojure version is released, perform the following steps to update
the syntax files to add syntax highlighting for new functions, macros and
special forms.

```
$ cd clj/
$ lein repl
> (require 'vim-clojure-static.generate)
> (ns vim-clojure-static.generate)
> (update-project! "..")
```

### Update Unicode syntax

Update the file used to generate the Unicode character classes highlighted in Clojure
regex strings.

```sh
cd clj/
./bin/update-unicode
```

Then update the syntax files using the steps in the previous section.


## Run tests

Run the test suite using this command:

```
lein test
```


## Submit latest changes to upstream Vim

**Note** this should be done only by the Clojure.vim maintainers, this is here
to serve as a reminder on how to do it.

```
$ cd clj/
$ lein repl
> (require 'vim-clojure-static.generate)
> (ns vim-clojure-static.generate)
> (update-vim! ".." "../../vim")
```

Open PR in official [Vim repository][], this will automatically send an email
to the [Vim-dev mailing list](https://www.vim.org/maillist.php#vim-dev) and run
the CI tests.

More information on how Vim's contribution process works can be found here:
<https://github.com/vim/vim/blob/master/CONTRIBUTING.md>

Neovim periodically pulls runtime patches from Vim.


[Vim repository]: https://github.com/vim/vim
[Leiningen]: https://leiningen.org/#install
