Vim gitignore plugin
====================

* This plugin provides syntax highlighting and up-to-date code snippets for `.gitignore` file.
* The snippets are based on github's [gitignore][7] files.


Requirements
------------

* For using code snippets you should install
[snipMate][1], [neosnippet][2], or [ultisnips][3] first.


Installation
------------

* Using [vundle][4], or [neobundle][5], or other options.

* [vundle][4]

    ```VimL
     Plugin 'gisphm/vim-gitignore'
    ```

* [neobundle][5]

    ```VimL
     NeoBundle 'gisphm/vim-gitignore'
    ```


[Snippets](snippets/gitignore) List
-----------------------------------

* See the list in [List](List.md).

* If you find that the snippets are not keeping up with the [gitignore][7] repo,
you can manually execute the script [rebuild-snippets](rebuild-snippets)
to rebuild snippets.


Snippets Usage
--------------

* `vim-gitignore` contains a set of snippets. To use snippets,
just open your `.gitignore` file, type in proper snippet name
(for example, `Python`, or `Vim`)
and press `<Tab>` key or other key defined in your vimrc,
which will expand snippets.


TODO
----

* [x] add indent file
* [ ] add more highlighting in syntax file
    + [x] basic highlight
    + [ ] self-defined color

Alternative Option
--------------

You can also follow [gitignore.io][6] to generate `.gitignore` file in your project.


Contributions
-------------

Any issue or pull request is welcome.


License
--------
> Copyright 2015 gisphm <phmfk@hotmail.com>
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>> http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.


[1]: https://github.com/garbas/vim-snipmate
[2]: https://github.com/Shougo/neosnippet.vim
[3]: https://github.com/SirVer/ultisnips
[4]: https://github.com/gmarik/vundle
[5]: https://github.com/Shougo/neobundle.vim
[6]: https://www.gitignore.io
[7]: https://github.com/github/gitignore
