# Contributing to vim-rescript

## Development

- Clone the repo
- `npm install` dependencies
- `make test` to run the tests

**Working within VIM**

First make sure to register your locally checked out vim-rescript project as a plugin within your vim configuration:

```vim
" vim-plug
Plug ~/Projects/vim-rescript
```

- Run `:PlugInstall` (you'll not see the plugin in the interactive vim-plug list, because it is a local project)
- You can open and edit functionality in any plugin file. After any changes, just run `:so %` in the same buffer to source the current file, then proceed to do your manual tests

**Integration Specs:**
For all the informal specs about editor integration & the ReScript platform, check out the [CONTRIBUTING](https://github.com/rescript-lang/rescript-vscode/blob/master/CONTRIBUTING.md) file of the rescript-vscode reference implementation.

### Use custom rescript-editor-support.exe

> Note: Don't do this as a ReScript user. This is only intended for extension development purposes only.
> We won't consider bug reports that are caused by custom editor-support setups.

We are currently using a forked version of RLS to be able to do type-hinting (without using an LSP client actually). To build the binary, do the following:

```bash
cd ~/Projects

git clone https://github.com/rescript-lang/rescript-editor-support.git

# You will need esy to build the project
esy
```

After a successful build, you will find a binary at path `_esy/default/build/install/default/bin/Bin`. To make things easier, we will symlink it:

```bash
cd ~/Projects/rescript-editor-support
ln -s _esy/default/build/install/default/bin/Bin bin.exe
```

Now open your `vimrc` file and add following line:

```vim
let g:rescript_editor_support_exe = "~/Projects/reason-language-server/bin.exe"
```

#### Testing the local setup

That's it! Now you should be able to use `RescriptTypeHint` / omnicompletion on a `.res` file:

- Within a ReScript project, create a new `myfile.res`
- Add `let a = ""`
- Move your cursor above the empty string `""`
- Type `:RescriptTypeHint`. A preview window will open to show the type information



## Vendoring a new rescript-vscode version

We are currently vendoring rescript-vscode to provide all the binaries + LSP for our coc-vim setup.

First, `curl` the tagged zip bundle from the `rescript-vscode` GH releases:

```
curl -L https://github.com/rescript-lang/rescript-vscode/releases/download/1.1.1/rescript-vscode-1.1.1.vsix -o rescript-vscode-1.1.1.zip
```

Unzip it and replace the `rescript-vscode` directory. Like this:

```
unzip rescript-vscode-1.1.1.zip -d rescript-vscode-1.1.1
rm -rf server

mv rescript-vscode-1.1.1/extension/server server
```

Lastly:

- Check in the changes and push to a working branch
- Do a last sanity check, update your `PlugInstall` to point to the newly created branch. Run `PlugUpdate` and check if the LSP / vim setup works as expected
- Run `:RescriptInfo` and check if the output reflects all the changes
- To wrap up, merge the branch, update CHANGELOG, push a new tag
