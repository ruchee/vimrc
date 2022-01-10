# Changelog

## master

## 2.1.0

**Improvements:**
- Add syntax highlighting for int polyvariants
- Improved syntax highlighting for string interpolation ([#44](https://github.com/rescript-lang/vim-rescript/pull/44))
- Updated vendored rescript-vscode server to `1.1.3` (see changes introduced in [1.1.2](https://github.com/rescript-lang/rescript-vscode/blob/master/CHANGELOG.md#112) and [1.1.3](https://github.com/rescript-lang/rescript-vscode/blob/master/CHANGELOG.md#113))
  - Rename support for let-binding, types, record labels, module names etc
  - **Important:** If you are using coc.nvim, make sure to upgrade to coc's latest `release` version first, otherwise renaming will not work across files!
  - Jump to type definition support
  - New jump-to-definition behavior when having `.res` and `.resi` files in place

## 2.0.1

- Fixes an issue where `:RescriptTypeHint`, `:RescriptUpgrade` fail with an 127 exit code (due to missing analysis binaries)
- Fixes `coc` server crashing issues

## 2.0.0

**Improvements:**

- Add support for the new `rescript` npm package. This version will handle both, `bs-platform` and `rescript` projects.
  - Tip: Running `:RescriptInfo` will show you if the plugin is running in `legacy` (`bsc.exe` / `bsb.exe`) or `modern` (`rescript.exe`) mode
- Updated vendored rescript-vscode server to `1.1.1` (see changes [here](https://github.com/rescript-lang/rescript-vscode/blob/master/CHANGELOG.md#110))

**Breaking Changes:**

We slimmed down our vendored LSP code and changed the binary paths. Within your `CocConfig`, change your server path accordingly:

```diff
"languageserver": {
  "rescript": {
    "enable": true,
-   "module": "~/.config/nvim/plugged/vim-rescript/rescript-vscode/extension/server/out/server.js",
+   "module": "~/.config/nvim/plugged/vim-rescript/server/out/server.js",
    "args": ["--node-ipc"],
    "filetypes": ["rescript"],
    "rootPatterns": ["bsconfig.json"]
  }
}
```

We also renamed the following plugin commands:

- `:RescriptBuildWorld` to `:RescriptBuildWithDeps`
- `:RescriptCleanWorld` to `:RescriptBuildWithDeps`

## 1.4.0

**Improvements:**

- Upgrade to `rescript-vscode@1.7.0` (see changes [here](https://github.com/rescript-lang/rescript-vscode/blob/master/CHANGELOG.md#107))
  - Includes autocompletion for `->` / `~` (labeled arguments).

Example:

```res
let l = Belt.List.make(3, 1)
l-> //starting here, you will get suggestions for functions that accept a Belt.List.t

let test = (~name: string, ~age: int) => {
  Js.log2(name, age)
}

test(~ // starting here, you will get suggestions for `name` and `age`
```

- Improved syntax highlighting for polyvariants [#31](https://github.com/rescript-lang/vim-rescript/pull/31)

**Bugfixes:**

- Fix an issue that causes the plugin to change cwd for the whole vim instance (e.g. in a monorepo setup) [#32](https://github.com/rescript-lang/vim-rescript/pull/32)

## 1.3.0

**Improvements:**

- Upgrade to `rescript-vscode@1.6.0` (see changes [here](https://github.com/rescript-lang/rescript-vscode/blob/master/CHANGELOG.md#106))
  - Improved type-hinting, and better Windows support for our LSP service

**Bugfixes:**

- Fix a syntax error in autocmd file pattern which caused weird error on buffer reload
- Fix highlighting of nested multiline comments

## 1.2.0

**Improvements:**

- Upgrade to `rescript-vscode@1.4.0` (see changes [here](https://github.com/rescript-lang/rescript-vscode/blob/1.0.4/HISTORY.md#104))
- Add proper monorepo support (`e.g. yarn workspaces`)
  - Detects `bsb` / `bsc` correctly for each file separately and finds the right (sub-)project context
  - Heuristic for detecting the binaries: For the current file, find the nearest `node_modules/bs-platform` folder for the binaries
  - Adds an `augroup RescriptAutoProjectEnv` that sets the environment on every `.res` / `.resi` related read / write / new file event
  - Will also update the environment on each `format` and `build` call to make it sync up for all non-rescript buffers
  - On each env update, it updates the local working directory to the updated project root path as well
- Add new commands `:RescriptBuildWorld` and `:RescriptCleanWorld` for cleaning / building all sources + dependencies

**Bugfixes:**

- Fixes issue with long template strings breaking the syntax highlighting
- Fixes an issue where `:RescriptBuild` would fail in non-rescript buffers due to a wrongly scoped script variable (was buffer only)

## 1.1.0

**Improvements:**

- Add detected `rescript-vscode` plugin version for `:RescriptInfo`
- Upgrades to `rescript-vscode-1.0.1` with improved editor-support ([see changelog](https://github.com/rescript-lang/rescript-editor-support/blob/master/Changes.md#release-101-of-rescript-vscode))
- Improved syntax highlighting for ReScript decorators

**Breaking Changes:**

- Moved `rescript-vscode-1.0.0` to `rescript-vscode` (make sure to update your coc-vim config to point to the new path!)


## 1.0.1

- Fixes installation issues that required an additional `npm install` in the rescript-vscode vendor folder

## 1.0.0 "Zero Config"

- Vendor rescript-vscode-1.0.0 lsp + rescript-editor-support
- Fixes issue where certain compiler versions could not be detected (x.x.x-dev.y)
- `:RescriptInfo` now outputs more relevant executable paths for debugging purposes
- Updates `rescript#Complete()` to handle the new data format introduced in editor-support 1.0.0

**Breaking Changes:**

- `g:rescript_type_hint_bin` renamed to `g:rescript_editor_support_exe`
- `g:resc_command` renamed to `g:rescript_compile_exe`
- `g:resb_command` renamed to `g:rescript_build_exe`


## 0.0.0 "Experimental"
