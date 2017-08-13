# vim-js-pretty-template

A Vim plgin to highlight JavaScript's [Template Strings](http://tc39wiki.calculist.org/es6/template-strings/) contents in other FileType syntax rule which you want.

```js
var htmlTempl = `
<div class="row">
  <div class="col-md-12">
    <span>{{ctrl.message}}</span>
  </div>
</div>
`;
```

![capture](screencast01.gif)

Template Strings is available with [Babel](https://babeljs.io/), [google/traceur-compile](https://github.com/google/traceur-compiler) and [TypeScript](http://www.typescriptlang.org/).

## How to install 

### Vundle

Place this in your `.vimrc`:

```vim
Plugin 'Quramy/vim-js-pretty-template'
```

then run the following in Vim:

```vim
:source %
:PluginInstall
```

### NeoBundle

```vim
NeoBundle 'Quramy/vim-js-pretty-template'
```

then run the following in Vim:

```vim
:source %
:NeoBundleInstall
```

### Pathogen
Run the following in a terminal:

```sh
cd ~/.vim/bundle
git clone https://github.com/Quramy/vim-js-pretty-template
```

## Usage

This plugin provides the `:JsPreTmpl` command.  For example:

```vim
:JsPreTmpl html
```

Executing the above, a template string is highlighted with HTML way.

This command requires an argument. It's a `FileType` name which you can apply into templates in your JavaScript code.

If you want to apply automatically, you can append the following to your `.vimrc`:

```vim
autocmd FileType javascript JsPreTmpl html
```

### Tagged Template Literal
You can override the default rule defined `:JsPreTml` command with another rule using `jspretmpl#register_tag()` function. For example,

```vim
" Register tag name associated the filetype
call jspretmpl#register_tag('gql', 'graphql')

autocmd FileType javascript JsPreTmpl html
```

Then your JavaScript codes are Highlighted as the following:

```javascript
// HTML way default
const template = `
  <div>html</div>
`;

// GraphQL way if gql tagged
const query = gql`
  fragment on User {
    name
  }
`;
```

### For alternative JavaScript users

vim-js-pretty-template is also compatible for TypeScript, Dart and CoffeeScript.

* TypeScript
* Dart
* CoffeeScript

For example:

```vim
autocmd FileType typescript JsPreTmpl markdown
autocmd FileType typescript syn clear foldBraces " For leafgarland/typescript-vim users only. Please see #1 for details.
```

then the following template string is highlighted:

```typescript
var tmpl: string = `
## Title
*Highlighted in Markdown way.*
`;
```

or for example:

```vim
autocmd FileType dart JsPreTmpl xml
```

then: 

```dart
var tmpl = """
<!-- highlighted in XML way -->
<svg:svg xmlns:svg="http://www.w3.org/2000/svg">
  <svg:circle cx="100" cy="100" r="50"></svg:circle>
</svg:svg>
""";
```

## License
This plugin is released under the MIT license, see `LICENSE.txt`.

