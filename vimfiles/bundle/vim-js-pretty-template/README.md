# vim-js-pretty-template

A Vim plugin to highlight JavaScript's [Template Strings](http://tc39wiki.calculist.org/es6/template-strings/) contents in other `FileType` syntax rule which you want.

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

## Vim 8 native plugins

Replace "FOOBAR" with any directory name that you like:

```
$ mkdir -p ~/.vim/pack/FOOBAR/start/
$ git clone https://github.com/Quramy/vim-js-pretty-template.git ~/.vim/pack/FOOBAR/start/vim-js-pretty-template
```

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
git clone https://github.com/Quramy/vim-js-pretty-template.git ~/.vim/bundle/vim-js-pretty-template
```

## Usage

### Tagged Template Literal
Set the highlighting of template strings with the  `jspretmpl#register_tag()` function. For example,

```vim
" Register tag name associated the filetype
call jspretmpl#register_tag('gql', 'graphql')

autocmd FileType javascript JsPreTmpl
autocmd FileType javascript.jsx JsPreTmpl
```

Then your JavaScript codes are Highlighted as the following:

```javascript
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
autocmd FileType typescript JsPreTmpl
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
autocmd FileType dart JsPreTmpl
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

