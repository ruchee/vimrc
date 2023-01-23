# vim-svelte

[![vim-svelte](https://github.com/evanleck/vim-svelte/actions/workflows/main.yml/badge.svg)](https://github.com/evanleck/vim-svelte/actions/workflows/main.yml)

Vim syntax highlighting and indentation for [Svelte 3][svelte] components.

This is mostly just HTML syntax highlighting with some keywords added and all
expressions inside of `{` and `}` highlighted as JavaScript.

Highlighting includes:

- HTML attributes with a colon like `on:click` or `transition:fade` highlighted
    as `Keyword`.
- `#if`, `/if`, `:else`, and `:else if` highlighted as `Conditional`.
- `#await`, `/await`, `:catch`, `:then`, and `@html` highlighted as `Keyword`.
- `#each` and `/each` highlighted as `Repeat`.


## Dependencies

1. [pangloss/vim-javascript][vim-javascript] for JavaScript syntax highlighting.
2. [othree/html5.vim][html5-vim] for HTML indent.

Both of those dependencies are included in [sheerun/vim-polyglot][vim-polyglot]
so if you're already using that then you should be set.


## Installation

The simplest way to install vim-svelte is via a package manager like
[Pathogen][pathogen], [Vundle][vundle], [NeoBundle][neobundle],
[Plug][vim-plug], or [minpac][minpac].

For example, using minpac:

```vimscript
call minpac#add('othree/html5.vim')
call minpac#add('pangloss/vim-javascript')
call minpac#add('evanleck/vim-svelte')
```

Or using Plug:

```vimscript
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'evanleck/vim-svelte', {'branch': 'main'}
```

vim-svelte works just fine with Vim 8's native package loading as well.


## Options

To disable indentation within `<script>` and `<style>` tags, set one of these
variables in your `vimrc`:

```vim
let g:svelte_indent_script = 0
let g:svelte_indent_style = 0
```


## Preprocessed languages

Syntax highlighting for additional languages is supported, assuming you have a
corresponding syntax definition installed. For example, newer versions of Vim
ship with a TypeScript syntax definition, so you wouldn't need anything
additional installed for that to work. Supported languages include:

- `less`
- `scss`
- `sass`
- `stylus`
- `typescript`

Since Svelte doesn't support these out of the box (see
[svelte-preprocess][preprocess] for how to set up some common language
preprocessors with e.g. Rollup), they're all disabled by default so the first
thing you'll need to do is enable your languages via the
`g:svelte_preprocessors` variable:

```vim
let g:svelte_preprocessors = ['typescript']
```

Then, use your language in your Svelte components like this:

```html
<script lang='typescript'>
</script>

<!-- Or... -->
<style type='text/scss'>
</style>
```

### Customizing the list of preprocessed languages

In addition to enabling the built-in preprocessors, you can add your own
preprocessors that this plugin will detect using the
`g:svelte_preprocessor_tags` variable. It should be a list of dictionaries with
at least a `name` and a `tag` attribute. You can optionally include an `as`
attribute which maps to the syntax you'd like to use within the tag.

Here's an example:

```vim
let g:svelte_preprocessor_tags = [
  \ { 'name': 'postcss', 'tag': 'style', 'as': 'scss' }
  \ ]
" You still need to enable these preprocessors as well.
let g:svelte_preprocessors = ['postcss']
```

This would highlight `<style type="postcss">` contents as `scss`, useful if you
use something like [postcss-nested][nested].

You can also create shorthand names if, for example, writing out
`lang='typescript'` takes too long:

```vim
let g:svelte_preprocessor_tags = [
  \ { 'name': 'ts', 'tag': 'script', 'as': 'typescript' }
  \ ]
let g:svelte_preprocessors = ['ts']
```

<table>
  <thead>
    <tr>
      <th>Field</th>
      <th>Usage</th>
      <th>Required</th>
      <th>Default value</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        The value within the attribute <code>lang</code> or <code>type</code> on
        the <code>tag</code> as well as the value to include in
        <code>g:svelte_preprocessors</code>.
      </td>
      <td>Yes</td>
      <td>None</td>
    </tr>
    <tr>
      <td><code>tag</code></td>
      <td>The HTML tag to target e.g. <code>script</code> or <code>style</code>.</td>
      <td>Yes</td>
      <td>None</td>
    </tr>
    <tr>
      <td><code>as</code></td>
      <td>The syntax name to use for highlighting.</td>
      <td>No</td>
      <td>The <code>name</code> attribute.</td>
    </tr>
  </tbody>
</table>

Note, that enabling and loading a lot of different syntax definitions can
considerably degrade Vim's performance. Consider yourself warned.


## Integrations

- [ALE][ale]: vim-svelte should work out of the box with `eslint` and a few
  other linters/fixers. PRs welcome if the one you want is missing.
- [matchit.vim][matchit]: vim-svelte should work out of the box and allow moving
  between HTML tags as well as flow control like `#if/:else//if`.
- [Syntastic][syntastic]: vim-syntastic will work with javascript and html checkers, for example:
  ```vim
  let g:syntastic_svelte_checkers = ['javascript/eslint', 'html/htmlhint']
  ```


## Tests

Indentation tests are provided and any contributions would be much appreciated.
They can be run with `make test` which will clone [vader.vim][vader] into the
current working directory and run the test suite.


## Alternatives

1. [burner/vim-svelte][burner]
2. [leafOfTree/vim-svelte-plugin][leafOfTree]


[ale]: https://github.com/dense-analysis/ale
[burner]: https://github.com/burner/vim-svelte
[html5-vim]: https://github.com/othree/html5.vim
[leafOfTree]: https://github.com/leafOfTree/vim-svelte-plugin
[matchit]: https://github.com/adelarsq/vim-matchit
[minpac]: https://github.com/k-takata/minpac
[neobundle]: https://github.com/Shougo/neobundle.vim
[nested]: https://github.com/postcss/postcss-nested
[pathogen]: https://github.com/tpope/vim-pathogen
[preprocess]: https://github.com/sveltejs/svelte-preprocess
[svelte]: https://svelte.dev
[syntastic]: https://github.com/vim-syntastic/syntastic
[vader]: https://github.com/junegunn/vader.vim
[vim-javascript]: https://github.com/pangloss/vim-javascript
[vim-plug]: https://github.com/junegunn/vim-plug
[vim-polyglot]: https://github.com/sheerun/vim-polyglot
[vundle]: https://github.com/VundleVim/Vundle.vim
