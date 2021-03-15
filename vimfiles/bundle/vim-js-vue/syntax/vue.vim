""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim syntax file
"
" Language: Vue
" Maintainer: leaf <leafvocation@gmail.com>
"
" CREDITS: Inspired by mxw/vim-jsx.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists("b:current_syntax") && b:current_syntax == 'vue'
  finish
endif

" For advanced users, this variable can be used to avoid overload
let b:current_loading_main_syntax = 'vue'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Config {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:use_pug = vue#GetConfig("use_pug", 0)
let s:use_less = vue#GetConfig("use_less", 0)
let s:use_sass = vue#GetConfig("use_sass", 0)
let s:use_scss = vue#GetConfig("use_scss", 0)
let s:use_stylus = vue#GetConfig("use_stylus", 0)
let s:use_coffee = vue#GetConfig("use_coffee", 0)
let s:use_typescript = vue#GetConfig("use_typescript", 0)
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Load main syntax {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Load syntax/html.vim to syntax group, which loads full JavaScript and CSS
" syntax. It defines group @html, @htmlJavaScript, and @htmlCss.
call vue#LoadSyntax('@html', 'html')

" Avoid overload
if !hlexists('cssTagName')
  call vue#LoadSyntax('@htmlCss', 'css')
endif

" Avoid overload
if !hlexists('javaScriptComment')
  call vue#Log('load javascript cluster')
  call vue#LoadSyntax('@htmlJavaScript', 'javascript')
endif

" Load vue-html syntax
runtime syntax/vue-html.vim

" Load vue-javascript syntax
runtime syntax/vue-javascript.vim
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Load pre-processors syntax {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" If pug is enabled, load vim-pug syntax
if s:use_pug
  call vue#LoadFullSyntax('@PugSyntax', 'pug')
  syn cluster htmlJavascript remove=javascriptParenthesisBlock
endif

" If less is enabled, load less syntax 
if s:use_less
  call vue#LoadSyntax('@LessSyntax', 'less')
  runtime! after/syntax/less.vim
endif

" If sass is enabled, load sass syntax 
if s:use_sass
  call vue#LoadSyntax('@SassSyntax', 'sass')
  runtime! after/syntax/sass.vim
endif

" If scss is enabled, load sass syntax 
if s:use_scss
  call vue#LoadSyntax('@ScssSyntax', 'scss')
  runtime! after/syntax/scss.vim
endif

" If stylus is enabled, load stylus syntax 
if s:use_stylus
  call vue#LoadFullSyntax('@StylusSyntax', 'stylus')
  runtime! after/syntax/stylus.vim
endif

" If CoffeeScript is enabled, load the syntax. Keep name consistent with
" vim-coffee-script/after/html.vim
if s:use_coffee
  call vue#LoadFullSyntax('@htmlCoffeeScript', 'coffee')
endif

" If TypeScript is enabled, load the syntax.
if s:use_typescript
  call vue#LoadFullSyntax('@TypeScript', 'typescript')
endif
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Syntax highlight {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" All start with html/javascript/css for emmet-vim in-file type detection
syntax region htmlVueTemplate fold
      \ start=+<template[^>]*>+
      \ end=+^</template>+
      \ keepend contains=@html
" When template code is not well indented
syntax region htmlVueTemplate fold
      \ start=+<template[^>]*>+
      \ end=+</template>\ze\n\(^$\n\)*<\(script\|style\)+
      \ keepend contains=@html

syntax region javascriptVueScript fold 
      \ start=+<script[^>]*>+
      \ end=+</script>+
      \ keepend contains=@htmlJavaScript,jsImport,jsExport,vueTag


syntax region cssVueStyle fold
      \ start=+<style[^>]*>+
      \ end=+</style>+
      \ keepend contains=@htmlCss,vueTag

" Preprocessors syntax
syntax region pugVueTemplate fold
      \ start=+<template[^>]*lang=["']pug["'][^>]*>+
      \ end=+</template>+
      \ keepend contains=@PugSyntax,vueTag

syntax region coffeeVueScript fold 
      \ start=+<script[^>]*lang=["']coffee["'][^>]*>+
      \ end=+</script>+
      \ keepend contains=@htmlCoffeeScript,jsImport,jsExport,vueTag

syntax region typescriptVueScript fold
      \ start=+<script[^>]*lang=["']ts["'][^>]*>+
      \ end=+</script>+
      \ keepend contains=@TypeScript,vueTag

syntax region cssLessVueStyle fold
      \ start=+<style[^>]*lang=["']less["'][^>]*>+
      \ end=+</style>+
      \ keepend contains=@LessSyntax,vueTag
syntax region sassVueStyle fold
      \ start=+<style[^>]*lang=["']sass["'][^>]*>+
      \ end=+</style>+
      \ keepend contains=@SassSyntax,vueTag
syntax region cssScssVueStyle fold
      \ start=+<style[^>]*lang=["']scss["'][^>]*>+
      \ end=+</style>+
      \ keepend contains=@ScssSyntax,vueTag

" Backward compatiable for `use_sass` option
if s:use_sass && !s:use_scss
  syntax region cssScssVueStyle fold
        \ start=+<style[^>]*lang=["']scss["'][^>]*>+
        \ end=+</style>+
        \ keepend contains=@SassSyntax,vueTag
endif

syntax region cssStylusVueStyle fold
      \ start=+<style[^>]*lang=["']stylus["'][^>]*>+
      \ end=+</style>+
      \ keepend contains=@StylusSyntax,vueTag

syntax region vueTag fold
      \ start=+^<[^/]+ end=+>+ skip=+></+
      \ contained contains=htmlTagN,htmlString,htmlArg
syntax region vueTag 
      \ start=+^</+ end=+>+
      \ contains=htmlTagN,htmlString,htmlArg

highlight default link vueTag htmlTag
highlight default link cssUnitDecorators2 Number
highlight default link cssKeyFrameProp2 Constant
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Custom blocks {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
runtime syntax/vue-custom-blocks.vim
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Syntax patch {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Patch 7.4.1142
if has("patch-7.4-1142")
  if has("win32")
    syntax iskeyword @,48-57,_,128-167,224-235,$,-
  else
    syntax iskeyword @,48-57,_,192-255,$,-
  endif
else
  setlocal iskeyword+=-
endif

" Style
" Redefine (less|sass|stylus)Definition to highlight <style> correctly and 
" enable emmet-vim css type.
if s:use_less
  silent! syntax clear lessDefinition
  syntax region cssLessDefinition matchgroup=cssBraces 
        \ contains=@LessSyntax,cssLessDefinition 
        \ contained containedin=cssLessVueStyle
        \ start="{" end="}" 
endif
if s:use_sass
  silent! syntax clear sassDefinition
  syntax region sassDefinition matchgroup=cssBraces 
        \ contains=@SassSyntax,sassDefinition 
        \ contained containedin=sassVueStyle
        \ start="{" end="}" 

  " Extend to highlight all numbers in expression
  syntax match cssValueNumber
        \ /\W\zs\d\+\(\.\d\+\)\?%\?\ze\W/
        \ contained containedin=sassDefinition
endif
" If not loading https://github.com/cakebaker/scss-syntax.vim
if s:use_scss && !hlexists('scssNestedProperty')
  silent! syntax clear scssDefinition
  syntax region cssScssDefinition transparent matchgroup=cssBraces 
        \ contains=@ScssSyntax,cssScssDefinition
        \ contained containedin=cssScssVueStyle
        \ start="{" end="}" 

  " Extend to highlight all numbers in expression
  syntax match cssValueNumber
        \ /\W\zs\d\+\(\.\d\+\)\?%\?\ze\W/
        \ contained containedin=cssScssDefinition
endif
if s:use_stylus
  silent! syntax clear stylusDefinition
  syntax region cssStylusDefinition matchgroup=cssBraces 
        \ contains=@StylusSyntax,cssStylusDefinition
        \ contained containedin=cssStylusVueStyle
        \ start="{" end="}" 
endif

" Use a different name in order to avoid css syntax interference
silent! syntax clear cssUnitDecorators
syntax match cssUnitDecorators2 
      \ /\(#\|-\|+\|%\|mm\|cm\|in\|pt\|pc\|em\|ex\|px\|ch\|rem\|vh\|vw\|vmin\|vmax\|dpi\|dppx\|dpcm\|Hz\|kHz\|s\|ms\|deg\|grad\|rad\)\ze\(;\|$\)/
      \ contained
      \ containedin=cssAttrRegion,sassCssAttribute,lessCssAttribute,stylusCssAttribute

silent! syntax clear cssKeyFrameProp
syn match cssKeyFrameProp2 /\d*%\|from\|to/ 
      \ contained nextgroup=cssDefinition
      \ containedin=cssAttrRegion,sassCssAttribute,lessCssAttribute,stylusCssAttribute

" Coffee
if s:use_coffee
  silent! syntax clear coffeeConstant
  syn match coffeeConstant '\v<\u\C[A-Z0-9_]+>' display 
        \ containedin=@coffeeIdentifier
endif

" JavaScript
" Number with minus
syntax match javaScriptNumber '\v<-?\d+L?>|0[xX][0-9a-fA-F]+>' 
      \ containedin=@javascriptVueScript display

" HTML
" Clear htmlHead that may cause highlighting out of bounds
silent! syntax clear htmlHead

" html5 data-*
syntax match htmlArg '\v<data(-[.a-z0-9]+)+>' containedin=@html
"}}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Syntax sync {{{
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax sync clear
syntax sync minlines=100
syntax sync match vueHighlight groupthere NONE "</\(script\|template\|style\)"
syntax sync match scriptHighlight groupthere javascriptVueScript "<script"
syntax sync match scriptHighlight groupthere coffeeVueScript "<script[^>]*lang=["']coffee["'][^>]*>"
syntax sync match scriptHighlight groupthere typescriptVueScript "<script[^>]*lang=["']ts["'][^>]*>"
syntax sync match templateHighlight groupthere htmlVueTemplate "<template"
syntax sync match templateHighlight groupthere pugVueTemplate "<template[^>]*lang=["']pug["'][^>]*>"
syntax sync match styleHighlight groupthere cssVueStyle "<style"
syntax sync match styleHighlight groupthere cssLessVueStyle "<style[^>]*lang=["']less["'][^>]*>"
syntax sync match styleHighlight groupthere sassVueStyle "<style[^>]*lang=["']sass["'][^>]*>"
syntax sync match styleHighlight groupthere cssScssVueStyle "<style[^>]*lang=["']scss["'][^>]*>"
syntax sync match styleHighlight groupthere cssStylusVueStyle "<style[^>]*lang=["']stylus["'][^>]*>"
"}}}

let b:current_syntax = 'vue'
" vim: fdm=marker
