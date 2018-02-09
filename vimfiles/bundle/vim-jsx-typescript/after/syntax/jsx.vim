
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim syntax file
"
" Language: JSX (JavaScript)
" https://github.com/neoclide/vim-jsx-improve/blob/master/after/syntax/javascript.vim
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" These are the plugin-to-syntax-element correspondences:
"   - leafgarland/typescript-vim:             typescriptFuncBlock


let s:jsx_cpo = &cpo
set cpo&vim

syntax case match

if exists('b:current_syntax')
  let s:current_syntax = b:current_syntax
  unlet b:current_syntax
endif

if exists('s:current_syntax')
  let b:current_syntax = s:current_syntax
endif

"  <tag></tag>
" s~~~~~~~~~~~e
syntax region jsxRegion
      \ start=+\%(<\|\w\)\@<!<\z([/a-zA-Z][a-zA-Z0-9:\-.]*\)+
      \ skip=+<!--\_.\{-}-->+
      \ end=+</\z1\_\s\{-}[^(=>)]>+
      \ end=+>\n*\t*\n*\s*)\@=+
      \ end=+>\n*\t*\n*\s*,\@=+
      \ end=+>\n*\t*\n*\s*\(}\n*\t*\s*[a-zA-Z()\t/]\)\@=+
      \ end=+>\n*\t*\n*\s*\({\n*\t*\s*[a-zA-Z()\t/]\)\@=+
      \ end=+>;\(\n*\t*\s*[a-zA-Z()]\)\@=+
      \ end=+\n\?\s\*,+
      \ end=+\s\+:\@=+
      \ fold
      \ contains=jsxTag,jsxCloseTag,jsxComment,Comment,@Spell,typescriptFuncBlock
      \ keepend
      \ extend



" matches template strings in jsx `this is a ${string}`
" syn region xmlString contained start=+\({[ ]*\zs`[0-9a-zA-Z/:.#!@% ?-_=+]*\|}\zs[0-9a-zA-Z/:.#!@% ?-_+=]*`\)+ end=++ contains=jsBlock,javascriptBlock

" matches jsx Comments: {/* .....  /*}
syn region Comment contained start=+{/\*+ end=+\*/}+ contains=Comment
  \ extend


" <tag id="sample">
" s~~~~~~~~~~~~~~~e
syntax region jsxTag
      \ matchgroup=jsxCloseTag
      \ start=+<[^ }/!?<>"'=:]\@=+
      \ end=+\/\?>+
      \ contained
      \ contains=jsxTagName,jsxAttrib,jsxEqual,jsxString,jsxEscapeJs,jsxAttributeComment

syn region jsxAttributeComment contained start=+//+ end=+\n+ contains=Comment
  \ extend

" </tag>
" ~~~~~~
syntax region jsxCloseTag
      \ start=+</[^ /!?<>"'=:]\@=+
      \ end=+>+
      \ contained
      \ contains=jsxCloseString

syntax match jsxCloseString
    \ +\w\++
    \ contained

" <!-- -->
" ~~~~~~~~
syntax match jsxComment /<!--\_.\{-}-->/ display

syntax match jsxEntity "&[^; \t]*;" contains=jsxEntityPunct
syntax match jsxEntityPunct contained "[&.;]"

" <tag key={this.props.key}>
"  ~~~
syntax match jsxTagName
    \ +[<]\@<=[^ /!?<>"']\++
    \ contained
    \ display

" <tag key={this.props.key}>
"      ~~~
syntax match jsxAttrib
    \ +[-'"<]\@<!\<[a-zA-Z:_][-.0-9a-zA-Z0-9:_]*\>\(['">]\@!\|$\)+
    \ contained
    \ contains=jsxAttribPunct,jsxAttribHook
    \ display

syntax match jsxAttribPunct +[:.]+ contained display

" <tag id="sample">
"        ~
syntax match jsxEqual +=+ contained display

" <tag id="sample">
"         s~~~~~~e
syntax region jsxString contained start=+"+ end=+"+ contains=jsxEntity,@Spell display

" <tag id=`sample${var}`>
syntax region jsxString contained start=+`+ end=+`+ contains=jsxEntity,@Spell display

" <tag id='sample'>
"         s~~~~~~e
syntax region jsxString contained start=+'+ end=+'+ contains=jsxEntity,@Spell display

" <tag key={this.props.key}>
"          s~~~~~~~~~~~~~~e
syntax region jsxEscapeJs matchgroup=jsxAttributeBraces
    \ contained
    \ start=+=\@<={+
    \ end=+}\ze\%(\/\|\n\|\s\|>\)+
    \ contains=TOP
    \ keepend
    \ extend

syntax match jsxIfOperator +?+
syntax match jsxElseOperator +:+

syntax cluster jsExpression add=jsxRegion

" highlight def link jsxTagName htmlTagName
highlight def link jsxTagName xmlTagName
" highlight def link jsxCloseTag htmlTag
highlight def link jsxCloseTag xmlEndTag

highlight def link jsxEqual htmlTag
highlight def link jsxString String
highlight def link jsxNameSpace Function
highlight def link jsxComment Error
highlight def link jsxAttrib htmlArg
highlight def link jsxEscapeJs jsxEscapeJs

highlight def link jsxCloseString htmlTagName
highlight def link jsxAttributeBraces htmlTag
highlight def link jsxAttributeComment Comment


" Custom React Highlights
syn keyword ReactState state nextState prevState setState
" Then EITHER (define your own colour scheme):
hi ReactState guifg=#E06C75
" OR (make the colour scheme match an existing one):
" hi link ReactKeywords typescriptRComponent

syn keyword ReactProps props defaultProps ownProps nextProps prevProps
hi ReactProps guifg=#D19A66

syn keyword Events e event
hi Events ctermfg=204 guifg=#56B6C2


syn keyword ReduxKeywords dispatch payload
hi ReduxKeywords ctermfg=204 guifg=#C678DD

syn keyword ReactLifeCycleMethods componentWillMount shouldComponentUpdate componentWillUpdate componentDidUpdate componentWillReceiveProps componentWillUnmount componentDidMount
hi ReactLifeCycleMethods ctermfg=204 guifg=#D19A66


let b:current_syntax = 'javascript.jsx'

let &cpo = s:jsx_cpo
unlet s:jsx_cpo

