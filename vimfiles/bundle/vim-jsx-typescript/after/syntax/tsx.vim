
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim syntax file
"
" Language: TSX (TypeScript)
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" These are the plugin-to-syntax-element correspondences:
"   - leafgarland/typescript-vim:             typescriptFuncBlock


let s:tsx_cpo = &cpo
set cpo&vim

syntax case match

if exists('b:current_syntax')
  let s:current_syntax = b:current_syntax
  unlet b:current_syntax
endif

syn include @HTMLSyntax syntax/html.vim
if exists('s:current_syntax')
  let b:current_syntax = s:current_syntax
endif

"  <tag></tag>
" s~~~~~~~~~~~e
syntax region tsxRegion
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
      \ contains=tsxTag,tsxCloseTag,tsxComment,Comment,@Spell,typescriptFuncBlock
      \ keepend
      \ extend



" matches template strings in tsx `this is a ${string}`
" syn region xmlString contained start=+\({[ ]*\zs`[0-9a-zA-Z/:.#!@% ?-_=+]*\|}\zs[0-9a-zA-Z/:.#!@% ?-_+=]*`\)+ end=++ contains=jsBlock,javascriptBlock

" matches tsx Comments: {/* .....  /*}
syn region Comment contained start=+{/\*+ end=+\*/}+ contains=Comment
  \ extend


" <tag id="sample">
" s~~~~~~~~~~~~~~~e
syntax region tsxTag
      \ matchgroup=tsxCloseTag
      \ start=+<[^ }/!?<>"'=:]\@=+
      \ end=+\/\?>+
      \ contained
      \ contains=tsxTagName,tsxAttrib,tsxEqual,tsxString,tsxEscapeJs,tsxAttributeComment

syn region tsxAttributeComment contained start=+//+ end=+\n+ contains=Comment
  \ extend

" </tag>
" ~~~~~~
syntax region tsxCloseTag
      \ start=+</[^ /!?<>"'=:]\@=+
      \ end=+>+
      \ contained
      \ contains=tsxCloseString

syntax match tsxCloseString
    \ +\w\++
    \ contained

" <!-- -->
" ~~~~~~~~
syntax match tsxComment /<!--\_.\{-}-->/ display

syntax match tsxEntity "&[^; \t]*;" contains=tsxEntityPunct
syntax match tsxEntityPunct contained "[&.;]"

" <tag key={this.props.key}>
"  ~~~
syntax match tsxTagName
    \ +[<]\@<=[^ /!?<>"']\++
    \ contained
    \ display

" <tag key={this.props.key}>
"      ~~~
syntax match tsxAttrib
    \ +[-'"<]\@<!\<[a-zA-Z:_][-.0-9a-zA-Z0-9:_]*\>\(['">]\@!\|$\)+
    \ contained
    \ contains=tsxAttribPunct,tsxAttribHook
    \ display

syntax match tsxAttribPunct +[:.]+ contained display

" <tag id="sample">
"        ~
syntax match tsxEqual +=+ contained display

" <tag id="sample">
"         s~~~~~~e
syntax region tsxString contained start=+"+ end=+"+ contains=tsxEntity,@Spell display

" <tag id=`sample${var}`>
syntax region tsxString contained start=+`+ end=+`+ contains=tsxEntity,@Spell display

" <tag id='sample'>
"         s~~~~~~e
syntax region tsxString contained start=+'+ end=+'+ contains=tsxEntity,@Spell display

" <tag key={this.props.key}>
"          s~~~~~~~~~~~~~~e
syntax region tsxEscapeJs matchgroup=tsxAttributeBraces
    \ contained
    \ start=+=\@<={+
    \ end=+}\ze\%(\/\|\n\|\s\|>\)+
    \ contains=TOP
    \ keepend
    \ extend

syntax match tsxIfOperator +?+
syntax match tsxElseOperator +:+

syntax cluster jsExpression add=tsxRegion

" highlight def link tsxTagName htmlTagName
highlight def link tsxTagName xmlTagName
" highlight def link tsxCloseTag htmlTag
highlight def link tsxCloseTag xmlEndTag

highlight def link tsxEqual htmlTag
highlight def link tsxString String
highlight def link tsxNameSpace Function
highlight def link tsxComment Error
highlight def link tsxAttrib htmlArg
highlight def link tsxEscapeJs tsxEscapeJs

highlight def link tsxCloseString htmlTagName
highlight def link tsxAttributeBraces htmlTag
highlight def link tsxAttributeComment Comment





" Custom React Highlights
syn keyword ReactState state nextState prevState setState
" Then EITHER (define your own colour scheme):
" OR (make the colour scheme match an existing one):
" hi link ReactKeywords typescriptRComponent
syn keyword ReactProps props defaultProps ownProps nextProps prevProps
syn keyword Events e event target value
syn keyword ReduxKeywords dispatch payload
syn keyword WebBrowser window localStorage
syn keyword ReactLifeCycleMethods componentWillMount shouldComponentUpdate componentWillUpdate componentDidUpdate componentWillReceiveProps componentWillUnmount componentDidMount

let b:current_syntax = 'typescript.tsx'

let &cpo = s:tsx_cpo
unlet s:tsx_cpo

