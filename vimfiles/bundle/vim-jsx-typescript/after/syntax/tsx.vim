
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

"""""" Vim Syntax Help """"""
" `keepend` and `extend` docs:
" https://github.com/othree/til/blob/master/vim/syntax-keepend-extend.md

" \@<=    positive lookbehind
" \@<!    negative lookbehind
" \@=     positive lookahead
" \@!     negative lookahead

"  <tag></tag>
" s~~~~~~~~~~~e
syntax region tsxRegion
      \ start=+\(<>\|\%(<\|\w\)\@<!<\z([/a-zA-Z][a-zA-Z0-9:\-.]*\)\)+
      \ skip=+<!--\_.\{-}-->+
      \ end=+</\z1\_\s\{-}[^(=>)]>+
      \ end=+>\n*\t*\s*[),]\@=+
      \ end=+>[;]*\n*\t*\s*\([};]\n*\t*\s*\)\@=+
      \ contains=tsxTag,tsxCloseTag,tsxComment,Comment,@Spell,jsBlock,tsxColon
      \ keepend

" <tag>{content}</tag>
"      s~~~~~~~e
syn region jsBlock start=+{+ end=+}+
  \ contained
  \ contains=TOP

" \@<=    positive lookbehind
" \@<!    negative lookbehind
" \@=     positive lookahead
" \@!     negative lookahead
" RULE: capture expression, then apply rule AFTER
" e.g foo\(bar\)\@!
" match all `foo` which is not followed by `bar`
" https://jbodah.github.io/blog/2016/11/01/positivenegative-lookaheadlookbehind-vim/

" <tag key={this.props.key}>
"          s~~~~~~~~~~~~~~e
syntax region tsxJsBlock
    \ matchgroup=tsxAttributeBraces start=+\([=]\|\s\)\@<={+
    \ matchgroup=tsxAttributeBraces end=+}\(}\|)\)\@!+
    \ contained
    \ keepend
    \ extend
    \ contains=TOP

" <tag id="sample">
" s~~~~~~~~~~~~~~~e
syntax region tsxTag
      \ start=+<[^ /!?<"'=:]\@=+
      \ end=+\/\?>+
      \ contained
      \ contains=tsxTagName,tsxAttrib,tsxEqual,tsxString,tsxJsBlock,tsxAttributeComment,jsBlock

" </tag>
" ~~~~~~
syntax region tsxCloseTag
      \ start=+</[^ /!?<"'=:]\@=+
      \ end=+>+
      \ contained
      \ contains=tsxCloseString

" matches tsx Comments: {/* .....  /*}
syn region Comment contained start=+{/\*+ end=+\*/}+ contains=Comment
  \ extend

syn region tsxAttributeComment contained start=+//+ end=+\n+ contains=Comment
  \ extend

syntax match tsxCloseString
    \ +\w\++
    \ contained

syntax match tsxColon
    \ +[;]+
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

syntax match tsxIfOperator +?+
syntax match tsxElseOperator +:+

" highlight def link tsxTagName htmlTagName
highlight def link tsxTagName xmlTagName
highlight def link tsxTag htmlTag
highlight def link tsxCloseTag xmlEndTag
highlight def link tsxRegionEnd xmlEndTag
highlight def link tsxEqual htmlTag
highlight def link tsxString String
highlight def link tsxNameSpace Function
highlight def link tsxComment Error
highlight def link tsxAttrib htmlArg
highlight def link tsxCloseString htmlTagName
highlight def link tsxAttributeBraces htmlTag
highlight def link tsxAttributeComment Comment
highlight def link tsxColon typescriptEndColons

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

