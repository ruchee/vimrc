" Vim syntax file
" Language:    SCSS (Sassy CSS)
" Author:      Daniel Hofstetter (daniel.hofstetter@42dh.com)
" URL:         https://github.com/cakebaker/scss-syntax.vim
" Last Change: 2014-02-25
" Inspired by the syntax files for sass and css. Thanks to the authors of
" those files!

if !exists("main_syntax")
  if exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'scss'
endif

runtime! syntax/css.vim
runtime! syntax/css/*.vim

syn case ignore

" XXX fix for #20, can be removed once the patch is in vim's css.vim
syn match cssSpecialCharQQ +\\\\\|\\"+ contained
syn match cssSpecialCharQ +\\\\\|\\'+ contained

" XXX fix for #37
if v:version < 704
  " replace the definition of cssBoxProp with the one from Vim 7.4 to prevent
  " highlighting issues
  syn clear cssBoxProp
  syn match cssBoxProp contained "\<padding\(-\(top\|right\|bottom\|left\)\)\=\>"
  syn match cssBoxProp contained "\<margin\(-\(top\|right\|bottom\|left\)\)\=\>"
  syn match cssBoxProp contained "\<overflow\(-\(x\|y\|style\)\)\=\>"
  syn match cssBoxProp contained "\<rotation\(-point\)\=\>"
endif

syn region scssDefinition matchgroup=cssBraces start='{' end='}' contains=cssComment,cssInclude,scssAtRootStatement,scssComment,scssDefinition,scssProperty,scssSelector,scssVariable,scssImport,scssExtend,scssInclude,@scssControl,scssWarn,scssError containedin=cssMediaBlock

syn match scssSelector "^\zs\([^:@]\|:[^ ]\)\+{\@=" contained contains=@scssSelectors
syn match scssSelector "^\s*\zs\([^:@{]\|:[^ ]\)\+\_$" contained contains=@scssSelectors
syn cluster scssSelectors contains=cssTagName,cssPseudoClass,cssAttributeSelector,scssSelectorChar,scssAmpersand,scssInterpolation

syn match scssProperty "\([[:alnum:]-]\)\+\s*\(:\)\@=" contained contains=css.*Prop,cssVendor containedin=cssMediaBlock nextgroup=scssAttribute,scssAttributeWithNestedDefinition
syn match scssAttribute ":[^;]*;" contained contains=css.*Attr,cssValue.*,cssColor,cssFunction,cssString.*,cssURL,scssFunction,scssInterpolation,scssVariable

syn match scssAttributeWithNestedDefinition ": [^#]*{\@=" nextgroup=scssNestedDefinition contained contains=cssValue.*,scssVariable
syn region scssNestedDefinition matchgroup=cssBraces start="{" end="}" contained contains=cssComment,scssComment,scssProperty,scssNestedProperty

" CSS properties from https://developer.mozilla.org/en-US/docs/Web/CSS/Reference
" align
syn keyword scssNestedProperty contained content items self nextgroup=scssAttribute
" animation
syn keyword scssNestedProperty contained delay direction duration fill-mode iteration-count name play-state timing-function nextgroup=scssAttribute
" background
syn keyword scssNestedProperty contained attachment clip color image origin position repeat size nextgroup=scssAttribute
" border
syn keyword scssNestedProperty contained bottom bottom-color bottom-left-radius bottom-right-radius bottom-style bottom-width nextgroup=scssAttribute
syn keyword scssNestedProperty contained collapse color nextgroup=scssAttribute
syn keyword scssNestedProperty contained image image-outset image-repeat image-slice image-source image-width nextgroup=scssAttribute
syn keyword scssNestedProperty contained left left-color left-style left-width nextgroup=scssAttribute
syn keyword scssNestedProperty contained radius nextgroup=scssAttribute
syn keyword scssNestedProperty contained right right-color right-style right-width nextgroup=scssAttribute
syn keyword scssNestedProperty contained spacing style nextgroup=scssAttribute
syn keyword scssNestedProperty contained top top-color top-left-radius top-right-radius top-style top-width nextgroup=scssAttribute
syn keyword scssNestedProperty contained width nextgroup=scssAttribute
" box
syn keyword scssNestedProperty contained decoration-break shadow sizing nextgroup=scssAttribute
" break
syn keyword scssNestedProperty contained after before inside nextgroup=scssAttribute
" column
syn keyword scssNestedProperty contained count fill gap rule rule-color rule-style rule-width span width nextgroup=scssAttribute
" counter
syn keyword scssNestedProperty contained increment reset nextgroup=scssAttribute
" flex
syn keyword scssNestedProperty contained basis direction flow grow shrink wrap nextgroup=scssAttribute
" font
syn keyword scssNestedProperty contained family feature-settings kerning language-override size size-adjust stretch style synthesis nextgroup=scssAttribute
syn keyword scssNestedProperty contained variant variant-alternates variant-caps variant-east-asian variant-ligatures variant-numeric variant-position nextgroup=scssAttribute
syn keyword scssNestedProperty contained weight nextgroup=scssAttribute
" image
syn keyword scssNestedProperty contained rendering resolution orientation nextgroup=scssAttribute
" list
syn keyword scssNestedProperty contained style style-image style-position style-type nextgroup=scssAttribute
" margin/padding
syn keyword scssNestedProperty contained bottom left right top nextgroup=scssAttribute
" max/min
syn keyword scssNestedProperty contained height width nextgroup=scssAttribute
" nav
syn keyword scssNestedProperty contained down index left right up nextgroup=scssAttribute
" object
syn keyword scssNestedProperty contained fit position nextgroup=scssAttribute
" outline
syn keyword scssNestedProperty contained color offset style width nextgroup=scssAttribute
" overflow
syn keyword scssNestedProperty contained wrap x y nextgroup=scssAttribute
" page
syn keyword scssNestedProperty contained break-after break-before break-inside nextgroup=scssAttribute
" text
syn keyword scssNestedProperty contained align align-last combine-horizontal nextgroup=scssAttribute
syn keyword scssNestedProperty contained decoration decoration-color decoration-line decoration-style nextgroup=scssAttribute
syn keyword scssNestedProperty contained indent orientation overflow rendering shadow transform underline-position nextgroup=scssAttribute
" transform
syn keyword scssNestedProperty contained origin style nextgroup=scssAttribute
" transition
syn keyword scssNestedProperty contained delay duration property timing-function nextgroup=scssAttribute
" unicode
syn keyword scssNestedProperty contained bidi range nextgroup=scssAttribute
" word
syn keyword scssNestedProperty contained break spacing wrap nextgroup=scssAttribute

syn region scssInterpolation matchgroup=scssInterpolationDelimiter start="#{" end="}" contains=cssValue.*,cssColor,cssString.*,scssFunction,scssVariable containedin=cssInclude,cssString.*,cssURL,scssFunction

" ignores the url() function so it can be handled by css.vim
syn region scssFunction contained matchgroup=scssFunctionName start="\<\(url(\)\@!\([[:alnum:]-]\)\+\s*(" end=")" oneline keepend extend containedin=cssMediaType
syn match scssParameterList ".*" contained containedin=cssFunction,scssFunction contains=css.*Attr,cssColor,cssString.*,cssValue.*,scssFunction,scssVariable

syn match scssVariable "$[[:alnum:]_-]\+" containedin=cssFunction,scssFunction,cssMediaType nextgroup=scssVariableAssignment skipwhite
syn match scssVariableAssignment ":" contained nextgroup=scssVariableValue skipwhite
syn match scssVariableValue "[^;)]\+[;)]\@=" contained contains=css.*Attr,cssValue.*,cssColor,cssFunction,cssString.*,cssURL,scssDefault,scssFunction,scssInterpolation,scssNull,scssVariable,scssMap,scssGlobal,scssAmpersand
syn match scssGlobal "!global" contained
syn keyword scssNull null contained

syn match scssMixin "^@mixin" nextgroup=scssMixinName skipwhite
syn match scssMixinName "[[:alnum:]_-]\+" contained nextgroup=scssDefinition,scssMixinParams
syn region scssMixinParams contained contains=css.*Attr,cssColor,cssValue.*,cssString.*,scssVariable,scssFunction start="(" end=")" oneline extend
syn match scssInclude "@include" nextgroup=scssMixinName skipwhite containedin=cssMediaBlock
syn match scssContent "@content" contained containedin=scssDefinition

syn match scssFunctionDefinition "^@function" nextgroup=scssFunctionName skipwhite
syn match scssFunctionName "[[:alnum:]_-]\+" contained nextgroup=scssFunctionParams
syn region scssFunctionParams contained start="(" end=")" nextgroup=scssFunctionBody contains=scssVariable skipwhite
syn region scssFunctionBody contained matchgroup=cssBraces start="{" end="}" contains=cssString.*,cssValue.*,scssVariable,scssReturn,scssFunction
syn match scssReturn "@return" contained
syn match scssExtend "@extend" nextgroup=scssExtendedSelector skipwhite containedin=cssMediaBlock
syn match scssExtendedSelector "[^ ;]\+" contained contains=cssTagName,cssPseudoClass,scssSelectorChar nextgroup=scssOptional skipwhite
syn match scssOptional "!optional" contained
syn match scssImport "@import" nextgroup=scssImportList
syn match scssImportList "[^;]\+" contained contains=cssString.*,cssMediaType,cssURL

syn match scssSelectorChar "\(#\|\.\|%\)\([[:alnum:]_-]\|#{.*}\)\@=" nextgroup=scssSelectorName containedin=cssMediaBlock
syn match scssSelectorName "\([[:alnum:]_-]\|#{[^}]*}\)\+" contained contains=scssInterpolation

syn match scssAmpersand "&" nextgroup=cssPseudoClass,scssSelectorName containedin=cssMediaBlock

syn match scssDebug "@debug" nextgroup=scssOutput
syn match scssWarn "@warn" nextgroup=scssOutput
syn match scssError "@error" nextgroup=scssOutput
syn match scssOutput "[^;]\+" contained contains=cssValue.*,cssString.*,scssFunction,scssVariable
syn match scssDefault "!default" contained

syn match scssIf "@\=if" nextgroup=scssCondition
syn match scssCondition "[^{]\+" contained contains=cssValue.*,cssString.*,scssFunction,scssNull,scssVariable,scssAmpersand
syn match scssElse "@else" nextgroup=scssIf
syn match scssElse "@else\(\s*\({\|$\)\)\@="
syn match scssWhile "@while" nextgroup=scssCondition
syn match scssFor "@for\s\+.*from\s\+.*\(to\|through\)\s\+[^{ ]\+" contains=cssValueNumber,scssFunction,scssVariable,scssForKeyword
syn match scssForKeyword "@for\|from\|to\|through" contained
syn region scssEach matchgroup=scssEachKeyword start="@each" end="in" contains=scssVariable nextgroup=scssCollection
syn region scssCollection start=" " end="\ze{" contained contains=scssFunction,scssMap,scssVariable
syn cluster scssControl contains=scssIf,scssElse,scssWhile,scssFor,scssEach

syn region scssMap matchgroup=cssBraces start=" \zs(\ze.\+:" end=")" contains=scssMapKey,scssMapValue
syn match scssMapKey "[^: ]\+\ze[:]" contained contains=css.*Attr,cssString.*,scssVariable
syn match scssMapValue "[^, ]\+\ze[,)]" contained contains=cssColor,css.*Prop,cssString.*,scssVariable

syn region scssAtRootStatement start="@at-root" end="\ze{" contains=@scssSelectors,scssAtRoot
syn match scssAtRoot "@at-root" contained

syn match scssComment "//.*$" contains=@Spell containedin=cssMediaBlock
syn keyword scssTodo TODO FIXME NOTE OPTIMIZE XXX contained containedin=cssComment,scssComment

hi def link scssNestedProperty cssProp
hi def link scssVariable  Identifier
hi def link scssGlobal    Special
hi def link scssNull      Constant
hi def link scssMixin     PreProc
hi def link scssMixinName Function
hi def link scssContent   PreProc
hi def link scssFunctionDefinition  PreProc
hi def link scssFunctionName Function
hi def link scssReturn    Statement
hi def link scssInclude   PreProc
hi def link scssExtend    PreProc
hi def link scssOptional  Special
hi def link scssComment   Comment
hi def link scssSelectorChar Special
hi def link scssSelectorName Identifier
hi def link scssAmpersand Character
hi def link scssDebug     Debug
hi def link scssWarn      Debug
hi def link scssError     Debug
hi def link scssDefault   Special
hi def link scssIf        Conditional
hi def link scssElse      Conditional
hi def link scssWhile     Repeat
hi def link scssForKeyword  Repeat
hi def link scssEachKeyword Repeat
hi def link scssInterpolationDelimiter Delimiter
hi def link scssImport    Include
hi def link scssTodo      Todo
hi def link scssAtRoot    Keyword

let b:current_syntax = "scss"
if main_syntax == 'scss'
  unlet main_syntax
endif
