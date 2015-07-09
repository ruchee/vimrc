" Vim syntax file
" Language:        Latte Templates
" Maintainer:      Martin Janiczek <martin@janiczek.cz>
" Latest Revision: 11 March 2012
" URL:             http://forum.nette.org/cs/10222-latte-vim-syntax-highlighter
" URL:             https://github.com/janiczek/vim-latte

" ============================================================
" ================================ INCLUDES AND WHATNOT ======
" ============================================================

" check if we loaded this file before
if exists('b:current_syntax') && b:current_syntax == 'latte' 
  finish
endif

" we want to highlight PHP, HTML, CSS, JS
runtime! syntax/php.vim

" ============================================================
" ========================================= DEFINITIONS ======
" ============================================================

" TODO: should we recognize things like => , : or not?
" TODO: more specifically, should we allow only the macros
"       that really are in Latte? like:
"       {link Presenter:action} <-- good
"       {link} <-- bad
" TODO: is this style (#blocks same color as $phpVars) ok?
"                     (n:attrs different color than attrs)
" TODO: macros inside <a> links

" ------------------------------------------------------------
" ------------------------------------------- n:attribs ------
" ------------------------------------------------------------

" Commented out because it breaks highlighting of inline CSS and JS.
" TODO?

"" n:attrib: <tag n:foo="$bar">
"syn match latteAttribute contained /n:[a-zA-Z]\+\>/

"" for attributes:
"syn region htmlTag start=+<[^/]+ end=+>+
"    \ contains=htmlTagN,htmlString,htmlArg,htmlValue,htmlTagError,htmlEvent,htmlCssDefinition,@htmlPreproc,@htmlArgCluster,latteAttribute

" ------------------------------------------------------------
" ---------------------------------------------- macros ------
" ------------------------------------------------------------

" macro: {a macro}
syn region latteMacro start="{\S" end="}"
    \ contains=latteBlockName,latteString,latteVariable

" block name: #blockName
syn match latteBlockName /#[$_a-zA-Z0-9]\+\>/ contained

" string: 'abcdę'
syn region latteString start=+'+ end=+'+ contained
    \ contains=latteVariable

syn region latteString start=+"+ end=+"+ contained
    \ contains=latteVariable

" ------------------------------------------------------------
" -------------------------------------------- comments ------
" ------------------------------------------------------------

" comments: {* a basic comment *}
syn region latteComment start="{\*" end="\*}"
    \ contains=latteTodo

" todo: {* TODO something *}
syn keyword latteTodo contained TODO FIXME XXX

" ------------------------------------------------------------
" ----------------------------------------- annotations ------
" ------------------------------------------------------------

" annotation comments: {** an @annotation comment *}
syn region latteAnnotationComment start="{\*\*" end="\*}"
    \ contains=latteTodo,latteAnnotation,latteVariable,latteType

" annotation: @param
syn match latteAnnotation /@[$_a-zA-Z][$_a-zA-Z0-9]*\>/ contained

" variable: $myVar
syn match latteVariable /$[_a-zA-Z][_a-zA-Z0-9]*\>/ contained

" type: string
syn keyword latteType contained boolean integer float double
    \ string array object resource NULL

" ------------------------------------------------------------
" ---------------------------------------- HTML strings ------
" ------------------------------------------------------------
"  (so we can highlight things inside them too ;) )

" for values:
syn region htmlString contained start=+"+ end=+"+
    \ contains=htmlSpecialChar,javaScriptExpression,@htmlPreproc,latteMacro,latteVariable

syn region htmlString contained start=+'+ end=+'+
    \ contains=htmlSpecialChar,javaScriptExpression,@htmlPreproc,latteMacro,latteVariable

" ============================================================
" =========================================== WIRING ;) ======
" ============================================================

" finally, use the definitions!

"hi def link latteAttribute         Type

hi def link latteMacro             PreProc
hi def link latteBlockName         Identifier
hi def link latteString            String

hi def link latteComment           Comment
hi def link latteTodo              Todo

hi def link latteAnnotationComment Comment
hi def link latteAnnotation        PreProc
hi def link latteVariable          Identifier
hi def link latteType              Type

" prevent the file from loading again (see top)
let b:current_syntax = 'latte'
