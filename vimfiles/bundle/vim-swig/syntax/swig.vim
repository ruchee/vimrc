" Swig syntax
" Language:    Swig
" Maintainer:  Brett
" Last Change: Jul 6th, 2013
" Version:	   0.1
" URL:         https://github.com/brettof86/vim-swigjs

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'html'
endif

runtime! syntax/javascript.vim
runtime! syntax/html.vim
unlet b:current_syntax


syn keyword swgTodo             TODO FIXME XXX contained

syn cluster htmlSwgContainer   add=htmlHead,htmlTitle,htmlString,htmlH1,htmlH2,htmlH3,htmlH4,htmlH5,htmlH6,htmlLink,String
" syn region  swgInside          start=/{(%|{)/ end=/(%|})}/  keepend transparent containedin=@htmlSwgContainer

syn region   swgBrackets        start=/{%/ end=/%}/  keepend transparent containedin=@htmlSwgContainer
syn region   swgVarBrackets     start=/{{/ end=/}}/  keepend transparent containedin=@htmlSwgContainer

syn match swgBracketsMtch       "{{\|{%\|%}\|}}"            containedin=@htmlSwgContainer
syn match   swgVars             "\<[A-Za-z_]\+\>"           containedin=@swgVarBrackets

syn match swgFilters          "|\s?[A-Za-z_]\+"             containedin=@swgBrackets

" not working
" syn match   swgInsideError      /{[{%][{]\?/    containedin=@hbsInside

syn match  swgPartial         "\<include\|partial\>"        containedin=@swgBrackets

syn match   swgConditionals    "\<if\|else\|elseif\|endif\>"                containedin=@swgBrackets,@htmlSwgContainer
syn match   swgHelpers         "\<for\|endfor\>"            containedin=@swgBrackets,@htmlSwgContainer


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lisp_syntax_inits")
  if version < 508
    let did_lisp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink swgTodo          Todo

  HiLink swgBracketsMtch        Identifier
  HiLink swgVarBrackets     Identifier

  HiLink swgConditionals    Conditional
  HiLink swgHelpers         Repeat

  HiLink swgPartial         Include
  HiLink swgFilters         Identifier

  HiLink swgVars            NONE

  delcommand HiLink
endif


let b:current_syntax = 'swigjs'
