if exists("b:current_syntax")
  finish
endif


" Keywords
" https://github.com/ruby/rbs/blob/05515c3afeff52302accf090286b4ccc7abd7214/lib/ruby/signature/parser.y#L1141-L1173
syn keyword rbsDefine end alias
syn keyword rbsDefine class module interface nextgroup=rbsClassDeclaration skipwhite
syn keyword rbsDefine def nextgroup=rbsMethodDeclaration skipwhite
syn keyword rbsType void any untyped top bot bool boolish nil
syn keyword rbsMacro include extend prepend attr_reader attr_writer attr_accessor public private
" I'm not sure what should I categorize them
syn keyword rbsKeyword singleton super incompatible unchecked out in type self overload

" Comment
syn match rbsComment "#.*$" contains=@Spell

" Class
syn match rbsClassDeclaration "[^[:space:];#]\+" contained contains=rbsClassName
syn match rbsClassName "\%(\%(^\|[^.]\)\.\s*\)\@<!\<\u\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!" contained
syn match rbsConstant "\%(\%(^\|[^.]\)\.\s*\)\@<!\<\u\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!"

" Method
syn match rbsMethodDeclaration "[^[:space:]:]\+\ze:" contained

hi default link rbsKeyword		Keyword
hi default link rbsDefine		Define
hi default link rbsComment		Comment
hi default link rbsMacro		Macro
hi default link rbsType			Type
hi default link rbsClassName		rbsConstant
hi default link rbsConstant		Type
hi default link rbsMethodDeclaration	Identifier

let b:current_syntax = "rbs"

" vim: sw=2 sts=2 ts=8 noet:
