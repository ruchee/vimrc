" Vim syntax file
" Language: Ruby Signature (RBS) <github.com/ruby/rbs>
" Author: Jeffrey Crochet <jlcrochet91@pm.me>
" URL: https://github.com/jlcrochet/vim-rbs

if exists("b:current_syntax")
  finish
endif

" Syntax {{{1
" Declarations {{{2
syn cluster rbsDeclarations contains=rbsClass,rbsModule,rbsInterface,rbsTypeAlias,rbsConstant,rbsGlobal,rbsComment

syn match rbsDefine /\%#=1\<class\>/ contained containedin=rbsClass nextgroup=rbsClassNameDefinition skipwhite
syn region rbsClass start=/\%#=1\<class\>/ matchgroup=rbsDefine end=/\%#=1\<end\>/ contains=@rbsDeclarations,@rbsMembers fold
syn match rbsClassNameDefinition /\%#=1\u\w*/ contained nextgroup=rbsClassTypeParameters,rbsInheritanceOperator skipwhite
syn region rbsClassTypeParameters matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=rbsTypeVariable nextgroup=rbsInheritanceOperator skipwhite
syn match rbsTypeVariable /\%#=1\u\w*/ contained
syn match rbsInheritanceOperator /\%#=1</ contained nextgroup=rbsSuperclassName skipwhite
syn match rbsSuperclassName /\%#=1\u\w*/ contained nextgroup=rbsTypeArguments
syn region rbsTypeArguments matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=@rbsTypes nextgroup=rbsTypeOperator skipwhite skipempty

syn match rbsDefine /\%#=1\<module\>/ contained containedin=rbsModule nextgroup=rbsModuleNameDefinition skipwhite
syn region rbsModule start=/\%#=1\<module\>/ matchgroup=rbsDefine end=/\%#=1\<end\>/ contains=@rbsDeclarations,@rbsMembers fold
syn match rbsModuleNameDefinition /\%#=1\u\w*/ contained nextgroup=rbsModuleTypeParameters,rbsInclusionOperator skipwhite
syn region rbsModuleTypeParameters matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=rbsTypeVariable nextgroup=rbsInclusionOperator skipwhite
syn match rbsInclusionOperator /\%#=1:/ contained nextgroup=rbsModuleSelfType skipwhite
syn match rbsModuleSelfType /\%#=1_\=\u\w*/ contained nextgroup=rbsModuleSelfTypeArguments,rbsModuleSelfTypeComma skipwhite
syn region rbsModuleSelfTypeArguments matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=@rbsTypes nextgroup=rbsModuleSelfTypeComma skipwhite
syn match rbsModuleSelfTypeComma /\%#=1,/ contained nextgroup=rbsModuleSelfType skipwhite

syn match rbsDefine /\%#=1\<interface\>/ contained containedin=rbsInterface nextgroup=rbsInterfaceNameDefinition skipwhite
syn region rbsInterface start=/\%#=1\<interface\>/ matchgroup=rbsDefine end=/\%#=1\<end\>/ contains=rbsMethod,rbsInclude,rbsMethodAlias fold
syn match rbsInterfaceNameDefinition /\%#=1_\u\w*/ contained nextgroup=rbsInterfaceTypeParameters skipwhite
syn region rbsInterfaceTypeParameters matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=rbsTypeVariable

syn keyword rbsKeyword out in unchecked contained containedin=rbsClassTypeParameters,rbsModuleTypeParameters,rbsInterfaceTypeParameters

syn keyword rbsTypeAlias type nextgroup=rbsAliasNameDefinition,rbsAliasNamespace,rbsAliasNamespaceSeparator skipwhite
syn match rbsAliasNameDefinition /\%#=1\l\w*/ contained nextgroup=rbsAliasOperator skipwhite
syn match rbsAliasOperator /\%#=1=/ contained nextgroup=@rbsTypes skipwhite
syn match rbsAliasNamespace /\%#=1\u\w*/ contained nextgroup=rbsAliasNamespaceSeparator skipwhite
syn match rbsAliasNamespaceSeparator /\%#=1::/ contained nextgroup=rbsAliasNamespace,rbsAliasNameDefinition skipwhite

syn match rbsConstant /\%#=1\u\w*/ nextgroup=rbsDeclarationOperator,rbsNamespaceSeparator skipwhite
syn match rbsDeclarationOperator /\%#=1:/ contained nextgroup=@rbsTypes skipwhite

syn match rbsGlobal /\%#=1\$\h\w*/ nextgroup=rbsDeclarationOperator skipwhite

syn region rbsComment matchgroup=rbsCommentStart start=/\%#=1#/ end=/\%#=1$/ contains=rbsTodo containedin=ALLBUT,rbsString,rbsSymbol,rbsComment
syn keyword rbsTodo BUG DEPRECATED FIXME NOTE OPTIMIZE TODO contained

" Members {{{2
syn cluster rbsMembers contains=rbsInstanceVariable,rbsMethod,rbsAttribute,rbsInclude,rbsExtend,rbsPrepend,rbsMethodAlias,rbsVisibility

syn match rbsInstanceVariable /\%#=1@\h\w*/ contained nextgroup=rbsDeclarationOperator skipwhite

syn keyword rbsMethod def contained nextgroup=rbsMethodName skipwhite
syn match rbsMethodName /\%#=1\%(\<self\>?\=\.\)\=[[:lower:]_]\w*[=?!]\=/ contained contains=rbsMethodSelf nextgroup=rbsMethodDeclarationOperator skipwhite
syn match rbsMethodName /\%#=1\%(\<self\>?\=\.\)\=\%([&|^/%]\|=\%(==\=\|\~\)\|>[=>]\=\|<\%(<\|=>\=\)\=\|[+\-~]@\=\|\*\*\=\|\[]=\=\|![@=~]\=\)/ contained nextgroup=rbsMethodDeclarationOperator skipwhite
syn match rbsMethodDeclarationOperator /\%#=1:/ contained nextgroup=rbsMethodType,rbsMethodParameters,rbsMethodTypeParameters,rbsMethodBlock skipwhite
syn region rbsMethodParameters matchgroup=rbsDelimiter start=/\%#=1(/ end=/\%#=1)/ contained contains=@rbsTypes,rbsMethodUnaryOperator nextgroup=rbsMethodArrow,rbsMethodBlock,rbsTypeOperator skipwhite skipempty
syn match rbsMethodUnaryOperator /\%#=1[?*]/ contained
syn match rbsMethodArrow /\%#=1->/ contained nextgroup=@rbsTypes skipwhite
syn region rbsMethodBlock matchgroup=rbsDelimiter start=/\%#=1?\={/ end=/\%#=1}/ contained contains=rbsMethodParameters nextgroup=rbsMethodArrow skipwhite
syn keyword rbsMethodType super contained
syn match rbsMethodType /\%#=1\.\.\./ contained
syn region rbsMethodTypeParameters matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=rbsClassName nextgroup=rbsMethodType,rbsMethodParameters,rbsMethodBlock skipwhite

syn keyword rbsAttribute attr_reader attr_writer attr_accessor contained nextgroup=rbsAttributeName skipwhite
syn match rbsAttributeName /\%#=1[[:lower:]_]\w*[=?!]\=/ contained nextgroup=rbsDeclarationOperator,rbsAttributeVariable skipwhite
syn match rbsAttributeName /\%#=1\%([&|^/%]\|=\%(==\=\|\~\)\|>[=>]\=\|<\%(<\|=>\=\)\=\|[+\-~]@\=\|\*\*\=\|\[]=\=\|![@=~]\=\)/ contained nextgroup=rbsAttributeDeclarationOperator skipwhite
syn region rbsAttributeVariable matchgroup=rbsDelimiter start=/\%#=1(/ end=/\%#=1)/ contained contains=rbsAttributeVariableName nextgroup=rbsDeclarationOperator skipwhite
syn match rbsAttributeVariableName /\%#=1@\h\w*/ contained

syn keyword rbsInclude include contained nextgroup=rbsClassName,rbsInterfaceName,rbsNamespaceSeparator skipwhite
syn keyword rbsExtend extend contained nextgroup=rbsClassName,rbsInterfaceName,rbsNamespaceSeparator skipwhite
syn keyword rbsPrepend prepend contained nextgroup=rbsClassName,rbsNamespaceSeparator skipwhite

syn keyword rbsMethodAlias alias contained nextgroup=rbsMethodAliasName skipwhite
syn match rbsMethodAliasName /\%#=1\%(\<self\>\.\)\=[[:lower:]_]\w*[=?!]\=/ contained contains=rbsMethodSelf nextgroup=rbsMethodAliasName skipwhite
syn match rbsMethodAliasName /\%#=1\%(\<self\>\.\)\=\%([&|^/%]\|=\%(==\=\|\~\)\|>[=>]\=\|<\%(<\|=>\=\)\=\|[+\-~]@\=\|\*\*\=\|\[]=\=\|![@=~]\=\)/ contained nextgroup=rbsMethodAliasName skipwhite

syn keyword rbsMethodSelf self contained nextgroup=rbsMethodSelfModifier,rbsMethodDot
syn match rbsMethodSelfModifier /\%#=1?/ contained
syn match rbsMethodDot /\%#=1\./ contained

" Types {{{2
syn cluster rbsTypes contains=
      \ rbsNamespaceSeparator,rbsClassName,rbsInterfaceName,rbsSingleton,rbsAliasName,
      \ @rbsLiterals,rbsRecord,rbsMethodParameters,rbsTuple,rbsProc,rbsType

syn match rbsNamespaceSeparator /\%#=1::/ contained nextgroup=rbsClassName,rbsInterfaceName,rbsAliasName

syn match rbsClassName /\%#=1\u\w*/ contained nextgroup=rbsTypeArguments,rbsNamespaceSeparator,rbsDeclarationOperator,rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn match rbsInterfaceName /\%#=1_\u\w*/ contained nextgroup=rbsTypeArguments,rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn keyword rbsSingleton singleton contained nextgroup=rbsSingletonArgument
syn region rbsSingletonArgument matchgroup=rbsDelimiter start=/\%#=1(/ end=/\%#=1)/ contained contains=rbsClassName,rbsNamespaceSeparator nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn match rbsAliasName /\%#=1\l\w*/ contained nextgroup=rbsDeclarationOperator,rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn cluster rbsLiterals contains=rbsString,rbsSymbol,rbsUnaryOperator,rbsInteger,rbsBoolean

syn region rbsString matchgroup=rbsStringStart start=/\%#=1"/ matchgroup=rbsStringEnd end=/\%#=1"/ contained contains=rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn match rbsStringEscape /\%#=1\\\%(\o\{1,3}\|x\x\x\=\|u\%(\x\{4}\|{\x\{1,6}\%(\s\x\{1,6}\)*}\)\|\%(c\|C-\)\%(\\M-\)\=.\|M-\%(\\c\|\\C-\)\=.\|\_.\)/ contained
syn region rbsString matchgroup=rbsStringStart start=/\%#=1'/ matchgroup=rbsStringEnd end=/\%#=1'/ contained contains=rbsQuoteEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn match rbsQuoteEscape /\%#=1\\[\\']/ contained

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%Q\=(/ matchgroup=rbsStringEnd end=/\%#=1)/ contained contains=rbsStringParentheses,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsStringParentheses matchgroup=rbsString start=/\%#=1(/ end=/\%#=1)/ transparent contained contains=rbsStringParentheses,rbsStringEscape

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%Q\=\[/ matchgroup=rbsStringEnd end=/\%#=1]/ contains=rbsStringSquareBrackets,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsStringSquareBrackets matchgroup=rbsString start=/\%#=1\[/ end=/\%#=1]/ transparent contained contains=rbsStringSquareBrackets,rbsStringEscape

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%Q\={/ matchgroup=rbsStringEnd end=/\%#=1}/ contains=rbsStringCurlyBraces,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsStringCurlyBraces matchgroup=rbsString start=/\%#=1{/ end=/\%#=1}/ transparent contained contains=rbsStringCurlyBraces,rbsStringEscape

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%Q\=</ matchgroup=rbsStringEnd end=/\%#=1>/ contains=rbsStringAngleBrackets,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsStringAngleBrackets matchgroup=rbsString start=/\%#=1</ end=/\%#=1>/ transparent contained contains=rbsStringAngleBrackets,rbsStringEscape

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%Q\=\z([~`!@#$%^&*_\-+=|\\:;"',.?/]\)/ matchgroup=rbsStringEnd end=/\%#=1\z1/ skip=/\%#=1\\\\\|\\\z1/ contained contains=rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%q(/ matchgroup=rbsStringEnd end=/\%#=1)/ contains=rbsRawStringParentheses,rbsParenthesisEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsRawStringParentheses matchgroup=rbsString start=/\%#=1(/ end=/\%#=1)/ transparent contained contains=rbsRawStringParentheses,rbsParenthesisEscape
syn match rbsParenthesisEscape /\%#=1\\[\\()]/ contained

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%q\[/ matchgroup=rbsStringEnd end=/\%#=1]/ contains=rbsRawStringSquareBrackets,rbsSquareBracketEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsRawStringSquareBrackets matchgroup=rbsString start=/\%#=1\[/ end=/\%#=1]/ transparent contained contains=rbsRawStringSquareBrackets,rbsSquareBracketEscape
syn match rbsSquareBracketEscape /\%#=1\\[\\\[\]]/ contained

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%q{/ matchgroup=rbsStringEnd end=/\%#=1}/ contains=rbsRawStringCurlyBraces,rbsCurlyBraceEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsRawStringCurlyBraces matchgroup=rbsString start=/\%#=1{/ end=/\%#=1}/ transparent contained contains=rbsRawStringCurlyBraces,rbsCurlyBraceEscape
syn match rbsCurlyBraceEscape /\%#=1\\[\\{}]/ contained

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%q</ matchgroup=rbsStringEnd end=/\%#=1>/ contains=rbsRawStringAngleBrackets,rbsAngleBracketEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsRawStringAngleBrackets matchgroup=rbsString start=/\%#=1</ end=/\%#=1>/ transparent contained contains=rbsRawStringAngleBrackets,rbsAngleBracketEscape
syn match rbsAngleBracketEscape /\%#=1\\[\\<>]/ contained

syn region rbsString matchgroup=rbsStringStart start=/\%#=1%q\z([~`!@#$%^&*_\-+=|\\:;"',.?/]\)/ matchgroup=rbsStringEnd end=/\%#=1\z1/ skip=/\%#=1\\\\\|\\\z1/ contained nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn match rbsSymbol /\%#=1:\h\w*[=?!]\=/ contained contains=rbsSymbolStart nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn match rbsSymbolStart /\%#=1:/ contained

syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1:"/ matchgroup=rbsSymbolEnd end=/\%#=1"/ contained contains=rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1:'/ matchgroup=rbsSymbolEnd end=/\%#=1'/ contained contains=rbsQuoteEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1%s(/  matchgroup=rbsSymbolEnd end=/\%#=1)/ contained contains=rbsStringParentheses,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1%s\[/ matchgroup=rbsSymbolEnd end=/\%#=1]/ contained contains=rbsStringSquareBrackets,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1%s{/  matchgroup=rbsSymbolEnd end=/\%#=1}/ contained contains=rbsStringCurlyBraces,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1%s</  matchgroup=rbsSymbolEnd end=/\%#=1>/ contained contains=rbsStringAngleBrackets,rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn region rbsSymbol matchgroup=rbsSymbolStart start=/\%#=1%s\z([~`!@#$%^&*_\-+=|\\:;"',.?/]\)/ matchgroup=rbsSymbolEnd end=/\%#=1\z1/ skip=/\%#=1\\\\\|\\\z1/ contained contains=rbsStringEscape nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn match rbsUnaryOperator /\%#=1[+-]/ contained nextgroup=rbsInteger
syn match rbsInteger /\%#=1[1-9]\d*\%(_\d\+\)*\>/ contained nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn match rbsInteger /\%#=10\%([bB][01]\+\%(_[01]\+\)*\|[oO]\o\+\%(_\o\+\)*\|[dD]\d\+\%(_\d\+\)*\|\o*\%(_\o\+\)\+\)\=\>/ contained nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn keyword rbsBoolean true false contained nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn region rbsRecord matchgroup=rbsDelimiter start=/\%#=1{/ end=/\%#=1}/ contained contains=rbsRecordName nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty
syn match rbsRecordName /\%#=1\h\w*[?!]\=:/ contained contains=rbsSymbolStart nextgroup=@rbsTypes skipwhite
syn region rbsRecordName matchgroup=rbsSymbolStart start=/\%#=1"/ matchgroup=rbsSymbolEnd end=/\%#=1":/ contained contains=rbsStringEscape nextgroup=@rbsTypes skipwhite
syn region rbsRecordName matchgroup=rbsSymbolStart start=/\%#=1'/ matchgroup=rbsSymbolEnd end=/\%#=1':/ contained contains=rbsQuoteEscape nextgroup=@rbsTypes skipwhite

syn region rbsTuple matchgroup=rbsDelimiter start=/\%#=1\[/ end=/\%#=1]/ contained contains=@rbsTypes nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn region rbsProc matchgroup=rbsDelimiter start=/\%#=1\^(/ end=/\%#=1)/ contained contains=@rbsTypes,rbsMethodUnaryOperator nextgroup=rbsMethodArrow skipwhite

syn keyword rbsType self instance class bool boolish untyped nil top bot void contained nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

syn match rbsTypeOperator /\%#=1[|&]/ contained nextgroup=@rbsTypes skipwhite skipempty
syn match rbsOptionalTypeOperator /\%#=1?/ contained nextgroup=rbsTypeOperator,rbsOptionalTypeOperator skipwhite skipempty

" Synchronization {{{1
syn sync fromstart

" Highlighting {{{1
hi def link rbsDefine Define
hi def link rbsClassNameDefinition Typedef
hi def link rbsDelimiter Delimiter
hi def link rbsTypeVariable Identifier
hi def link rbsInheritanceOperator Operator
hi def link rbsSuperclassName rbsClassName
hi def link rbsModuleNameDefinition Typedef
hi def link rbsInclusionOperator Operator
hi def link rbsModuleSelfType rbsClassName
hi def link rbsInterfaceNameDefinition Typedef
hi def link rbsKeyword Keyword
hi def link rbsTypeAlias Define
hi def link rbsAliasNameDefinition Typedef
hi def link rbsAliasOperator Operator
hi def link rbsAliasNamespace rbsClassName
hi def link rbsAliasNamespaceSeparator rbsNamespaceSeparator
hi def link rbsConstant Identifier
hi def link rbsDeclarationOperator Operator
hi def link rbsGlobal Identifier
hi def link rbsComment Comment
hi def link rbsCommentStart rbsComment
hi def link rbsInstanceVariable Identifier
hi def link rbsMethod Define
hi def link rbsMethodName Function
hi def link rbsMethodDeclarationOperator rbsDeclarationOperator
hi def link rbsMethodUnaryOperator rbsUnaryOperator
hi def link rbsMethodArrow Operator
hi def link rbsMethodType Type
hi def link rbsAttribute Define
hi def link rbsAttributeName Typedef
hi def link rbsAttributeVariableName rbsInstanceVariable
hi def link rbsInclude Statement
hi def link rbsExtend Statement
hi def link rbsPrepend Statement
hi def link rbsMethodAlias Define
hi def link rbsMethodAliasName rbsMethodName
hi def link rbsMethodSelf Constant
hi def link rbsMethodSelfModifier rbsMethodSelf
hi def link rbsMethodDot Operator
hi def link rbsNamespaceSeparator Operator
hi def link rbsClassName Type
hi def link rbsInterfaceName Type
hi def link rbsSingleton Type
hi def link rbsString String
hi def link rbsStringStart rbsString
hi def link rbsStringEnd rbsStringStart
hi def link rbsStringEscape PreProc
hi def link rbsQuoteEscape rbsStringEscape
hi def link rbsParenthesisEscape rbsStringEscape
hi def link rbsSquareBracketEscape rbsStringEscape
hi def link rbsCurlyBraceEscape rbsStringEscape
hi def link rbsAngleBracketEscape rbsStringEscape
hi def link rbsSymbol String
hi def link rbsSymbolStart rbsSymbol
hi def link rbsSymbolEnd rbsSymbolStart
hi def link rbsUnaryOperator Operator
hi def link rbsInteger Number
hi def link rbsBoolean Boolean
hi def link rbsRecordName rbsSymbol
hi def link rbsType Type
hi def link rbsTypeOperator Operator
hi def link rbsOptionalTypeOperator rbsTypeOperator
" }}}1

let b:current_syntax = "rbs"

" vim:fdm=marker
