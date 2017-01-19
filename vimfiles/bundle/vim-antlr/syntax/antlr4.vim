" Vim syntax file
" Language: ANTLR4
" Maintainer: Adam Blinkinsop <blinks@acm.org>
" Last Change: 2014 May 1
" Remark: None.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax region antlrBlockComment start=/\/\*/ end=/\*\//
syntax match antlrLineComment /\/\/.*$/

syntax match antlrToken /[A-Z][a-zA-Z0-9_]*/
syntax region antlrLiteral start=/'/ end=/'/ contains=antlrEscape,antlrUnicodeEscape
syntax region antlrAlt start=/\[/ skip=/\\]/ end=/\]/ contains=antlrEscape,antlrUnicodeEscape
syntax match antlrEscape "\\[nrtbf\\']" contained
syntax match antlrUnicodeEscape "\\u[0-9a-fA-F]\{4}" contained

syntax match antlrRule /[a-z][a-zA-Z0-9_]*/
syntax match antlrNamedAction /@[a-zA-Z][a-zA-Z0-9_]*/

syntax keyword antlrKeyword import fragment lexer parser grammar returns locals throws catch finally mode options tokens rule

syntax match antlrLabel /#\s*[a-zA-Z][a-zA-Z0-9_]*/

syntax region antlrAction start=/{/ skip=/\\}\|\\{/ end=/}/

hi def link antlrBlockComment Comment
hi def link antlrLineComment Comment
hi def link antlrLiteral Character
hi def link antlrAlt Character
hi def link antlrToken String
hi def link antlrRule Function
hi def link antlrNamedAction Include
hi def link antlrKeyword Keyword
hi def link antlrEscape SpecialChar
hi def link antlrUnicodeEscape SpecialChar
hi def link antlrLabel Define
hi def link antlrAction PreCondit

let b:current_syntax = "antlr4"
