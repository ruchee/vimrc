" Vim syntax file
" Language: RAML
" Maintainer: Alex Wynter

if exists("b:current_syntax")
  finish
endif

" Keywords
syn match ramlVersion '#%RAML.*$'
syn match types '\v(string|integer|int|boolean|date|number)\ze\n'
syn match genericKey '\w\{-}\ze:\(\s\|\n\)'
syn match namedParameters '\v(title|baseUri|version|schemas|resourceTypes|traits)\ze:'
syn match namedParameters '\v(description|queryParameters|responses|body|example)\ze:'
syn match namedParameters '\v(is|displayName|type|required)\ze:'
syn match methods '\v(get|post|put|delete)\ze:'
syn match resource '\/.\{-}\ze:'
syn match responseCode '\d\d\d\ze:'
syn match stringLiteral '\".\{-}\"'
syn match responseType 'application\/json'
syn match responseType 'application\/xml'
syn match comment '#.*$'

let b:current_syntax = "raml"

hi def link ramlVersion Comment
hi def link comment Comment
hi def link namedParameters Statement
hi def link methods Type
hi def link types Type
hi def link responseCode Comment
hi def link resource Special
hi def link genericKey Constant
hi def link stringLiteral String
hi def link responseType Identifier
