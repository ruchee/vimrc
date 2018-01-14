" Vim syntax file
" Language: Carthage Files
" Maintainer: Colin Drake
" Latest Revision: 18 June 2015 

if exists("b:current_syntax")
  finish
endif

" Keywords
syntax keyword carthageKeyword git github binary

" Version numbers
syn match carthageNumber '\v\d+(\.\d+){,2}'

" Comments
syntax match carthageComment "\v#.*$"

" Operators
syntax match carthageOperator "\v\>\="
syntax match carthageOperator "\v\~>"
syntax match carthageOperator "\v\=\="

" Strings (version numbers, tags, etc.)
syntax region carthageString start=/\v"/ skip=/\v\\./ end=/\v"/
syntax region carthageString start=/\v'/ skip=/\v\\./ end=/\v'/

highlight link carthageKeyword Keyword
highlight link carthageNumber Constant
highlight link carthageComment Comment
highlight link carthageOperator Operator
highlight link carthageString String

let b:current_syntax = "carthage"
