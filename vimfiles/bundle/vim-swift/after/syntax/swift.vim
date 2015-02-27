" File: after/syntax/swift.vim
" Author: Kevin Ballard
" Description: Conceal support for Swift
" Last Change: June 26, 2014

if !has('conceal') || &enc != 'utf-8' || get(b:, 'swift_no_conceal', get(g:, 'swift_no_conceal'))
    finish
endif

syn match swiftOperatorArrowHead contained '>' transparent contains=NONE conceal cchar= 
syn match swiftOperatorArrowTail contained '-' transparent contains=NONE conceal cchar=⟶
syn match swiftOperatorArrow '->\%([-/=+!*%<>&|^~.]\)\@!' contains=swiftOperatorArrowHead,swiftOperatorArrowTail transparent

syn match swiftIdentPrime /\i\@<=__*\>/me=s+1 conceal cchar=′ containedin=swiftIdentifier transparent contains=NONE

" vim: set et sw=4 ts=4:
