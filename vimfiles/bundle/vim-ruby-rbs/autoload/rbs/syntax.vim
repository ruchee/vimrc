" Vim indent file
" Language: Ruby Signature (RBS) <github.com/ruby/rbs>
" Author: Jeffrey Crochet <jlcrochet@pm.me>
" URL: https://github.com/jlcrochet/vim-rbs

" This pattern matches all operators that can be used as methods; these
" are also the only operators that can be referenced as symbols.
let s:overloadable_operators = [
      \ '[&|^/%]',
      \ '=\%(==\=\|\~\)',
      \ '>[=>]\=',
      \ '<\%(<\|=>\=\)\=',
      \ '[+\-~]@\=',
      \ '\*\*\=',
      \ '\[]=\=',
      \ '![@=~]\='
      \ ]
const g:rbs#syntax#overloadable_operators = '\%('.join(s:overloadable_operators, '\|').'\)'

unlet s:overloadable_operators
