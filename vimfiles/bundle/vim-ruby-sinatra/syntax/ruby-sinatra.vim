" Vim syntax file
" Language:   Ruby, Sinatra
" Maintainer: Hallison Batista <email@hallisonbatista.com>
" URL:        http://gibhub.com/hallison/vim-ruby-sinatra
" License:    This script is released under the Vim License.
" Remark:     This script file was based in the original Ruby syntax highlight.

" Read the Ruby syntax.
if version < 600
  so <sfile>:p:h/ruby.vim
else
  runtime! syntax/ruby.vim
  unlet b:current_syntax
endif

if exists("b:current_syntax")
  finish
endif

syntax keyword rubyInclude   use register helpers
syntax keyword rubyAttribute set enable disable
syntax match   rubyControl   "\<\%(get\|put\|post\|delete\)\>[?!]\@!"
"syntax match rubyControl     "\<\%(set\|enable\|disable\)\>[?!]\@!"
"syntax match rubyControl     "\<\%(register\|use\)\>[?!]\@!"
syntax match   rubyControl   "\<\%(configure\|before\|after\)\>[?!]\@!"
"syntax keyword rubyHelpers   helpers
"syntax region  rubyDoBlock matchgroup=rubyHelpers start="\<do\>" end="\<end\>" contains=ALLBUT,@rubyNotTop fold

" Note: the following statements was extracted from the original Ruby syntax file for a hack that prevent wrong 'keywords'.
syn match rubyKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(get\|put\|post\|delete\|\)\>"       transparent contains=NONE
syn match rubyKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(set\|enable\|disable\|template\)\>" transparent contains=NONE
syn match rubyKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(register\|helpers\|use\)\>"         transparent contains=NONE

highlight def link rubyInclude    Include
highlight def link rubyHelpers    Statement
highlight def link rubyAttribute  Statement
highlight def link rubyControl    Statement
if !exists("ruby_no_identifiers")
  highlight def link rubyIdentifier Identifier
else
  highlight def link rubyIdentifier NONE
endif

let b:current_syntax = 'ruby-sinatra'

