" Vim syntax file
"
" Language:   eco
" Maintainer: Jay Adkisson
" URL:        https://github.com/AndrewRadev/vim-eco
"
" Mostly stolen from eruby.vim found at
" https://github.com/vim-ruby/vim-ruby

if !exists("g:eco_default_subtype")
  let g:eco_default_subtype = "html"
endif

if !exists("b:eco_subtype")
  let s:lines = getline(1)."\n".getline(2)."\n".getline(3)."\n".getline(4)."\n".getline(5)."\n".getline("$")
  let b:eco_subtype = matchstr(s:lines,'eco_subtype=\zs\w\+')
  if b:eco_subtype == ''
    let b:eco_subtype = matchstr(substitute(expand("%:t"),'\c\%(\.eco\)\+$','',''),'\.\zs\w\+$')
  endif
  if b:eco_subtype == 'rhtml'
    let b:eco_subtype = 'html'
  elseif b:eco_subtype == 'jst'
    let b:eco_subtype = 'html'
  elseif b:eco_subtype == 'rb'
    let b:eco_subtype = 'ruby'
  elseif b:eco_subtype == 'yml'
    let b:eco_subtype = 'yaml'
  elseif b:eco_subtype == 'js' || b:eco_subtype == 'json'
    let b:eco_subtype = 'javascript'
  elseif b:eco_subtype == 'txt'
    " Conventional; not a real file type
    let b:eco_subtype = 'text'
  elseif b:eco_subtype == ''
    if exists('b:current_syntax') && b:current_syntax != ''
      let b:eco_subtype = b:current_syntax
    else
      let b:eco_subtype = g:eco_default_subtype
    endif
  endif
endif

if exists("b:eco_subtype") && b:eco_subtype != '' && b:eco_subtype != 'eco'
  exec "runtime! syntax/".b:eco_subtype.".vim"
  silent! unlet b:current_syntax
  syn include @coffeeTop syntax/coffee.vim
endif

syn cluster ecoRegions contains=ecoBlock,ecoExpression,ecoComment

syn region ecoBlock matchgroup=ecoDelimiter start=/<%/ end=/%>/ contains=@coffeeTop containedin=ALLBUT,@ecoRegions keepend
syn region ecoExpression matchgroup=ecoDelimiter start=/<%[=\-]/ end=/%>/ contains=@coffeeTop containedin=ALLBUT,@ecoRegions keepend
syn region ecoComment matchgroup=ecoComment start=/<%#/ end=/%>/ contains=@coffeeTodo,@Spell containedin=ALLBUT,@ecoRegions keepend

" Eco features not in coffeescript proper
syn keyword ecoEnd end containedin=@ecoRegions
syn match ecoIndentColon /\s+\w+:/ containedin=@ecoRegions

" Missing colon errors. These show errors if you haven't ended a block-level
" clause with ":". They attempt to avoid the following:
"
"   - single-line postfix forms:      "foo" if bar()
"   - single-line if-then-else forms: if bar() then "foo" else "baz"
"
syn match ecoMissingColonError /\(<%[=-]\=\s*\)\@<=else\>\ze[^:]\{-}\s*%>/ containedin=@ecoRegions
syn match ecoMissingColonError /\(<%[=-]\=\s*\)\@<=if\>\%(.*then.*else\)\@![^:]\{-}\ze\s*%>/ containedin=@ecoRegions
syn match ecoMissingColonError /\(<%[=-]\=\s*\)\@<=for\>[^:]\{-}\<in\>[^:]\{-}\ze\s*%>/ containedin=@ecoRegions

" Define the default highlighting.

hi def link ecoDelimiter Delimiter
hi def link ecoComment Comment
hi def link ecoEnd coffeeConditional
hi def link ecoIndentColon None
hi def link ecoMissingColonError Error

let b:current_syntax = 'eco'

" vim: nowrap sw=2 sts=2 ts=8:
