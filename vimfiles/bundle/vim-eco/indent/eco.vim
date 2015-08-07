" Vim indent file
"
" Language:   eco
" Maintainer: Andrew Radev <andrey.radev@gmail.com>
" URL:        https://github.com/AndrewRadev/vim-eco
"
" Modified version of eruby indent found at
" https://github.com/vim-ruby/vim-ruby

if exists("b:did_indent")
  finish
endif

runtime! indent/coffee.vim
unlet! b:did_indent
setlocal indentexpr=

let b:html_indent_usestate = 0

runtime! indent/html.vim
unlet! b:did_indent

if &l:indentexpr == ''
  if &l:cindent
    let &l:indentexpr = 'cindent(v:lnum)'
  else
    let &l:indentexpr = 'indent(prevnonblank(v:lnum-1))'
  endif
endif
let b:html_indentexpr = &l:indentexpr

let b:did_indent = 1

setlocal indentexpr=GetEcoIndent(v:lnum)
setlocal indentkeys=*<Return>,<>>,{,},0),0],o,O,!^F,=end,=else,=when,<:>

" Only define the function once.
if exists("*GetEcoIndent")
  finish
endif

function! GetEcoIndent(lnum)
  " Workaround for Andy Wokula's HTML indent. This should be removed after
  " some time, since the newest version is fixed in a different way.
  if b:html_indentexpr =~# '^HtmlIndent('
	\ && exists('b:indent')
	\ && type(b:indent) == type({})
	\ && has_key(b:indent, 'lnum')
    " Force HTML indent to not keep state
    let b:indent.lnum = -1
  endif

  let vcol = col('.')
  call cursor(v:lnum,1)
  let incoffee = searchpair('<%','','%>','W')
  call cursor(a:lnum,vcol)
  if incoffee && getline(a:lnum) !~ '^<%\|^\s*[-=]\=%>'
    let ind = GetCoffeeIndent(a:lnum)
  else
    exe "let ind = ".b:html_indentexpr
  endif
  let lnum = prevnonblank(a:lnum-1)
  let line = getline(lnum)
  let cline = getline(a:lnum)

  if cline =~# '^\s*<%[-=]\=\s*\%(}\|end\|else:\|\%(else if\|when\).\{-\}\)\s*\%([-=]\=%>\|$\)'
    let ind = ind - &sw
  endif

  if line =~# '\S\s*<%[-=]\=\s*\%(}\|end\).\{-\}\s*\%([-=]\=%>\|$\)'
    let ind = ind - &sw
  endif

  if line =~# '[-=]>\s*[-=]\=%>'
    let ind = ind + &sw
  elseif line =~# '<%[-=]\=\s*\%(if\|for\|while\|until\|else\|else if\|switch\|when\|unless\)\>.*%>'
    let ind = ind + &sw
  endif

  if line =~# '<%[-=]\=\s*\%(if.\{-}then.\{-}else\).*%>'
    let ind = ind - &sw
  endif

  if line =~# '^\s*<%[=#-]\=\s*$' && cline !~# '^\s*end\>'
    let ind = ind + &sw
  endif

  if line !~# '^\s*<%' && line =~# '%>\s*$'
    let ind = ind - &sw
  endif

  if cline =~# '^\s*[-=]\=%>\s*$'
    let ind = ind - &sw
  endif

  return ind
endfunction

" vim:set sw=2 sts=2 ts=8 noet:
