" Vim indent file
" Language: Io
" Maintainer: June Kim <juneaftn@...>
" Last Change: 2005 Oct 29
" URL:
" Changes:
"

if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

setlocal indentexpr=GetIoIndent()
setlocal nolisp
setlocal nosmartindent
setlocal autoindent
setlocal indentkeys+=0)

if exists("*GetIoIndent")
	finish
endif

function GetIoIndent()
	let lnum = prevnonblank(v:lnum - 1)

	if lnum == 0
		return 0
	endif

	let ind = indent(lnum)
	let flag = 0
	if getline(lnum) =~ '([^)]*$'
		let ind = ind + &sw
		let flag = 1
	endif

	if getline(v:lnum) =~ '^\s*)'
		let ind = ind - &sw
	endif

	return ind
endfunction

