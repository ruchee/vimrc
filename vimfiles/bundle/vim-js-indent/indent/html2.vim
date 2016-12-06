" Vim indent script for HTML5
" General:
" File:			html.vim
" Maintainer:	Jason Cheatham
" Last Change: 	2016-04-17
" Description:
"	JavaScript indenter.

" Only load one indent script per buffer
if exists('b:did_indent')
	finish
endif
let b:did_indent = 1

" Options
" ------------------------------------------------------------------------------
" set to 1 to enable debug logging
if !exists('g:html_indent_logging')
	let g:html_indent_logging = 0
endif

" Setup
" ------------------------------------------------------------------------------
" The expression used to calculate the indent for a line
setlocal indentexpr=GetHtmlIndent(v:lnum)

" The keys that, when typed, will cause the indent to be calculated
setlocal indentkeys=o,O,<Return>,<>>,{,},!^F

" If this script has already been loaded, exit
if exists('*GetHtmlIndent')
	finish 
endif

" Get proper shiftwidth value
if exists('*shiftwidth')
	func s:sw()
		return shiftwidth()
	endfunc
else
	func s:sw()
		return &sw
	endfunc
endif

" Debug logging
if g:html_indent_logging
	function s:Log(...)
		echom join(a:000, '')
	endfunction
else
	function s:Log(...)
	endfunction
endif

" Variables
" ----------------------------------------------------------------------------
" Inline comments (for anchoring other statements)
let s:html_mid_line_comment = '<!--.\{-}\(-->\)'

" Indenter
" ----------------------------------------------------------------------------
function GetHtmlIndent(lnum)
	call s:Log('>> Getting HTML indent for ', a:lnum)
	" This calls a helper method so we can easily log the final indent value
	" (GetIndent has many return points).
	let ind = s:GetIndent(a:lnum)
	call s:Log('>> Indent = ', ind)
	return ind
endfunction

" Calculate the indent for a line number
function s:GetIndent(lnum)
	let pnbnum = prevnonblank(a:lnum - 1)
	let ind = indent(pnbnum)
	call s:Log('>> Starting indent for line ', a:lnum, ': ', ind)
	
	call s:Log('>> Checking if previous non-blank line (', pnbnum,
				\ ') opens a tag')
	if s:IsTagOpen(pnbnum)
		return ind + s:sw()
	endif
endfunction

function s:IsTagOpen(lnum)
	let line = getline(a:lnum)
	let result = 0
	let openTag = matchstr(line, '<\(\w\+\).\{-}>\s*\%(' . s:html_mid_line_comment . '\)\?\s*$')
	if openTag != ""
		let result = ta
	endif
	call s:Log('IsTagOpen(', line, '): ', result)
	return result
endfunction
