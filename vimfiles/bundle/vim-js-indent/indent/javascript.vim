" Vim indent script for JavaScript
" General:
" File:			javascript.vim
" Maintainer:	Jason Cheatham
" Last Change: 	2014-08-26
" Description:
" 	JavaScript indenter.
"
" Credits:
"	javascript.vim (2011 May 15) from Preston Koprivica
"	Improvements to comment handling by @Alvan (2014 Aug 26)

" Options: {{{

" set to 1 to make case statements align with the containing switch
if !exists('g:js_indent_flat_switch')
	let g:js_indent_flat_switch = 0
endif

" set to 1 to make case statements align with the containing switch
if !exists('g:js_indent_logging')
	let g:js_indent_logging = 0
endif

" }}}

" Setup: {{{

" Only load one indent script per buffer
if exists('b:did_indent')
	finish
endif
let b:did_indent = 1

setlocal indentexpr=GetJsIndent(v:lnum)
setlocal indentkeys=0],0),0},:,!^F,o,O,e

setlocal cindent
setlocal autoindent
" }}}

" Variables: {{{

" Inline comments (for anchoring other statements)
let s:js_mid_line_comment = '\s*\(\/\*.*\*\/\)*\s*'
let s:js_end_line_comment = s:js_mid_line_comment . '\s*\(\/\/.*\)*'
let s:js_line_comment = s:js_end_line_comment

" Comment/string syntax key
let s:syn_comment = '\(Comment\|String\|Regexp\|jsDoc\)'
" }}}

" Indenter: {{{
function! GetJsIndent(lnum)
	call s:Log('starting indent for line ' . a:lnum)

	" Grab the number of the first non-comment line prior to lnum
	let pnum = s:GetNonLineCommentLine(a:lnum-1)
	call s:Log('previous non-comment line: ' . pnum)

	" First line, start at indent = 0
	if pnum == 0
		call s:Log('first line -- returning')
		return 0
	endif

	let pline = getline(pnum)

	" Determine the current level of indentation
	let ind = indent(pnum)
	call s:Log('current indent: ' . ind)

	" Figure out what the indent should be

	if s:IsBlockCommentStart(pline) && !s:IsBlockCommentEnd(pline)
		call s:Log('block comment start')
		return ind + 1
	endif

	let pnbnum = prevnonblank(a:lnum - 1)

    if s:IsLineComment(getline(a:lnum)) && s:IsLineComment(getline(pnbnum))
        return indent(pnbnum)
    endif

    if s:IsBlockComment(pnbnum)
        if s:IsBlockCommentEnd(getline(pnbnum))
            call s:Log('block comment end')
            return ind - 1
        else
            call s:Log('block comment')
            return ind
        endif
	elseif s:IsVarBlockStart(pline) && !s:IsStatementEnd(pline)
		call s:Log('var block start')
		return ind + &sw
	endif

	let line = getline(a:lnum)

	if s:IsSwitchStartSameLine(pline) && !s:IsBlockEnd(line)
		s:Log('start switch')
		return ind + (g:js_indent_flat_switch ? 0 : &sw)
	elseif s:IsBlockEnd(line) && !s:IsComment(a:lnum)
		call s:Log('end block')
		return indent(s:GetBlockStart(a:lnum))
	elseif s:IsBlockStart(pline) 
		call s:Log('start block')
		return ind + &sw
	elseif s:IsArrayEnd(line) && !s:IsComment(a:lnum)
		call s:Log('end array')
		return indent(s:GetArrayStart(a:lnum))
	elseif s:IsArrayStart(pline) 
		call s:Log('start array')
		return ind + &sw 
	elseif s:IsParenEnd(line) && !s:IsComment(a:lnum)
		call s:Log('end parens')
		return indent(s:GetParenStart(a:lnum))
	elseif s:IsParenStart(pline) 
		call s:Log('start parens')
		return ind + &sw 
	endif

	" Grab the prior prior non-comment line number
	let ppline = getline(s:GetNonLineCommentLine(pnum-1))
	call s:Log('previous-previous non-comment line: ' . ppline)

	if s:IsStatementEnd(pline) && (s:IsVarBlockStart(ppline) || s:IsVarBlockMid(ppline))
		call s:Log('end of var block')
		return ind - &sw
	elseif s:IsContinuationLine(pline) 
		call s:Log('first continuation line')
		return indent(s:GetContinuationStart(pnum)) + &sw
	elseif s:IsContinuationLine(ppline)
		call s:Log('second continuation line')
		return ind - &sw
	elseif s:IsSwitchMid(pline) && !(s:IsSwitchMid(line) || s:IsBlockEnd(line))
		call s:Log('first line in case block')
		return ind + &sw
	elseif s:IsSwitchMid(line)
		call s:Log('case label')
		return ind - &sw
	elseif s:IsControlStart(pline) && !(s:IsControlMid(line) || line =~ '^\s*{\s*$')
		call s:Log('first line in a control statement')
		return ind + &sw
	elseif s:IsControlMid(pline) && !(s:IsControlMid(line) || s:IsBlockStart(line))
		call s:Log('non-block start within control statement')
		return ind + &sw
	elseif s:IsControlMid(line) && !(s:IsControlEnd(pline) || s:IsBlockEnd(pline))
		call s:Log('within control statement')
		return ind - &sw
	elseif (s:IsControlStart(ppline) || s:IsControlMid(ppline)) &&
			\ !(s:IsBlockStart(pline) || s:IsBlockEnd(pline))
		call s:Log('prior-prior control beg or mid')
		return ind - &sw
	endif

	call s:Log('no match')
	return ind
endfunction
" }}}

" Auxiliary functions: {{{

" IsInComment {{{
" Determine whether the specified position is contained in a comment.
function! s:IsInComment(lnum, cnum)
	return synIDattr(synID(a:lnum, a:cnum, 1), 'name') =~? s:syn_comment
endfunction'
" }}}

" IsComment {{{
" Determine whether a line is a comment or not.
function! s:IsComment(lnum)
	let line = getline(a:lnum)
	"Doesn't absolutely work.  Only Probably!
	let answer = s:IsInComment(a:lnum, 1) && s:IsInComment(a:lnum, strlen(line))
	call s:Log('line ' . a:lnum . ' is comment? ' . answer)
	return answer
endfunction
" }}}

" IsLineComment {{{
" Determine whether a line is a comment or not.
function! s:IsLineComment(line)
	let answer = a:line =~ '^\s*\/\/'
	call s:Log('IsLineComment(' . a:line . '): ' . answer)
	return answer
endfunction
" }}}

" IsInBlockComment {{{
" Determine whether a line is in a block comment or not.
function! s:IsInBlockComment(lnum, cnum)
	let lineType = synIDattr(synID(a:lnum, a:cnum, 1), 'name')
	call s:Log('IsInBlockComment(' . a:lnum . ', ' . a:cnum . '): ' . lineType)
	return lineType ==? 'javascriptComment' || lineType =~? '^jsDoc' || lineType =~? '^javascriptDoc'
endfunction
" }}}

" IsBlockCommentStart {{{
" Determine whether a line starts a block comment or not.
function! s:IsBlockCommentStart(line)
	let answer = a:line =~ '^\s*\/\*\*\?\(\s\+.*\)\?$'
	call s:Log('IsBlockCommentStart(' . a:line . '): ' . answer)
	return answer
endfunction
" }}}

" IsBlockCommentEnd {{{
" Determine whether a line ends a block comment or not.
function! s:IsBlockCommentEnd(line)
	let answer = a:line =~ '\*\/'
	call s:Log('IsBlockCommentEnd(' . a:line . '): ' . answer)
	return answer
endfunction
" }}}

" IsBlockComment {{{
" Determine whether a line is in a block comment or not.
function! s:IsBlockComment(lnum)
	let answer = s:IsInBlockComment(a:lnum, 1)
	call s:Log('is line ' . a:lnum . ' in a block comment? ' . answer)
	return answer
endfunction
" }}}

" GetBlockCommentStart {{{
" Get the first line of a javadoc-style comment
function! s:GetBlockCommentStart(lnum)
	let lnum = prevnonblank(a:lnum)
	while lnum > 0
		if IsBlockCommentStart(getline(lnum))
			return lnum
		else
			let lnum = prevnonblank(lnum - 1)
		endif
	endwhile

	return lnum
endfunction
" }}}

" GetNonCommentLine {{{
" Grab the nearest prior non-commented line.
function! s:GetNonLineCommentLine(lnum)
	let lnum = prevnonblank(a:lnum)

	while lnum > 0
		call s:Log('checking if line ' . lnum . ' is line comment...')
		if s:IsLineComment(lnum)
			call s:Log('line ' . lnum . ' is line comment')
			let lnum = prevnonblank(lnum - 1)
		else
			call s:Log('line ' . lnum . ' is not line comment')
			return lnum
		endif
	endwhile

	return lnum
endfunction
" }}}

" FirstCommaLine {{{
" Grab the first line that starts with a comma in the current block of
" lines ending with commas.
function! s:FirstCommaLine(lnum)
	let lnum = a:lnum
	let plnum = lnum

	while plnum > 0
		if s:IsVarBlockMid(plnum)
			let lnum = plnum
			let plnum = prevnonblank(plnum - 1)
		else
			return lnum
		endif
	endwhile

	return lnum
endfunction
" }}}

" SearchForPair {{{
" Return the beginning tag of a given pair starting from the given line.
function! s:SearchForPair(lnum, beg, end)
	" Save the cursor position
	let curpos = getpos(".")

	" Set the cursor position to the beginning of the line (default
	" behavior when using ==)
	call cursor(a:lnum, 1)

	" Search for the opening tag
	let mnum = searchpair(a:beg, '', a:end, 'bW', 
				\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? s:syn_comment' )

	"Restore the cursor position
	call cursor(curpos)
	
	return mnum
endfunction
" }}}

" Log {{{
if g:js_indent_logging
	function! s:Log(msg)
		echom a:msg
	endfunction
else
	function! s:Log(msg)
	endfunction
endif
"}}}

" }}}

" Helper functions: {{{

" Block helpers {{{
let s:object_beg = '{[^}]*' . s:js_end_line_comment . '$'
let s:object_end = '^' . s:js_mid_line_comment . '}[;,]\='

function! s:IsBlockStart(line)
	return a:line =~ s:object_beg
endfunction

function! s:IsBlockEnd(line)
	return a:line =~ s:object_end
endfunction 

function! s:GetBlockStart(lnum)
	return s:SearchForPair(a:lnum, '{', '}')
endfunction
" }}}

" Array helpers {{{
let s:array_beg = '\[[^\]]*' . s:js_end_line_comment . '$'
let s:array_end = '^' . s:js_mid_line_comment . '[^\[]*\]'

function! s:IsArrayStart(line)
	return a:line =~ s:array_beg
endfunction

function! s:IsArrayEnd(line)
	return a:line =~ s:array_end
endfunction 

function! s:GetArrayStart(lnum)
	return s:SearchForPair(a:lnum, '\[', '\]')
endfunction
" }}}

" MultiLine declaration/invocation helpers {{{
let s:paren_beg = '([^)]*' . s:js_end_line_comment . '$'
let s:paren_end = '^' . s:js_mid_line_comment . ')'

function! s:IsParenStart(line)
	return a:line =~ s:paren_beg
endfunction

function! s:IsParenEnd(line)
	return a:line =~ s:paren_end
endfunction 

function! s:GetParenStart(lnum)
	return s:SearchForPair(a:lnum, '(', ')')
endfunction
" }}}

" Continuation helpers {{{
let s:continuation = '\(+\|\\\)\{1}' . s:js_line_comment . '$' 

function! s:IsContinuationLine(line)
	return a:line =~ s:continuation
endfunction

function! s:GetContinuationStart(lnum) 
	let cur = a:lnum
	
	while s:IsContinuationLine(getline(cur)) 
		let cur -= 1
	endwhile
	
	return cur + 1
endfunction 
" }}}

" Switch helpers {{{
let s:switch_beg_next_line = 'switch\s*(.*)\s*' . s:js_mid_line_comment . s:js_end_line_comment . '$'
let s:switch_beg_same_line = 'switch\s*(.*)\s*' . s:js_mid_line_comment . '{\s*' . s:js_line_comment . '$'
let s:switch_mid = '^.*\(case.*\|default\)\s*:\s*' 

function! s:IsSwitchStartNextLine(line) 
	return a:line =~ s:switch_beg_next_line 
endfunction

function! s:IsSwitchStartSameLine(line) 
	return a:line =~ s:switch_beg_same_line 
endfunction

function! s:IsSwitchMid(line)
	return a:line =~ s:switch_mid
endfunction 
" }}}

" Control helpers {{{
let s:cntrl_beg_keys = '\(\(\(if\|for\|with\|while\)\s*(.*)\)\|\(try\|do\)\)\s*'
let s:cntrl_mid_keys = '\(\(\(else\s*if\|catch\)\s*(.*)\)\|\(finally\|else\)\)\s*'

let s:cntrl_beg = s:cntrl_beg_keys . s:js_end_line_comment . '$' 
let s:cntrl_mid = s:cntrl_mid_keys . s:js_end_line_comment . '$' 

let s:cntrl_end = '\(while\s*(.*)\)\s*;\=\s*' . s:js_end_line_comment . '$'

function! s:IsControlStart(line)
	return a:line =~ s:cntrl_beg
endfunction

function! s:IsControlMid(line)
	return a:line =~ s:cntrl_mid
endfunction

function! s:IsControlMidStrict(line)
	return a:line =~ s:cntrl_mid
endfunction

function! s:IsControlEnd(line)
	return a:line =~ s:cntrl_end
endfunction
" }}}

" Var block helpers {{{
let s:var_block_beg = '\<var\s\+\w\+\>.*,' . s:js_end_line_comment . '$'
let s:var_block_mid = ',' . s:js_end_line_comment . '$'
let s:statement_end = ';' . s:js_end_line_comment . '$'

function! s:IsVarBlockStart(line)
	let answer = a:line =~ s:var_block_beg
	call s:Log('checking if var block start: ' . answer)
	return answer
endfunction

function! s:IsVarBlockMid(line)
	return a:line =~ s:var_block_mid
endfunction

function! s:IsVarBlock(lnum)
	let first = s:FirstCommaLine(lnum)
	let answer = s:IsVarBlockStart(getline(first))
	call s:Log('is var block: ' . answer)
	return answer
endfunction

function! s:IsStatementEnd(line)
	let answer = a:line =~ s:statement_end
	call s:Log('is statement end: ' . answer)
	return answer
endfunction
" }}}

" }}}

" vim:set fdm=marker fdl=0 ts=4:
