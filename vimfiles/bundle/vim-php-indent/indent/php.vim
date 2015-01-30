" Vim indent file
" Language:	PHP
" Author:	John Wellesz <John.wellesz (AT) teaser (DOT) fr>
" URL:		http://www.2072productions.com/vim/indent/php.vim
" Home:		https://github.com/2072/PHP-Indenting-for-VIm
" Last Change:	2015 January 23rd
" Version:	1.58
"
"
"	Type :help php-indent for available options
"
"	A fully commented version of this file is available on github
"
"
"  If you find a bug, please open a ticket on github.org
"  ( https://github.com/2072/PHP-Indenting-for-VIm/issues ) with an example of
"  code that breaks the algorithm.
"

" NOTE: This script must be used with PHP syntax ON and with the php syntax
"	script by Lutz Eymers (http://www.isp.de/data/php.vim ) or with the
"	script by Peter Hodge (http://www.vim.org/scripts/script.php?script_id=1571 )
"	the later is bunbdled by default with Vim 7.
"
"
"	In the case you have syntax errors in your script such as HereDoc end
"	identifiers not at col 1 you'll have to indent your file 2 times (This
"	script will automatically put HereDoc end identifiers at col 1 if
"	they are followed by a ';').
"

" NOTE: If you are editing files in Unix file format and that (by accident)
"	there are '\r' before new lines, this script won't be able to proceed
"	correctly and will make many mistakes because it won't be able to match
"	'\s*$' correctly.
"	So you have to remove those useless characters first with a command like:
"
"	:%s /\r$//g
"
"	or simply 'let' the option PHP_removeCRwhenUnix to 1 and the script will
"	silently remove them when VIM load this script (at each bufread).
"
"
" Changes: 1.58		- Check shiftwidth() instead of 'shiftwidth' (will use
"			  the 'tabstop' value if 'shiftwidth' is 0)
"
" Changes: 1.57		- Fix an unreported non-blocking syntax error (VimLint)
"
" Changes: 1.56		- Enhance closure support in array definition
"			- Correctly indent line starting by a /**/ comment
"			- Don't indent last line of multiline string
"			  declarations.
"
" Changes: 1.55		- Remove optimization related to labels detection that
"			  could trigger indent issues when URLs are found in
"			  comments at end of lines...
"
" Changes: 1.54		- Add support for 'phpDocComment' syntax identifier
"
" Changes: 1.53		- Add support for `label:` (used with `goto`)
"			- Add `0]` to indentkeys
"
" Changes: 1.52		- Fix an edge case in conditional block declarations
"			  when the ')' of the condition is put on the same
"			  line as the following '{' (complement to issue #4)
"
" Changes: 1.51		- Fix issue #34 where indentation could get wrong with
"			  arrays defined using the short [] declaration.
"
" Changes: 1.50		- Allow the PHP_autoformatcomment option (default on)
"			  to work for any filetype containing 'php'.
"
" Changes: 1.49		- Added 'finally' as a block starter
"
" Changes: 1.48		- The 'use' keyword is now seen as a block starter (as
"			  used to handle trait conflicts resolution).
"
"			- Fix a issue with lines following a {} block defined
"			  on a single line if this {} block was just beneath a '{'
"
" Changes: 1.47		- Code in traits was not indented
"
" Changes: 1.46		- Fix issue #32 ('case:/default:' indentation issues in
"			  complex 'switch' blocks)
"
" Changes: 1.45		- Implemented support for multi-line block
"			  declarations (issue #4).
"
"			- Other small and very specific issues were discovered
"			  and fixed while implementing this.
"
" Changes: 1.44		- Fix issue #31 introduced in 1.43
"
" Changes: 1.43		- Fix issue #17 where closures' content would get
"			  extra indenting.
"
" Changes: 1.42		- Added support (with some restrictions) for
"			  multi-line string declarations (issue #22).
"
" Changes: 1.41		- Fix handing of ^}\s*else\n{ blocks which were not
"			  detected as new blocks and resulted in wrong indentation.
"
"			- Fix issue #23 where the script could hang in some
"			  specific cases involving closing braces at column 0;
"
"			- Fix issue #6 where nested switches would not indent
"			  correctly.
"
" Changes: 1.40		- Added the 'final' keyword as a block starter so final
"			  classes' code is indented correctly.
"
"			- No longer add 'w' to formatoptions VIm' setting as
"			  no other file-type plug-in uses it by default. This
"			  prevents leaving trailing white spaces when text
"			  wrapping.
"
" Changes: 1.39		- Also add 'StorageClass' syntax identifier (with an uppercase C) as it
"			  also exists in the syntax file.
"
" Changes: 1.38		- Fix an incredibly old bug that managed to survive
"			  unnoticed until today: the PHP code identifier routine was missing a few
"			  syntax names (Define, Structure, Storageclass and Exception). If you started
"			  indenting on such a line, nothing would happen as
"			  the script thought it wasn't actual PHP code...
"
" Changes: 1.37		- Fix a bug for inline script element [imicky]
"			- Fix issue #11: https://github.com/2072/PHP-Indenting-for-VIm/issues/11
"
" Changes: 1.36		- Added support for short array declaration (Thanks to
"			  Warren Seymour)
"
" Changes: 1.35		- New option: PHP_outdentSLComments to add extra
"			  indentation to single-line comments.
"
"
" Changes: 1.34		- Fix: string with /* would be treated as comment
"			  start when using single quote. (Thanks to Manic Chuang
"			  for the fix)
"
"
" Changes: 1.33		- Rewrote Switch(){case:default:} handling from
"			  scratch in a simpler more logical and infallible way...
"			- Removed PHP_ANSI_indenting which is no longer
"			  needed.
"
"
" Changes: 1.32b	- Added PHP_ANSI_indenting and PHP_outdentphpescape
"			  options details to VIm documentation (:help php-indent).
"
"
" Changes: 1.32		- Added a new option: PHP_ANSI_indenting
"
"
" Changes: 1.31a	- Added a new option: PHP_outdentphpescape to indent
"			  PHP tags as the surrounding code.
"
" Changes: 1.30		- Fixed empty case/default indentation again :/
"			- The ResetOptions() function will be called each time
"			  the ftplugin calls this script, previously it was
"			  executed on BufWinEnter and Syntax events.
"
"
" Changes: 1.29		- Fixed php file detection for ResetOptions() used for
"			  comments formatting. It now uses the same tests as
"			  filetype.vim. ResetOptions() will be correctly
"			  called for *.phtml, *.ctp and *.inc files.
"
"
" Changes: 1.28		- End HEREDOC delimiters were not considered as such
"			  if they were not followed by a ';'.
"			- Added support for NOWDOC tags ($foo = <<<'bar')
"
"
" Changes: 1.27		- if a "case" was preceded by another "case" on the
"			  previous line, the second "case" was indented incorrectly.
"
" Changes: 1.26		- '/*' character sequences found on a line
"			  starting by a '#' were not dismissed by the indenting algorithm
"			  and could cause indentation problem in some cases.
"
"
" Changes: 1.25		- Fix some indentation errors on multi line conditions
"			  and multi line statements.
"			- Fix when array indenting is broken and a closing
"			');' is placed at the start of the line, following
"			lines will be indented correctly.
"			- New option: PHP_vintage_case_default_indent (default off)
"			- Minor fixes and optimizations.
"
"
" Changes: 1.24		- Added compatibility with the latest version of
"			  php.vim syntax file by Peter Hodge (http://www.vim.org/scripts/script.php?script_id=1571)
"			  This fixes wrong indentation and ultra-slow indenting
"			  on large php files...
"			- Fixed spelling in comments.
"
"
" Changes: 1.23		- <script> html tags are now correctly indented the same
"			  way their content is.
"			- <?.*?> (on a single line) PHP declarations are now
"			  always considered as non-PHP code and let untouched.
"
" Changes: 1.22		- PHPDoc comments are now indented according to the
"			  surrounding code.
"			- This is also true for '/* */' multi-line comments
"			  when the second line begins by a '*'.
"			- Single line '/* */' comments are also indented.
"
"
" Changes: 1.21		- 'try' and 'catch' were not registered as block starters so the '{'
"			  after a 'try' or 'catch' could be wrongly indented...
"			  (thanks to Gert Muller for finding this issue)
"
" Changes: 1.20		- Line beginning by a single or double quote followed
"			  by a space would cause problems... this was related
"			  to the bug correction of version 1.10 - Thanks to
"			  David Fishburn for finding this (he was lucky).
"			- Changed the way this script set the 'formatoptions'
"			  setting, now it uses '-=' and '+='
"			- New option: PHP_autoformatcomment (defaults to 1),
"			  if set to 0 the 'formatoptions' setting will not be
"			  altered.
"			- When PHP_autoformatcomment is not 0, the 'comments'
"			  setting is set to the type of comments that PHP
"			  supports.
"
" Changes: 1.19		- Indentation of '*/' delimiter of '/**/' won't be broken by
"			  strings or '//' comments containing the "/*" character sequence.
"
" Changes: 1.182	- I Forgot to register 'interface' and 'abstract' as block starters so the '{'
"			  after them could be wrongly indented...
"
" Changes: 1.181	- I Forgot to register 'class' as a block starter so the '{'
"			  after a 'class' could be wrongly indented...
"
" Changes: 1.18		- No more problems with Vim 6.3 and UTF-8.
"			- Opening braces "{" are always indented according to their block starter.
"
"				Instead of:
"
"					if( $test
"					    && $test2 )
"					    {
"					    }
"
"				You have:
"
"					if( $test
"					    && $test2 )
"					{
"					}
"
"
" Changes: 1.17		- Now following parts of split lines are indented:
"
"				Instead of:
"
"					$foo=
"					"foo"
"					."foo";
"
"				You have:
"
"					$foo=
"					    "foo"
"					    ."foo";
"
"			- If a "case : break;" was declared on a single line, the
"			  following "case" was not indented correctly.
"			- If a </script> html tag was preceded by a "?>" it wasn't indented.
"			- Some other minor corrections and improvements.
"
"
" Changes: 1.16		- Now starting and ending '*' of multiline '/* */' comments are aligned
"			  on the '*' of the '/*' comment starter.
"			- Some code improvements that make indentation faster.
"
" Changes: 1.15		- Corrected some problems with the indentation of
"			  multiline "array()" declarations.
"
" Changes: 1.14		- Added auto-formatting for comments (using the Vim option formatoptions=qroc).
"			- Added the script option PHP_BracesAtCodeLevel to
"			  indent the '{' and '}' at the same level than the
"			  code they contain.
"
" Changes: 1.13		- Some code cleaning and typo corrections (Thanks to
"			  Emanuele Giaquinta for his patches)
"
" Changes: 1.12		- The bug involving searchpair() and utf-8 encoding in Vim 6.3 will
"			  not make this script to hang but you'll have to be
"			  careful to not write '/* */' comments with other '/*'
"			  inside the comments else the indentation won't be correct.
"			  NOTE: This is true only if you are using utf-8 and vim 6.3.
"
" Changes: 1.11		- If the "case" of a "switch" wasn't alone on its line
"			  and if the "switch" was at col 0 (or at default indenting)
"			  the lines following the "case" were not indented.
"
" Changes: 1.10		- Lines beginning by a single or double quote were
"			  not indented in some cases.
"
" Changes: 1.09		- JavaScript code was not always directly indented.
"
" Changes: 1.08		- End comment tags '*/' are indented like start tags '/*'.
"			- When typing a multiline comment, '}' are indented
"			  according to other commented '{'.
"			- Added a new option 'PHP_removeCRwhenUnix' to
"			  automatically remove CR at end of lines when the file
"			  format is Unix.
"			- Changed the file format of this very file to Unix.
"			- This version seems to correct several issues some people
"			  had with 1.07.
"
" Changes: 1.07		- Added support for "Here document" tags:
"			   - HereDoc end tags are indented properly.
"			   - HereDoc content remains unchanged.
"			- All the code that is outside PHP delimiters remains
"			  unchanged.
"			- New feature: The content of <script.*> html tags is considered as PHP
"			  and indented according to the surrounding PHP code.
"			- "else if" are detected as "elseif".
"			- Multiline /**/ are indented when the user types it but
"			  remain unchanged when indenting from their beginning.
"			- Fixed indenting of // and # comments.
"			- php_sync_method option is set to 0 (fromstart).
"			  This is required for complex PHP scripts else the indent
"			  may fail.
"			- Files with non PHP code at the beginning could alter the indent
"			  of the following PHP code.
"			- Other minor improvements and corrections.
"
" Changes: 1.06:    - Switch block were no longer indented correctly...
"		    - Added an option to use a default indenting instead of 0.
"		      (whereas I still can't find any good reason to use it!)
"		    - A problem with ^\s*);\= lines where ending a non '{}'
"		      structure.
"		    - Changed script local variable to be buffer local
"		      variable instead.
"
" Changes: 1.05:    - Lines containing "<?php ?>" and "?> <?php"
"		      (start and end tag on the same line) are no
"		      longer indented at col 1 but as normal code.
"
" Changes: 1.04:    - Strings containing "//" could break the indenting
"		      algorithm.
"		    - When a '{}' block was at col 1, the second line of the
"		      block was not indented at all (because of a stupid
"		      optimization coupled with a bug).
"
" Changes: 1.03:    - Some indenting problems corrected: end of non '{}'
"		      structures was not detected in some cases. The part of
"		      code concerned have been re-written
"		    - PHP start tags were not indented at col 1
"		    - Wrong comment in the code have been corrected
"
" Changes: 1.02:    - The bug I was talking about in version 1.01 (right below) has
"		      been corrected :)
"		    - Also corrected another bug that could occur in
"		      some special cases.
"		    - I removed the debug mode left in 1.01 that could
"		      cause some Vim messages at loading if other script were
"		      bugged.
"
" Changes: 1.01:    - Some little bug corrections regarding automatic optimized
"		      mode that missed some tests and could break the indenting.
"		    - There is also a problem with complex non bracketed structures, when several
"		      else are following each other, the algorithm do not indent the way it
"		      should.
"		      That will be corrected in the next version.
"

"
" Remove all the comments from this file (when sending this file to Bram):
" :%s /^\s*".*\({{{\|xxx\)\@<!\n\c//g
" }}} the header needs to be re-included afterwards

" The 4 following lines prevent this script from being loaded several times per buffer.
" They also prevent the load of different indent scripts for PHP at the same time.
if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

"	This script sets the option php_sync_method of PHP syntax script to 0
"	(fromstart indenting method) in order to have an accurate syntax.
"	If you are using very big PHP files (which is a bad idea) you will
"	experience slowings down while editing, if your code contains only PHP
"	code you can comment the line below.

let g:php_sync_method = 0

" Get the effective value of 'shiftwidth'. Vim since 7.3-703 allows a value of
" 0, which uses the value of 'tabstop', in which case we need to use the
" shiftwidth() function.
if exists('*shiftwidth')
  function! s:sw()
    return shiftwidth()
  endfunction
else
  function! s:sw()
    return &shiftwidth
  endfunction
endif

" Apply options

if exists("PHP_default_indenting")
    let b:PHP_default_indenting = PHP_default_indenting * s:sw()
else
    let b:PHP_default_indenting = 0
endif

if exists("PHP_outdentSLComments")
    let b:PHP_outdentSLComments = PHP_outdentSLComments * s:sw()
else
    let b:PHP_outdentSLComments = 0
endif

if exists("PHP_BracesAtCodeLevel")
    let b:PHP_BracesAtCodeLevel = PHP_BracesAtCodeLevel
else
    let b:PHP_BracesAtCodeLevel = 0
endif


if exists("PHP_autoformatcomment")
    let b:PHP_autoformatcomment = PHP_autoformatcomment
else
    let b:PHP_autoformatcomment = 1
endif

if exists("PHP_outdentphpescape")
    let b:PHP_outdentphpescape = PHP_outdentphpescape
else
    let b:PHP_outdentphpescape = 1
endif


if exists("PHP_vintage_case_default_indent") && PHP_vintage_case_default_indent
    let b:PHP_vintage_case_default_indent = 1
else
    let b:PHP_vintage_case_default_indent = 0
endif



let b:PHP_lastindented = 0
let b:PHP_indentbeforelast = 0
let b:PHP_indentinghuge = 0
let b:PHP_CurrentIndentLevel = b:PHP_default_indenting
let b:PHP_LastIndentedWasComment = 0
let b:PHP_InsideMultilineComment = 0
" PHP code detect variables
let b:InPHPcode = 0
let b:InPHPcode_checked = 0
let b:InPHPcode_and_script = 0
let b:InPHPcode_tofind = ""
let b:PHP_oldchangetick = b:changedtick
let b:UserIsTypingComment = 0
let b:optionsset = 0

" The 4 options belows are overridden by indentexpr so they are always off
" anyway...
setlocal nosmartindent
setlocal noautoindent
setlocal nocindent
" autoindent must be on, so the line below is also useless...
setlocal nolisp

setlocal indentexpr=GetPhpIndent()
setlocal indentkeys=0{,0},0),0],:,!^F,o,O,e,*<Return>,=?>,=<?,=*/



let s:searchpairflags = 'bWr'

" Clean CR when the file is in Unix format
if &fileformat == "unix" && exists("PHP_removeCRwhenUnix") && PHP_removeCRwhenUnix
    silent! %s/\r$//g
endif

" Only define the functions once per Vim session.
if exists("*GetPhpIndent")
    call ResetPhpOptions()
    finish " XXX -- comment this line for easy dev
endif
"setlocal debug=msg " XXX -- do not comment this line when modifying this file

" enable debug calls: :%s /" DEBUG \zec//g
" disable debug calls: :%s /^\s*\zs\zecall DebugPrintReturn/" DEBUG /g

let s:PHP_validVariable = '[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*'
let s:notPhpHereDoc = '\%(break\|return\|continue\|exit\|die\|else\)'
let s:blockstart = '\%(\%(\%(}\s*\)\=else\%(\s\+\)\=\)\=if\>\|\%(}\s*\)\?else\>\|do\>\|while\>\|switch\>\|case\>\|default\>\|for\%(each\)\=\>\|declare\>\|class\>\|trait\>\|use\>\|interface\>\|abstract\>\|final\>\|try\>\|\%(}\s*\)\=catch\>\|\%(}\s*\)\=finally\>\)'
let s:functionDecl = '\<function\>\%(\s\+'.s:PHP_validVariable.'\)\=\s*(.*'
let s:endline= '\s*\%(//.*\|#.*\|/\*.*\*/\s*\)\=$'

" Terminated line?
    " - a line terminated by a ";" optionally followed by a "?>" or "}"
    " - a HEREDOC starter line (the content of such block is never seen by this script)
    " - a "}" not followed by a "{"
    " - a goto's label

let s:terminated = '\%(\%(;\%(\s*\%(?>\|}\)\)\=\|<<<''\=\a\w*''\=$\|^\s*}\|^\s*'.s:PHP_validVariable.':\)'.s:endline.'\)\|^[^''"`]*[''"`]$'
let s:PHP_startindenttag = '<?\%(.*?>\)\@!\|<script[^>]*>\%(.*<\/script>\)\@!'



let s:escapeDebugStops = 0
function! DebugPrintReturn(scriptLine)

    if ! s:escapeDebugStops
	echo "debug:" . a:scriptLine
	let c = getchar()
	if c == "\<Del>"
	    let s:escapeDebugStops = 1
	end
    endif

endfunction

function! GetLastRealCodeLNum(startline) " {{{
    "Inspired from the function SkipJavaBlanksAndComments by Toby Allsopp for indent/java.vim

    let lnum = a:startline

    " Used to indent <script.*> html tag correctly
    if b:GetLastRealCodeLNum_ADD && b:GetLastRealCodeLNum_ADD == lnum + 1
	let lnum = b:GetLastRealCodeLNum_ADD
    endif

    while lnum > 1
	let lnum = prevnonblank(lnum)
	let lastline = getline(lnum)

	" if we are inside an html <script> we must skip ?> tags to indent
	" everything as php
	if b:InPHPcode_and_script && lastline =~ '?>\s*$'
	    let lnum = lnum - 1
	elseif lastline =~ '^\s*?>.*<?\%(php\)\=\s*$'
	    let lnum = lnum - 1
	elseif lastline =~ '^\s*\%(//\|#\|/\*.*\*/\s*$\)'
	    " if line is under comment
	    let lnum = lnum - 1
	elseif lastline =~ '\*/\s*$'
	    " skip multiline comments
	    call cursor(lnum, 1)
	    if lastline !~ '^\*/'
		call search('\*/', 'W')
		" position the cursor on the first */
	    endif
	    let lnum = searchpair('/\*', '', '\*/', s:searchpairflags, 'Skippmatch2()')
	    " find the most outside /*

	    let lastline = getline(lnum)
	    if lastline =~ '^\s*/\*'
		" if line contains nothing but comment
		" do the job again on the line before (a comment can hide another...)
		let lnum = lnum - 1
	    else
		break
	    endif


	elseif lastline =~? '\%(//\s*\|?>.*\)\@<!<?\%(php\)\=\s*$\|^\s*<script\>'
	    " skip non php code

	    while lastline !~ '\(<?.*\)\@<!?>' && lnum > 1
		let lnum = lnum - 1
		let lastline = getline(lnum)
	    endwhile
	    if lastline =~ '^\s*?>'
		" if line contains nothing but end tag
		let lnum = lnum - 1
	    else
		break
		" else there is something important before the ?>
	    endif


	    " Manage "here document" tags
	elseif lastline =~? '^\a\w*;\=$' && lastline !~? s:notPhpHereDoc
	    " match the end of a heredoc
	    let tofind=substitute( lastline, '\(\a\w*\);\=', '<<<''\\=\1''\\=$', '')
	    while getline(lnum) !~? tofind && lnum > 1
		let lnum = lnum - 1
	    endwhile
	elseif lastline =~ '^[^''"`]*[''"`][;,]'.s:endline
	    " match multiline strings
	    let tofind=substitute( lastline, '^.*\([''"`]\)[;,].*$', '^[^\1]\\+[\1]$', '')
	    " DEBUG call DebugPrintReturn( 'mls end, to find:' . tofind)
	    while getline(lnum) !~? tofind && lnum > 1
		let lnum = lnum - 1
	    endwhile
	else
	    " if none of these were true then we are done
	    break
	endif
    endwhile

    if lnum==1 && getline(lnum) !~ '<?'
	let lnum=0
    endif

    " This is to handle correctly end of script tags; to return the real last php
    " code line else a '?>' could be returned has last_line
    if b:InPHPcode_and_script && 1 > b:InPHPcode
	let b:InPHPcode_and_script = 0
    endif

    return lnum
endfunction " }}}

function! Skippmatch2()

    let line = getline(".")

    " Skip opening /* if they are inside a string or preceded  by a single
    " line comment starter
    if line =~ "\\([\"']\\).*/\\*.*\\1" || line =~ '\%(//\|#\).*/\*'
	return 1
    else
	return 0
    endif
endfun

function! Skippmatch()	" {{{
    " the slowest instruction of this script, remove it and the script is 3
    " times faster but you may have troubles with '{' inside comments or strings
    " that will break the indent algorithm...
    let synname = synIDattr(synID(line("."), col("."), 0), "name")
	" DEBUG call DebugPrintReturn('Skippmatch():'.synname ." ". b:UserIsTypingComment .' online: ' . line("."))
    if synname == "Delimiter" || synname == "phpRegionDelimiter" || synname =~# "^phpParent" || synname == "phpArrayParens" || synname =~# '^php\%(Block\|Brace\)' || synname == "javaScriptBraces" || synname =~# '^php\%(Doc\)\?Comment' && b:UserIsTypingComment
	return 0
    else
	return 1
    endif
endfun " }}}

function! FindOpenBracket(lnum, blockStarter) " {{{
    " set the cursor to the start of the lnum line
    call cursor(a:lnum, 1)
    let line = searchpair('{', '', '}', 'bW', 'Skippmatch()')

    if a:blockStarter == 1
	while line > 1
	    let linec = getline(line)

	    if linec =~ s:terminated || linec =~ '^\s*\%(' . s:blockstart . '\)\|'. s:functionDecl . s:endline
		break
	    endif

	    let line = GetLastRealCodeLNum(line - 1)
	endwhile
    endif

    return line
endfun " }}}

function! FindTheIfOfAnElse (lnum, StopAfterFirstPrevElse) " {{{

    if getline(a:lnum) =~# '^\s*}\s*else\%(if\)\=\>'
	" we do this so we can find the opened bracket to speed up the process
	let beforeelse = a:lnum
    else
	let beforeelse = GetLastRealCodeLNum(a:lnum - 1)
    endif

    if !s:level
	let s:iftoskip = 0
    endif

    " If we've found another "else" then it means we need to skip the next "if"
    " we'll find.
    if getline(beforeelse) =~# '^\s*\%(}\s*\)\=else\%(\s*if\)\@!\>'
	let s:iftoskip = s:iftoskip + 1
    endif

    " A closing bracket? let skip the whole block to save some recursive calls
    if getline(beforeelse) =~ '^\s*}'
	let beforeelse = FindOpenBracket(beforeelse, 0)

	" Put us on the block starter
	if getline(beforeelse) =~ '^\s*{'
	    let beforeelse = GetLastRealCodeLNum(beforeelse - 1)
	endif
    endif


    " sometimes it's not useful to find the very first if of a long if elseif
    " chain. The previous elseif will be enough
    if !s:iftoskip && a:StopAfterFirstPrevElse && getline(beforeelse) =~# '^\s*\%([}]\s*\)\=else\%(if\)\=\>'
	return beforeelse
    endif

    " if there was an else, then there is a if...
    if getline(beforeelse) !~# '^\s*if\>' && beforeelse>1 || s:iftoskip && beforeelse>1

	if s:iftoskip && getline(beforeelse) =~# '^\s*if\>'
	    let s:iftoskip = s:iftoskip - 1
	endif

	let s:level =  s:level + 1
	let beforeelse = FindTheIfOfAnElse(beforeelse, a:StopAfterFirstPrevElse)
    endif

    return beforeelse

endfunction " }}}

let s:defaultORcase = '^\s*\%(default\|case\).*:'

function! FindTheSwitchIndent (lnum) " {{{

    let test = GetLastRealCodeLNum(a:lnum - 1)

    if test <= 1
	return indent(1) - s:sw() * b:PHP_vintage_case_default_indent
    end

    " A closing bracket? let skip the whole block to save some recursive calls
    while getline(test) =~ '^\s*}' && test > 1
	" DEBUG call DebugPrintReturn(test . ' is a } skipping...')
	let test = GetLastRealCodeLNum(FindOpenBracket(test, 0) - 1)

	" Put us on the line above the block starter if it's a switch since
	" it's not the one we want.
	if getline(test) =~ '^\s*switch\>'
	    let test = GetLastRealCodeLNum(test - 1)
	endif
	" DEBUG call DebugPrintReturn('skipped to '. test)
    endwhile

    " did we find it?
    if getline(test) =~# '^\s*switch\>'
	" DEBUG call DebugPrintReturn('found the switch on ' . test)
	return indent(test)
    elseif getline(test) =~# s:defaultORcase
	" DEBUG call DebugPrintReturn('found a default/case on ' . test)
	return indent(test) - s:sw() * b:PHP_vintage_case_default_indent
    else
	" DEBUG call DebugPrintReturn('recursing from ' . test)
	return FindTheSwitchIndent(test)
    endif

endfunction "}}}

" 2013-08-02: I wish I had lists and dictionaries when I designed this
" script 9 years ago (wait... what? 9 years !?!?!)...
let s:SynPHPMatchGroups = {'phpParent':1, 'Delimiter':1, 'Define':1, 'Storageclass':1, 'StorageClass':1, 'Structure':1, 'Exception':1}
function! IslinePHP (lnum, tofind) " {{{
    " This function asks to the syntax if the pattern 'tofind' on the line
    " number 'lnum' is PHP code (very slow...).
    let cline = getline(a:lnum)

    if a:tofind==""
	" This correct the issue where lines beginning by a
	" single or double quote were not indented in some cases.
	let tofind = "^\\s*[\"'`]*\\s*\\zs\\S"
    else
	let tofind = a:tofind
    endif

    " ignore case
    let tofind = tofind . '\c'

    " find the first non blank char in the current line
    let coltotest = match (cline, tofind) + 1

    " ask to syntax what is its name
    let synname = synIDattr(synID(a:lnum, coltotest, 0), "name")

    " don't see string content as php
    if synname == 'phpStringSingle' || synname == 'phpStringDouble' || synname == 'phpBacktick'
	if cline !~ '^\s*[''"`]'
	    return "SpecStringEntrails"
	else
	    return synname
	end
    end

    " DEBUG call DebugPrintReturn('IslinePHP(): ' . synname)
    if get(s:SynPHPMatchGroups, synname) || synname =~ '^php' ||  synname =~? '^javaScript'
	return synname
    else
	return ""
    endif
endfunction " }}}

" make sure the options needed for this script to work correctly are set here
" for the last time. They could have been overridden by any 'onevent'
" associated setting file...
let s:autoresetoptions = 0
if ! s:autoresetoptions
    "au BufWinEnter,Syntax	*.php,*.php\d,*.phtml,*.ctp,*.inc	call ResetPhpOptions()
    let s:autoresetoptions = 1
endif

function! ResetPhpOptions()
    if ! b:optionsset && &filetype =~ "php"
	if b:PHP_autoformatcomment

	    " Set the comment setting to something correct for PHP
	    setlocal comments=s1:/*,mb:*,ex:*/,://,:#

	    " disable Auto-wrap of text
	    setlocal formatoptions-=t
	    " Allow formatting of comments with "gq"
	    setlocal formatoptions+=q
	    " Insert comment leader after hitting <Enter>
	    setlocal formatoptions+=r
	    " Insert comment leader after hitting o or O in normal mode
	    setlocal formatoptions+=o
	    " Autowrap comments using textwidth
	    setlocal formatoptions+=c
	    " Do not wrap if you modify a line after textwidth
	    setlocal formatoptions+=b
	endif
	let b:optionsset = 1
    endif
endfunc

call ResetPhpOptions()

function! GetPhpIndent()
    "##############################################
    "########### MAIN INDENT FUNCTION #############
    "##############################################

    let b:GetLastRealCodeLNum_ADD = 0

    " This detect if the user is currently typing text between each call
    let UserIsEditing=0
    if	b:PHP_oldchangetick != b:changedtick
	let b:PHP_oldchangetick = b:changedtick
	let UserIsEditing=1
    endif

    if b:PHP_default_indenting
	let b:PHP_default_indenting = g:PHP_default_indenting * s:sw()
    endif

    " current line
    let cline = getline(v:lnum)

    " Let's detect if we are indenting just one line or more than 3 lines
    " in the last case we can slightly optimize our algorithm (by trusting
    " what is above the current line)
    if !b:PHP_indentinghuge && b:PHP_lastindented > b:PHP_indentbeforelast
	if b:PHP_indentbeforelast
	    let b:PHP_indentinghuge = 1
	    " echom 'Large indenting detected, speed optimizations engaged (v1.38)'
	endif
	let b:PHP_indentbeforelast = b:PHP_lastindented
    endif

    " If the line we are indenting isn't directly under the previous non-blank
    " line of the file then deactivate the optimization procedures and reset
    " status variable (we restart from scratch)
    if b:InPHPcode_checked && prevnonblank(v:lnum - 1) != b:PHP_lastindented
	if b:PHP_indentinghuge
	    " echom 'Large indenting deactivated'
	    let b:PHP_indentinghuge = 0
	    let b:PHP_CurrentIndentLevel = b:PHP_default_indenting
	endif
	let real_PHP_lastindented = v:lnum
	let b:PHP_LastIndentedWasComment=0
	let b:PHP_InsideMultilineComment=0
	let b:PHP_indentbeforelast = 0

	let b:InPHPcode = 0
	let b:InPHPcode_checked = 0
	let b:InPHPcode_and_script = 0
	let b:InPHPcode_tofind = ""

    elseif v:lnum > b:PHP_lastindented
	" we are indenting line in > order (we can rely on the line before)
	let real_PHP_lastindented = b:PHP_lastindented
    else
	let real_PHP_lastindented = v:lnum
    endif

    let b:PHP_lastindented = v:lnum

    " We must detect if we are in PHPCODE or not, but one time only, then
    " we will detect php end and start tags, comments /**/ and HereDoc
    " tags

    if !b:InPHPcode_checked " {{{ One time check
	let b:InPHPcode_checked = 1

	let synname = ""
	if cline !~ '<?.*?>'
	    " the line could be blank (if the user presses 'return' so we use
	    " prevnonblank()) We ask to Syntax
	    let synname = IslinePHP (prevnonblank(v:lnum), "")
	endif

	" DEBUG call DebugPrintReturn(869 . ' synname? ' . synname)
	if synname!=""
	    if synname == "SpecStringEntrails"
		" the user has done something ugly *stay calm*
		let b:InPHPcode = -1 " thumb down
		let b:UserIsTypingComment = 0
		" All hope is lost at this point, nothing will be indented
		" further on.
		let b:InPHPcode_tofind = ""
	    elseif synname != "phpHereDoc" && synname != "phpHereDocDelimiter"
		let b:InPHPcode = 1
		let b:InPHPcode_tofind = ""

		if synname =~# '^php\%(Doc\)\?Comment'
		    let b:UserIsTypingComment = 1
		else
		    let b:UserIsTypingComment = 0
		endif

		if synname =~? '^javaScript'
		    let b:InPHPcode_and_script = 1
		endif

	    else
		"We are inside an "HereDoc"
		let b:InPHPcode = 0
		let b:UserIsTypingComment = 0

		let lnum = v:lnum - 1
		while getline(lnum) !~? '<<<''\=\a\w*''\=$' && lnum > 1
		    let lnum = lnum - 1
		endwhile

		let b:InPHPcode_tofind = substitute( getline(lnum), '^.*<<<''\=\(\a\w*\)''\=$', '^\\s*\1;\\=$', '')
	    endif
	else
	    " IslinePHP returned "" => we are not in PHP or Javascript
	    let b:InPHPcode = 0
	    let b:UserIsTypingComment = 0
	    " Then we have to find a php start tag...
	    let b:InPHPcode_tofind = s:PHP_startindenttag
	endif
    endif "!b:InPHPcode_checked }}}

    " Now we know where we are so we can verify the line right above the
    " current one to see if we have to stop or restart php indenting

    " Test if we are indenting PHP code {{{
    " Find an executable php code line above the current line.
    let lnum = prevnonblank(v:lnum - 1)
    let last_line = getline(lnum)
    let endline= s:endline

    " If we aren't in php code, then there is something we have to find
    if b:InPHPcode_tofind!=""
	if cline =~? b:InPHPcode_tofind
	    " DEBUG call DebugPrintReturn('tofind found')
	    let b:InPHPcode_tofind = ""
	    let b:UserIsTypingComment = 0

	    if b:InPHPcode == -1
		" leave that last line of less than nothing alone
		let b:InPHPcode = 1
		" DEBUG call DebugPrintReturn(939)
		return -1
	    end

	    let b:InPHPcode = 1

	    if cline =~ '\*/'
		" End comment tags must be indented like start comment tags
		call cursor(v:lnum, 1)
		if cline !~ '^\*/'
		    call search('\*/', 'W')
		endif
		" find the most outside /*
		let lnum = searchpair('/\*', '', '\*/', s:searchpairflags, 'Skippmatch2()')

		let b:PHP_CurrentIndentLevel = b:PHP_default_indenting

		" prevent a problem if multiline /**/ comment are surrounded by
		" other types of comments
		let b:PHP_LastIndentedWasComment = 0

		if cline =~ '^\s*\*/'
		    " DEBUG call DebugPrintReturn(862)
		    return indent(lnum) + 1
		else
		    " DEBUG call DebugPrintReturn(865)
		    return indent(lnum)
		endif

	    elseif cline =~? '<script\>'
		" a more accurate test is useless since there isn't any other possibility
		let b:InPHPcode_and_script = 1
		" this will make GetLastRealCodeLNum to add one line to its
		" given argument so it can detect the <script> easily (that is
		" simpler/quicker than using a regex...)
		let b:GetLastRealCodeLNum_ADD = v:lnum
	    endif
	endif
    endif

    " ### If we are in PHP code, we test the line before to see if we have to stop indenting
    if 1 == b:InPHPcode

	" Was last line containing a PHP end tag ?
	if !b:InPHPcode_and_script && last_line =~ '\%(<?.*\)\@<!?>\%(.*<?\)\@!' && IslinePHP(lnum, '?>')=~"Delimiter"
	    if cline !~? s:PHP_startindenttag
		let b:InPHPcode = 0
		let b:InPHPcode_tofind = s:PHP_startindenttag
	    elseif cline =~? '<script\>'
		let b:InPHPcode_and_script = 1
	    endif

	    " was last line a very bad idea? (multiline string definition)
	elseif last_line =~ '^[^''"`]\+[''"`]$' " a string identifier with nothing after it and no other string identifier before
	    " DEBUG call DebugPrintReturn( 'mls dcl')
	    let b:InPHPcode = -1
	    let b:InPHPcode_tofind = substitute( last_line, '^.*\([''"`]\).*$', '^[^\1]*\1[;,]$', '')
	    " DEBUG call DebugPrintReturn( 'mls dcl, to find:' . b:InPHPcode_tofind)
	    " Was last line the start of a HereDoc ?
	elseif last_line =~? '<<<''\=\a\w*''\=$'
	    let b:InPHPcode = 0
	    let b:InPHPcode_tofind = substitute( last_line, '^.*<<<''\=\(\a\w*\)''\=$', '^\\s*\1;\\=$', '')

	    " Skip /* \n+ */ comments except when the user is currently
	    " writing them or when it is a comment (ie: not a code put in comment)
	elseif !UserIsEditing && cline =~ '^\s*/\*\%(.*\*/\)\@!' && getline(v:lnum + 1) !~ '^\s*\*'
	    let b:InPHPcode = 0
	    let b:InPHPcode_tofind = '\*/'

	    " is current line the end of a HTML script ? (we indent scripts the
	    " same as php code)
	elseif cline =~? '^\s*</script>'
	    let b:InPHPcode = 0
	    let b:InPHPcode_tofind = s:PHP_startindenttag
	    " Note that b:InPHPcode_and_script is still true so that the
	    " </script> can be indented correctly
	endif
    endif " }}}


    " Non PHP code is let as it is
    if 1 > b:InPHPcode && !b:InPHPcode_and_script
	" DEBUG call DebugPrintReturn(996 . ' ipc? ' . b:InPHPcode . ' ipcs? ' . b:InPHPcode_and_script)
	return -1
    endif

    " Align correctly multi // or # lines
    " Indent successive // or # comment the same way the first is {{{
    let addSpecial = 0
    if cline =~ '^\s*\%(//\|#\|/\*.*\*/\s*$\)'
	let addSpecial = b:PHP_outdentSLComments
	if b:PHP_LastIndentedWasComment == 1
	    " DEBUG call DebugPrintReturn(1031)
	    return indent(real_PHP_lastindented)
	endif
	let b:PHP_LastIndentedWasComment = 1
    else
	let b:PHP_LastIndentedWasComment = 0
    endif " }}}

    " Indent multiline /* comments correctly {{{

    "if we are on the start of a MULTI * beginning comment or if the user is
    "currently typing a /* beginning comment.
    if b:PHP_InsideMultilineComment || b:UserIsTypingComment
	if cline =~ '^\s*\*\%(\/\)\@!'
	    " if cline == '*'
	    if last_line =~ '^\s*/\*'
		" if last_line == '/*'
		" DEBUG call DebugPrintReturn(1048)
		return indent(lnum) + 1
	    else
		" DEBUG call DebugPrintReturn(1051)
		return indent(lnum)
	    endif
	else
	    let b:PHP_InsideMultilineComment = 0
	endif
    endif

    " If cline is the start of a _multiline_ /**/ comment
    if !b:PHP_InsideMultilineComment && cline =~ '^\s*/\*\%(.*\*/\)\@!'
	if getline(v:lnum + 1) !~ '^\s*\*'
	    " DEBUG call DebugPrintReturn(1062)
	    return -1
	endif
	let b:PHP_InsideMultilineComment = 1
    endif " }}}

    " Some tags are always indented to col 1

    " Things always indented at col 1 (PHP delimiter: <?, ?>, Heredoc end) {{{
    " PHP start tags are always at col 1, useless to indent unless the end tag
    " is on the same line
    if cline =~# '^\s*<?' && cline !~ '?>' && b:PHP_outdentphpescape
	return 0
    endif

    " PHP end tags are always at col 1, useless to indent unless if it's
    " followed by a start tag on the same line
    if	cline =~ '^\s*?>' && cline !~# '<?' && b:PHP_outdentphpescape
	return 0
    endif

    " put HereDoc end tags at start of lines
    if cline =~? '^\s*\a\w*;$\|^\a\w*$\|^\s*[''"`][;,]' && cline !~? s:notPhpHereDoc
	return 0
    endif " }}}

    let s:level = 0

    " Find an executable php code line above the current line.
    let lnum = GetLastRealCodeLNum(v:lnum - 1)

    " last line
    let last_line = getline(lnum)
    " by default
    let ind = indent(lnum)

    if ind==0 && b:PHP_default_indenting
	let ind = b:PHP_default_indenting
    endif

    " Hit the start of the file, use default indent.
    if lnum == 0
	" DEBUG call DebugPrintReturn(993)
	return b:PHP_default_indenting + addSpecial
    endif


    " Search the matching open bracket (with searchpair()) and set the indent of cline
    " to the indent of the matching line. (unless it's a VIm folding end tag)
    if cline =~ '^\s*}\%(}}\)\@!'
	let ind = indent(FindOpenBracket(v:lnum, 1))
	let b:PHP_CurrentIndentLevel = b:PHP_default_indenting
	" DEBUG call DebugPrintReturn("1002 " . FindOpenBracket(v:lnum, 1) )
	return ind
    endif

    " Check for end of comment and indent it like its beginning
    if cline =~ '^\s*\*/'
	" End comment tags must be indented like start comment tags
	call cursor(v:lnum, 1)
	if cline !~ '^\*/'
	    call search('\*/', 'W')
	endif
	" find the most outside /*
	let lnum = searchpair('/\*', '', '\*/', s:searchpairflags, 'Skippmatch2()')

	let b:PHP_CurrentIndentLevel = b:PHP_default_indenting

	if cline =~ '^\s*\*/'
	    " DEBUG call DebugPrintReturn(1018)
	    return indent(lnum) + 1
	else
	    " DEBUG call DebugPrintReturn(1021)
	    return indent(lnum)
	endif
    endif


    " if the last line is a stated line and it's not indented then why should
    " we indent this one??
    " Do not do this if the last line is a ')' because array indentation can
    " fail... and defaultORcase can be at col 0.
    " if optimized mode is active and nor current or previous line are an 'else'
    " or the end of a possible bracketless thing then indent the same as the previous
    " line
    if last_line =~ '[;}]'.endline && last_line !~ '^[)\]]' && last_line !~# s:defaultORcase
	if ind==b:PHP_default_indenting
	    " DEBUG call DebugPrintReturn(1034)
	    " if no indentation for the previous line
	    return b:PHP_default_indenting + addSpecial
	elseif b:PHP_indentinghuge && ind==b:PHP_CurrentIndentLevel && cline !~# '^\s*\%(else\|\%(case\|default\).*:\|[})];\=\)' && last_line !~# '^\s*\%(\%(}\s*\)\=else\)' && getline(GetLastRealCodeLNum(lnum - 1))=~';'.endline
	    " DEBUG call DebugPrintReturn(1037)
	    return b:PHP_CurrentIndentLevel + addSpecial
	endif
    endif

    " used to prevent redundant tests in the last part of the script
    let LastLineClosed = 0

    let terminated = s:terminated

    let unstated   = '\%(^\s*'.s:blockstart.'.*)\|\%(//.*\)\@<!\<e'.'lse\>\)'.endline
    " What is an unstated line?
    " - an "else" at the end of line
    " - a  s:blockstart (if while etc...) followed by anything but a ";" at
    "	the end of line

    " if the current line is an 'else' starting line
    if ind != b:PHP_default_indenting && cline =~# '^\s*else\%(if\)\=\>'
	" prevent optimized to work at next call  XXX why ?
	let b:PHP_CurrentIndentLevel = b:PHP_default_indenting
	" DEBUG call DebugPrintReturn(1062)
	return indent(FindTheIfOfAnElse(v:lnum, 1))
    elseif cline =~# s:defaultORcase
	" DEBUG call DebugPrintReturn(1064)
	" case and default need a special treatment
	return FindTheSwitchIndent(v:lnum) + s:sw() * b:PHP_vintage_case_default_indent
    elseif cline =~ '^\s*)\=\s*{'
	let previous_line = last_line
	let last_line_num = lnum

	" DEBUG call DebugPrintReturn(1099)
	" let's find the indent of the block starter (if, while, for, etc...)
	while last_line_num > 1

	    if previous_line =~ terminated || previous_line =~ '^\s*\%(' . s:blockstart . '\)\|'. s:functionDecl . endline

		let ind = indent(last_line_num)

		" If the PHP_BracesAtCodeLevel is set then indent the '{'
		if  b:PHP_BracesAtCodeLevel
		    let ind = ind + s:sw()
		endif

		" DEBUG call DebugPrintReturn(1083)
		return ind
	    endif

	    let last_line_num = GetLastRealCodeLNum(last_line_num - 1)
	    let previous_line = getline(last_line_num)
	endwhile

    elseif last_line =~# unstated && cline !~ '^\s*);\='.endline
	let ind = ind + s:sw() " we indent one level further when the preceding line is not stated
	" DEBUG call DebugPrintReturn(1093)
	return ind + addSpecial

	" If the last line is terminated by ';' or if it's a closing '}'
	" We need to check if this isn't the end of a multilevel non '{}'
	" structure such as:
	" Exemple:
	"			if ($truc)
	"				echo 'truc';
	"
	"	OR
	"
	"			if ($truc)
	"				while ($truc) {
	"					lkhlkh();
	"					echo 'infinite loop\n';
	"				}
	"
	"	OR even (ADDED for version 1.17 - no modification required )
	"
	"			$thing =
	"				"something";
    elseif (ind != b:PHP_default_indenting || last_line =~ '^[)\]]' ) && last_line =~ terminated
	" If we are here it means that the previous line is:
	" - a *;$ line
	" - a [beginning-blanck] } followed by anything but a { $
	let previous_line = last_line
	let last_line_num = lnum
	let LastLineClosed = 1
	" The idea here is to check if the current line is after a non '{}'
	" structure so we can indent it like the top of that structure.
	" The top of that structure is characterized by a if (ff)$ style line
	" preceded by a stated line. If there is no such structure then we
	" just have to find two 'normal' lines following each other with the
	" same indentation and with the first of these two lines terminated by
	" a ; or by a }...

	let isSingleLineBlock = 0
	while 1
	    " let's skip '{}' blocks
	    if ! isSingleLineBlock && previous_line =~ '^\s*}\|;\s*}'.endline " XXX
		" find the opening '{'

		call cursor(last_line_num, 1)
		if previous_line !~ '^}'
		    call search('}\|;\s*}'.endline, 'W')
		end
		let oldLastLine = last_line_num
		let last_line_num = searchpair('{', '', '}', 'bW', 'Skippmatch()')

		" DEBUG call DebugPrintReturn("on line:" . line(".") . " { of } is on line " . last_line_num . ' } was on ' . oldLastLine)
		" if the '{' is alone on the line, get the line before
		if getline(last_line_num) =~ '^\s*{'
		    let last_line_num = GetLastRealCodeLNum(last_line_num - 1)
		elseif oldLastLine == last_line_num
		    " if we're on a {}, then there was nothing to skip in the
		    " first place...
		    let isSingleLineBlock = 1
		    continue
		endif

		let previous_line = getline(last_line_num)

		continue
	    else
		let isSingleLineBlock = 0
		" DEBUG call DebugPrintReturn(1230 . " previous_line: " . previous_line)
		" At this point we know that the previous_line isn't a closing
		" '}' so we can check if we really are in such a structure.

		" it's not a '}' but it could be an else alone...
		if getline(last_line_num) =~# '^\s*else\%(if\)\=\>'
		    let last_line_num = FindTheIfOfAnElse(last_line_num, 0)
		    " re-run the loop (we could find a '}' again)
		    continue
		endif

		" So now it's ok we can check :-)
		" A good quality is to have confidence in oneself so to know
		" if yes or no we are in that struct lets test the indent of
		" last_line_num and of last_line_num - 1!
		" If those are == then we are almost done.
		"
		" That isn't sufficient, we need to test how the first of
		" these 2 lines ends...

		" Remember the 'topest' line we found so far
		let last_match = last_line_num

		let one_ahead_indent = indent(last_line_num)
		let last_line_num = GetLastRealCodeLNum(last_line_num - 1)
		let two_ahead_indent = indent(last_line_num)
		let after_previous_line = previous_line
		let previous_line = getline(last_line_num)


		" DEBUG call DebugPrintReturn(1260 . " previous_line: " . previous_line)
		" If we find a '{' or a case/default then we are inside that block so lets
		" indent properly... Like the line following that block starter
		if previous_line =~# s:defaultORcase.'\|{'.endline
		    " DEBUG call DebugPrintReturn(1264 . ' last_match: ' . last_match)
		    break
		endif

		" The 3 lines below are not necessary for the script to work
		" but it makes it work a little faster in some (rare) cases.
		" We verify if we are at the top of a non '{}' struct.
		if after_previous_line=~# '^\s*'.s:blockstart.'.*)'.endline && previous_line =~# '[;}]'.endline
		    " DEBUG call DebugPrintReturn(1272)
		    break
		endif

		if one_ahead_indent == two_ahead_indent || last_line_num < 1
		    " So the previous line and the line before are at the same
		    " col. Now we just have to check if the line before is a ;$ or [}]$ ended line
		    " we always check the most ahead line of the 2 lines so
		    " it's useless to match ')$' since the lines couldn't have
		    " the same indent...
		    if previous_line =~# '\%(;\|^\s*}\)'.endline || last_line_num < 1
			" DEBUG call DebugPrintReturn(1283 . " last_match: " . last_match . " - previous_line (".last_line_num .'): ' . previous_line)
			break
		    endif
		endif
	    endif
	endwhile

	if indent(last_match) != ind
	    " let's use the indent of the last line matched by the algorithm above
	    let ind = indent(last_match)
	    let b:PHP_CurrentIndentLevel = b:PHP_default_indenting

	    " DEBUG call DebugPrintReturn(1297 . " last match:" . last_match)
	    return ind + addSpecial
	endif
	" if nothing was done lets the old script continue
    endif

    " previous to last line
    if (last_line !~ '^\s*}\%(}}\)\@!')
	let plinnum = GetLastRealCodeLNum(lnum - 1)
    else
	let plinnum = GetLastRealCodeLNum(FindOpenBracket(lnum, 1) - 1)
    endif

    let AntepenultimateLine = getline(plinnum)

    " REMOVE comments at end of line before treatment
    " the first part of the regex removes // from the end of line when they are
    " followed by a number of '"' which is a multiple of 2. The second part
    " removes // that are not followed by any '"'
    " Sorry for this unreadable thing...
    let last_line = substitute(last_line,"\\(//\\|#\\)\\(\\(\\([^\"']*\\([\"']\\)[^\"']*\\5\\)\\+[^\"']*$\\)\\|\\([^\"']*$\\)\\)",'','')


    if ind == b:PHP_default_indenting
	if last_line =~ terminated && last_line !~# s:defaultORcase
	    let LastLineClosed = 1
	endif
    endif

    " Indent blocks enclosed by {} or () (default indenting)
    if !LastLineClosed

	" the last line isn't a .*; or a }$ line
	" Indent correctly multilevel and multiline '(.*)' things

	" if the last line is a [{(\[]$ or a multiline function call (or array
	" declaration) with already one parameter on the opening ( line
	if last_line =~# '[{(\[]'.endline || last_line =~? '\h\w*\s*(.*,$' && AntepenultimateLine !~ '[,(\[]'.endline

	    let dontIndent = 0
	    " the last line contains a '{' with other meaningful characters
	    " before it but is not a block starter / function declaration.
	    " It should mean that it's a multi-line block declaration and that
	    " the previous line is already indented...
	    if last_line =~ '\S\+\s*{'.endline && last_line !~ '^\s*)\s*{'.endline && last_line !~ '^\s*\%(' . s:blockstart . '\)\|'. s:functionDecl . s:endline
		let dontIndent = 1
	    endif

	    " DEBUG call DebugPrintReturn(1290. '   ' . dontIndent . ' lastline: ' . last_line)
	    " indent if we don't want braces at code level or if the last line
	    " is not a lonely '{' (default indent for the if block)
	    if !dontIndent && (!b:PHP_BracesAtCodeLevel || last_line !~# '^\s*{')
		let ind = ind + s:sw()
	    endif

	    if b:PHP_BracesAtCodeLevel || b:PHP_vintage_case_default_indent == 1
		" case and default are not indented inside blocks
		let b:PHP_CurrentIndentLevel = ind

		" DEBUG call DebugPrintReturn(1299)
		return ind + addSpecial
	    endif

	    " If the last line isn't empty and ends with a '),' then check if the
	    " ')' was opened on the same line, if not it means it closes a
	    " multiline '(.*)' thing and that the current line need to be
	    " de-indented one time.
	elseif last_line =~ '\S\+\s*),'.endline
	    call cursor(lnum, 1)
	    call search('),'.endline, 'W') " line never begins with ) so no need for 'c' flag
	    " DEBUG call DebugPrintReturn(1373)
	    let openedparent = searchpair('(', '', ')', 'bW', 'Skippmatch()')
	    if openedparent != lnum
		let ind = indent(openedparent)
	    endif

	    " if the line before starts a block then we need to indent the
	    " current line.
	elseif last_line =~ '^\s*'.s:blockstart
	    let ind = ind + s:sw()

	    " In all other cases if !LastLineClosed indent 1 level higher
	    " _only_ if the ante-penultimate line _is_ 'closed' or if it's a
	    " block starter
	    "
	    " IE: We test the line before the last one to check if we already
	    " were in this "list"

    elseif AntepenultimateLine =~ '{'.endline || AntepenultimateLine =~ terminated || AntepenultimateLine =~# s:defaultORcase
	    let ind = ind + s:sw()
	    " DEBUG call DebugPrintReturn(1422 . '  ' . AntepenultimateLine)
	endif

    endif

    " If the current line closes a multiline function call or array def
    if cline =~  '^\s*[)\]];\='
	let ind = ind - s:sw()
    endif

    let b:PHP_CurrentIndentLevel = ind
    " DEBUG call DebugPrintReturn(1433)
    return ind + addSpecial
endfunction

" vim:ts=8:sw=4:sts=4:nosta:noet:
