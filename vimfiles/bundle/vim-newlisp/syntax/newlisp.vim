" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <cyril@slobin.ru>
" URL:          http://www.vim.org/scripts/script.php?script_id=2067
" Another URL:  http://slobin.ru/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th anniversary)
" Last change:  2013 Jun 02
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.37 2013/06/02 22:30:55 slobin Exp $

" *** Some syntax quirks of newLISP and how this file treats them: ***

" * In the following text, the word "character" refers to the ASCII
"   characters, and the word "symbol" to the lisp symbols (= atoms).
"
" * All built-in symbols are in the same class (Statement). Functions,
"   definitions, macros and control structures aren't distinguished.
"
" * The quoting character ' (apostrophe) is allowed anywhere, even
"   before closing parenthesis.
"
" * The comma character , makes a symbol by itself and need not be
"   separated from adjacent symbols.
"
" * Brackets [ ] and braces { } are allowed in symbols, even unmatched
"   ones. They albeit will break the Vim parentheses matching logic.
"
" * Special syntax for : character (colon) and FOOP is underway.
"   Meantime it is just highlighted and not checked otherwise.
"
" * newLISP sometimes allows numbers, symbols or strings to follow one
"   another without a separator (like 1.2.3e4e5, which is valid and
"   equal to three tokens 1.2 .3e4 e5). Such a beast is rather a typo
"   than  someones intent, so it is highlighted as error.
"
" * If a backlash character \ in a quoted string is not followed by
"   a valid escape sequence, the interpreter doesn't alarm you, it just
"   silently removes this character from the string. E.g. the string
"   "a\b" is equal to the string "ab". Again, this is rather a typo than
"   someones intent, so it is highlighted as error.
"
" * New in this version: based on newLISP 10.5.0 release; new syntax for
"   big integer numbers; maintainer's e-mail and web site changed.

if exists("b:current_syntax")
  finish
endif

syntax case match

setlocal iskeyword=33,36-38,42,43,45-47,48-57,60-64,@,91-94,_,123-126

syn region newlispComment oneline start="[;#]" end="$" contains=newlispTodo,@Spell
syn keyword newlispTodo FIXME TODO XXX contained

syn region newlispDocComment start="^;;\(\s\|$\)" end="^\(;;\(\s\|$\)\)\@!" contains=newlispTodo,newlispDocCommentExample,newlispDocCommentUserKeyword,newlispDocCommentKeyword,newlispDocCommentItalic,newlispDocCommentMonospace,newlispDocCommentHTMLTag,newlispDocCommentHTMLEntity,@Spell
syn region newlispDocCommentExample matchgroup=newlispDocCommentKeyword start="^;;\s@example$"ms=s+3 matchgroup=NONE end="^\(;;\(\s\|$\)\)\@!" contained
syn match newlispDocCommentUserKeyword "^;;\s@[a-zA-Z_-]*\s"ms=s+3,me=e-1 contained
syn match newlispDocCommentKeyword "^;;\s@\(module\|index\|description\|location\|version\|author\|syntax\|param\|return\)\s"ms=s+3,me=e-1 contained
syn match newlispDocCommentKeyword "@link\s"me=e-1 contained
syn match newlispDocCommentItalic "<[^<>]\+>"hs=s+1,he=e-1 contained
syn match newlispDocCommentMonospace "'[^']\+'"hs=s+1,he=e-1 contained
syn match newlispDocCommentHTMLTag "<\/\=\(h1\|h2\|h3\|h4\|i\|em\|b\|tt\|p\|pre\|center\|li\|ul\|blockquote\)>" contained
syn match newlispDocCommentHTMLTag "<\(br\|hr\)\/\=>" contained
syn match newlispDocCommentHTMLEntity "&\w\+;" contained

syn cluster newlispListContent contains=TOP,newlispRightParenError
syn region newlispList matchgroup=newlispParenthesis start="(" end=")" contains=@newlispListContent,newlispListError
syn region newlispListError matchgroup=newlispLeftParenError start="^(" matchgroup=newlispParenthesis end=")" contains=@newlispListContent,newlispListError contained
syn match newlispRightParenError ")"

syn match newlispSymbol "\(\[\|[+-]\=\d\)\@!\k\+" nextgroup=newlispNoSeparatorError
syn region newlispSymbolString start="\[" end="\]" nextgroup=newlispNoSeparatorError

syn match newlispQuote "'" nextgroup=newlispQuote,newlispQuotedSymbol skipwhite
syn match newlispQuotedSymbol "\(\[\|[+-]\=\d\)\@!\k\+" contained nextgroup=newlispNoSeparatorError

syn match newlispNumberDec "[+-]\=[1-9]\d*L\=" nextgroup=newlispNoSeparatorError
syn match newlispNumberOct "[+-]\=0\o*" nextgroup=newlispNoSeparatorError
syn match newlispNumberHex "[+-]\=0[Xx]\x\+" nextgroup=newlispNoSeparatorError

syn match newlispFloat "[+-]\=\d\+[Ee][+-]\=\d\+" nextgroup=newlispNoSeparatorError
syn match newlispFloat "[+-]\=\.\d\+\([Ee][+-]\=\d\+\)\=" nextgroup=newlispNoSeparatorError
syn match newlispFloat "[+-]\=\d\+\.\d*\([Ee][+-]\=\d\+\)\=" nextgroup=newlispNoSeparatorError

syn region newlispStringQuoted matchgroup=newlispStringDelimiter start=+"+ skip=+\\"+ end=+"+ contains=newlispStringEscapeError,newlispStringEscape nextgroup=newlispNoSeparatorError
syn match newlispStringEscapeError +\\.+ contained
syn match newlispStringEscape +\\["\\nrt]+ contained
syn match newlispStringEscape +\\\d\d\d+ contained
syn match newlispStringEscape +\\x\x\x+ contained

syn region newlispStringBraced matchgroup=newlispStringDelimiter start="{" end="}" contains=newlispStringNested nextgroup=newlispNoSeparatorError
syn region newlispStringNested matchgroup=newlispStringDelimiter start="{" end="}" contains=newlispStringNested contained

syn region newlispStringTexted matchgroup=newlispStringDelimiter start="\[text\]" end="\[\/text\]" nextgroup=newlispNoSeparatorError

syn match newlispNoSeparatorError "\(\"\|\k\)\+" contained

" This keywords list is based on newLISP v.10.5.0 build primes.h file
" XXX Don't forget to remove colon ":" and escape vertical bar "|"

syn keyword newlispFunction ! != $ % & * + ++ - -- / < << <= = > >= >> NaN? ^ abort abs acos acosh
syn keyword newlispFunction add address amb and append append-file apply args array array-list
syn keyword newlispFunction array? asin asinh assoc atan atan2 atanh atom? base64-dec base64-enc
syn keyword newlispFunction bayes-query bayes-train begin beta betai bigint bigint? bind binomial
syn keyword newlispFunction bits callback case catch ceil change-dir char chop clean close
syn keyword newlispFunction command-event cond cons constant context context? continue copy
syn keyword newlispFunction copy-file corr cos cosh count cpymem crc32 crit-chi2 crit-f crit-t
syn keyword newlispFunction crit-z current-line curry date date-list date-parse date-value debug dec
syn keyword newlispFunction def-new default define define-macro delete delete-file delete-url
syn keyword newlispFunction destroy det device difference directory directory? div do-until do-while
syn keyword newlispFunction doargs dolist dostring dotimes dotree dump dump-symbol dup empty?
syn keyword newlispFunction encrypt ends-with env erf error-event eval eval-string even? exec exists
syn keyword newlispFunction exit exp expand explode extend factor fft file-info file? filter find
syn keyword newlispFunction find-all first flat float float? floor flt for for-all fork format fv
syn keyword newlispFunction gammai gammaln gcd get-char get-float get-int get-long get-string
syn keyword newlispFunction get-url global global? if if-not ifft import inc index inf? int integer
syn keyword newlispFunction integer? intersect invert irr join json-error json-parse lambda? last
syn keyword newlispFunction last-error legal? length let letex letn list list? load local log lookup
syn keyword newlispFunction lower-case macro? main-args make-dir map mat match max member min mod
syn keyword newlispFunction mul multiply net-accept net-close net-connect net-error net-eval
syn keyword newlispFunction net-interface net-ipv net-listen net-local net-lookup net-packet
syn keyword newlispFunction net-peek net-peer net-ping net-receive net-receive-from net-receive-udp
syn keyword newlispFunction net-select net-send net-send-to net-send-udp net-service net-sessions
syn keyword newlispFunction new nil? normal not now nper npv nth null? number? odd? open or pack
syn keyword newlispFunction parse parse-date peek pipe pmt pop pop-assoc post-url pow prefix
syn keyword newlispFunction pretty-print primitive? print println prob-chi2 prob-f prob-t prob-z
syn keyword newlispFunction process prompt-event protected? push put-url pv quote quote? rand random
syn keyword newlispFunction randomize read read-buffer read-char read-expr read-file read-key
syn keyword newlispFunction read-line read-utf8 reader-event real-path receive ref ref-all regex
syn keyword newlispFunction regex-comp remove-dir rename-file replace reset rest reverse rotate
syn keyword newlispFunction round save search seed seek select self semaphore send sequence series
syn keyword newlispFunction set set-locale set-ref set-ref-all setf setq sgn share signal silent sin
syn keyword newlispFunction sinh sleep slice sort source spawn sqrt starts-with stats string string?
syn keyword newlispFunction struct sub swap sym symbol? symbols sync sys-error sys-info t-test tan
syn keyword newlispFunction tanh term throw throw-error time time-of-day timer title-case trace
syn keyword newlispFunction trace-highlight transpose trim true? unicode unify union unique unless
syn keyword newlispFunction unpack until upper-case utf8 utf8len uuid wait-pid when while write
syn keyword newlispFunction write-buffer write-char write-file write-line xfer-event xml-error
syn keyword newlispFunction xml-parse xml-type-tags zero? \| ~

syn keyword newlispVariable $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15
syn keyword newlispVariable ostype $args $count $error-event $idx $it $main-args $x
syn keyword newlispKeyword fn lambda
syn keyword newlispPreloaded Class Tree module

" functions that became obsolete starting with newLISP 9.3 or 9.4
syn keyword newlispObsolete replace-assoc
syn keyword newlispObsolete set-assoc assoc-set set-nth nth-set ref-set
syn keyword newlispObsolete name
syn keyword newlispObsolete error-number error-text

" functions that still exist but are deprecated
syn keyword newlispDeprecated integer parse-date if-not

" functions that are not compiled in by default
syn keyword newlispDebugging dump-symbol continue

syn match newlispColon ":"
syn match newlispComma ","

syn keyword newlispBoolean nil true

hi def link newlispNumberDec newlispNumber
hi def link newlispNumberOct newlispNumber
hi def link newlispNumberHex newlispNumber

hi def link newlispStringQuoted newlispString
hi def link newlispStringBraced newlispString
hi def link newlispStringNested newlispString
hi def link newlispStringTexted newlispString

hi def link newlispLeftParenError newlispError
hi def link newlispRightParenError newlispError
hi def link newlispNoSeparatorError newlispError
hi def link newlispStringEscapeError newlispError

hi def link newlispComment Comment
hi def link newlispTodo Todo
hi def link newlispDocComment Comment
hi def link newlispDocCommentExample Comment
hi def link newlispDocCommentUserKeyword Identifier
hi def link newlispDocCommentKeyword Type
hi def link newlispDocCommentItalic CommentItalic
hi def link newlispDocCommentMonospace CommentUnderlined
hi def link newlispDocCommentHTMLTag Statement
hi def link newlispDocCommentHTMLEntity Special
hi def link newlispList Normal
hi def link newlispParenthesis Delimiter
hi def link newlispSymbol Identifier
hi def link newlispSymbolString Type
hi def link newlispQuote Type
hi def link newlispQuotedSymbol Type
hi def link newlispNumber Number
hi def link newlispFloat Float
hi def link newlispStringDelimiter String
hi def link newlispString String
hi def link newlispStringEscape Special
hi def link newlispFunction Statement
hi def link newlispVariable Statement
hi def link newlispKeyword Statement
hi def link newlispPreloaded Statement
hi def link newlispObsolete Todo
hi def link newlispDeprecated Todo
hi def link newlispDebugging Todo
hi def link newlispColon Type
hi def link newlispComma Type
hi def link newlispBoolean Boolean
hi def link newlispError Error

" CommentItalic and CommentUnderlined groups have the same colors as
" plain Comment, but italic and underlined respectively. Therefore we
" need to recalculate those attributes on each colorscheme change.
"
" But we can't implement this for old Vim versions and semi-compatible
" highlighting tools, so defaults are supplied as a fallback.

hi link CommentItalic Identifier
hi link CommentUnderlined Underlined

function! s:color_of(where, what)
  let val = synIDattr(hlID("Comment"), a:what, a:where)
  return val == "" || val == -1 ? "" : printf(" %s%s=%s", a:where, a:what, val)
endfunction

function! s:set_colors()
  let colors =  s:color_of("cterm", "fg") . s:color_of("cterm", "bg") . s:color_of("gui", "fg") . s:color_of("gui", "bg")
  exec "hi CommentItalic term=italic cterm=italic gui=italic" . colors
  exec "hi CommentUnderlined term=underline cterm=underline gui=underline" . colors
endfunction

if version >= 700 && !exists("g:newlisp_compat")
  au ColorScheme <buffer> call s:set_colors()
  call s:set_colors()
endif

let b:current_syntax = "newlisp"

" vim: textwidth=72
