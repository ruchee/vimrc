" Vim syntax file
" Language:	Ceylon
" Maintainer:	Enrique Zamudio
" Version: 	0.1
" URL:	  http://ceylon-lang.org/
" Last Change: 2012-08-01

" This is my very first vim script, I hope to have
" done it the right way.
" 
" I must directly or indirectly thank the author of ceylon.vim

" HOWTO USE IT (INSTALL):
" [Ceylon is still not recognized by vim! :-( ]
"
" 1) copy the file in the (global or user's $HOME/.vim/syntax/) syntax folder
" 2) add this line to recognize Ceylon files by filename extension:
"
" au BufNewFile,BufRead *.ceylon  setf ceylon
" in the global vim filetype.vim file or inside $HOME/.vim/filetype.vim
"
" 4) open/write a .ceylon file...
"
" Let me know if you like it or send me patches, so that I can improve it
" when I have time

" Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  " we define it here so that included files can test for it
  let main_syntax='ceylon'
endif

" don't use standard HiLink, it will not work with included syntax files
if version < 508
  command! -nargs=+ CeylonHiLink hi link <args>
else
  command! -nargs=+ CeylonHiLink hi def link <args>
endif

" ##########################
" Java stuff taken from java.vim
" some characters that cannot be in a ceylon program (outside a string)
" syn match ceylonError "[\\@`]"
"syn match ceylonError "<<<\|\.\.\|=>\|<>\|||=\|&&=\|[^-]->\|\*\/"
"syn match ceylonOK "\.\.\."

" keyword definitions
syn keyword ceylonExternal        package module
syn match ceylonExternal          "\<import\(\s\+static\>\)\?"
" syn keyword ceylonError           goto const
syn keyword ceylonConditional     if else switch then case
syn keyword ceylonRepeat          while for do
syn keyword ceylonBoolean         true false
syn keyword ceylonConstant        null
syn keyword ceylonTypedef         this super outer
syn keyword ceylonOperator        is nonempty given of abstracts
syn keyword ceylonType            void
syn match   ceylonType            "\<[A-Z][A-Za-z0-9_]*\>"
syn keyword ceylonStatement       return
syn keyword ceylonExceptions      throw try catch finally dynamic
syn keyword ceylonMethodDecl      given satisfies of
syn keyword ceylonClassDecl       extends satisfies interface alias
" to differentiate the keyword class from MyClass.class we use a match here
syn match   ceylonTypedef         "\.\s*\<class\>"ms=s+1
syn match   ceylonClassDecl       "^class\>"
syn match   ceylonClassDecl       "[^.]\s*\<class\>"ms=s+1
syn keyword ceylonObjectDecl      object
syn keyword ceylonBranch          break continue
syn match   ceylonUserLabelRef    "\k\+" contained
syn keyword ceylonScopeDecl       shared abstract formal actual default late native annotation final


if exists("ceylon_highlight_ceylon_lang_ids") || exists("ceylon_highlight_ceylon_lang") || exists("ceylon_highlight_all")
  " ceylon.lang.*
"  syn keyword ceylonLangClass  Closure MetaMethod CeylonObject
  
  syn match ceylonJavaLangClass "\<System\>"
  syn keyword ceylonJavaLangClass  Cloneable Comparable Runnable Serializable Boolean Byte Class Object
  syn keyword ceylonJavaLangClass  Character CharSequence ClassLoader Compiler
  syn keyword ceylonJavaLangClass  Integer Float
  syn keyword ceylonJavaLangClass  InheritableThreadLocal Math Number Object Package Process
  syn keyword ceylonJavaLangClass  Runtime RuntimePermission InheritableThreadLocal
  syn keyword ceylonJavaLangClass  SecurityManager Short StrictMath StackTraceElement
  syn keyword ceylonJavaLangClass  StringBuilder Thread ThreadGroup
  syn keyword ceylonJavaLangClass  ThreadLocal Throwable Void ArithmeticException
  syn keyword ceylonJavaLangClass  ArrayIndexOutOfBoundsException AssertionError
  syn keyword ceylonJavaLangClass  ArrayStoreException ClassCastException
  syn keyword ceylonJavaLangClass  ClassNotFoundException
  syn keyword ceylonJavaLangClass  CloneNotSupportedException Exception
  syn keyword ceylonJavaLangClass  IllegalAccessException
  syn keyword ceylonJavaLangClass  IllegalArgumentException
  syn keyword ceylonJavaLangClass  IllegalMonitorStateException
  syn keyword ceylonJavaLangClass  IllegalStateException
  syn keyword ceylonJavaLangClass  IllegalThreadStateException
  syn keyword ceylonJavaLangClass  IndexOutOfBoundsException
  syn keyword ceylonJavaLangClass  InstantiationException InterruptedException
  syn keyword ceylonJavaLangClass  NegativeArraySizeException NoSuchFieldException
  syn keyword ceylonJavaLangClass  NoSuchMethodException NullPointerException
  syn keyword ceylonJavaLangClass  NumberFormatException RuntimeException
  syn keyword ceylonJavaLangClass  SecurityException StringIndexOutOfBoundsException
  syn keyword ceylonJavaLangClass  UnsupportedOperationException
  syn keyword ceylonJavaLangClass  AbstractMethodError ClassCircularityError
  syn keyword ceylonJavaLangClass  ClassFormatError Error ExceptionInInitializerError
  syn keyword ceylonJavaLangClass  IllegalAccessError InstantiationError
  syn keyword ceylonJavaLangClass  IncompatibleClassChangeError InternalError
  syn keyword ceylonJavaLangClass  LinkageError NoClassDefFoundError
  syn keyword ceylonJavaLangClass  NoSuchFieldError NoSuchMethodError
  syn keyword ceylonJavaLangClass  OutOfMemoryError StackOverflowError
  syn keyword ceylonJavaLangClass  ThreadDeath UnknownError UnsatisfiedLinkError
  syn keyword ceylonJavaLangClass  UnsupportedClassVersionError VerifyError
  syn keyword ceylonJavaLangClass  VirtualMachineError

  syn keyword ceylonJavaLangObject clone equals finalize getClass hashCode
  syn keyword ceylonJavaLangObject notify notifyAll toString wait

  CeylonHiLink ceylonLangClass                   ceylonConstant
  CeylonHiLink ceylonJavaLangClass               ceylonExternal
  CeylonHiLink ceylonJavaLangObject              ceylonConstant
  syn cluster ceylonTop add=ceylonJavaLangObject,ceylonJavaLangClass,ceylonLangClass
  syn cluster ceylonClasses add=ceylonJavaLangClass,ceylonLangClass
endif


" Ceylon stuff
syn match ceylonOperator "\.\."
syn match ceylonOperator "<\{2,3}"
syn match ceylonOperator ">\{2,3}"
syn match ceylonOperator "->"
syn match ceylonOperator "=>"
syn match ceylonOperator ":"
syn match ceylonExternal		'^#!.*[/\\]ceylon\>'
syn match ceylonExceptions        "\<Exception\>\|\<[A-Z]\{1,}[a-zA-Z0-9]*Exception\>"

" Ceylon JDK stuff
syn keyword ceylonJDKBuiltin    in out exists nonempty is variable value function assert assign shared late native satisfies of let new
syn keyword ceylonJDKMethods 	print className
syn cluster ceylonTop add=ceylonJDKBuiltin,ceylonJDKMethods

" no useful I think, so I comment it..
"if filereadable(expand("<sfile>:p:h")."/ceylonid.vim")
 " source <sfile>:p:h/ceylonid.vim
"endif

if exists("ceylon_space_errors")
  if !exists("ceylon_no_trail_space_error")
    syn match   ceylonSpaceError  "\s\+$"
  endif
  if !exists("ceylon_no_tab_space_error")
    syn match   ceylonSpaceError  " \+\t"me=e-1
  endif
endif

" The following cluster contains all ceylon groups except the contained ones
syn cluster ceylonTop add=ceylonExternal,ceylonError,ceylonError,ceylonBranch,ceylonLabelRegion,ceylonLabel,ceylonConditional,ceylonRepeat,ceylonBoolean,ceylonConstant,ceylonTypedef,ceylonOperator,ceylonType,ceylonType,ceylonStatement,ceylonStorageClass,ceylonAssert,ceylonExceptions,ceylonMethodDecl,ceylonClassDecl,ceylonClassDecl,ceylonClassDecl,ceylonScopeDecl,ceylonError,ceylonError2,ceylonUserLabel,ceylonLangObject,ceylonObjectDecl


" Comments
syn keyword ceylonTodo             contained TODO FIXME XXX
if exists("ceylon_comment_strings")
  syn region  ceylonCommentString    contained start=+"+ end=+"+ end=+$+ end=+\*/+me=s-1,he=s-1 contains=ceylonCommentStar,ceylonSpecialChar,@Spell
  syn region  ceylonComment2String   contained start=+"+  end=+$\|"+  contains=ceylonSpecialChar,@Spell
  syn match   ceylonCommentCharacter contained "'\\[^']\{1,6\}'" contains=ceylonSpecialChar
  syn match   ceylonCommentCharacter contained "'\\''" contains=ceylonSpecialChar
  syn match   ceylonCommentCharacter contained "'[^\\]'"
  syn cluster ceylonCommentSpecial add=ceylonCommentString,ceylonCommentCharacter,ceylonNumber
  syn cluster ceylonCommentSpecial2 add=ceylonComment2String,ceylonCommentCharacter,ceylonNumber
endif
syn region  ceylonComment          start="/\*"  end="\*/" contains=@ceylonCommentSpecial,ceylonTodo,@Spell
syn match   ceylonCommentStar      contained "^\s*\*[^/]"me=e-1
syn match   ceylonCommentStar      contained "^\s*\*$"
syn match   ceylonLineComment      "//.*" contains=@ceylonCommentSpecial2,ceylonTodo,@Spell
CeylonHiLink ceylonCommentString ceylonString
CeylonHiLink ceylonComment2String ceylonString
CeylonHiLink ceylonCommentCharacter ceylonCharacter
CeylonHiLink ceylonCommentString ceylonQuoted

syn cluster ceylonTop add=ceylonComment,ceylonLineComment

if !exists("ceylon_ignore_ceylondoc") && main_syntax != 'jsp'
  syntax case ignore
  " syntax coloring for ceylondoc comments (HTML)
  " syntax include @ceylonHtml <sfile>:p:h/html.vim
"   syntax include @ceylonHtml runtime! syntax/html.vim
"  unlet b:current_syntax
  syntax spell default  " added by Bram
  syn region  ceylonDocComment    start="/\*\*"  end="\*/" keepend contains=ceylonCommentTitle,@ceylonHtml,ceylonDocTags,ceylonTodo,@Spell
  syn region  ceylonCommentTitle  contained matchgroup=ceylonDocComment start="/\*\*"   matchgroup=ceylonCommentTitle keepend end="\.$" end="\.[ \t\r<&]"me=e-1 end="[^{]@"me=s-2,he=s-1 end="\*/"me=s-1,he=s-1 contains=@ceylonHtml,ceylonCommentStar,ceylonTodo,@Spell,ceylonDocTags

  syn region ceylonDocTags  contained start="{@\(link\|linkplain\|inherit[Dd]oc\|doc[rR]oot\|value\)" end="}"
  syn match  ceylonDocTags  contained "@\(see\|param\|by\|exception\|throws\|since\)\s\+\S\+" contains=ceylonDocParam
  syn match  ceylonDocParam contained "\s\S\+"
  syn match  ceylonDocTags  contained "@\(version\|return\|deprecated\|serial\|serialField\|serialData\)\>"
  syntax case match
endif

" match the special comment /**/
syn match   ceylonComment          "/\*\*/"

" Strings and constants
syn match   ceylonSpecialError     contained "\\^[{`"]"
syn match   ceylonSpecialCharError contained "[^`]"
syn match   ceylonSpecialChar      contained "\\{#[0-9a-fA-f]+}"
syn match   ceylonEscape +\\[btnfr\\"'`]+ contained
syn region  ceylonString
      \ start=+"+ end=+"+ end=+\\""+ keepend
      \ contains=ceylonEscape,ceylonSpecialChar,ceylonSpecialError,@Spell
syn region  ceylonQuoted
      \ start=+``+ end=+``+
      \ contains=ceylonSpecialChar,ceylonSpecialError,@Spell

syn match   ceylonCharacter        "'[^']+'" contains=ceylonSpecialChar,ceylonSpecialCharError
syn match   ceylonCharacter        "'\\''" contains=ceylonSpecialChar
syn match   ceylonCharacter        "'[^\\]'"
" 0x23452345345abcdef
syn match   ceylonNumber           "#\v<[0-9a-fA-F]+(_[0-9a-fA-F]+)*[kMGPT]?>"
" $01010101010
syn match   ceylonNumber           "$\v<[01]+(_[01]+)*[kMGPT]?>"
" 123_123_123G or 12345G
syn match   ceylonNumber           "\v<\d+(_\d+)*[kMGPT]?>"
" 123_123_123.123e+5M
syn match   ceylonNumber           "\v<\d+(_\d+)*\.\d+(_\d+)*[kMGPTmunpf]?>"
" 123_123.123e+5
syn match   ceylonNumber           "\v<\d+(_\d+)*\.\d+(_\d+)*[eE][+-]?\d+[kMGPTmunpf]?>"

syn cluster ceylonTop add=ceylonString,ceylonCharacter,ceylonNumber,ceylonStringError

if exists("ceylon_highlight_functions")
  if ceylon_highlight_functions == "indent"
    syn match  ceylonFuncDef "^\(\t\| \{8\}\)[_$a-zA-Z][_$a-zA-Z0-9_. \[\]]*([^-+*/()]*)" contains=ceylonScopeDecl,ceylonType,ceylonStorageClass,@ceylonClasses
    syn region ceylonFuncDef start=+^\(\t\| \{8\}\)[$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*,\s*+ end=+)+ contains=ceylonScopeDecl,ceylonType,ceylonStorageClass,@ceylonClasses
    syn match  ceylonFuncDef "^  [$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*)" contains=ceylonScopeDecl,ceylonType,ceylonStorageClass,@ceylonClasses
    syn region ceylonFuncDef start=+^  [$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*,\s*+ end=+)+ contains=ceylonScopeDecl,ceylonType,ceylonStorageClass,@ceylonClasses
  else
    " This line catches method declarations at any indentation>0, but it assumes
    " two things:
    "   1. class names are always capitalized (ie: Button)
    "   2. method names are never capitalized (except constructors, of course)
    syn region ceylonFuncDef start=+^\s\+\(\(shared\|actual\|formal\|default\|abstract\|final\|sealed\)\s\+\)*\(\(void\|function\|\([A-Za-z_][A-Za-z0-9_$]*\.\)*[A-Z][A-Za-z0-9_$]*\)\(<[^>]*>\)\=\(\[\]\)*\s\+[a-z][A-Za-z0-9_$]*\|[A-Z][A-Za-z0-9_$]*\)\s*([^0-9]+ end=+)+ contains=ceylonScopeDecl,ceylonType,ceylonStorageClass,ceylonComment,ceylonLineComment,@ceylonClasses
  endif
  syn match  ceylonBraces  "[{}]"
  syn cluster ceylonTop add=ceylonFuncDef,ceylonBraces
endif

if exists("ceylon_highlight_debug")

  " Strings and constants
  syn match   ceylonDebugSpecial          contained "\\\d\d\d\|\\."
  syn region  ceylonDebugString           contained start=+"+  end=+"+  contains=ceylonDebugSpecial
  syn match   ceylonDebugStringError      +"\([^"\\]\|\\.\)*$+
  syn match   ceylonDebugCharacter        contained "'[^\\]'"
  syn match   ceylonDebugSpecialCharacter contained "'\\.'"
  syn match   ceylonDebugSpecialCharacter contained "'\\''"
  syn match   ceylonDebugNumber           contained "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
  syn match   ceylonDebugNumber           contained "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
  syn match   ceylonDebugNumber           contained "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
  syn match   ceylonDebugNumber           contained "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"
  syn keyword ceylonDebugBoolean          contained true false
  syn keyword ceylonDebugType             contained null this super
  syn region ceylonDebugParen  start=+(+ end=+)+ contained contains=ceylonDebug.*,ceylonDebugParen

  " to make this work you must define the highlighting for these groups
  syn match ceylonDebug "\<print\(ln\)*\s*("me=e-1 contains=ceylonDebug.* nextgroup=ceylonDebugParen
  syn match ceylonDebug "\<p\s*("me=e-1 contains=ceylonDebug.* nextgroup=ceylonDebugParen
  syn match ceylonDebug "[A-Za-z][a-zA-Z0-9_]*\.printStackTrace\s*("me=e-1 contains=ceylonDebug.* nextgroup=ceylonDebugParen
  syn match ceylonDebug "\<trace[SL]\=\s*("me=e-1 contains=ceylonDebug.* nextgroup=ceylonDebugParen

  syn cluster ceylonTop add=ceylonDebug

  if version >= 508 || !exists("did_c_syn_inits")
    CeylonHiLink ceylonDebug                 Debug
    CeylonHiLink ceylonDebugString           DebugString
    CeylonHiLink ceylonDebugStringError      ceylonError
    CeylonHiLink ceylonDebugType             DebugType
    CeylonHiLink ceylonDebugBoolean          DebugBoolean
    CeylonHiLink ceylonDebugNumber           Debug
    CeylonHiLink ceylonDebugSpecial          DebugSpecial
    CeylonHiLink ceylonDebugSpecialCharacter DebugSpecial
    CeylonHiLink ceylonDebugCharacter        DebugString
    CeylonHiLink ceylonDebugParen            Debug
  
    CeylonHiLink DebugString               String
    CeylonHiLink DebugSpecial              Special
    CeylonHiLink DebugBoolean              Boolean
    CeylonHiLink DebugType                 Type
  endif
endif

" Match all Exception classes 
syn match ceylonExceptions        "\<Exception\>\|\<[A-Z]\{1,}[a-zA-Z0-9]*Exception\>"


if !exists("ceylon_minlines")
  let ceylon_minlines = 10
endif
exec "syn sync ccomment ceylonComment minlines=" . ceylon_minlines


" ################### 
" Ceylon stuff
" syn match ceylonOperator		"|[ ,a-zA-Z0-9_*]\+|"

" All ceylon valid tokens
" syn match ceylonTokens ";\|,\|<=>\|<>\|:\|>\|>=\|=\|==\|===\|<\|<=\|!=\|/\|/=\|\.\.|\.\.\.\|\~=\|\~=="
" syn match ceylonTokens "\*=\|&\|&=\|\*\|->\|\~\|+\|-\|/\|?\|<<<\|>>>\|<<\|>>"

" Must put explicit these ones because ceylon.vim mark them as errors otherwise
" syn match ceylonTokens "<=>\|<>\|==\~"
"syn cluster ceylonTop add=ceylonTokens

" Mark these as operators

" Hightlight brackets
" syn match  ceylonBraces		"[{}]"
" syn match  ceylonBraces		"[\[\]]"
" syn match  ceylonBraces		"[\|]"

if exists("ceylon_mark_braces_in_parens_as_errors")
  syn match ceylonInParen          contained "[{}]"
  CeylonHiLink ceylonInParen        ceylonError
  syn cluster ceylonTop add=ceylonInParen
endif

" catch errors caused by wrong parenthesis
syn region  ceylonParenT  transparent matchgroup=ceylonParen  start="("  end=")" contains=@ceylonTop,ceylonParenT1
syn region  ceylonParenT1 transparent matchgroup=ceylonParen1 start="(" end=")" contains=@ceylonTop,ceylonParenT2 contained
syn region  ceylonParenT2 transparent matchgroup=ceylonParen2 start="(" end=")" contains=@ceylonTop,ceylonParenT  contained
syn match   ceylonParenError       ")"
CeylonHiLink ceylonParenError       ceylonError

" catch errors caused by wrong square parenthesis
syn region  ceylonParenT  transparent matchgroup=ceylonParen  start="\["  end="\]" contains=@ceylonTop,ceylonParenT1
syn region  ceylonParenT1 transparent matchgroup=ceylonParen1 start="\[" end="\]" contains=@ceylonTop,ceylonParenT2 contained
syn region  ceylonParenT2 transparent matchgroup=ceylonParen2 start="\[" end="\]" contains=@ceylonTop,ceylonParenT  contained
syn match   ceylonParenError       "\]"

" ###############################
" java.vim default highlighting
if version >= 508 || !exists("did_ceylon_syn_inits")
  if version < 508
    let did_ceylon_syn_inits = 1
  endif
  CeylonHiLink ceylonFuncDef		Function
  CeylonHiLink ceylonBraces		Function
  CeylonHiLink ceylonBranch		Conditional
  CeylonHiLink ceylonUserLabelRef	ceylonUserLabel
  CeylonHiLink ceylonLabel		Label
  CeylonHiLink ceylonUserLabel		Label
  CeylonHiLink ceylonConditional	Conditional
  CeylonHiLink ceylonRepeat		Repeat
  CeylonHiLink ceylonExceptions		Exception
  CeylonHiLink ceylonAssert 		Statement
  CeylonHiLink ceylonStorageClass	StorageClass
  CeylonHiLink ceylonMethodDecl		ceylonStorageClass
  CeylonHiLink ceylonClassDecl		ceylonStorageClass
  CeylonHiLink ceylonScopeDecl		ceylonStorageClass
  CeylonHiLink ceylonObjectDecl     ceylonStorageClass
  CeylonHiLink ceylonBoolean		Boolean
"  CeylonHiLink ceylonSpecial		Special
  CeylonHiLink ceylonSpecialError	Error
  CeylonHiLink ceylonSpecialCharError	Error
  CeylonHiLink ceylonString		String
  CeylonHiLink ceylonCharacter		Character
  CeylonHiLink ceylonSpecialChar	SpecialChar
  CeylonHiLink ceylonNumber		Number
  CeylonHiLink ceylonError		Error
  CeylonHiLink ceylonStringError	Error
  CeylonHiLink ceylonStatement		Statement
  CeylonHiLink ceylonOperator		Operator
  CeylonHiLink ceylonComment		Comment
  CeylonHiLink ceylonDocComment		Comment
  CeylonHiLink ceylonLineComment	Comment
  CeylonHiLink ceylonConstant		Constant
  CeylonHiLink ceylonTypedef		Typedef
  CeylonHiLink ceylonTodo		Todo
  
  CeylonHiLink ceylonCommentTitle	SpecialComment
  CeylonHiLink ceylonDocTags		Special
  CeylonHiLink ceylonDocParam		Function
  CeylonHiLink ceylonCommentStar	ceylonComment
  
  CeylonHiLink ceylonType		Type
  CeylonHiLink ceylonExternal		Include
  
  CeylonHiLink htmlComment		Special
  CeylonHiLink htmlCommentPart		Special
  CeylonHiLink ceylonSpaceError		Error
  CeylonHiLink ceylonJDKBuiltin         Special
  CeylonHiLink ceylonJDKOperOverl       Operator
  CeylonHiLink ceylonJDKMethods         Function
endif

delcommand CeylonHiLink


let b:current_syntax = "ceylon"
if main_syntax == 'ceylon'
  unlet main_syntax
endif

let b:spell_options="contained"

" vim: ts=8
