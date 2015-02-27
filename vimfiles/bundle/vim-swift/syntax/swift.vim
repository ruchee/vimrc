" Vim syntax file
" Language:    Swift
" Maintainer:  Kevin Ballard
" Last Change: Jul 25, 2014

if exists("b:current_syntax")
    finish
endif

" Settings {{{1

" iskeyword defines word characters, which affects \< and \>
" The default seems to be @,48-57,_,192-255
" We want it to match anything from identifier-character
" Unfortunately we can only match unicode values up to 0xFF, anything higher
" is assumed to be contained. But it's better than nothing
let &l:iskeyword = '@,' " isalpha(), should cover [a-zA-Z]
            \ . '48-57,' " 0-9
            \ . '_,'
            \ . '168,170,173,175,178-181,183-186,'
            \ . '188-190,192-214,216-246,248-255'
let &l:isident = &l:iskeyword

" Syntax definitions {{{1

" NB: These clusters are expected to only contain items that are at the
" toplevel. Furthermore, the following set of clusters should cover all
" toplevel syntax items.

" @swiftDefs are definitions that cannot occur inside closures/functions
syn cluster swiftDefs contains=NONE
" @swiftItems are things that cannot be found in an expression, but are
" not @swiftDefs
syn cluster swiftItems contains=NONE
" @swiftExprs are all other toplevel syntax items
syn cluster swiftExprs contains=NONE

" In order to support :syn-include embedding, we can't use contains values
" like contains=TOP,@swiftDefs. This is because the syntax embedding marks all
" our syntax items as `contained`. Instead, use combinations of the clusters.

" Define a few lists that we're going to need later
" These are all regex snippets, with implied word boundaries
let s:declarations = ['class', 'struct', 'enum', 'protocol', 'extension',
            \'var', 'func', 'subscript', 'init', 'deinit', 'operator']
let s:modifiers = ['final', 'lazy', 'optional', 'required', 'override',
            \'dynamic', 'prefix', 'infix', 'postfix', 'convenience', 'weak',
            \'unowned', 'mutating', 'nonmutating']
let s:modifiers_suffixed = ['unowned\_s*(safe)', 'unowned\_s*(unsafe)']
let s:accessControl = ['public', 'private', 'internal']

let s:declarations_re = '\<\%(init\>[?!]\|\%('.join(s:declarations, '\|').'\)\>\)'
let s:modifiers_re = '\<\%('.join(map(s:modifiers, 'v:val."\\>"'), '\|')
            \.'\|'.join(s:modifiers_suffixed, '\|').'\)'
let s:accessControl_re = '\<\%('.join(s:accessControl, '\|').'\)\>'
" inclues an optional parenthesized suffix
let s:accessControl2_re = s:accessControl_re.'\%(\_s*(\_[^)]*)\)\='
" includes (optionally-suffixed) access conrol
" as well as occasional modifiers like class/static
let s:modifiers2_re = '\%('.s:modifiers_re.'\|'.s:accessControl2_re.'\|\<static\>\|\<class\>\)'

" Identifiers {{{2

syn match swiftIdentifier /\<\i\+\>/ display transparent contains=NONE

syn match swiftIdentifier /`\i\+`/ transparent contains=NONE

syn cluster swiftExprs add=swiftIdentifier


" Keywords {{{2

" Declarations {{{3

" Keywords have priority over other matches, so use syn-match for the few
" keywords that we want to reuse in other matches.
exe 'syn match swiftKeyword /'.s:declarations_re.'/'

" Access control {{{3

" Define the keywords once because they're keywords, and again for @swiftItems
" to support the (set) modifier.

exe 'syn keyword swiftKeyword' join(s:accessControl)

exe 'syn keyword swiftAccessControl' join(s:accessControl) 'nextgroup=swiftAccessControlScope skipwhite skipempty'
exe 'syn match swiftAccessControlScope /(\_s*set\_s*)\ze\%(\_s*'.s:modifiers2_re.'\)*\_s*\<\%(var\|subscript\)\>/ contained skipwhite skipempty'
syn cluster swiftItems add=swiftAccessControl

" Other keywords {{{3

syn keyword swiftKeyword import let
syn keyword swiftKeyword static typealias
syn keyword swiftKeyword break case continue default do else fallthrough if in
syn keyword swiftKeyword for return switch where while
syn keyword swiftKeyword dynamicType is super self Self
syn keyword swiftKeyword __COLUMN__ __FILE__ __FUNCTION__ __LINE__
syn match swiftKeyword ,\<as\>[?!]\?,

" Special types {{{3

syn match swiftKeyword /\.\@1<=\%(Type\|Protocol\)\>/ display

" }}}3

syn cluster swiftExprs add=swiftKeyword

" Built-in types {{{2
" This is just the types that represent primitives or other commonly-used
" types, not everything that exists in Swift.

" Structs/Classes {{{3

" Primitive types
syn keyword swiftType Int Int8 Int16 Int32 Int64 Word IntMax
syn keyword swiftType UInt UInt8 UInt16 UInt32 UInt64 UWord UIntMax
syn keyword swiftType Double Float Float32 Float64 Float80
syn keyword swiftType Bool Bit Void

" Containers
syn keyword swiftType Array Dictionary Set Slice
syn keyword swiftType Optional ImplicitlyUnwrappedOptional
syn keyword swiftType ContiguousArray
syn keyword swiftType ManagedBuffer ManagedBufferPointer ManagedProtoBuffer

" String-related types
syn keyword swiftType String StaticString UTF8 UTF16 UTF32 UnicodeScalar Character

" Ranges/intervals
syn keyword swiftType Range ClosedInterval HalfOpenInterval
syn keyword swiftType StrideTo StrideThrough

" Pointers
syn keyword swiftType UnsafePointer UnsafeMutablePointer
syn keyword swiftType AutoreleasingUnsafeMutablePointer
syn keyword swiftType COpaquePointer CFunctionPointer
syn keyword swiftType UnsafeBufferPointer UnsafeMutableBufferPointer

" Sequences/Collections/Generators
syn keyword swiftType IndexingGenerator LazySequence
syn keyword swiftType LazyForwardCollection LazyBidirectionalCollection LazyRandomAccessCollection
syn keyword swiftType FilterCollectionView FilterSequenceView MapCollectionView MapSequenceView
syn keyword swiftType BidirectionalReverseView RandomAccessReverseView
syn keyword swiftType UnsafeBufferPointerGenerator
syn keyword swiftType EmptyCollection GeneratorOf SequenceOf GeneratorSequence
syn keyword swiftType CollectionOfOne GeneratorOfOne
syn keyword swiftType Zip2 Repeat PermutationGenerator

" C compatibility
syn keyword swiftType CInt CUnsignedInt CShort CUnsignedShort CLong CUnsignedLong CLongLong CUnsignedLongLong
syn keyword swiftType \CChar CSignedChar CUnsignedChar CChar16 CChar32 CWideChar CBool
syn keyword swiftType CDouble CFloat
syn keyword swiftType CVarArgType CVaListPointer VaListBuilder

" Miscellaneous
syn keyword swiftType Unmanaged
syn keyword swiftType ObjectIdentifier
syn keyword swiftType FloatingPointClassification
syn keyword swiftType MirrorDisposition QuickLookObject
syn keyword swiftType UnicodeDecodingResult
syn keyword swiftType RawRepresentable
syn keyword swiftType NonObjectiveCBase

syn cluster swiftExprs add=swiftType

" Protocols {{{3

syn keyword swiftProtocol Any AnyObject AnyClass
syn keyword swiftProtocol ForwardIndexType BidirectionalIndexType RandomAccessIndexType
syn keyword swiftProtocol Comparable Hashable Equatable Strideable Reflectable Sliceable MutableSliceable
syn keyword swiftProtocol NilLiteralConvertible DictionaryLiteralConvertible ArrayLiteralConvertible IntegerLiteralConvertible ExtendedGraphemeClusterLiteralConvertible CharacterLiteralConvertible FloatLiteralConvertible StringLiteralConvertible StringInterpolationConvertible BooleanLiteralConvertible
syn keyword swiftProtocol Streamable Printable DebugPrintable
syn keyword swiftProtocol UnicodeCodecType
syn keyword swiftProtocol IntegerType SignedIntegerType UnsignedIntegerType BooleanType FloatingPointType
syn keyword swiftProtocol SignedNumberType AbsoluteValuable IntervalType
syn keyword swiftProtocol IntegerArithmeticType
syn keyword swiftProtocol CollectionType MutableCollectionType SequenceType GeneratorType
syn keyword swiftProtocol RawOptionSetType BitwiseOperationsType
syn keyword swiftProtocol OutputStreamType SinkType
syn keyword swiftProtocol ExtensibleCollectionType
syn keyword swiftProtocol MirrorType
syn keyword swiftProtocol StringElementType

syn cluster swiftExprs add=swiftProtocol

" Built-in functions {{{2

let s:functions = [
            \ 'abs', 'advance', 'alignof', 'alignofValue', 'assert', 'assertionFailure', 'contains', 'count',
            \ 'debugPrint', 'debugPrintln', 'distance', 'dropFirst', 'dropLast', 'dump', 'enumerate', 'equal', 'extend',
            \ 'fatalError', 'filter', 'find', 'first', 'getVaList', 'indices', 'insert', 'isEmpty',
            \ 'isUniquelyReferenced', 'isUniquelyReferencedNonObjC', 'join', 'last', 'lazy', 'lexicographicalCompare',
            \ 'map', 'max', 'maxElement', 'min', 'minElement', 'numericCast', 'overlaps', 'partition', 'precondition',
            \ 'preconditionFailure', 'prefix', 'print', 'println', 'reduce', 'reflect', 'removeAll', 'removeAtIndex',
            \ 'removeLast', 'removeRange', 'reverse', 'sizeof', 'sizeofValue', 'sort', 'sorted', 'splice', 'split',
            \ 'startsWith', 'stride', 'strideof', 'strideofValue', 'suffix', 'swap', 'toDebugString', 'toString',
            \ 'transcode', 'underestimateCount', 'unsafeAddressOf', 'unsafeBitCast', 'unsafeDowncast',
            \ 'withExtendedLifetime', 'withUnsafeMutablePointer', 'withUnsafeMutablePointers', 'withUnsafePointer',
            \ 'withUnsafePointers', 'withVaList', 'zip'
            \]
let s:functions_re = '\<\%(\.\@1<!\|\%(\.\@1<!Swift\.\)\@6<=\)\%('.join(s:functions, '\|').'\)\>\ze('
exe 'syn match swiftStdlibFunction /'.s:functions_re.'/ display'
syn cluster swiftExprs add=swiftStdlibFunction

" Literals {{{2

syn cluster swiftLiteral contains=NONE
syn cluster swiftExprs add=@swiftLiteral

" Dec, Bin, Oct, Hex integer literals {{{3
syn match swiftInteger display /\<\d[0-9_]*/
syn match swiftInteger display /\<0b[01][01_]*/
syn match swiftInteger display /\<0o\o[0-7_]*/
syn match swiftInteger display /\<0x\x[0-9a-fA-F_]*/

syn cluster swiftLiteral add=swiftInteger

" Float and hex float literals {{{3
" NB: Swift's documentation allows a decimal integer literal to also match a
" float literal. We don't want that here.
syn match swiftFloat display /\<\d[0-9_]*\.\d[0-9_]*\%([eE][-+]\?\d[0-9_]*\)\?\>/
syn match swiftFloat display /\<\d[0-9_]*\%(\.\d[0-9_]*\)\?[eE][-+]\?\d[0-9_]*\>/
syn match swiftFloat display /\<0x\x[0-9a-fA-F_]*\%(\.\x[0-9a-fA-F_]*\)\?[pP][-+]\?\d[0-9_]*\>/

syn cluster swiftLiteral add=swiftFloat

" String literals {{{3

syn region swiftString start=/"/ end=/"/ end=/$/ keepend oneline contains=swiftStringEscape,swiftStringEscapeError,swiftInterpolation,@Spell
syn match swiftStringEscapeError display contained /\\./
syn match swiftStringEscape contained /\\[0\\tnr"']/ extend
syn match swiftStringEscapeError display contained /\\\%(x\x\{,2}\|u\x\{,4}\|U\x\{,8}\)/
syn region swiftStringEscape matchgroup=swiftStringEscapeUnicode start="\\u{" end=/}\|\ze"/ display contained contains=swiftStringEscapeUnicodeError keepend
syn region swiftStringEscapeUnicodeError start=/\_X\|{\@1<=\x\{8}\zs\_[^}]/ end=/}/ display contained

syn region swiftInterpolation matchgroup=swiftInterpolationDelim start=/\\(/ end=/)/ contained oneline contains=@swiftDefs,@swiftItems,@swiftExprs

syn cluster swiftLiteral add=swiftString

" Boolean literals {{{3

syn keyword swiftBoolean true false

syn cluster swiftLiteral add=swiftBoolean

" Nil literal {{{3

syn keyword swiftNil nil

syn cluster swiftLiteral add=swiftNil

" Library values {{{2

syn match swiftLibraryValue /\.\@1<!\<Process\>/

syn match swiftLibraryEnumValue /\.\@1<=\%(Some\ze(\|None\>\)/

syn cluster swiftExprs add=swiftLibraryValue,swiftLibraryEnumValue

" Miscellaneous {{{2

syn match swiftOperator display ,\%(//\|/\*\)\@![-/=+!*%<>&|^~.]\+, transparent contains=NONE

syn region swiftBalancedParens matchgroup=swiftBalancedParens start="(" end=")" transparent contains=@swiftExprs

syn region swiftClosure matchgroup=swiftClosure start="{" end="}" contains=swiftClosureCaptureList,swiftClosureSimple fold
syn region swiftClosureSimple start='[^}[:space:]]' end='\ze}' transparent contained contains=@swiftItems,@swiftExprs,swiftPlaceholder
syn region swiftClosureCaptureList start="\[" end="\]" contained contains=@swiftItems,@swiftExprs,swiftClosureCaptureListOwnership nextgroup=swiftClosureSimple skipwhite skipempty
syn match swiftClosureCaptureListOwnership /\<\%(strong\>\|weak\>\|unowned\%((safe)\|(unsafe)\|\>\)\)/ contained
syn match swiftPlaceholder /\$\d\+/ contained

syn match swiftAttribute /@\i\+/ nextgroup=swiftAttributeArguments skipwhite skipempty
syn region swiftAttributeArguments matchgroup=swiftAttributeArguments start="(" end=")" contains=@swiftExprs,swiftAttributeArgumentsNest contained
syn region swiftAttributeArgumentsNest matchgroup=swiftAttributeArguments start="(" end=")" transparent contained
syn region swiftAttributeArgumentsNest matchgroup=swiftAttributeArguments start="\[" end="\]" transparent contained
syn region swiftAttributeArgumentsNest matchgroup=swiftAttributeArguments start="{" end="}" transparent contained

syn cluster swiftExprs add=swiftOperator,swiftBalancedParens,swiftClosure,swiftAttribute

" Declarations {{{2

" Types (struct/class/etc) {{{3
syn region swiftTypeDef start=/\<\%(class\|struct\|enum\|protocol\|extension\)\>/ end=/\ze{/ end=/\ze;/ contains=@swiftExprs,swiftGenerics nextgroup=swiftTypeBody keepend
syn region swiftTypeBody matchgroup=swiftTypeBody start="{" end="}" contained contains=@swiftDefs,@swiftItems,@swiftExprs fold
syn cluster swiftDefs add=swiftTypeDef

" Operators {{{3
syn match swiftOperatorDef /\<operator\_s*[^[:space:]]\+\_s*\ze{/ contains=swiftKeyword,swiftOperator nextgroup=swiftOperatorBody
syn region swiftOperatorBody start="{" end="}" contained contains=swiftOperatorPrecedence,swiftOperatorAssociativity,swiftOperatorAssignment fold
syn keyword swiftOperatorPrecedence contained nextgroup=swiftOperatorPrecedenceLevel skipwhite skipempty precedence
syn match swiftOperatorPrecedenceLevel contained /\d\+/
syn keyword swiftOperatorAssociativity contained nextgroup=swiftOperatorAssociativityValue skipwhite skipempty associativity
syn keyword swiftOperatorAssociativityValue contained left right none
syn keyword swiftOperatorAssignment contained assignment
syn cluster swiftDefs add=swiftOperatorDef

" Functions {{{3

syn match swiftFuncDef /\<func\>\_s*[^[:space:]();<]\+\_s*\ze\%((\|<\)/ contains=@swiftExprs nextgroup=swiftFuncArgs,swiftFuncGenerics
syn match swiftSpecialFuncDef /\<\%(init\>[?!]\?\|deinit\>\)\_s*\ze(/ contains=swiftKeyword nextgroup=swiftFuncArgs
syn region swiftFuncGenerics start="<" end="\ze\_." contained contains=swiftGenerics nextgroup=swiftFuncArgs skipwhite skipempty
syn region swiftFuncArgs matchgroup=swiftFuncArgs start="(" end=")" contained contains=@swiftExprs,swiftFuncArgInout transparent nextgroup=swiftFuncBody skipwhite skipempty
syn region swiftFuncBody matchgroup=swiftFuncBody start="{" end="}" contained contains=@swiftItems,@swiftExprs fold
syn keyword swiftFuncArgInout contained inout
syn cluster swiftItems add=swiftFuncDef
syn cluster swiftDefs add=swiftSpecialFuncDef

" Subscripts {{{3

syn match swiftSubscriptDef /\<subscript\>\_[^{]*\ze{/ contains=@swiftExprs nextgroup=swiftSubscriptBody
syn region swiftSubscriptBody matchgroup=swiftSubscriptBody start="{" end="}" fold contained contains=@swiftItems,@swiftExprs,swiftSubscriptAttribute
syn keyword swiftSubscriptAttribute contained nextgroup=swiftSubscriptAttriuteBlock skipwhite skipempty get
syn match swiftSubscriptAttribute /\<set\>/ contained nextgroup=swiftSubscriptAttributeArg,swiftSubscriptAttriuteBlock skipwhite skipempty
syn match swiftSubscriptAttribute /\<\%(mutating\|nonmutating\)\>\ze\_s*\<\%(get\|set\)\>/ contained nextgroup=swiftSubscriptAttribute skipwhite skipempty
syn region swiftSubscriptAttributeArg matchgroup=swiftSubscriptAttributeArg start="(" end=")" contained contains=@swiftExprs nextgroup=swiftSubscriptAttributeBlock skipwhite skipempty
syn region swiftSubscriptAttributeBlock matchgroup=swiftSubscriptAttributeBlock start="{" end="}" contained contains=@swiftItems,@swiftExprs fold
syn cluster swiftDefs add=swiftSubscriptDef

" Variables {{{3

syn match swiftVarDef /\<var\>\_s*\%([^[:space:][:punct:][:cntrl:][:digit:]]\|\i\)\%([^[:space:][:punct:][:cntrl:]]\|\i\)*/ contains=swiftKeyword,swiftIdentifier nextgroup=swiftVarTypeAscription,swiftVarDefaultValue,swiftVarBody skipwhite skipempty
syn match swiftVarTypeAscription /:/ contained nextgroup=swiftVarType skipwhite skipempty
syn region swiftVarType start=/./ end=/\ze\_./ contained contains=swiftTypeExpr nextgroup=swiftVarDefaultValue,swiftVarBody skipwhite skipempty
syn match swiftVarDefaultValue /=/ contained nextgroup=swiftVarDefaultValueExpr skipwhite skipempty
syn region swiftVarDefaultValueExpr start=/\S/ end=/\ze\_./ contained contains=@swiftExprs nextgroup=swiftVarBody skipwhite skipempty
syn region swiftVarBody matchgroup=swiftVarBody start="{" end="}" fold contained contains=@swiftItems,@swiftExprs,swiftVarAttribute
syn keyword swiftVarAttribute contained nextgroup=swiftVarAttributeBlock skipwhite skipempty get
syn match swiftVarAttribute /\<\%(set\|willSet\|didSet\)\>/ contained nextgroup=swiftVarAttributeArg,swiftVarAttributeBlock skipwhite skipempty
syn match swiftVarAttribute /\<\%(mutating\|nonmutating\)\>\ze\_s*\<\%(get\|set\|willSet\|didSet\)\>/ contained nextgroup=swiftVarAttribute skipwhite skipempty
syn region swiftVarAttributeArg matchgroup=swiftVarAttributeArg start="(" end=")" contained contains=@swiftExprs nextgroup=swiftVarAttributeBlock skipwhite skipempty
syn region swiftVarAttributeBlock matchgroup=swiftVarAttributeBlock start="{" end="}" contained contains=@swiftItems,@swiftExprs fold
syn cluster swiftItems add=swiftVarDef

" Type patterns {{{3

syn region swiftTypeExpr start=/./ end=/\ze\_[[:space:]{})\]]/ contained contains=@swiftExprs,swiftTypeArrayDict,swiftGenerics skipwhite skipempty
syn region swiftTypeArrayDict matchgroup=swiftTypeBrackets start='\[' end='\]' contained contains=@swiftExprs,swiftTypeArrayDict,swiftTypeBracketsColon keepend extend
syn match swiftTypeBracketsColon /:/ contained
syn region swiftGenerics matchgroup=swiftGenerics start=/</ end=/>/ contained contains=swiftKeyword,swiftType,swiftProtocol,@swiftLiteral,swiftIdentifier,swiftBalancedParens,swiftTypeArrayDict,swiftGenerics,swiftGenericsEqual keepend extend
syn match swiftGenericsEqual /==/ contained transparent contains=NONE

" Modifiers {{{3

exe 'syn match swiftDeclarationModifier /'.s:modifiers_re.'\ze\%(\_s*'.s:modifiers2_re.'\)*\_s*'.s:declarations_re.'/'
exe 'syn match swiftDeclarationModifier /\<\%(class\|static\)\>\ze\%(\_s*\w\+\%(\_s*(\_s*\w*\_s*)\)\=\)\{,}\_s*'.s:declarations_re.'/'
syn cluster swiftItems add=swiftDeclarationModifier

" Comments {{{2

syn region swiftCommentLine excludenl start="//" end="$" contains=@swiftCommentLineMarker,@Spell oneline
syn region swiftCommentBlock matchgroup=swiftCommentBlockDelim start="/\*" end="\*/" contains=swiftCommentBlockNest,@swiftCommentBlockMarker,@Spell keepend
syn region swiftCommentBlockNest matchgroup=swiftCommentBlockDelim start="/\*" end="\*/" contains=swiftCommentBlockNest,@swiftCommentBlockMarker,@Spell contained keepend extend

syn region swiftDocCommentLine excludenl start="///" end="$" contains=@swiftCommentLineMarker,@Spell oneline
syn region swiftDocCommentBlock matchgroup=swiftDocCommentBlockDelim start="/\*\*" end="\*/" contains=swiftCommentBlockNest,@swiftCommentBlockMarker,@Spell keepend

" it seems the markers don't care about word boundaries, only that the literal
" substring matches
syn match swiftCommentTodo /TODO:\|FIXME:/ contained
" for MARK: we want to highlight the rest of the line as well. TODO: and
" FIXME: actually use the rest of the line too, but marks are used for
" separation and should be more distinct
syn region swiftCommentLineMark excludenl start=/MARK:/ end=/$/ contained contains=@Spell oneline
syn cluster swiftCommentLineMarker contains=swiftCommentTodo,swiftCommentLineMark
syn region swiftCommentBlockMark excludenl start=/MARK:/ end=/$/ end=,\ze/\*, contained contains=@Spell oneline
syn cluster swiftCommentBlockMarker contains=swiftCommentTodo,swiftCommentBlockMark

syn cluster swiftExprs add=swiftCommentLine,swiftCommentBlock,swiftDocCommentLine,swiftDocCommentBlock

" Conditional Compilation {{{2

syn region swiftConditionalCompilation excludenl start=/^\s*#if\>/ end=/$/ contains=swiftBuildConfiguration oneline
syn region swiftConditionalCompilation excludenl start=/^\s*#else\>/ end=/$/ oneline
syn region swiftConditionalCompilation excludenl start=/^\s*#elseif\>/ end=/$/ contains=swiftBuildConfiguration oneline
syn region swiftConditionalCompilation excludenl start=/^\s*#endif\>/ end=/$/ oneline

syn match swiftBuildConfiguration /\<os([^)]*)/ contained contains=swiftBuildConfigurationOS
syn match swiftBuildConfiguration /\<arch([^)]*)/ contained contains=swiftBuildConfigurationArch
syn match swiftBuildConfiguration /\<\%(true\|false\)\>/ contained contains=swiftBoolean

syn keyword swiftBuildConfigurationOS OSX iOS contained
syn keyword swiftBuildConfigurationArch x86_64 arm arm64 i386 contained

syn cluster swiftExprs add=swiftConditionalCompilation

" Shebang {{{2

syn region swiftShebang excludenl start=/^\%1l#!/ end=/$/ oneline

" Default highlighting {{{1

hi def link swiftKeyword Keyword
hi def link swiftType    Type
hi def link swiftProtocol PreProc

hi def link swiftAccessControl      Keyword
hi def link swiftAccessControlScope swiftAccessControl

hi def link swiftStdlibFunction Function

hi def link swiftLiteral  Number
hi def link swiftInteger  swiftLiteral
hi def link swiftFloat    swiftLiteral
hi def link swiftBoolean  swiftLiteral
hi def link swiftNil      swiftKeyword

hi def link swiftLibraryValue Identifier
hi def link swiftLibraryEnumValue swiftLibraryValue

hi def link swiftString String
hi def link swiftStringEscapeError Error
hi def link swiftStringEscape Special
hi def link swiftStringEscapeUnicode swiftStringEscape
hi def link swiftStringEscapeUnicodeError swiftStringEscapeError
hi def link swiftInterpolationDelim Delimiter

hi def link swiftClosureCaptureListOwnership swiftKeyword
hi def link swiftPlaceholder Identifier

hi def link swiftAttribute          Macro
hi def link swiftAttributeArguments Macro

hi def link swiftOperatorDefKeywords swiftKeyword
hi def link swiftOperatorPrecedence swiftKeyword
hi def link swiftOperatorPrecedenceLevel swiftInteger
hi def link swiftOperatorAssociativity swiftKeyword
hi def link swiftOperatorAssociativityValue swiftKeyword
hi def link swiftOperatorAssignment swiftKeyword

hi def link swiftFuncArgInout swiftKeyword

hi def link swiftSubscriptAttribute swiftKeyword
hi def link swiftVarAttribute swiftKeyword

hi def link swiftDeclarationModifier swiftKeyword

hi def link swiftCommentLine  Comment
hi def link swiftCommentBlock swiftCommentLine
hi def link swiftCommentBlockDelim swiftCommentBlock
hi def link swiftCommentBlockNest swiftCommentBlock

hi def link swiftDocCommentLine SpecialComment
hi def link swiftDocCommentBlock swiftDocCommentLine
hi def link swiftDocCommentBlockDelim swiftDocCommentBlock

hi def link swiftCommentTodo Todo
hi def link swiftCommentLineMark PreProc
hi def link swiftCommentBlockMark PreProc

hi def link swiftConditionalCompilation PreProc
hi def link swiftBuildConfiguration swiftConditionalCompilation
hi def link swiftBuildConfigurationOS Special

hi def link swiftShebang swiftCommentLine

" }}}1

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "swift"

" vim: set et sw=4 ts=4:
