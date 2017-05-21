
" This is an automatically generated syntax file created on Sat, 26 Mar 2016 23:49:26 GMT
" Origin: https://github.com/NLKNguyen/vim-lisp-syntax
syn keyword lispPredicate eq
syn keyword lispPredicate eql
syn keyword lispPredicate equal
syn keyword lispPredicate equalp
syn keyword lispPredicate atom
syn keyword lispPredicate listp
syn keyword lispPredicate symbolp
syn keyword lispPredicate null
syn keyword lispPredicate numberp
syn keyword lispPredicate =
syn keyword lispPredicate >
syn keyword lispPredicate >=
syn keyword lispPredicate <
syn keyword lispPredicate <=
syn keyword lispSideEffect defun
syn keyword lispSideEffect print
syn keyword lispSideEffect setf
syn keyword lispExtract first
syn keyword lispExtract second
syn keyword lispExtract third
syn keyword lispExtract four
syn keyword lispExtract last
syn keyword lispExtract butlast
syn keyword lispExtract rest
syn match lispExtract "\<c[ad]\+r\>"
syn keyword lispCompute length
syn keyword lispCompute search
syn keyword lispCompute random
syn keyword lispConstruct sort
syn keyword lispConstruct append
syn keyword lispConstruct cons
syn keyword lispOperator +
syn keyword lispOperator -
syn keyword lispOperator *
syn keyword lispOperator /
syn keyword lispOperator not
syn keyword lispOperator and
syn keyword lispOperator or
syn keyword lispOperator cond
hi def link lispPredicate Conditional
hi def link lispExtract StorageClass
hi def link lispCompute Structure
