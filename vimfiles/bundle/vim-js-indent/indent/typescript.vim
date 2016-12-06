" Vim indent script for TypeScript
" General:
" File:			typescript.vim
" Maintainer:	Jason Cheatham
" Last Change: 	2016-04-17
" Description:
"	Stub to use JavaScript indenter for TypeScript.

" Only load one indent script per buffer
if exists('b:did_indent')
	finish
endif

" set to 1 to use the JS intenter for TypeScript
if !exists('g:js_indent_typescript')
	let g:js_indent_typescript = 1
endif

if g:js_indent_typescript
	runtime! indent/javascript.vim
endif
