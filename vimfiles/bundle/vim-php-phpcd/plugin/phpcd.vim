let s:save_cpo = &cpo
set cpo&vim

let g:phpcd_root = '/'
let g:phpcd_php_cli_executable = 'php'
let g:phpcd_autoload_path = 'vendor/autoload.php'
let g:phpcd_need_update = 0
let g:phpcd_disable_modifier = 0

autocmd BufLeave,VimLeave *.php if g:phpcd_need_update > 0 | call phpcd#UpdateIndex() | endif
autocmd BufWritePost *.php let g:phpcd_need_update = 1
autocmd FileType php setlocal omnifunc=phpcd#CompletePHP
autocmd InsertLeave,CompleteDone *.php if pumvisible() == 0 | pclose | endif

if get(g:, 'phpcd_auto_restart', 0)
	autocmd FileType php autocmd BufEnter <buffer> call phpcd#EnterBufferWithAutoRestart()
endif

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker:noexpandtab:ts=2:sts=2:sw=2
