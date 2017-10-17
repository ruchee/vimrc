let s:save_cpo = &cpo
set cpo&vim

silent! nnoremap <silent> <unique> <buffer> <C-]>
			\ :<C-u>call phpcd#JumpToDefinition('normal')<CR>
silent! nnoremap <silent> <unique> <buffer> <C-W><C-]>
			\ :<C-u>call phpcd#JumpToDefinition('split')<CR>
silent! nnoremap <silent> <unique> <buffer> <C-W><C-\>
			\ :<C-u>call phpcd#JumpToDefinition('vsplit')<CR>
silent! nnoremap <silent> <unique> <buffer> K
			\ :<C-u>call phpcd#JumpToDefinition('preview')<CR>
silent! nnoremap <silent> <unique> <buffer> <C-t>
			\ :<C-u>call phpcd#JumpBack()<CR>

command! -nargs=0 PHPID call phpcd#Index()

if has('nvim')
	let messenger = 'msgpack'
else
	let messenger = 'json'
end

function! Init() " {{{
	let g:phpcd_root = phpcd#GetRoot()
	let phpcd_vim = g:phpcd_root.'/.phpcd.vim'
	if filereadable(phpcd_vim)
		exec 'source '.phpcd_vim
	endif

	if exists('g:phpcd_channel_id')
		call rpc#stop(g:phpcd_channel_id)

		unlet g:phpcd_channel_id
		if exists('g:phpid_channel_id')
			unlet g:phpid_channel_id
		endif
	endif
endfunction " }}}

if (g:phpcd_auto_restart == 0 && g:phpcd_root == '/') || (g:phpcd_auto_restart == 1 && phpcd#GetRoot() != g:phpcd_root)
	call Init()
endif

if !exists('g:phpcd_channel_id')
	let phpcd_path = expand('<sfile>:p:h:h') . '/php/main.php'
	let g:php_autoload_path = g:phpcd_root.'/'.g:phpcd_autoload_path
	let g:phpcd_channel_id = rpc#start(g:phpcd_php_cli_executable,
				\ phpcd_path, g:phpcd_root, messenger, g:php_autoload_path,
				\ g:phpcd_disable_modifier)

	if g:phpcd_root != '/'
		let g:phpid_channel_id = g:phpcd_channel_id
	endif
endif

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker:noexpandtab:ts=2:sts=2:sw=2
