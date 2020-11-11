" Vim syntastic plugin helper
" Language: vala
" Maintainer: Adrià Arrufat

if exists('g:loaded_vala_vim')
    finish
endif
let g:loaded_vala_vim = 1
let s:save_cpo = &cpoptions
set cpoptions&vim

" This is to let Syntastic know about the Vala filetype.
" It enables tab completion for the 'SyntasticInfo' command.
" (This does not actually register the syntax checker.)
if exists('g:syntastic_extra_filetypes')
    call add(g:syntastic_extra_filetypes, 'vala')
else
    let g:syntastic_extra_filetypes = ['vala']
endif

if !exists('g:systastic_vala_checkers')
    let g:syntastic_vala_checkers = ['valac']
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo
