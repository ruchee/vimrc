"=============================================================================
" File: plugin/php_cs_fixer.vim
" Author: Stéphane PY

if exists("g:vim_php_cs_fixer") || &cp
    finish
endif
let g:vim_php_cs_fixer = 1

let g:php_cs_fixer_enable_default_mapping = get(g:, 'php_cs_fixer_enable_default_mapping', '1')
let g:php_cs_fixer_dry_run = get(g:, 'php_cs_fixer_dry_run', 0)
let g:php_cs_fixer_verbose = get(g:, 'php_cs_fixer_verbose', 0)

" Backwards compatibility
fun! PhpCsFixerFix(path, dry_run)
    call php_cs_fixer#fix(a:path, a:dry_run)
endfun

fun! PhpCsFixerFixDirectory()
    call php_cs_fixer#fix(expand('%:p:h'), g:php_cs_fixer_dry_run)
endfun

fun! PhpCsFixerFixFile()
    call php_cs_fixer#fix(expand('%:p'), g:php_cs_fixer_dry_run)
endfun

if(g:php_cs_fixer_enable_default_mapping == 1)
    nnoremap <silent><leader>pcd :call PhpCsFixerFixDirectory()<CR>
    nnoremap <silent><leader>pcf :call PhpCsFixerFixFile()<CR>
endif

" vim: foldmethod=marker
