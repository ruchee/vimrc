"=============================================================================
" File: php-cs-fixer.vim
" Author: Stéphane PY

if exists("g:vim_php_cs_fixer") || &cp
    finish
endif
let g:vim_php_cs_fixer = 1

" Global options definition."{{{
let g:php_cs_fixer_path = get(g:, 'php_cs_fixer_path', '~/php-cs-fixer.phar')
let g:php_cs_fixer_level = get(g:, 'php_cs_fixer_level', 'symfony')
let g:php_cs_fixer_php_path = get(g:, 'php_cs_fixer_php_path', 'php')
let g:php_cs_fixer_enable_default_mapping = get(g:, 'php_cs_fixer_enable_default_mapping', '1')
let g:php_cs_fixer_dry_run = get(g:, 'php_cs_fixer_dry_run', 0)
let g:php_cs_fixer_verbose = get(g:, 'php_cs_fixer_verbose', 0)

if executable('php-cs-fixer')
  let g:php_cs_fixer_command = 'php-cs-fixer fix'
else
  let g:php_cs_fixer_command = g:php_cs_fixer_php_path.' '.g:php_cs_fixer_path.' fix'
end

if exists('g:php_cs_fixer_config')
    let g:php_cs_fixer_command = g:php_cs_fixer_command.' --config='.g:php_cs_fixer_config
endif

if exists('g:php_cs_fixer_config_file') && filereadable(g:php_cs_fixer_config_file)
    let g:php_cs_fixer_command = g:php_cs_fixer_command . ' --config-file=' . g:php_cs_fixer_config_file
endif
"}}}


fun! PhpCsFixerFix(path, dry_run)

    if !executable('php-cs-fixer')
      if !filereadable(expand(g:php_cs_fixer_path))
        echoerr(g:php_cs_fixer_path.' is not found')
      endif
    endif

    let command = g:php_cs_fixer_command.' '.a:path

    if a:dry_run == 1
        echohl Title | echo "[DRY RUN MODE]" | echohl None
        let command = command.' --dry-run'
    endif

    if exists('g:php_cs_fixer_level') && g:php_cs_fixer_level != 'all'
        let command = command.' --level='.g:php_cs_fixer_level
    endif

    if exists('g:php_cs_fixer_fixers_list')
        let command = command.' --fixers='.g:php_cs_fixer_fixers_list
    endif

    let s:output = system(command)

    if a:dry_run != 1
      exec 'edit!'
    endif

    let fix_num = 0
    let errors_report = 0
    let error_num = 0
    for line in split(s:output, '\n')
        if match(line, 'Files that were not fixed due to errors reported during linting before fixing:') != -1
            let errors_report = 1
        endif

        if match(line, '^\s\+\d\+)') != -1
            if errors_report == 0
                let fix_num = fix_num + 1
            else
                let error_num = error_num + 1
            endif
        endif
    endfor

    if !(v:shell_error == 0 || v:shell_error == 8 || (v:shell_error == 1 && fix_num > 0))
        echohl Error | echo s:output | echohl None
    else

        if g:php_cs_fixer_verbose == 1
            echohl Title | echo s:output | echohl None
        else
            if fix_num > 0
                echohl Title | echo fix_num." file(s) modified(s)" | echohl None
            else
                echohl Title | echo "There is no cs to fix" | echohl None
            endif
            if error_num > 0
                echohl Error | echo error_num." error(s)" | echohl None
            endif
        endif

        " if there is no cs to fix, we have not to ask for remove dry run
        if a:dry_run == 1 && fix_num > 0
            let l:confirmed = confirm("Do you want to launch command without dry-run option ?", "&Yes\n&No", 2)
            if l:confirmed == 1
                call PhpCsFixerFix(a:path, 0)
            endif
        endif
    endif
endfun

fun! PhpCsFixerFixDirectory()
    call PhpCsFixerFix(expand('%:p:h'), g:php_cs_fixer_dry_run)
endfun

fun! PhpCsFixerFixFile()
    call PhpCsFixerFix(expand('%:p'), g:php_cs_fixer_dry_run)
endfun

if(g:php_cs_fixer_enable_default_mapping == 1)
    nnoremap <silent><leader>pcd :call PhpCsFixerFixDirectory()<CR>
    nnoremap <silent><leader>pcf :call PhpCsFixerFixFile()<CR>
endif

" vim: foldmethod=marker
