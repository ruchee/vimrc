let s:suite  = themis#suite('vim-php-cs-fixer')
let s:assert = themis#helper('assert')

function! s:PhpCsFixerBaseTest(fileName) abort "{{{
    let l:tmpFile = '/tmp/' . a:fileName
    call system('cp tests/fixtures/misc/' . a:fileName . ' /tmp/')
    call PhpCsFixerFix(l:tmpFile, 0)
    call s:assert.equals(readfile(l:tmpFile), readfile('tests/results/misc/' . a:fileName))
    call delete(l:tmpFile)
endfunction "}}}

function! s:suite.before_each() abort "{{{
    let g:php_cs_fixer_command = 'php-cs-fixer fix --cache-file=/tmp/.php_cs.cache'
endfunction "}}}

function! s:suite.class_definitionNo_trailing_whitespace() abort "{{{
    call s:PhpCsFixerBaseTest('class_definition,no_trailing_whitespace.php')
endfunction "}}}

function! s:suite.imports() abort "{{{
    call s:PhpCsFixerBaseTest('imports.php')
endfunction "}}}

function! s:suite.no_closing_tagFull_opening_tag() abort "{{{
    call s:PhpCsFixerBaseTest('no_closing_tag,full_opening_tag.php')
endfunction "}}}

function! s:suite.phpdoc_to_commentPhpdoc_var_without_name() abort "{{{
    call s:PhpCsFixerBaseTest('phpdoc_to_comment,phpdoc_var_without_name.php')
endfunction "}}}

function! s:suite.phpdocsTest() abort "{{{
    call s:PhpCsFixerBaseTest('phpdocs.test.php')
endfunction "}}}

function! s:suite.semicolon_after_instructionNo_unneeded_control_parentheses() abort "{{{
    call s:PhpCsFixerBaseTest('semicolon_after_instruction,no_unneeded_control_parentheses.php')
endfunction "}}}

function! s:suite.simplified_null_returnNo_useless_returnNo_whitespace_in_blank_lineBlank_line_before_returnNo_extra_consecutive_blank_lines() abort "{{{
    call s:PhpCsFixerBaseTest('simplified_null_return,no_useless_return,no_whitespace_in_blank_line,blank_line_before_return,no_extra_consecutive_blank_lines.php')
endfunction "}}}

function! s:suite.single_import_per_statementOrdered_imports() abort "{{{
    call s:PhpCsFixerBaseTest('single_import_per_statement,ordered_imports.php')
endfunction "}}}
