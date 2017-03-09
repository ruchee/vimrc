" Author: diartyz <diartyz@gmail.com>

let g:ale_wxss_stylelint_executable =
\   get(g:, 'ale_wxss_stylelint_executable', 'stylelint')

let g:ale_wxss_stylelint_use_global =
\   get(g:, 'ale_wxss_stylelint_use_global', 1)

function! ale_linters#wxss#stylelint#GetExecutable(buffer) abort
    if g:ale_wxss_stylelint_use_global
        return g:ale_wxss_stylelint_executable
    endif

    return ale#util#ResolveLocalPath(
    \   a:buffer,
    \   'node_modules/.bin/stylelint',
    \   g:ale_wxss_stylelint_executable
    \)
endfunction

function! ale_linters#wxss#stylelint#GetCommand(buffer) abort
    return ale_linters#wxss#stylelint#GetExecutable(a:buffer)
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('wxss', {
\   'name': 'stylelint',
\   'executable_callback': 'ale_linters#wxss#stylelint#GetExecutable',
\   'command_callback': 'ale_linters#wxss#stylelint#GetCommand',
\   'callback': 'ale#handlers#HandleStyleLintFormat',
\})
