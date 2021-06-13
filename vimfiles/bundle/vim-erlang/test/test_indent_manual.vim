" The function in this file shall be called when test_indent_manual.erl is
" open.

function! TestIndentManual()
    normal 3gg
    exec "normal! I  "
    normal ==
endfunction

noremap <F5> :call TestIndentManual()<cr>
