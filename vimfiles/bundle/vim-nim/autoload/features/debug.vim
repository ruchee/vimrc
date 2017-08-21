scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


function! features#debug#run()
    echo "Nim tools debugging info"
    echo "------------------------"
    echo "\n"
    echo "-------- Tools ---------"
    echo "Nim:        " . g:nvim_nim_exec_nim
    echo "Nimble:     " . g:nvim_nim_exec_nimble
    echo "Nimsuggest: " . g:nvim_nim_exec_nimsuggest
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
