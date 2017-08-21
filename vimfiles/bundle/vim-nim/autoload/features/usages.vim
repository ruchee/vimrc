scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


let s:highlights = -1

let s:UsagesImpl = {}
function! s:UsagesImpl.run(data)
    if s:highlights >= 0
        call matchdelete(s:highlights)
    end
    let s:highlights = -1
    let highlights = []

    if len(a:data.lines) < 1
        echohl Comment | echo "No usages found"
    else
        for line in a:data.lines
            if len(line) == 0
                continue
            endif

            let res = util#ParseV2(line)
            if !s:findInProject && res.file != expand("%:p")
                continue
            endif

            call setqflist([{
                        \ 'filename': res.file,
                        \ 'lnum': res.line,
                        \ 'col': res.col + 1,
                        \ 'text': res.ctype . ": " . res.name . " (" . res.location . ")"}],
                        \ 'a')
            " call add(highlights, [line, column + 1, len(name)])
        endfor

        " let s:highlights = matchaddpos("Search", highlights)

        copen
        nnoremap <buffer><silent> <return> :call util#JumpFromQuickfix(0)<cr>
        nnoremap <buffer><silent> o :call util#JumpFromQuickfix(1)<cr>
    endif
endfunction

let s:UsagesDefinitionImpl = {}
function! s:UsagesDefinitionImpl.run(data)
    call suggest#NewKnown("use", !g:nvim_nim_enable_async, 1, a:data.file, a:data.line, a:data.col, s:UsagesImpl)
endfunction

function! features#usages#run(findInProject)
    cclose
    call setqflist([])
    let s:findInProject = a:findInProject
    call suggest#New("def", !g:nvim_nim_enable_async, 1, s:UsagesDefinitionImpl)
endfunction



let &cpo = s:save_cpo
unlet s:save_cpo
