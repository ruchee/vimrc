scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


let s:repl_pid = -1
let s:repl_window = -1

function! s:Window()
    return winbufnr(s:repl_window)
endfunction

function! s:IsOpen()
    return winbufnr(s:Window()) > 0
endfunction

function! s:Focus()
    if s:IsOpen()
        exec ":" . s:Window() . "wincmd w"
    endif
endfunction

function! features#repl#start()
    if s:IsOpen()
        call s:Focus()
    else
        if g:nvim_nim_repl_vsplit
            vnew
            wincmd L
            wincmd L
            wincmd L
        else
            new
            wincmd J
            wincmd J
            wincmd J
        endif

        let s:repl_window = winnr()
        let s:repl_pid = termopen("sh -c \"nim secret\"")
    endif
endfunction

function! features#repl#send(lines)
    if !s:IsOpen()
        call features#repl#start()
        sleep 100m
    endif

    for line in a:lines
        call jobsend(s:repl_pid, line . "\r")
        sleep 20m
    endfor
    call jobsend(s:repl_pid, "\r")
    sleep 20m
    norm! i
endfunction

function! features#repl#stop()
    if s:repl_pid > 0
        call jobstop(s:repl_pid)
        call s:Focus()
        exec ":bd!"
    else
        echom "No REPL running"
    endif
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
