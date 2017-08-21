scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


let s:edb_terminal_job = -2

let s:NimDebugger = {
            \ 'pty': 1,
            \ }

function! s:NimDebugger.on_stdout(job, chunk, ...)
    for line in a:chunk
        " *** endb| reached edb.nim(4) wat ***
        " let matched = matchlist(line, "\\*\\*\\* endb\|\(.*\)$")
        let matched = matchlist(line, "")
    endfor
endfunction

function! s:NimDebugger.on_stderr(job, chunk, ...)
    " echoerr "Error" . join(a:chunk, "\n")
endfunction

function! s:NimDebugger.on_exit(...)
    echoerr "Done"
    let s:edb_terminal_job = -2
endfunction

function! features#debugger#run()
    if s:edb_terminal_job > 0
        echo "Debugger already running"
    else
        " vnew
        let s:edb_terminal_job = jobstart([
                    \ "nim", 
                    \ "c", 
                    \ "--colors:off", 
                    \ "--debugger:native", 
                    \ "--lineDir:on", 
                    \ "--lineTrace:on", 
                    \ "edb.nim"], s:NimDebugger)
        " let s:edb_terminal_job = termopen("nim c --debugger:on -r edb.nim")
        " wincmd p
    endif
endfunction

function! features#debugger#stop()
    if s:edb_terminal_job > 0
        call jobstop([s:edb_terminal_job])
    else
        echo "No debugger running"
    endif
endfunction

function! s:SendCommand(cmd)
    if s:edb_terminal_job > 0
        call jobsend(s:edb_terminal_job, a:cmd . "\n")
        call jobsend(s:edb_terminal_job, "w\n")
        call jobsend(s:edb_terminal_job, "g\n")
        call jobsend(s:edb_terminal_job, "l\n")
    else
        echom "Debugger not running"
    endif
endfunction

function! features#debugger#continue()
    call s:SendCommand("c")
endfunction

function! features#debugger#stepinto()
    call s:SendCommand("s")
endfunction

function! features#debugger#stepover()
    call s:SendCommand("n")
endfunction

function! features#debugger#skipcurrent()
    call s:SendCommand("f")
endfunction

function! features#debugger#ignore()
    call s:SendCommand("i")
endfunction

function! features#debugger#togglebp()
    call s:SendCommand("s")
endfunction

autocmd! TermResponse * call s:ParseTerminal()

function! s:ParseTerminal()
    echoerr "WAAAT"
    echoerr &t_RV
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
