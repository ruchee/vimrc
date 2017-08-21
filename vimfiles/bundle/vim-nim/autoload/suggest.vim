scriptencoding utf-8


if exists("s:loaded")
    finish
endif
let s:loaded = 1


let s:findInProject = 1
let s:NimSuggest = {
            \ 'pty': 0,
            \ }

function! s:NimSuggest.on_stdout(job, chunk)
    call extend(self.lines, a:chunk)
endfunction

function! s:NimSuggest.on_stderr(job, chunk)
endfunction


function! s:NimSuggest.on_exit()
    echo ""
    let self.lines = util#FilterCompletions(self.lines)
    if len(self.lines) > 0
        call self.handler.run(self)
    else
        echohl Comment | echo "Got nothing" | echohl Normal
    endif
endfunction


function! suggest#CreateJob(useV2, file, callbacks)
    return jobstart([g:nvim_nim_exec_nimsuggest, '--stdin', (a:useV2 ? '--v2' : ''), a:file], a:callbacks)
endfunction


" TODO: Refactor combine (1)
function! suggest#NewKnown(command, sync, useV2, file, line, col, handler)
    let result = copy(s:NimSuggest)
    let result.lines = []
    let result.file = a:file
    let result.line = a:line
    let result.col = a:col
    let result.handler = a:handler
    let result.isAsync = has("nvim") && !a:sync && g:nvim_nim_enable_async
    let result.tempfile = util#WriteMemfile()
    let query = a:command . " " . result.file . ";" . result.tempfile . ":" . result.line . ":" . result.col

    if !result.isAsync
        let jobcmdstr = g:nvim_nim_exec_nimsuggest . " " . (a:useV2 ? '--v2' : '') . " " . '--stdin' . " " . result.file
        let fullcmd = 'echo -e "' . query . '"|' . jobcmdstr
        let result.lines = util#FilterCompletions(split(system(fullcmd), "\n"))
        if len(result.lines) > 0
            call a:handler.run(result)
        else
            echohl Comment | echo "Got nothing" | echohl Normal
        endif
    else
        call util#StartQuery()
        let result.job = suggest#CreateJob(a:useV2, result.file, result)
        if result.job > 0
            call jobsend(result.job, query . "\nquit\n")
        else
            echoerr "Unable to start server"
        endif
    endif
    return result
endfunction


function! suggest#New(command, sync, useV2, handler)
    return suggest#NewKnown(a:command, a:sync, a:useV2, expand("%:p"), line("."), col("."), a:handler)
endfunction
