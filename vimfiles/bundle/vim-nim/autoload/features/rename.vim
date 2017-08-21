scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


let s:RenameImpl = {}


function! s:RenameImpl.run(data)
    let oldName = split(split(util#FirstNonEmpty(a:data.lines), "	")[2], "\\.")[-1]
    let newName = input("Rename symbol: ", oldName)

    for line in a:data.lines
        let res = util#ParseV2(line)
        if !s:findInProject && res.file != expand("%:p")
            continue
        endif

        if res.file != expand("%:p")
            execute ":e " . res.file
        endif

        if getline(res.line)[res.col] == '*'
            let res.col -= len(res.name)
        endif

        let left = res.col != 0 ? getline(res.line)[0:res.col - 1] : ""
        let right = getline(res.line)[res.col + len(oldName):-1]
        call setline(res.line, left . newName . right)
    endfor
endfunction


function! features#rename#run(inProject)
    let s:findInProject = a:inProject
    call suggest#New("use", !g:nvim_nim_enable_async, 1, s:RenameImpl)
endfunction



let &cpo = s:save_cpo
unlet s:save_cpo
