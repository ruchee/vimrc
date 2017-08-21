scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


let s:InfoImpl = {}

function! s:New(useWeb)
    let result = copy(s:InfoImpl)
    let result.useWeb = a:useWeb
    return result
endfunction

function! s:InfoImpl.run(data)
    if len(a:data.lines) == 0
        echo "No information found"
        return
    endif

    let res = util#ParseV1(a:data.lines[0])

    if self.useWeb
        call util#open_module_doc(res.location, res.lname)
    else
        echohl None
        echohl Function | echon res.lname
        echohl Comment | echon "\n » "
        echohl Type | echon res.kindstr

        if len(res.name) > 0 && res.lname != res.name
            echon "\n"
            echohl Comment | echon " » "
            echohl Typedef | echon res.name
        end

        echohl Comment | echon "\n » "
        echohl Include | echon res.location
        echohl Comment | echon " ("
        echohl String | echon res.file
        echohl Comment | echon ")"

        if res.doc != "\"\""
            echohl Comment | echon "\n » "
            echohl Normal | echon res.doc
        endif
    endif
endfunction


function! features#info#web()
    let current_word = expand("<cword>")
    if modules#isGlobalImport(current_word)
        call util#open_module_doc(current_word, "")
    else
        call suggest#New("def", !g:nvim_nim_enable_async, 0, s:New(1))
    endif
endfunction


function! features#info#run()
    call suggest#New("def", !g:nvim_nim_enable_async, 0, s:New(0))
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
