" Vim completion script
" Language:    R
" Maintainer:  Jakson Alves de Aquino <jalvesaq@gmail.com>
"

" Tell R to create a list of objects file listing all currently available
" objects in its environment. The file is necessary for omni completion.
function BuildROmniList()
    if string(g:SendCmdToR) == "function('SendCmdToR_fake')"
        return
    endif

    let omnilistcmd = 'vimcom:::vim.bol("' . g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID . '"'
    if g:vimrplugin_allnames == 1
        let omnilistcmd = omnilistcmd . ', allnames = TRUE'
    endif
    let omnilistcmd = omnilistcmd . ')'

    call delete(g:rplugin_tmpdir . "/vimbol_finished")
    call delete(g:rplugin_tmpdir . "/eval_reply")
    call SendToVimCom("\x08" . $VIMINSTANCEID . omnilistcmd)
    if g:rplugin_vimcomport == 0
        sleep 500m
        return
    endif
    let g:rplugin_lastev = ReadEvalReply()
    if g:rplugin_lastev == "R is busy." || g:rplugin_lastev == "No reply"
        call RWarningMsg(g:rplugin_lastev)
        sleep 800m
        return
    endif
    sleep 20m
    let ii = 0
    while !filereadable(g:rplugin_tmpdir . "/vimbol_finished") && ii < 5
        let ii += 1
        sleep
    endwhile
    echon "\r               "
    if ii == 5
        call RWarningMsg("No longer waiting...")
        return
    endif

    let g:rplugin_globalenvlines = readfile(g:rplugin_tmpdir . "/GlobalEnvList_" . $VIMINSTANCEID)
endfunction

fun! rcomplete#CompleteR(findstart, base)
    if &filetype == "rnoweb" && RnwIsInRCode(0) == 0 && exists("*LatexBox_Complete")
        let texbegin = LatexBox_Complete(a:findstart, a:base)
        return texbegin
    endif
    if a:findstart
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && (line[start - 1] =~ '\w' || line[start - 1] =~ '\.' || line[start - 1] =~ '\$')
            let start -= 1
        endwhile
        call BuildROmniList()
        return start
    else
        let resp = []
        if strlen(a:base) == 0
            return resp
        endif

        if len(g:rplugin_omni_lines) == 0
            call add(resp, {'word': a:base, 'menu': " [ List is empty. Did you load vimcom package? ]"})
        endif

        let flines = g:rplugin_omni_lines + g:rplugin_globalenvlines
        " The char '$' at the end of 'a:base' is treated as end of line, and
        " the pattern is never found in 'line'.
        let newbase = '^' . substitute(a:base, "\\$$", "", "")
        for line in flines
            if line =~ newbase
                " Skip cols of data frames unless the user is really looking for them.
                if a:base !~ '\$' && line =~ '\$'
                    continue
                endif
                let tmp1 = split(line, "\x06", 1)
                if g:vimrplugin_show_args
                    let info = tmp1[4]
                    let info = substitute(info, "\t", ", ", "g")
                    let info = substitute(info, "\x07", " = ", "g")
                    let tmp2 = {'word': tmp1[0], 'menu': tmp1[1] . ' ' . tmp1[3], 'info': info}
                else
                    let tmp2 = {'word': tmp1[0], 'menu': tmp1[1] . ' ' . tmp1[3]}
                endif
                call add(resp, tmp2)
            endif
        endfor

        return resp
    endif
endfun

