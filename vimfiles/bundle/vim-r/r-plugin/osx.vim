" This file contains code used only on OS X

function StartR_OSX()
    if IsSendCmdToRFake()
        return
    endif
    if g:rplugin_r64app && g:vimrplugin_i386 == 0
        let rcmd = "/Applications/R64.app"
    else
        let rcmd = "/Applications/R.app"
    endif

    if b:rplugin_r_args != " "
        " https://github.com/jcfaria/Vim-R-plugin/issues/63
        " https://stat.ethz.ch/pipermail/r-sig-mac/2013-February/009978.html
        call RWarningMsg('R.app does not support command line arguments. To pass "' . b:rplugin_r_args . '" to R, you must run it in a console. Set "vimrplugin_applescript = 0"')
    endif
    let rlog = system("open " . rcmd)
    if v:shell_error
        call RWarningMsg(rlog)
    endif
    let g:SendCmdToR = function('SendCmdToR_OSX')
    if WaitVimComStart()
        call SendToVimCom("\005B Update OB [StartR]")
        sleep 200m
        if g:vimrplugin_after_start != ''
            call system(g:vimrplugin_after_start)
        endif
    endif
endfunction

function SendCmdToR_OSX(cmd)
    if g:vimrplugin_ca_ck
        let cmd = "\001" . "\013" . a:cmd
    else
        let cmd = a:cmd
    endif

    if g:rplugin_r64app && g:vimrplugin_i386 == 0
        let rcmd = "R64"
    else
        let rcmd = "R"
    endif

    " for some reason it doesn't like "\025"
    let cmd = a:cmd
    let cmd = substitute(cmd, "\\", '\\\', 'g')
    let cmd = substitute(cmd, '"', '\\"', "g")
    let cmd = substitute(cmd, "'", "'\\\\''", "g")
    call system("osascript -e 'tell application \"".rcmd."\" to cmd \"" . cmd . "\"'")
    return 1
endfunction

