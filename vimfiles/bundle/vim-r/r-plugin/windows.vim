" This file contains code used only on Windows

let g:rplugin_sumatra_path = ""
let g:rplugin_python_initialized = 0

call RSetDefaultValue("g:vimrplugin_sleeptime", 100)

" Avoid invalid values defined by the user
exe "let s:sleeptimestr = " . '"' . g:vimrplugin_sleeptime . '"'
let s:sleeptime = str2nr(s:sleeptimestr)
if s:sleeptime < 1 || s:sleeptime > 1000
    let g:vimrplugin_sleeptime = 100
endif
unlet s:sleeptimestr
unlet s:sleeptime

let g:rplugin_sleeptime = g:vimrplugin_sleeptime . 'm'

if !exists("g:rplugin_rpathadded")
    if exists("g:vimrplugin_r_path")
        if !isdirectory(g:vimrplugin_r_path)
            call RWarningMsgInp("vimrplugin_r_path must be a directory (check your vimrc)")
            let g:rplugin_failed = 1
            finish
        endif
        if !filereadable(g:vimrplugin_r_path . "\\Rgui.exe")
            call RWarningMsgInp('File "' . g:vimrplugin_r_path . '\Rgui.exe" is unreadable (check vimrplugin_r_path in your vimrc).')
            let g:rplugin_failed = 1
            finish
        endif
        let $PATH = g:vimrplugin_r_path . ";" . $PATH
        let g:rplugin_Rgui = g:vimrplugin_r_path . "\\Rgui.exe"
    else
        let rip = filter(split(system('reg.exe QUERY "HKLM\SOFTWARE\R-core\R" /s'), "\n"), 'v:val =~ ".*InstallPath.*REG_SZ"')
        let g:rdebug_reg_rpath_1 = rip
        if len(rip) > 0
            let s:rinstallpath = substitute(rip[0], '.*InstallPath.*REG_SZ\s*', '', '')
            let s:rinstallpath = substitute(s:rinstallpath, '\n', '', 'g')
            let s:rinstallpath = substitute(s:rinstallpath, '\s*$', '', 'g')
            let g:rdebug_reg_rpath_2 = s:rinstallpath
        endif

        if !exists("s:rinstallpath")
            call RWarningMsgInp("Could not find R path in Windows Registry. If you have already installed R, please, set the value of 'vimrplugin_r_path'.")
            let g:rplugin_failed = 1
            finish
        endif
        if isdirectory(s:rinstallpath . '\bin\i386')
            if !isdirectory(s:rinstallpath . '\bin\x64')
                let g:vimrplugin_i386 = 1
            endif
            if g:vimrplugin_i386
                let $PATH = s:rinstallpath . '\bin\i386;' . $PATH
                let g:rplugin_Rgui = s:rinstallpath . '\bin\i386\Rgui.exe'
            else
                let $PATH = s:rinstallpath . '\bin\x64;' . $PATH
                let g:rplugin_Rgui = s:rinstallpath . '\bin\x64\Rgui.exe'
            endif
        else
            let $PATH = s:rinstallpath . '\bin;' . $PATH
            let g:rplugin_Rgui = s:rinstallpath . '\bin\Rgui.exe'
        endif
        unlet s:rinstallpath
    endif
    let g:rplugin_rpathadded = 1
endif
let g:vimrplugin_term_cmd = "none"
let g:vimrplugin_term = "none"
if !exists("g:vimrplugin_r_args")
    let g:vimrplugin_r_args = "--sdi"
endif
if g:vimrplugin_Rterm
    let g:rplugin_Rgui = substitute(g:rplugin_Rgui, "Rgui", "Rterm", "")
endif

if !exists("g:vimrplugin_R_window_title")
    if g:vimrplugin_Rterm
        let g:vimrplugin_R_window_title = "Rterm"
    else
        let g:vimrplugin_R_window_title = "R Console"
    endif
endif

function FindSumatra()
    if executable($ProgramFiles . "\\SumatraPDF\\SumatraPDF.exe")
        let g:rplugin_sumatra_path = $ProgramFiles . "\\SumatraPDF\\SumatraPDF.exe"
        return 1
    endif
    let smtr = system('reg.exe QUERY "HKLM\Software\Microsoft\Windows\CurrentVersion\App Paths" /v "SumatraPDF.exe"')
    if len(smtr) > 0
        let g:rdebug_reg_personal = smtr
        let smtr = substitute(smtr, '.*REG_SZ\s*', '', '')
        let smtr = substitute(smtr, '\n', '', 'g')
        let smtr = substitute(smtr, '\s*$', '', 'g')
        if executable(smtr)
            let g:rplugin_sumatra_path = smtr
            return 1
        else
            call RWarningMsg('Sumatra not found: "' . smtr . '"')
        endif
    else
        call RWarningMsg("SumatraPDF not found in Windows registry.")
    endif
    return 0
endfunction

function StartR_Windows()
    if string(g:SendCmdToR) != "function('SendCmdToR_fake')"
        let repl = libcall(g:rplugin_vimcom_lib, "IsRRunning", 'No argument')
        if repl =~ "^Yes"
            call RWarningMsg('R is already running.')
            return
        else
            let g:SendCmdToR = function('SendCmdToR_fake')
            let g:rplugin_r_pid = 0
        endif
    endif

    if !executable(g:rplugin_Rgui)
        call RWarningMsg('R executable "' . g:rplugin_Rgui . '" not found.')
        if exists("g:rdebug_reg_rpath_1")
            call RWarningMsg('DEBUG message 1: >>' . g:rdebug_reg_rpath_1 . '<<')
        endif
        if exists("g:rdebug_reg_rpath_1")
            call RWarningMsg('DEBUG message 2: >>' . g:rdebug_reg_rpath_2 . '<<')
        endif
        return
    endif

    " R and Vim use different values for the $HOME variable.
    let saved_home = $HOME
    let prs = system('reg.exe QUERY "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" /v "Personal"')
    if len(prs) > 0
        let g:rdebug_reg_personal = prs
        let prs = substitute(prs, '.*REG_SZ\s*', '', '')
        let prs = substitute(prs, '\n', '', 'g')
        let prs = substitute(prs, '\s*$', '', 'g')
        let $HOME = prs
    endif

    let rcmd = g:rplugin_Rgui
    if g:vimrplugin_Rterm
        let rcmd = substitute(rcmd, "Rgui", "Rterm", "")
    endif
    let rcmd = '"' . rcmd . '" ' . g:vimrplugin_r_args

    silent exe "!start " . rcmd

    let $HOME = saved_home

    let g:SendCmdToR = function('SendCmdToR_Windows')
    if WaitVimComStart()
        call foreground()
        if g:vimrplugin_arrange_windows && v:servername != "" && filereadable(g:rplugin_compldir . "/win_pos")
            let repl = libcall(g:rplugin_vimcom_lib, "ArrangeWindows", $VIMRPLUGIN_COMPLDIR)
            if repl != "OK"
                call RWarningMsg(repl)
            endif
        endif
        if g:vimrplugin_after_start != ''
            call system(g:vimrplugin_after_start)
        endif
    endif
endfunction

function SendCmdToR_Windows(cmd)
    if g:vimrplugin_ca_ck
        let cmd = "\001" . "\013" . a:cmd . "\n"
    else
        let cmd = a:cmd . "\n"
    endif
    let save_clip = getreg('+')
    call setreg('+', cmd)
    if g:vimrplugin_Rterm
        let repl = libcall(g:rplugin_vimcom_lib, "SendToRTerm", cmd)
    else
        let repl = libcall(g:rplugin_vimcom_lib, "SendToRConsole", cmd)
    endif
    if repl != "OK"
        call RWarningMsg(repl)
        call ClearRInfo()
    endif
    exe "sleep " . g:rplugin_sleeptime
    call foreground()
    call setreg('+', save_clip)
    return 1
endfunction

