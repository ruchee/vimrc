
if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif

" Source scripts common to R, Rnoweb, Rhelp, Rmd, Rrst and rdoc files:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rnoweb, Rhelp, Rmd, Rrst and rdoc files
" need be defined after the global ones:
runtime r-plugin/common_buffer.vim

if !exists("b:did_ftplugin") && !exists("g:rplugin_runtime_warn")
    runtime ftplugin/r.vim
    if !exists("b:did_ftplugin")
        call RWarningMsgInp("Your runtime files seems to be outdated.\nSee: https://github.com/jalvesaq/R-Vim-runtime")
    endif
    let g:rplugin_runtime_warn = 1
endif

" Run R CMD BATCH on current file and load the resulting .Rout in a split
" window
function! ShowRout()
    let b:routfile = expand("%:r") . ".Rout"
    if bufloaded(b:routfile)
        exe "bunload " . b:routfile
        call delete(b:routfile)
    endif

    " if not silent, the user will have to type <Enter>
    silent update

    if has("win32") || has("win64")
        let rcmd = 'Rcmd.exe BATCH --no-restore --no-save "' . expand("%") . '" "' . b:routfile . '"'
    else
        let rcmd = g:rplugin_R . " CMD BATCH --no-restore --no-save '" . expand("%") . "' '" . b:routfile . "'"
    endif

    if has("win32") || has("win64") || v:servername == ""
        echon "Please wait for: " . rcmd
        redraw
        let rlog = system(rcmd)
        if v:shell_error && rlog != ""
            call RWarningMsg('Error: "' . rlog . '"')
            sleep 1
        endif
        if filereadable(b:routfile)
            if g:vimrplugin_routnotab == 1
                exe "split " . b:routfile
            else
                exe "tabnew " . b:routfile
            endif
            set filetype=rout
        else
            call RWarningMsg("The file '" . b:routfile . "' is not readable.")
        endif
    else
        let shlines = [rcmd,
                    \ 'vim --servername ' . v:servername . " --remote-expr '" . 'GetROutput("' . b:routfile . '")' . "'",
                    \ 'rm "' . g:rplugin_tmpdir . '/runRcmdbatch.sh' . '"']
        call writefile(shlines, g:rplugin_tmpdir . '/runRcmdbatch.sh')
        call system('sh "' .  g:rplugin_tmpdir . '/runRcmdbatch.sh" >/dev/null 2>/dev/null &')
    endif
endfunction

" Convert R script into Rmd, md and, then, html.
function! RSpin()
    update
    call g:SendCmdToR('require(knitr); .vim_oldwd <- getwd(); setwd("' . expand("%:p:h") . '"); spin("' . expand("%:t") . '"); setwd(.vim_oldwd); rm(.vim_oldwd)')
endfunction

" Default IsInRCode function when the plugin is used as a global plugin
function! DefaultIsInRCode(vrb)
    return 1
endfunction

let b:IsInRCode = function("DefaultIsInRCode")

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()

" Only .R files are sent to R
call RCreateMaps("ni", '<Plug>RSendFile',     'aa', ':call SendFileToR("silent")')
call RCreateMaps("ni", '<Plug>RESendFile',    'ae', ':call SendFileToR("echo")')
call RCreateMaps("ni", '<Plug>RShowRout',     'ao', ':call ShowRout()')

" Knitr::spin
" -------------------------------------
call RCreateMaps("ni", '<Plug>RSpinFile',     'ks', ':call RSpin()')

call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')


" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode"
endif
