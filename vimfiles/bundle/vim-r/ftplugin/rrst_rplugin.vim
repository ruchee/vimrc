
if exists("g:disable_r_ftplugin") || has("nvim")
    finish
endif

" Source scripts common to R, Rrst, Rnoweb, Rhelp and Rdoc:
runtime r-plugin/common_global.vim
if exists("g:rplugin_failed")
    finish
endif

" Some buffer variables common to R, Rrst, Rnoweb, Rhelp and Rdoc need to be
" defined after the global ones:
runtime r-plugin/common_buffer.vim

if !exists("b:did_ftplugin") && !exists("g:rplugin_runtime_warn")
    runtime ftplugin/rrst.vim
    if !exists("b:did_ftplugin")
        call RWarningMsgInp("Your runtime files seems to be outdated.\nSee: https://github.com/jalvesaq/R-Vim-runtime")
        let g:rplugin_runtime_warn = 1
    endif
endif

function! RrstIsInRCode(vrb)
    let chunkline = search("^\\.\\. {r", "bncW")
    let docline = search("^\\.\\. \\.\\.", "bncW")
    if chunkline > docline && chunkline != line(".")
        return 1
    else
        if a:vrb
            call RWarningMsg("Not inside an R code chunk.")
        endif
        return 0
    endif
endfunction

function! RrstPreviousChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let curline = line(".")
        if RrstIsInRCode(0)
            let i = search("^\\.\\. {r", "bnW")
            if i != 0
                call cursor(i-1, 1)
            endif
        endif
        let i = search("^\\.\\. {r", "bnW")
        if i == 0
            call cursor(curline, 1)
            call RWarningMsg("There is no previous R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RrstNextChunk() range
    let rg = range(a:firstline, a:lastline)
    let chunk = len(rg)
    for var in range(1, chunk)
        let i = search("^\\.\\. {r", "nW")
        if i == 0
            call RWarningMsg("There is no next R code chunk to go.")
            return
        else
            call cursor(i+1, 1)
        endif
    endfor
    return
endfunction

function! RMakeHTMLrrst(t)
    call RSetWD()
    update
    if g:rplugin_has_rst2pdf == 0
        if executable("rst2pdf")
            let g:rplugin_has_rst2pdf = 1
        else
            call RWarningMsg("Is 'rst2pdf' application installed? Cannot convert into HTML/ODT: 'rst2pdf' executable not found.")
            return
        endif
    endif

    let rcmd = 'require(knitr)'
    if g:vimrplugin_strict_rst
        let rcmd = rcmd . '; render_rst(strict=TRUE)'
    endif
    let rcmd = rcmd . '; knit("' . expand("%:t") . '")'

    if a:t == "odt"
        let rcmd = rcmd . '; system("rst2odt ' . expand("%:r:t") . ".rst " . expand("%:r:t") . '.odt")'
    else
        let rcmd = rcmd . '; system("rst2html ' . expand("%:r:t") . ".rst " . expand("%:r:t") . '.html")'
    endif

    if g:vimrplugin_openhtml && a:t == "html"
        let rcmd = rcmd . '; browseURL("' . expand("%:r:t") . '.html")'
    endif
    call g:SendCmdToR(rcmd)
endfunction

function! RMakePDFrrst()
    if g:rplugin_vimcomport == 0
        call RWarningMsg("The vimcom package is required to make and open the PDF.")
    endif
    update
    call RSetWD()
    if g:rplugin_has_rst2pdf == 0
        if exists("g:vimrplugin_rst2pdfpath") && executable(g:vimrplugin_rst2pdfpath)
            let g:rplugin_has_rst2pdf = 1
        elseif executable("rst2pdf")
            let g:rplugin_has_rst2pdf = 1
        else
            call RWarningMsg("Is 'rst2pdf' application installed? Cannot convert into PDF: 'rst2pdf' executable not found.")
            return
        endif
    endif

    let rrstdir = expand("%:p:h")
    if has("win32") || has("win64")
        let rrstdir = substitute(rrstdir, '\\', '/', 'g')
    endif
    let pdfcmd = 'vim.interlace.rrst("' . expand("%:t") . '", rrstdir = "' . rrstdir . '"'
    if exists("g:vimrplugin_rrstcompiler")
        let pdfcmd = pdfcmd . ", compiler='" . g:vimrplugin_rrstcompiler . "'"
    endif
    if exists("g:vimrplugin_knitargs")
        let pdfcmd = pdfcmd . ", " . g:vimrplugin_knitargs
    endif
    if exists("g:vimrplugin_rst2pdfpath")
        let pdfcmd = pdfcmd . ", rst2pdfpath='" . g:vimrplugin_rst2pdfpath . "'"
    endif
    if exists("g:vimrplugin_rst2pdfargs")
        let pdfcmd = pdfcmd . ", " . g:vimrplugin_rst2pdfargs
    endif
    let pdfcmd = pdfcmd . ")"
    let ok = g:SendCmdToR(pdfcmd)
    if ok == 0
        return
    endif
endfunction

" Send Rrst chunk to R
function! SendRrstChunkToR(e, m)
    if RrstIsInRCode(0) == 0
        call RWarningMsg("Not inside an R code chunk.")
        return
    endif
    let chunkline = search("^\\.\\. {r", "bncW") + 1
    let docline = search("^\\.\\. \\.\\.", "ncW") - 1
    let lines = getline(chunkline, docline)
    let ok = RSourceLines(lines, a:e)
    if ok == 0
        return
    endif
    if a:m == "down"
        call RrstNextChunk()
    endif
endfunction

let b:IsInRCode = function("RrstIsInRCode")
let b:PreviousRChunk = function("RrstPreviousChunk")
let b:NextRChunk = function("RrstNextChunk")
let b:SendChunkToR = function("SendRrstChunkToR")

"==========================================================================
" Key bindings and menu items

call RCreateStartMaps()
call RCreateEditMaps()
call RCreateSendMaps()
call RControlMaps()
call RCreateMaps("nvi", '<Plug>RSetwd',        'rd', ':call RSetWD()')

" Only .Rrst files use these functions:
call RCreateMaps("nvi", '<Plug>RKnit',          'kn', ':call RKnit()')
call RCreateMaps("nvi", '<Plug>RMakePDFK',      'kp', ':call RMakePDFrrst()')
call RCreateMaps("nvi", '<Plug>RMakeHTML',      'kh', ':call RMakeHTMLrrst("html")')
call RCreateMaps("nvi", '<Plug>RMakeODT',       'ko', ':call RMakeHTMLrrst("odt")')
call RCreateMaps("nvi", '<Plug>RIndent',        'si', ':call RrstToggleIndentSty()')
call RCreateMaps("ni",  '<Plug>RSendChunk',     'cc', ':call b:SendChunkToR("silent", "stay")')
call RCreateMaps("ni",  '<Plug>RESendChunk',    'ce', ':call b:SendChunkToR("echo", "stay")')
call RCreateMaps("ni",  '<Plug>RDSendChunk',    'cd', ':call b:SendChunkToR("silent", "down")')
call RCreateMaps("ni",  '<Plug>REDSendChunk',   'ca', ':call b:SendChunkToR("echo", "down")')
call RCreateMaps("n",  '<Plug>RNextRChunk',     'gn', ':call b:NextRChunk()')
call RCreateMaps("n",  '<Plug>RPreviousRChunk', 'gN', ':call b:PreviousRChunk()')

" Menu R
if has("gui_running")
    runtime r-plugin/gui_running.vim
    call MakeRMenu()
endif

let g:rplugin_has_rst2pdf = 0

call RSourceOtherScripts()

if exists("b:undo_ftplugin")
    let b:undo_ftplugin .= " | unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
else
    let b:undo_ftplugin = "unlet! b:IsInRCode b:PreviousRChunk b:NextRChunk b:SendChunkToR"
endif
