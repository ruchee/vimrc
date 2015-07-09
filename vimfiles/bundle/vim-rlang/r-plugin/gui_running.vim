" This file contains code used only if has("gui_running")

if exists("g:did_r_plugin_gui_running")
    finish
endif
let g:did_r_plugin_gui_running = 1

function MakeRMenu()
    if g:rplugin_hasmenu == 1
        return
    endif

    " Do not translate "File":
    menutranslate clear

    "----------------------------------------------------------------------------
    " Start/Close
    "----------------------------------------------------------------------------
    call RCreateMenuItem("nvi", 'Start/Close.Start\ R\ (default)', '<Plug>RStart', 'rf', ':call StartR("R")')
    call RCreateMenuItem("nvi", 'Start/Close.Start\ R\ (custom)', '<Plug>RCustomStart', 'rc', ':call StartR("custom")')
    "-------------------------------
    menu R.Start/Close.-Sep1- <nul>
    call RCreateMenuItem("nvi", 'Start/Close.Close\ R\ (no\ save)', '<Plug>RClose', 'rq', ":call RQuit('no')")
    menu R.Start/Close.-Sep2- <nul>

    nmenu <silent> R.Start/Close.Stop\ R<Tab>:RStop :RStop<CR>

    "----------------------------------------------------------------------------
    " Send
    "----------------------------------------------------------------------------
    if &filetype == "r" || g:vimrplugin_never_unmake_menu
        call RCreateMenuItem("ni", 'Send.File', '<Plug>RSendFile', 'aa', ':call SendFileToR()')
        call RCreateMenuItem("ni", 'Send.File\ (open\ \.Rout)', '<Plug>RShowRout', 'ao', ':call ShowRout()')
    endif
    "-------------------------------
    menu R.Send.-Sep1- <nul>
    call RCreateMenuItem("ni", 'Send.Block\ (cur)', '<Plug>RSendMBlock', 'bb', ':call SendMBlockToR("stay")')
    call RCreateMenuItem("ni", 'Send.Block\ (cur,\ down)', '<Plug>RDSendMBlock', 'bd', ':call SendMBlockToR("down")')
    "-------------------------------
    if &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || g:vimrplugin_never_unmake_menu
        menu R.Send.-Sep2- <nul>
        call RCreateMenuItem("ni", 'Send.Chunk\ (cur)', '<Plug>RSendChunk', 'cc', ':call b:SendChunkToR("stay")')
        call RCreateMenuItem("ni", 'Send.Chunk\ (cur,\ down)', '<Plug>RDSendChunk', 'cd', ':call b:SendChunkToR("down")')
        call RCreateMenuItem("ni", 'Send.Chunk\ (from\ first\ to\ here)', '<Plug>RSendChunkFH', 'ch', ':call SendFHChunkToR()')
    endif
    "-------------------------------
    menu R.Send.-Sep3- <nul>
    call RCreateMenuItem("ni", 'Send.Function\ (cur)', '<Plug>RSendFunction', 'ff', ':call SendFunctionToR("stay")')
    call RCreateMenuItem("ni", 'Send.Function\ (cur\ and\ down)', '<Plug>RDSendFunction', 'fd', ':call SendFunctionToR("down")')
    "-------------------------------
    menu R.Send.-Sep4- <nul>
    call RCreateMenuItem("v", 'Send.Selection', '<Plug>RSendSelection', 'ss', ':call SendSelectionToR("stay")')
    call RCreateMenuItem("v", 'Send.Selection\ (and\ down)', '<Plug>RDSendSelection', 'sd', ':call SendSelectionToR("down")')
    "-------------------------------
    menu R.Send.-Sep5- <nul>
    call RCreateMenuItem("ni", 'Send.Paragraph', '<Plug>RSendParagraph', 'pp', ':call SendParagraphToR("stay")')
    call RCreateMenuItem("ni", 'Send.Paragraph\ (and\ down)', '<Plug>RDSendParagraph', 'pd', ':call SendParagraphToR("down")')
    "-------------------------------
    menu R.Send.-Sep6- <nul>
    call RCreateMenuItem("ni0", 'Send.Line', '<Plug>RSendLine', 'l', ':call SendLineToR("stay")')
    call RCreateMenuItem("ni0", 'Send.Line\ (and\ down)', '<Plug>RDSendLine', 'd', ':call SendLineToR("down")')
    call RCreateMenuItem("ni0", 'Send.Line\ (and\ insert\ output)', '<Plug>RDSendLineAndInsertOutput', 'o', ':call SendLineToRAndInsertOutput()')
    call RCreateMenuItem("i", 'Send.Line\ (and\ new\ one)', '<Plug>RSendLAndOpenNewOne', 'q', ':call SendLineToR("newline")')
    call RCreateMenuItem("n", 'Send.Left\ part\ of\ line\ (cur)', '<Plug>RNLeftPart', 'r<Left>', ':call RSendPartOfLine("left", 0)')
    call RCreateMenuItem("n", 'Send.Right\ part\ of\ line\ (cur)', '<Plug>RNRightPart', 'r<Right>', ':call RSendPartOfLine("right", 0)')
    call RCreateMenuItem("i", 'Send.Left\ part\ of\ line\ (cur)', '<Plug>RILeftPart', 'r<Left>', 'l:call RSendPartOfLine("left", 1)')
    call RCreateMenuItem("i", 'Send.Right\ part\ of\ line\ (cur)', '<Plug>RIRightPart', 'r<Right>', 'l:call RSendPartOfLine("right", 1)')

    "----------------------------------------------------------------------------
    " Control
    "----------------------------------------------------------------------------
    call RControlMenu()
    "-------------------------------
    menu R.Command.-Sep4- <nul>
    if &filetype != "rdoc"
        call RCreateMenuItem("nvi", 'Command.Set\ working\ directory\ (cur\ file\ path)', '<Plug>RSetwd', 'rd', ':call RSetWD()')
    endif
    "-------------------------------
    if &filetype == "rnoweb" || &filetype == "rmd" || &filetype == "rrst" || g:vimrplugin_never_unmake_menu
        if &filetype == "rnoweb" || g:vimrplugin_never_unmake_menu
            menu R.Command.-Sep5- <nul>
            call RCreateMenuItem("nvi", 'Command.Sweave\ (cur\ file)', '<Plug>RSweave', 'sw', ':call RSweave()')
            call RCreateMenuItem("nvi", 'Command.Sweave\ and\ PDF\ (cur\ file)', '<Plug>RMakePDF', 'sp', ':call RMakePDF("nobib", 0)')
            call RCreateMenuItem("nvi", 'Command.Sweave,\ BibTeX\ and\ PDF\ (cur\ file)', '<Plug>RBibTeX', 'sb', ':call RMakePDF("bibtex", 0)')
        endif
        menu R.Command.-Sep6- <nul>
        if &filetype == "rnoweb"
            call RCreateMenuItem("nvi", 'Command.Knit\ (cur\ file)', '<Plug>RKnit', 'kn', ':call RKnitRnw()')
        else
            call RCreateMenuItem("nvi", 'Command.Knit\ (cur\ file)', '<Plug>RKnit', 'kn', ':call RKnit()')
        endif
        if &filetype == "rnoweb" || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakePDF("nobib", 1)')
            call RCreateMenuItem("nvi", 'Command.Knit,\ BibTeX\ and\ PDF\ (cur\ file)', '<Plug>RBibTeXK', 'kb', ':call RMakePDF("bibtex", 1)')
        endif
        if &filetype == "rmd" || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakeRmd("pdf_document")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ Beamer\ PDF\ (cur\ file)', '<Plug>RMakePDFKb', 'kl', ':call RMakeRmd("beamer_presentation")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ HTML\ (cur\ file)', '<Plug>RMakeHTML', 'kh', ':call RMakeRmd("html_document")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ ODT\ (cur\ file)', '<Plug>RMakeODT', 'ko', ':call RMakeRmd("odt")')
        endif
        if &filetype == "rrst" || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ PDF\ (cur\ file)', '<Plug>RMakePDFK', 'kp', ':call RMakePDFrrst()')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ HTML\ (cur\ file)', '<Plug>RMakeHTML', 'kh', ':call RMakeHTMLrrst("html")')
            call RCreateMenuItem("nvi", 'Command.Knit\ and\ ODT\ (cur\ file)', '<Plug>RMakeODT', 'ko', ':call RMakeHTMLrrst("odt")')
        endif
        menu R.Command.-Sep61- <nul>
        call RCreateMenuItem("nvi", 'Command.Open\ PDF\ (cur\ file)', '<Plug>ROpenPDF', 'op', ':call ROpenPDF("Get Master")')
        if ($DISPLAY != "" && g:vimrplugin_synctex && &filetype == "rnoweb") || g:vimrplugin_never_unmake_menu
            call RCreateMenuItem("nvi", 'Command.Search\ forward\ (SyncTeX)', '<Plug>RSyncFor', 'gp', ':call SyncTeX_forward()')
            call RCreateMenuItem("nvi", 'Command.Go\ to\ LaTeX\ (SyncTeX)', '<Plug>RSyncTex', 'gt', ':call SyncTeX_forward(1)')
        endif
    endif
    "-------------------------------
    if &filetype == "r" || g:vimrplugin_never_unmake_menu
        menu R.Command.-Sep71- <nul>
        call RCreateMenuItem("nvi", 'Command.Spin\ (cur\ file)', '<Plug>RSpinFile', 'ks', ':call RSpin()')
    endif
    menu R.Command.-Sep72- <nul>
    if &filetype == "r" || &filetype == "rnoweb" || g:vimrplugin_never_unmake_menu
        nmenu <silent> R.Command.Build\ tags\ file\ (cur\ dir)<Tab>:RBuildTags :call g:SendCmdToR('rtags(ofile = "TAGS")')<CR>
        imenu <silent> R.Command.Build\ tags\ file\ (cur\ dir)<Tab>:RBuildTags <Esc>:call g:SendCmdToR('rtags(ofile = "TAGS")')<CR>a
    endif

    menu R.-Sep7- <nul>

    "----------------------------------------------------------------------------
    " Edit
    "----------------------------------------------------------------------------
    if &filetype == "r" || &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rhelp" || g:vimrplugin_never_unmake_menu
        if g:vimrplugin_assign == 1 || g:vimrplugin_assign == 2
            silent exe 'imenu <silent> R.Edit.Insert\ \"\ <-\ \"<Tab>' . g:vimrplugin_assign_map . ' <Esc>:call ReplaceUnderS()<CR>a'
        endif
        imenu <silent> R.Edit.Complete\ object\ name<Tab>^X^O <C-X><C-O>
        if hasmapto("<Plug>RCompleteArgs", "i")
            let boundkey = RIMapCmd("<Plug>RCompleteArgs")
            exe "imenu <silent> R.Edit.Complete\\ function\\ arguments<Tab>" . boundkey . " " . boundkey
        else
            imenu <silent> R.Edit.Complete\ function\ arguments<Tab>^X^A <C-X><C-A>
        endif
        menu R.Edit.-Sep71- <nul>
        nmenu <silent> R.Edit.Indent\ (line)<Tab>== ==
        vmenu <silent> R.Edit.Indent\ (selected\ lines)<Tab>= =
        nmenu <silent> R.Edit.Indent\ (whole\ buffer)<Tab>gg=G gg=G
        menu R.Edit.-Sep72- <nul>
        call RCreateMenuItem("ni", 'Edit.Toggle\ comment\ (line/sel)', '<Plug>RToggleComment', 'xx', ':call RComment("normal")')
        call RCreateMenuItem("v", 'Edit.Toggle\ comment\ (line/sel)', '<Plug>RToggleComment', 'xx', ':call RComment("selection")')
        call RCreateMenuItem("ni", 'Edit.Comment\ (line/sel)', '<Plug>RSimpleComment', 'xc', ':call RSimpleCommentLine("normal", "c")')
        call RCreateMenuItem("v", 'Edit.Comment\ (line/sel)', '<Plug>RSimpleComment', 'xc', ':call RSimpleCommentLine("selection", "c")')
        call RCreateMenuItem("ni", 'Edit.Uncomment\ (line/sel)', '<Plug>RSimpleUnComment', 'xu', ':call RSimpleCommentLine("normal", "u")')
        call RCreateMenuItem("v", 'Edit.Uncomment\ (line/sel)', '<Plug>RSimpleUnComment', 'xu', ':call RSimpleCommentLine("selection", "u")')
        call RCreateMenuItem("ni", 'Edit.Add/Align\ right\ comment\ (line,\ sel)', '<Plug>RRightComment', ';', ':call MovePosRCodeComment("normal")')
        call RCreateMenuItem("v", 'Edit.Add/Align\ right\ comment\ (line,\ sel)', '<Plug>RRightComment', ';', ':call MovePosRCodeComment("selection")')
        if &filetype == "rnoweb" || &filetype == "rrst" || &filetype == "rmd" || g:vimrplugin_never_unmake_menu
            menu R.Edit.-Sep73- <nul>
            call RCreateMenuItem("n", 'Edit.Go\ (next\ R\ chunk)', '<Plug>RNextRChunk', 'gn', ':call b:NextRChunk()')
            call RCreateMenuItem("n", 'Edit.Go\ (previous\ R\ chunk)', '<Plug>', 'gN', ':call b:PreviousRChunk()')
        endif
    endif

    "----------------------------------------------------------------------------
    " Object Browser
    "----------------------------------------------------------------------------
    call RBrowserMenu()

    "----------------------------------------------------------------------------
    " Help
    "----------------------------------------------------------------------------
    menu R.-Sep8- <nul>
    amenu R.Help\ (plugin).Overview :help r-plugin-overview<CR>
    amenu R.Help\ (plugin).Main\ features :help r-plugin-features<CR>
    amenu R.Help\ (plugin).Installation :help r-plugin-installation<CR>
    amenu R.Help\ (plugin).Use :help r-plugin-use<CR>
    amenu R.Help\ (plugin).Known\ bugs\ and\ workarounds :help r-plugin-known-bugs<CR>

    amenu R.Help\ (plugin).Options.Assignment\ operator\ and\ Rnoweb\ code :help vimrplugin_assign<CR>
    amenu R.Help\ (plugin).Options.Object\ Browser :help vimrplugin_objbr_place<CR>
    amenu R.Help\ (plugin).Options.Vim\ as\ pager\ for\ R\ help :help vimrplugin_vimpager<CR>
    if !(has("gui_win32") || has("gui_win64"))
        amenu R.Help\ (plugin).Options.Terminal\ emulator :help vimrplugin_term<CR>
    endif
    if g:rplugin_is_darwin
        amenu R.Help\ (plugin).Options.Integration\ with\ Apple\ Script :help vimrplugin_applescript<CR>
    endif
    if has("gui_win32") || has("gui_win64")
        amenu R.Help\ (plugin).Options.Use\ 32\ bit\ version\ of\ R :help vimrplugin_i386<CR>
    endif
    amenu R.Help\ (plugin).Options.R\ path :help vimrplugin_r_path<CR>
    amenu R.Help\ (plugin).Options.Arguments\ to\ R :help vimrplugin_r_args<CR>
    amenu R.Help\ (plugin).Options.Omni\ completion\ when\ R\ not\ running :help vimrplugin_start_libs<CR>
    amenu R.Help\ (plugin).Options.Syntax\ highlighting\ of\ \.Rout\ files :help vimrplugin_routmorecolors<CR>
    amenu R.Help\ (plugin).Options.Automatically\ open\ the\ \.Rout\ file :help vimrplugin_routnotab<CR>
    amenu R.Help\ (plugin).Options.Special\ R\ functions :help vimrplugin_listmethods<CR>
    amenu R.Help\ (plugin).Options.Indent\ commented\ lines :help vimrplugin_indent_commented<CR>
    amenu R.Help\ (plugin).Options.LaTeX\ command :help vimrplugin_latexcmd<CR>
    amenu R.Help\ (plugin).Options.Never\ unmake\ the\ R\ menu :help vimrplugin_never_unmake_menu<CR>

    amenu R.Help\ (plugin).Custom\ key\ bindings :help r-plugin-key-bindings<CR>
    amenu R.Help\ (plugin).Files :help r-plugin-files<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.All\ tips :help r-plugin-tips<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Indenting\ setup :help r-plugin-indenting<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Folding\ setup :help r-plugin-folding<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Remap\ LocalLeader :help r-plugin-localleader<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Customize\ key\ bindings :help r-plugin-bindings<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.ShowMarks :help r-plugin-showmarks<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.SnipMate :help r-plugin-snippets<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.LaTeX-Box :help r-plugin-latex-box<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Highlight\ marks :help r-plugin-showmarks<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Global\ plugin :help r-plugin-global<CR>
    amenu R.Help\ (plugin).FAQ\ and\ tips.Jump\ to\ function\ definitions :help r-plugin-tagsfile<CR>
    amenu R.Help\ (plugin).News :help r-plugin-news<CR>

    amenu R.Help\ (R)<Tab>:Rhelp :call g:SendCmdToR("help.start()")<CR>
    let g:rplugin_hasmenu = 1

    "----------------------------------------------------------------------------
    " ToolBar
    "----------------------------------------------------------------------------
    if g:rplugin_has_icons
        " Buttons
        amenu <silent> ToolBar.RStart :call StartR("R")<CR>
        amenu <silent> ToolBar.RClose :call RQuit('no')<CR>
        "---------------------------
        if &filetype == "r" || g:vimrplugin_never_unmake_menu
            nmenu <silent> ToolBar.RSendFile :call SendFileToR()<CR>
            imenu <silent> ToolBar.RSendFile <Esc>:call SendFileToR()<CR>
            let g:rplugin_hasRSFbutton = 1
        endif
        nmenu <silent> ToolBar.RSendBlock :call SendMBlockToR("down")<CR>
        imenu <silent> ToolBar.RSendBlock <Esc>:call SendMBlockToR("down")<CR>
        nmenu <silent> ToolBar.RSendFunction :call SendFunctionToR("down")<CR>
        imenu <silent> ToolBar.RSendFunction <Esc>:call SendFunctionToR("down")<CR>
        vmenu <silent> ToolBar.RSendSelection <ESC>:call SendSelectionToR("down")<CR>
        nmenu <silent> ToolBar.RSendParagraph :call SendParagraphToR("down")<CR>
        imenu <silent> ToolBar.RSendParagraph <Esc>:call SendParagraphToR("down")<CR>
        nmenu <silent> ToolBar.RSendLine :call SendLineToR("down")<CR>
        imenu <silent> ToolBar.RSendLine <Esc>:call SendLineToR("down")<CR>
        "---------------------------
        nmenu <silent> ToolBar.RListSpace :call g:SendCmdToR("ls()")<CR>
        imenu <silent> ToolBar.RListSpace <Esc>:call g:SendCmdToR("ls()")<CR>
        nmenu <silent> ToolBar.RClear :call RClearConsole()<CR>
        imenu <silent> ToolBar.RClear <Esc>:call RClearConsole()<CR>
        nmenu <silent> ToolBar.RClearAll :call RClearAll()<CR>
        imenu <silent> ToolBar.RClearAll <Esc>:call RClearAll()<CR>

        " Hints
        tmenu ToolBar.RStart Start R (default)
        tmenu ToolBar.RClose Close R (no save)
        if &filetype == "r" || g:vimrplugin_never_unmake_menu
            tmenu ToolBar.RSendFile Send file (echo)
        endif
        tmenu ToolBar.RSendBlock Send block (cur, echo and down)
        tmenu ToolBar.RSendFunction Send function (cur, echo and down)
        tmenu ToolBar.RSendSelection Send selection (cur, echo and down)
        tmenu ToolBar.RSendParagraph Send paragraph (cur, echo and down)
        tmenu ToolBar.RSendLine Send line (cur and down)
        tmenu ToolBar.RListSpace List objects
        tmenu ToolBar.RClear Clear the console screen
        tmenu ToolBar.RClearAll Remove objects from workspace and clear the console screen
        let g:rplugin_hasbuttons = 1
    else
        let g:rplugin_hasbuttons = 0
    endif
endfunction

function UnMakeRMenu()
    if g:rplugin_hasmenu == 0 || g:vimrplugin_never_unmake_menu == 1 || &previewwindow || (&buftype == "nofile" && &filetype != "rbrowser")
        return
    endif
    aunmenu R
    let g:rplugin_hasmenu = 0
    if g:rplugin_hasbuttons
        aunmenu ToolBar.RClearAll
        aunmenu ToolBar.RClear
        aunmenu ToolBar.RListSpace
        aunmenu ToolBar.RSendLine
        aunmenu ToolBar.RSendSelection
        aunmenu ToolBar.RSendParagraph
        aunmenu ToolBar.RSendFunction
        aunmenu ToolBar.RSendBlock
        if g:rplugin_hasRSFbutton
            aunmenu ToolBar.RSendFile
            let g:rplugin_hasRSFbutton = 0
        endif
        aunmenu ToolBar.RClose
        aunmenu ToolBar.RStart
        let g:rplugin_hasbuttons = 0
    endif
endfunction

