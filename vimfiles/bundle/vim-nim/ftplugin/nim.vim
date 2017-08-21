scriptencoding utf-8


setlocal iskeyword=a-z,A-Z,48-57,128-255,_
setlocal formatoptions-=t formatoptions+=l
setlocal comments=s1:#[,mb:#,ex:]#,:#,:##
setlocal commentstring=#\ %s
setlocal expandtab
setlocal omnifunc=omni#nim
setlocal makeprg=nim\ c\ --compileOnly\ --verbosity:0\ --colors:off\ %
setlocal errorformat=
            \%-GHint:\ %m,
            \%A%f(%l\\,\ %c)\ Hint:\ %m,
            \%E%f(%l\\,\ %c)\ Error:\ %m,
            \%W%f(%l\\,\ %c)\ Warning:\ %m

command! -buffer -nargs=* -complete=buffer NimDefinition          :call features#definition#run()
command! -buffer -nargs=* -complete=buffer NimInfo                :call features#info#run()
command! -buffer -nargs=* -complete=buffer NimWeb                 :call features#info#web()
command! -buffer -nargs=* -complete=buffer NimUsages              :call features#usages#run(0)
command! -buffer -nargs=* -complete=buffer NimUsagesProject       :call features#usages#run(1)
command! -buffer -nargs=* -complete=buffer NimRenameSymbol        :call features#rename#run(0)
command! -buffer -nargs=* -complete=buffer NimRenameSymbolProject :call features#rename#run(1)
command! -buffer -nargs=* -complete=buffer NimDebug               :call features#debug#run()
command! -buffer -nargs=* -complete=buffer NimOutline             :call features#outline#run(0)
command! -buffer -nargs=* -complete=buffer NimOutlineUpdate       :call features#outline#run(1)

command! -buffer -nargs=* -complete=buffer NimEdb                 :call features#debugger#run()
command! -buffer -nargs=* -complete=buffer NimEdbStop             :call features#debugger#stop()
command! -buffer -nargs=* -complete=buffer NimEdbContinue         :call features#debugger#continue()
command! -buffer -nargs=* -complete=buffer NimEdbStepInto         :call features#debugger#stepinto()
command! -buffer -nargs=* -complete=buffer NimEdbStepOver         :call features#debugger#stepover()
command! -buffer -nargs=* -complete=buffer NimEdbSkipCurrent      :call features#debugger#skipcurrent()
command! -buffer -nargs=* -complete=buffer NimEdbIgonore          :call features#debugger#ignore()
command! -buffer -nargs=* -complete=buffer NimEdbContinue         :call features#debugger#continue()
command! -buffer -nargs=* -complete=buffer NimEdbToggleBP         :call features#debugger#togglebp()

command! -buffer -nargs=* -complete=buffer NimREPL                :call features#repl#start()
command! -buffer -nargs=* -complete=buffer NimREPLEvalFile        :call features#repl#send(getline(0, line("$")))
command! -buffer -nargs=* -complete=buffer -range NimREPLEval     :call features#repl#send(getline(getpos("'<")[1], getpos("'>")[1]))

if g:nvim_nim_enable_default_binds == 1
    nnoremap <buffer> <c-]> :NimDefinition<cr>
    nnoremap <buffer> gf    :call util#goto_file()<cr>
    nnoremap <buffer> gd    :NimDefinition<cr>
    nnoremap <buffer> gt    :NimInfo<cr>
    nnoremap <buffer> gT    :NimWeb<cr>
    nnoremap <buffer> cr    :NimRenameSymbol<cr>
    nnoremap <buffer> cR    :NimRenameSymbolProject<cr>
endif

if g:nvim_nim_enable_custom_textobjects == 1
    onoremap <buffer> <silent>af :<C-U>call util#SelectNimProc(0)<cr>
    onoremap <buffer> <silent>if :<C-U>call util#SelectNimProc(1)<cr>
    vnoremap <buffer> <silent>af :<C-U>call util#SelectNimProc(0)<cr><Esc>gv
    vnoremap <buffer> <silent>if :<C-U>call util#SelectNimProc(1)<cr><Esc>gv
endif

augroup nvim_nim_highlighter
    autocmd! BufReadPost,BufWritePost,CursorHold,InsertLeave,TextChanged,InsertEnter *.nim call highlighter#guard()
augroup END

augroup nvim_nim_outline
    autocmd! FileWritePost *.nim call features#outline#run(1)
    autocmd! VimResized,WinEnter *.nim call features#outline#render()
augroup END

if g:nvim_nim_highlighter_enable
    call highlighter#guard()
endif
