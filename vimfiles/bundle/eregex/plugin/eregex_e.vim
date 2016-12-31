" $Id: eregex_e.vim,v 1.40 2003-06-03 18:25:59+09 ta Exp ta $
" An evaluater for eregex.vim
if exists('g:eregex_evaluater_cmd') && exists('g:eregex_evaluater_how_exe')
    if g:eregex_evaluater_how_exe==0
    " :s silently exec, handle errmsg
        silent! exec g:eregex_evaluater_cmd
    elseif g:eregex_evaluater_how_exe==1
    " :s invoked by :g         and 
    " :s with confirm option
        exec g:eregex_evaluater_cmd
    elseif g:eregex_evaluater_how_exe==2
    ":g
        redraw
        exec g:eregex_evaluater_cmd
    endif
endif

