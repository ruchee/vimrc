if !get(g:, 'vim_rescript_enabled', 1)
    finish
endif

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

call rescript#Init()

" On every *.res / *.resi file open, recalculate the project environment
" This helps us to always make sure that we are working off the right
" working directory etc
augroup RescriptAutoProjectEnv
  au!
  au BufReadPost,BufNewFile *.res,*.resi call rescript#UpdateProjectEnv()
augroup END
