"=============================================================================
" File: tig_explorer.vim
" Author: iberianpig
" Created: 2017-04-03
"=============================================================================

scriptencoding utf-8

if exists('g:loaded_tig_explorer')
    finish
endif
let g:loaded_tig_explorer = 1

let s:save_cpo = &cpoptions
set cpoptions&vim

command! -nargs=? Tig
      \  call tig_explorer#open(<q-args>)

command! TigOpenCurrentFile
      \  call tig_explorer#open_current_file()

command! TigOpenProjectRootDir
      \  call tig_explorer#open_project_root_dir()

command! -nargs=? TigGrep
      \  call tig_explorer#grep(<q-args>)

command! TigBlame
      \  call tig_explorer#blame()

command! TigGrepResume
      \  call tig_explorer#grep_resume()

command! TigStatus
      \  call tig_explorer#status()

command! -bang -nargs=* TigOpenFileWithCommit
      \ call tig_explorer#open_file_with_commit("<bang>",<q-mods>,<f-args>)

let &cpoptions = s:save_cpo
unlet s:save_cpo

