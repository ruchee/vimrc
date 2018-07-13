"=============================================================================
" File: tig_explorer.vim
" Author: iberianpig
" Created: 2017-04-03
"=============================================================================

scriptencoding utf-8

if !exists('g:loaded_tig_explorer')
  finish
endif
let g:loaded_tig_explorer = 1

let s:save_cpo = &cpo
set cpo&vim

let s:script_path = expand('<sfile>:p:h:h') . '/script/tig_vim.sh '

function! s:project_root_dir()
  let current_dir = expand('%:p:h')
  let relative_git_dir = finddir('.git', current_dir . ';') 
  let root_dir = fnamemodify(relative_git_dir, ':h')
  if !isdirectory(root_dir)
    return current_dir
  endif
  return root_dir
endfunction

function! tig_explorer#call(str) abort
  if !executable('tig')
    echo 'You need to install tig.'
    return
  endif
  exec 'silent !GIT_EDITOR=' . s:script_path . 'tig ' . a:str
  if filereadable('/tmp/vim_tig_current_file')
    exec 'edit ' . system('cat /tmp/vim_tig_current_file')
    call system('rm /tmp/vim_tig_current_file')
  endif
  redraw!
endfunction

function! tig_explorer#open(path)
  if !executable('tig')
    echo 'You need to install tig.'
    return
  endif
  exec 'silent !GIT_EDITOR=' . s:script_path . 'tig ' . a:path
  if filereadable('/tmp/vim_tig_current_file')
    exec 'edit ' . system('cat /tmp/vim_tig_current_file')
    call system('rm /tmp/vim_tig_current_file')
  endif
  redraw!
endfunction

function! tig_explorer#open_current_file() abort
  let current_path = expand('%:p')
  :call tig_explorer#open(current_path)
endfunction

function! tig_explorer#open_project_root_dir() abort
  let root_dir = s:project_root_dir()
  :call tig_explorer#open(root_dir)
endfunction

function! tig_explorer#grep(str) abort
  if a:str == ""
    let word = shellescape(input("Pattern: "), 1)
  else
    " escape special character
    let word = shellescape(a:str, 1)
  endif
  if !executable('tig')
    echo 'You need to install tig.'
    return
  endif
  exec 'silent !GIT_EDITOR=' . s:script_path . 'tig grep ' . word
  if filereadable('/tmp/vim_tig_current_file')
    exec 'edit ' . system('cat /tmp/vim_tig_current_file')
    call system('rm /tmp/vim_tig_current_file')
  endif
  redraw!
endfunction

function! tig_explorer#blame() abort
  if !executable('tig')
    echo 'You need to install tig.'
    return
  endif
  exec 'silent !GIT_EDITOR=' . s:script_path . 'tig blame +' . line('.') . ' ' . expand('%:p')
  if filereadable('/tmp/vim_tig_current_file')
    exec 'edit ' . system('cat /tmp/vim_tig_current_file')
    echomsg 'edit ' . system('cat /tmp/vim_tig_current_file')
    call system('rm /tmp/vim_tig_current_file')
  endif
  redraw!
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
