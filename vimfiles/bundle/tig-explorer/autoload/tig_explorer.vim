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

let s:save_cpo = &cpoptions
set cpoptions&vim

" Public 

function! tig_explorer#open(str) abort
  :call s:exec_tig_command(a:str)
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
  if a:str ==# ''
    let word = input('Pattern: ')
  else
    let word = a:str
  endif

  let g:tig_explorer_last_grep_keyword = word

  let args = s:shellwords(word)
  let escaped_word = ''

  for arg in args
    let escaped_word = join([escaped_word, shellescape(arg, 1)], ' ')
  endfor

  :call s:exec_tig_command('grep ' . escaped_word)
endfunction

function! tig_explorer#grep_resume() abort
  let keyword = get(g:, 'tig_explorer_last_grep_keyword', '')
  :call tig_explorer#grep(keyword)
endfunction

function! tig_explorer#blame() abort
  call s:exec_tig_command('blame +' . line('.') . ' ' . expand('%:p'))
endfunction


" Private 

function! s:initialize() abort
  function! s:set_orig_tigrc(path) abort
    if filereadable(expand(a:path))
      let s:orig_tigrc=a:path
      return 1 "true
    endif
    return 0 "fail
  endfunction

  if exists('g:tig_explorer_orig_tigrc')
    let result = s:set_orig_tigrc(g:tig_explorer_orig_tigrc)
  else
    let result = s:set_orig_tigrc('$XDG_CONFIG_HOME/tig/config') ||
          \ s:set_orig_tigrc('~/.config/tig/config') ||
          \ s:set_orig_tigrc('~/.tigrc') ||
          \ s:set_orig_tigrc('/etc/tigrc')
  endif
  if result
    echomsg('tig_explorer loaded tigrc from: ' . s:orig_tigrc)
  else
    echoerr 'tigrc is not found'
    let s:orig_tigrc = tempname() "workaround
  endif

  let s:tmp_tigrc = tempname()
  let s:path_file = tempname()

  let s:before_exec_tig  = s:plugin_root . '/script/setup_tmp_tigrc.sh'
        \ . ' ' . s:orig_tigrc
        \ . ' ' . s:tmp_tigrc
        \ . ' ' . s:path_file

  let s:tig_prefix = 'TIGRC_USER=' . s:tmp_tigrc . ' '
endfunction

function! s:exec_tig_command(tig_args) abort
  if !executable('tig')
    echoerr 'You need to install tig.'
    return
  endif

  let command = s:tig_prefix  . 'tig' . ' ' . a:tig_args
  exec 'silent !' . s:before_exec_tig
  if has('nvim')
    let tigCallback = { 'name': 'tig' }
    function! tigCallback.on_exit(job_id, code, event)
      if a:code == 0
        silent! Bclose!
      endif
      try
        call s:open_file()
      endtry
    endfunction
    enew
    call termopen(command, tigCallback)
    startinsert
  else
    echomsg command
    exec 'silent !' . command
    call s:open_file()
  endif
  redraw!
endfunction

function! s:open_file() abort
  if filereadable(s:path_file)
    for f in readfile(s:path_file)
      exec f
    endfor
    call delete(s:path_file)
  endif
endfunction

function! s:project_root_dir()
  let current_dir      = expand('%:p:h')
  let relative_git_dir = finddir('.git', current_dir . ';')
  let root_dir         = fnamemodify(relative_git_dir, ':h')
  if !isdirectory(root_dir)
    return current_dir
  endif
  return root_dir
endfunction

function! s:shellwords(str) abort "make list by splitting the string by whitespace
  let words = split(a:str, '\%(\([^ \t\''"]\+\)\|''\([^\'']*\)''\|"\(\%([^\"\\]\|\\.\)*\)"\)\zs\s*\ze')
  let words = map(words, 'substitute(v:val, ''\\\([\\ ]\)'', ''\1'', "g")')
  let words = map(words, 'matchstr(v:val, ''^\%\("\zs\(.*\)\ze"\|''''\zs\(.*\)\ze''''\|.*\)$'')')
  return words
endfunction

" Initialize

" NOTE: '<sfile>' must be called top level
let s:plugin_root=expand('<sfile>:p:h:h')

call s:initialize()

let &cpoptions = s:save_cpo
unlet s:save_cpo
