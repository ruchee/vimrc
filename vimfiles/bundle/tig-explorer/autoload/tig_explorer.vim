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
  :call s:exec_tig_command(s:strip_commit(a:str))
endfunction

function! tig_explorer#open_current_file() abort
  let current_path = expand('%:p')
  :call tig_explorer#open(current_path)
endfunction

function! tig_explorer#open_project_root_dir() abort
  try
    let root_dir = s:project_root_dir()
  catch
    echoerr 'tig-explorer.vim: ' . v:exception
    return
  endtry
  :call tig_explorer#open(root_dir)
endfunction

function! tig_explorer#grep(str) abort
  if a:str ==# ''
    let word = s:input('Pattern: ')
  else
    let word = a:str
  endif

  " if canceled
  if word ==# '0'
    return
  elseif word ==# '-1'
    return
  endif

  let g:tig_explorer_last_grep_keyword = word

  " NOTE: Escape shellwords
  if !get(g:, 'tig_explorer_use_builtin_term', has('terminal'))
    let args = s:shellwords(word)
    let escaped_word = ''

    for arg in args
      let escaped_word = join([escaped_word, shellescape(arg, 1)], ' ')
    endfor
    let word = escaped_word
  endif

  :call s:exec_tig_command('grep ' . word)
endfunction

function! tig_explorer#grep_resume() abort
  let keyword = get(g:, 'tig_explorer_last_grep_keyword', '')
  :call tig_explorer#grep(keyword)
endfunction

function! tig_explorer#blame() abort
  " extract the current commit if a path as the shape commit:file
  " which happend when using TigOpenWithCommit
  let parts = split(expand('%'), ':')
  if len(parts) == 2
    let commit = parts[0]
    let file = parts[1]
    call s:exec_tig_command('blame ' . commit .' +' . line('.') . ' -- '. file)
  else
    let root_dir = fnamemodify(s:project_root_dir(), ':p')
    let file = substitute(expand('%:p'), root_dir, "./", "")
    call s:exec_tig_command('blame +' . line('.') . ' ' . file)
  endif
endfunction

function! tig_explorer#status() abort
  call s:exec_tig_command('status')
endfunction

" Open a file for the given commit
" Usefull when editing file from tree or blame view
function! tig_explorer#open_file_with_commit(diff, mods, commit, file, lineno)
  let commit = get(a:, 'commit', 'HEAD')
  let file = get(a:, 'file', '')
  let lineno = get(a:, 'lineno', 0)

  let file0 = ''
  " if no file is provided use the current one
  if file == ''
    let file0 = expand('%')
    let diff = 1
  else
    let file0 = expand(file)
  endif
  " split commit file if needed
  echomsg file0
  let parts = split(file0, ':')
  if len(parts) == 2
    let commit = substitute(commit, '%',  parts[0],'' )
    let file = parts[1]
  else
    let file = parts[0]
    let commit = substitute(commit, '%', 'HEAD','')
  endif
  if a:diff == '!'
    diffthis
  endif
  let ref = commit.":".file
  echomsg ref
  if bufexists(ref)
    if a:diff == '!'
      execute a:mods "edit" ref
    else
      execute a:mods "split" ref
    endif
  else
    let ftype=&filetype
    if a:diff == '!'
      execute a:mods "new"
    else
      execute a:mods "enew"
    endif
    execute "file" ref
    execute "r !git show ".ref
    let &filetype=ftype
    setlocal nomodified
    setlocal nomodifiable
    setlocal readonly
    execute "+" lineno
  endif
  if a:diff=='!'
    diffthis
  endif
endfunction



" Private

function! s:tig_available() abort
  if !executable('tig')
    echoerr 'You need to install tig.'
    return 0
  endif
  return 1
endfunction

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
  if !result
    echomsg  'tig-explorer.vim: tigrc is not found'
    let s:orig_tigrc = tempname() "workaround
    exec 'silent ! touch ' . s:orig_tigrc
  endif

  let s:tmp_tigrc = tempname()
  let s:path_file = tempname()

  let s:keymap_edit_e  = get(g:, 'tig_explorer_keymap_edit_e',  'e')
  let s:keymap_edit    = get(g:, 'tig_explorer_keymap_edit',    '<C-o>')
  let s:keymap_tabedit = get(g:, 'tig_explorer_keymap_tabedit', '<C-t>')
  let s:keymap_split   = get(g:, 'tig_explorer_keymap_split',   '<C-s>')
  let s:keymap_vsplit  = get(g:, 'tig_explorer_keymap_vsplit',  '<C-v>')

  let s:keymap_commit_edit    = get(g:, 'tig_explorer_keymap_commit_edit',    '<ESC>o')
  let s:keymap_commit_tabedit = get(g:, 'tig_explorer_keymap_commit_tabedit', '<ESC>t')
  let s:keymap_commit_split   = get(g:, 'tig_explorer_keymap_commit_split',   '<ESC>s')
  let s:keymap_commit_vsplit  = get(g:, 'tig_explorer_keymap_commit_vsplit',  '<ESC>v')


  let s:before_exec_tig  = s:plugin_root . '/script/setup_tmp_tigrc.sh'
        \ . ' ' . s:orig_tigrc
        \ . ' ' . s:tmp_tigrc
        \ . ' ' . s:path_file
        \ . ' "' . s:keymap_edit_e  . '"'
        \ . ' "' . s:keymap_edit    . '"'
        \ . ' "' . s:keymap_tabedit . '"'
        \ . ' "' . s:keymap_split   . '"'
        \ . ' "' . s:keymap_vsplit  . '"'
        \ . ' "' . s:keymap_commit_edit    . '"'
        \ . ' "' . s:keymap_commit_tabedit . '"'
        \ . ' "' . s:keymap_commit_split   . '"'
        \ . ' "' . s:keymap_commit_vsplit  . '"'

  let s:tig_prefix = 'TIGRC_USER=' . s:tmp_tigrc . ' '
endfunction

function! s:tig_callback(exit_code) abort
  if a:exit_code == 0
    if has('nvim')
      silent! Bclose!
    else
      let current_buf = bufnr('%')
      silent! buffer #
      " NOTE: Prevent to quit vim
      if winnr('$') == 1 && bufnr('%') ==# current_buf
        enew
      endif
    endif
  endif

  try
    call s:open_file()
  endtry
endfunction

function! s:exec_tig_command(tig_args) abort
  if !s:tig_available()
    return
  endif

  let current_dir = getcwd()
  try
    let root_dir = s:project_root_dir()
  catch
    echoerr 'tig-explorer.vim: ' . v:exception
    return
  endtry
  " NOTE: It MUST execute tig command from project root
  " TigBlame or Edit are broken if execute from a relative path
  execute 'lcd ' . fnamemodify(root_dir, ':p')

  let command = s:tig_prefix  . 'tig' . ' ' . a:tig_args
  exec 'silent !' . s:before_exec_tig
  if has('nvim')
    enew
    call termopen(command, {
          \ 'name': 'tig',
          \ 'on_exit': {job_id, code, event -> s:tig_callback(code)},
          \ })
    startinsert
  elseif get(g:, 'tig_explorer_use_builtin_term', has('terminal'))
    call term_start('env ' . command, {
         \ 'term_name': 'tig',
         \ 'curwin': v:true,
         \ 'term_finish': 'close',
         \ 'exit_cb': {status, code -> s:tig_callback(code)},
         \ })
  else
    exec 'silent !' . command
    call s:open_file()
  endif
  " NOTE: Back to current_dir
  execute 'lcd ' . fnamemodify(current_dir, ':p')
  redraw!
endfunction

function! s:open_file() abort
  if !filereadable(s:path_file)
    return
  endif

  let current_dir = getcwd()
  try
    execute 'lcd ' . fnamemodify(s:project_root_dir(), ':p')
    for f in readfile(s:path_file)
      exec f
    endfor
  finally
    call delete(s:path_file)
    execute 'lcd ' . fnamemodify(current_dir, ':p')
  endtry
endfunction

function! s:project_root_dir() abort
  let current_file_dir = expand('%:p:h')
  let git_dir = findfile('.git', expand('%:p:h') . ';')
  if git_dir ==# ''
    let git_dir = finddir('.git', expand('%:p:h') . ';')
  endif

  if git_dir ==# ''
    throw 'Not a git repository: ' . expand('%:p:h')
  endif

  let root_dir = fnamemodify(git_dir, ':h')

  if !isdirectory(root_dir)
    return current_file_dir
  endif
  return root_dir
endfunction

function! s:shellwords(str) abort "make list by splitting the string by whitespace
  let words = split(a:str, '\%(\([^ \t\''"]\+\)\|''\([^\'']*\)''\|"\(\%([^\"\\]\|\\.\)*\)"\)\zs\s*\ze')
  let words = map(words, 'substitute(v:val, ''\\\([\\ ]\)'', ''\1'', "g")')
  let words = map(words, 'matchstr(v:val, ''^\%\("\zs\(.*\)\ze"\|''''\zs\(.*\)\ze''''\|.*\)$'')')
  return words
endfunction


" return 0 (<ESC>) or -1 (<Ctrl-c>)
function! s:input(...) abort
  new
  cnoremap <buffer> <silent> <Esc> __CANCELED__<CR>
  try
    let input = call('input', a:000)
    let input = input =~# '__CANCELED__$' ? 0 : input
  catch /^Vim:Interrupt$/
    let input = -1
  finally
    bwipeout!
    redraw!
    return input
  endtry
endfunction

function! s:strip_commit(path)
  return substitute(a:path, '^[^:]*:','','')
endfunction
" Initialize

" NOTE: '<sfile>' must be called top level
let s:plugin_root=expand('<sfile>:p:h:h')

call s:initialize()

let &cpoptions = s:save_cpo
unlet s:save_cpo


