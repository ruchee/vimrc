"=============================================================================
" Name Of File: csExplorer.vim
"  Description: Color Scheme Explorer Vim Plugin
"   Maintainer: Jeff Lanzarotta (delux256-vim at yahoo dot com)
" Last Changed: Thursday, 09 June 2005
"      Version: 7.0.1
"        Usage: Normally, this file should reside in the plugins
"               directory and be automatically sourced. If not, you must
"               manually source this file using ':source csExplorer.vim'.
"
"               You may use the default command of
"
"                 ":ColorSchemeExplorer" - Opens ColorSchemeExplorer
"
"               For more help see supplied documentation.
"      History: See supplied documentation.
"=============================================================================

" Define function once only
if exists('loaded_csExplorer') || &cp
  finish
endif

let loaded_csExplorer = 1

" Create commands
if !exists(":ColorSchemeExplorer")
  command ColorSchemeExplorer :call <SID>ColorSchemeExplorer()
endif

" ColorSchemeExplorer {{{1
function! <SID>ColorSchemeExplorer()
  let s:color_file_list = globpath(&runtimepath, 'colors/*.vim')
  let s:color_file_list = substitute(s:color_file_list, '\', '/', 'g')

  exe "silent bot ".10."new "."Color Explorer"

  setlocal bufhidden=delete
  setlocal buftype=nofile
  setlocal modifiable
  setlocal noswapfile
  setlocal nowrap

  map <buffer> <silent> ? :call <SID>ToggleHelp()<cr>
  map <buffer> <silent> <cr> :call <SID>SelectScheme()<cr>
  map <buffer> <silent> <2-leftmouse> :call <SID>SelectScheme(0)<cr>
  map <buffer> <silent> q :bd!<cr>

  silent! put! =s:color_file_list

  unlet! s:color_file_list

  setlocal nomodifiable
endfunction

" SelectScheme {{{1
function! <SID>SelectScheme()
  " Are we on a line with a file name?
  if strlen(getline('.')) == 0
    return
  endif

  call <SID>Reset()

  execute "source" getline('.')
endfunction

" Reset {{{1
function! <SID>Reset()
  hi clear Normal
  set bg&

  " Remove all existing highlighting and set the defaults.
  hi clear

  " Load the syntax highlighting defaults, if it's enabled.
  if exists("syntax_on")
    syntax reset
  endif
endfunction

" ToggleHelp {{{1
function! <SID>ToggleHelp()
  " Save position
  normal! mZ

  let header = "\" Press ? for Help\n"
  silent! put! =header

  " Jump back where we came from if possible.
  0
  if line("'Z") != 0
    normal! `Z
  endif
endfunction

"----------------------------------------------------------"
"call <SID>ColorSchemeExplorer()

" vim:ft=vim foldmethod=marker
