" MIT License. Copyright (c) 2019-2021 Peng Guanwen et al.
" vim: et ts=2 sts=2 sw=2
" Plugin: https://github.com/neoclide/coc

scriptencoding utf-8

let s:show_coc_status = get(g:, 'airline#extensions#coc#show_coc_status', 1)

function! airline#extensions#coc#get_warning() abort
  return airline#extensions#coc#get('warning')
endfunction

function! airline#extensions#coc#get_error() abort
  return airline#extensions#coc#get('error')
endfunction

function! airline#extensions#coc#get(type) abort
  if !exists(':CocCommand') | return '' | endif

  let is_err = (a:type  is# 'error')
  let info = get(b:, 'coc_diagnostic_info', {})
  if empty(info) | return '' | endif

  let cnt = get(info, a:type, 0)
  if empty(cnt) | return '' | endif

  let error_symbol = get(g:, 'airline#extensions#coc#error_symbol', 'E:')
  let warning_symbol = get(g:, 'airline#extensions#coc#warning_symbol', 'W:')
  let error_format = get(g:, 'airline#extensions#coc#stl_format_err', '%C(L%L)')
  let warning_format = get(g:, 'airline#extensions#coc#stl_format_warn', '%C(L%L)')

  " replace %C with error count and %L with line number
  return (is_err ? error_symbol : warning_symbol) .
    \ substitute(substitute(is_err ? error_format : warning_format,
      \ '%C', cnt, 'g'),
      \ '%L', (info.lnums)[is_err ? 0 : 1], 'g')
endfunction

function! airline#extensions#coc#get_status() abort
  " Shorten text for windows < 91 characters
  let status = airline#util#shorten(get(g:, 'coc_status', ''), 91, 9)
  return (s:show_coc_status ? status : '')
endfunction

function! airline#extensions#coc#get_current_function() abort
  return get(b:, 'coc_current_function', '')
endfunction

function! airline#extensions#coc#init(ext) abort
  call airline#parts#define_function('coc_error_count', 'airline#extensions#coc#get_error')
  call airline#parts#define_function('coc_warning_count', 'airline#extensions#coc#get_warning')
  call airline#parts#define_function('coc_status', 'airline#extensions#coc#get_status')
  call airline#parts#define_function('coc_current_function', 'airline#extensions#coc#get_current_function')
endfunction
