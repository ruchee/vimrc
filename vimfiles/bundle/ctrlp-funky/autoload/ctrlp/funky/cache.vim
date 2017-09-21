" File: autoload/ctrlp/funky/cache.vim
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License
" Copyright (c) 2014,2015 Takahiro Yoshihara

" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:

" The above copyright notice and this permission notice shall be included in all
" copies or substantial portions of the Software.

" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
" SOFTWARE.

if get(g:, 'loaded_ctrlp_funky_cache', 0)
  finish
endif
let g:loaded_ctrlp_funky_cache = 1

let s:saved_cpo = &cpo
set cpo&vim

let s:li = ctrlp#funky#literals#new()
let s:fu = ctrlp#funky#utils#new()
" private: {{{
function! s:ftime(bufnr)
  return getftime(s:fu.fname(a:bufnr))
endfunction

function! s:fsize(bufnr)
  return getfsize(s:fu.fname(a:bufnr))
endfunction

function! s:timesize(bufnr)
  return string(s:ftime(a:bufnr)) . string(s:fsize(a:bufnr))
endfunction

function! s:convsp(name)
  return substitute(a:name, '[\/:]', '%', 'g')
endfunction

function! s:replace_bufnr_in_cache(cache, bufnr)
  let lines = []
  for l in a:cache
    call add(lines, substitute(l, s:li.pat_meta_for_bufnr(), a:bufnr, ''))
  endfor
  return lines
endfunction
" }}}

" s:cache {{{
let s:cache = {}
let s:cache.list = {}
let s:cache.dirname = ''

let s:use_cache = get(g:, 'ctrlp_funky_use_cache', 0)
function! s:cache.is_enabled()
  return s:use_cache
endfunction

function! s:cache.filename(fname)
  return s:fu.build_path(self.dirname, s:convsp(a:fname))
endfunction

function! s:cache.mkdir_if_missing()
  let dir = self.dirname
  if !isdirectory(dir)
    try
      call mkdir(dir, 'p')
    catch /^Vim\%((\a\+)\)\=:E739/
      echoerr 'ERR: cannot create a directory - ' . dir
      finish
    endtry
  endif
endfunction

function! s:cache.save(bufnr, defs)
  call self.mkdir_if_missing()
  let h = s:timesize(a:bufnr)
  let fname = s:fu.fname(a:bufnr)
  " save function defs
  let self.list[fname] = extend([h], a:defs)
  call writefile(self.list[fname], self.filename(fname))
endfunction

function! s:cache.load(bufnr)
  call self.read(a:bufnr)
  " first line is hash value
  return s:replace_bufnr_in_cache(self.list[s:fu.fname(a:bufnr)][1:-1], a:bufnr)
endfunction

function! s:cache.read(bufnr)
  call self.mkdir_if_missing()
  let fname = s:fu.fname(a:bufnr)
  let cache_file = self.filename(fname)
  if empty(get(self.list, fname, {}))
    let self.list[fname] = []
    if filereadable(cache_file)
      let self.list[fname] = readfile(cache_file)
    endif
  endif
endfunction

" clear cache for just a file
function! s:cache.clear(path)
  " accurate?
  let bufnr = bufnr(a:path)
  let fname = s:fu.fname(bufnr)
  let cache = self.filename(a:path)
  call self.delete(cache)
  let self.list[fname] = []
endfunction

" delete all cache files
function! s:cache.clear_all()
  for f in split(glob(self.dirname . '/*'), '\n')
    call self.delete(f)
  endfor
  let self.list = {}
endfunction

function! s:cache.delete(cache)
  " files needn't to be readable for deletion
  if !empty(glob(a:cache))
    if delete(a:cache)
      echoerr 'ERR: cannot delete a cache file - ' . a:cache
    endif
  endif
endfunction

function! s:cache.timesize(bufnr)
  call self.read(a:bufnr)
  let fname = s:fu.fname(a:bufnr)
  return get(get(self.list, fname, []), 0, '')
endfunction

function! s:cache.is_maybe_unchanged(bufnr)
  if !s:fu.is_real_file(a:bufnr) | return 0 | endif
  let prev = self.timesize(a:bufnr)
  let cur = s:timesize(a:bufnr)
  call s:fu.debug(prev . ' = ' . cur . ': ' . (prev == cur ? 'unchanged' : 'changed'))
  return prev == cur
endfunction
" }}}

function! ctrlp#funky#cache#new(dir)
  if empty(a:dir)
    echoerr 'cache dir must be specified!!'
    return ''
  endif
  let s:cache.dirname = a:dir
  return s:cache
endfunction

let &cpo = s:saved_cpo
unlet s:saved_cpo
