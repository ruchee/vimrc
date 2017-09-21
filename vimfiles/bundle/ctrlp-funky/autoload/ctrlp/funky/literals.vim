" File: autoload/ctrlp/funky/literals.vim
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License
" Copyright (c) 2015 Takahiro Yoshihara

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

if get(g:, 'loaded_ctrlp_funky_literals', 0)
  finish
endif
let g:loaded_ctrlp_funky_literals = 1

let s:save_cpo = &cpo
set cpo&vim

let s:li = {}

function! ctrlp#funky#literals#new()
  return deepcopy(s:li)
endfunction

function! s:li.pat_meta()
  return '\t#:\d\+:\d\+$'
endfunction

" To split
function! s:li.pat_meta_for_split()
  return '\ze\t#:\(\d\+\):\(\d\+\)$'
endfunction

" To replace / get bufnr
function! s:li.pat_meta_for_bufnr()
  return '\t#:\zs\(\d\+\)\ze:\(\d\+\)$'
endfunction

" To replace / get line number
function! s:li.pat_meta_for_linenr()
  return '\t#:\(\d\+\):\zs\(\d\+\)\ze$'
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
