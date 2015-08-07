" Vim syntax file
" Language:     Brainfuck
" Maintainer:   Mathias Panzenböck <grosser.meister.morti@gmx.net>
" Version:      1.0.3
" Last Change:  2014 Oct 11
" URL:          http://www.vim.org/scripts/script.php?script_id=716
" License:      2-clause BSD license

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'brainfuck'
endif

syn match bfMath    "[-+]"
syn match bfPointer "[<>]"
syn match bfLoop    "[[\]]"
syn match bfIO      "[.,]"
syn match bfComment "[^-+<>[\].,]\+"

if version >= 508 || !exists("did_brainfuck_syn_inits")
  if version < 508
    let did_brainfuck_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink bfMath    Identifier
  HiLink bfPointer Type
  HiLink bfLoop    Conditional
  HiLink bfIO      PreProc
  HiLink bfComment Comment

  delcommand HiLink
endif

let b:current_syntax = "brainfuck"

if main_syntax == 'brainfuck'
  unlet main_syntax
endif

" vim: ts=8
