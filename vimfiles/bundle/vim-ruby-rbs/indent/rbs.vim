" Vim indent file
" Language: Ruby Signature (RBS) <github.com/ruby/rbs>
" Author: Jeffrey Crochet <jlcrochet91@pm.me>
" URL: https://github.com/jlcrochet/vim-rbs

if get(b:, "did_indent")
  finish
endif

let b:did_indent = 1

setlocal
      \ indentkeys=o,O,!^F,0<bar>,0=end
      \ indentexpr=GetRBSIndent()

if exists("*GetRBSIndent")
  finish
endif

function GetRBSIndent() abort
  let prev_lnum = prevnonblank(v:lnum - 1)

  if prev_lnum == 0
    return 0
  endif

  let line = getline(v:lnum)
  let prev_line = getline(prev_lnum)

  let first_idx = match(prev_line, '\S')

  if line =~# '^\s*|'
    if match(prev_line, '\C^def\>', first_idx) != -1
      let idx = stridx(prev_line, ":", first_idx + 4)

      while idx != -1
        if synID(prev_lnum, idx + 1, 0)->synIDattr("name") =~# '^rbs\%(DeclarationOperator\|MethodDeclarationOperator\)$'
          return idx
        endif

        let idx = stridx(prev_line, ":", idx + 1)
      endwhile

      return -1
    else
      return first_idx
    endif
  endif

  if prev_line[first_idx] ==# "|"
    let start_lnum = prevnonblank(prev_lnum - 1)

    while start_lnum
      let start_line = getline(start_lnum)
      let first_idx = match(start_line, '\S')

      if start_line[first_idx] !=# "|"
        break
      endif

      let start_lnum = prevnonblank(start_lnum - 1)
    endwhile
  else
    let start_lnum = prev_lnum
    let start_line = prev_line
  endif

  let shift = 0

  if line =~# '^\s*end\>'
    let shift -= 1
  endif

  if match(start_line, '\C^\%(class\|module\|interface\)\>', first_idx) != -1
    let shift += 1
  endif

  return first_idx + shift * shiftwidth()
endfunction
