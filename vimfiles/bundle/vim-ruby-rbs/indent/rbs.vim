if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentkeys+==end,0|
setlocal indentexpr=RubySignatureIndent(v:lnum)

function! RubySignatureIndent(lnum) abort
  let prev_lnum = prevnonblank(a:lnum-1)
  if prev_lnum == 0
    " top of file
    return 0
  endif

  let prev_line = substitute(getline(prev_lnum), '#.*$', '', '')
  let this_line = substitute(getline(a:lnum), '#.*$', '', '')
  let indent = indent(prev_lnum)

  if this_line =~ '\v^\s*\|'
    let i = stridx(prev_line, '|')
    if i != -1
      return i
    endif
    let i = stridx(prev_line, ':')
    if i != -1
      return i
    endif
  endif

  while prev_line =~ '\v^\s*\|' && prev_lnum > 0
    let prev_lnum -= 1
    let prev_line = getline(prev_lnum)
    let indent = indent(prev_lnum)
  endwhile

  if prev_line =~ '\v<(class|module|interface)>'
    let indent += shiftwidth()
  endif

  if this_line =~ 'end\s*$'
    let indent -= shiftwidth()
  endif


  return indent
endfunction
