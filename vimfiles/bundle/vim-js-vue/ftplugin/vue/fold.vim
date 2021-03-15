"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Config {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:use_foldexpr = vue#GetConfig("use_foldexpr", 0)
"}}}

if !s:use_foldexpr | finish | endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Settings {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if line('$') < 1000
  setlocal foldmethod=expr
endif
setlocal foldexpr=GetVueFold(v:lnum)
"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Variables {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:empty_line = '\v^\s*$'
let s:block_end = '\v^\s*}|]|\)'
let s:vue_tag_start = '\v^\s*\<(script|style|template)' 
let s:vue_tag_end = '\v^\s*\<\/(script|style|template)' 
"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Functions {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"  see :h fold-expr
"  value             meaning
"  0                 the line is not in a fold
"  1, 2, ..          the line is in a fold with this level
"  -1                the fold level is undefined, use the fold level of a
"                    line before or after this line, whichever is the
"                    lowest.
"  "="               use fold level from the previous line
"  "a1", "a2", ..    add one, two, .. to the fold level of the previous
"			               line, use the result for the current line
"  "s1", "s2", ..    subtract one, two, .. from the fold level of the
"  ">1", ">2", ..    a fold with this level starts at this line
"  "<1", "<2", ..    a fold with this level ends at this line
function! GetVueFold(lnum)
  let this_line = getline(a:lnum)
  let next_line = getline(a:lnum + 1)
  if a:lnum > 1
    let prev_line = getline(a:lnum - 1)
  endif

  " Handle empty lines
  if this_line =~ s:empty_line
    return -1
  endif

  " Handle start/end tags
  if this_line =~ s:vue_tag_start
    return '>1'
  endif
  if this_line =~ s:vue_tag_end
    " If return '<1', fold will get incorrect with prev line
    return 1
  endif

  " Fold by indent
  if a:lnum > 1
    let prev_indent = s:IndentLevel(a:lnum - 1)
  else
    let prev_indent = 0
  endif
  let this_indent = s:IndentLevel(a:lnum)
  let next_indent = s:IndentLevel(s:NextNonBlankLine(a:lnum))

  if GetVueTag(a:lnum) == 'script'
    " Handle closing '}'
    if this_line =~ '\v^\s*},?\s*$'
      return '<'.prev_indent
    endif

    " --this
    " ----next
    if this_indent < next_indent
      return '>'.next_indent
    endif

    " ----this
    " --next
    if this_indent >= next_indent 
      return this_indent
    endif
  else
    " Template or style
    return this_indent
  endif
endfunction

function! s:IndentLevel(lnum)
  " Add 1 to indentLevel, so start/end tags can fold properly
  return indent(a:lnum) / &shiftwidth + 1
endfunction

function! s:NextNonBlankLine(lnum)
  let next_line = a:lnum + 1
  let last_line = line('$')

  while next_line <= last_line
    if getline(next_line) =~ '\v\S'
      return next_line
    endif

    let next_line += 1
  endwhile

  return 0
endfunction
"}}}
" vim: fdm=marker
