let s:config = vue#GetConfig('config', {})
let s:enable_foldexpr = s:config.foldexpr

if !s:enable_foldexpr | finish | endif
" set debug=msg

function! VueFoldMain(...)
  if line('$') < 1000
    let s:empty_line = '^\s*$'
    let s:vue_tag_start = '^<\w\+'
    let s:vue_tag_end = '^<\/\w\+'

    setlocal foldexpr=GetVueFold(v:lnum)
    setlocal foldmethod=expr
  endif
endfunction

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
  let value = s:FoldForSpecialLine(this_line)
  if value == -2
    " Fold by indent
    let this_indent = s:IndentLevel(a:lnum)

    " For <script> block
    if GetVueTag(a:lnum) == 'script'
      let value = s:FoldForScript(a:lnum, this_line, this_indent)
    else
      let value = this_indent
    endif
  endif
  call vue#LogWithLnum('foldlevel '.value)
  return value
endfunction

function! s:FoldForScript(lnum, this_line, this_indent)
  let value = -2
  if a:lnum > 1
    let prev_indent = s:IndentLevel(a:lnum - 1)
  else
    let prev_indent = 0
  endif
  let next_indent = s:IndentLevel(nextnonblank(a:lnum + 1))

  if a:this_line =~ '^\s*[]})]\+,\?\s*$'
    " Closing ']})'
    let value = '<'.prev_indent
  elseif a:this_indent < next_indent
    " --this
    " ----next
    let value = '>'.next_indent
  else
    " ----this
    " --next
    let value = a:this_indent
  endif

  return value
endfunction

function! s:FoldForSpecialLine(this_line)
  if a:this_line =~ s:empty_line
    return -1
  elseif a:this_line =~ s:vue_tag_start
    return '>1'
  elseif a:this_line =~ s:vue_tag_end
    " If return '<1', fold will get incorrect with prev line
    return 1
  else
    return -2
  endif
endfunction

function! s:IndentLevel(lnum)
  " Add 1 to indentLevel, so start/end tags can fold properly
  return indent(a:lnum) / &shiftwidth + 1
endfunction
"}}}

let s:test = exists('g:vim_vue_plugin_test')
let s:timer = exists('*timer_start') && !exists('SessionLoad') && !s:test
if s:timer
  call timer_start(300, 'VueFoldMain')
else
  call VueFoldMain()
endif
