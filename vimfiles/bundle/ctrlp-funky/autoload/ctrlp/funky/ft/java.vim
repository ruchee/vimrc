" Language: Java (java)
" Author: pydave
" License: The MIT License

function! ctrlp#funky#ft#java#filters()
  " Java has a mostly standard format and style, so assume we've got
  " everything on one line (excluding annotations).
  let regex = '\v^\s+'                " preamble
  let regex .= '%(<\w+>\s+){0,3}'     " visibility, static, final
  let regex .= '%(\w|[<>[\]])+\s+'    " return type
  let regex .= '\w+\s*'               " method name
  let regex .= '\(\_[^\)]*\)'         " method parameters
  let regex .= '%(\S|\s?|\{)+$'        " postamble

  let filters = [
        \ { 'pattern': regex,
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] }
  \ ]
  return filters
endfunction

function! ctrlp#funky#ft#java#post_extract_hook(list)
  return filter(copy(a:list), "v:val !~# '^[\\t ]*else[\\t ]\\+if'")
endfunction
