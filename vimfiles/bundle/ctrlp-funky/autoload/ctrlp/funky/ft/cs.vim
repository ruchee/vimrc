" Language: C# (cs)
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License
" This is based on Java filter by pydave
" ctrlp/funky/ft/java.vim

function! ctrlp#funky#ft#cs#filters()
  let regex = '\v^\s+'                " preamble
  let regex .= '%(<\w+>\s+){0,3}'     " visibility, static, final
  let regex .= '%(\w|[<>[\]])+\s+'    " return type
  let regex .= '\w+\s*'               " method name
  let regex .= '\(\_[^\)]*\)'         " method parameters

  let filters = [
        \ { 'pattern': regex,
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] }
  \ ]
  return filters
endfunction
