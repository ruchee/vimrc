" Language: JavaScript (javascript)
" Author: curist
" License: The MIT License

function! ctrlp#funky#ft#javascript#filters()
  let filters = [
        \ { 'pattern': '\v\S+\s+\=\s+(\(?\S+.*\)?|\(\))\s+\=\>',
        \   'formatter': ['\v(^\s*(const|let|var)\s*)|(\s*\{.*\ze \t#)', '', 'g'] },
        \ { 'pattern': '\v\w+\s*\(.*\)\s*\{',
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] },
        \ { 'pattern': '\v\s*function\s+\w+\s*\(',
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] },
        \ { 'pattern': '\v\w.+\:\s*function\s*\(',
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] },
        \ { 'pattern': '\v\C\w.+\s*\=\s*function\s*\(',
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] },
        \ { 'pattern': '\v\s*function(\*\s+|\s+\*\s+|\s+\*)\w+\(',
        \   'formatter': ['\v(^\s*)|(\s*\{.*\ze \t#)', '', 'g'] }
  \ ]
  return filters
endfunction

function! ctrlp#funky#ft#javascript#post_extract_hook(list)
  " wtf??
  let l = filter(copy(a:list), "v:val !~# '^[\\t ]*\\(\\(else\\)\\?[\\t ]*if\\|switch[\\t ]*\\)'")
  return filter(l, "v:val !~# '^[\\t ]*for[\\t ]\\+('")
endfunction
