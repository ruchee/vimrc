" Language: make
" Author: yalin.wang
" License: The MIT License

"support make file target parse
"support format:
"A:B  or  $(A):B or A $(B) : B
function! ctrlp#funky#ft#make#filters()
  let filters = [
        \ { 'pattern': '\m^[^:\t#@''"]\+:=\@![^:\t]*\ze\n\|^\s*define\>',
        \   'formatter': [] }
  \ ]
  return filters
endfunction

function! ctrlp#funky#ft#make#post_extract_hook(list)
  let index = 0
  for i in a:list
    let i = substitute(i, '\m:=\@!.*$', '', 'g')
    let L = substitute(i, '[^(]', '', 'g')
    let R = substitute(i, '[^)]', '', 'g')


    if len(L) != len(R) || i =~# '^\.PHONY'
      call remove(a:list, index)
    else
      let index += 1
    endif
  endfor
  return a:list
endfunction
