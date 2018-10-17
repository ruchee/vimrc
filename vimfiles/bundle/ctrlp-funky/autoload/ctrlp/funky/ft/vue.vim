" Language: Vuejs
" Author: timfeirg
" License: The MIT License

function! ctrlp#funky#ft#vue#filters()
  return ctrlp#funky#ft#javascript#filters()
endfunction

function! ctrlp#funky#ft#vue#post_extract_hook(list)
  return ctrlp#funky#ft#javascript#post_extract_hook(a:list)
endfunction
