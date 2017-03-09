command! -nargs=+ -complete=dir Wxgen call s:generate(<f-args>)

function! s:generate(...) abort
  if a:0 == 2
    call wxapp#generate(a:1, a:2)
  elseif a:0 == 1
    call wxapp#generate('.', a:1)
  endif
endfunction
