function s:DetectRed()
  if did_filetype()
    return
  end
  if getline(1) =~# '^#!.*\<red\>'
    setf red
  endif
endfunction

au BufRead,BufNewFile *.red setf red
au BufRead,BufNewFile * call s:DetectRed()
