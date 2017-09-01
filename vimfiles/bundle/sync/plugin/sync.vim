function! SyncGetExe()
  if '.sync' == expand('%')
    return
  endif
  let l:exe_path = expand('%:p:h')
  let l:exe_file = l:exe_path . '/.sync'
  let l:found_exe = ''
  if filereadable(l:exe_file)
    let l:found_exe = l:exe_file
  else
    while !filereadable(l:exe_file)
      let slashindex = strridx(l:exe_path, '/')
      if slashindex >= 0
        let l:exe_path = l:exe_path[0:slashindex]
        let l:exe_file = l:exe_path . '.sync'
        let l:exe_path = l:exe_path[0:slashindex-1]
        if filereadable(l:exe_file)
          let l:found_exe = l:exe_file
          break
        endif
        if slashindex == 0 && !filereadable(l:exe_file)
          break
        endif
      else
        break
      endif
    endwhile
  endif
  return l:found_exe
endfunction

function! SyncUploadFile()
  let exe = SyncGetExe()
  if !empty(exe)
    let dir = expand('%:p:h')
    let fold = substitute(dir, exe[0:strridx(exe, '/')], "", "")

    " For root dir files
    if dir == fold
      let fold = '.'
    endif

    let filelist = split(expand('%:p'), '/')
    let file = filelist[-1]
    let cmd = printf("%s %s %s %s", exe, 'upload', fold, shellescape(file))
    execute '!' . cmd
  endif
endfunction

function! SyncDownloadFile()
  let exe = SyncGetExe()
  if !empty(exe)
    let dir = expand('%:p:h')
    let fold = substitute(dir, exe[0:strridx(exe, '/')], "", "")

    " For root dir files
    if dir == fold
      let fold = '.'
    endif

    let filelist = split(expand('%:p'), '/')
    let file = filelist[-1]
    let cmd = printf("%s %s %s %s", exe, 'download', fold, shellescape(file))
    execute '!' . cmd
  endif
endfunction

nmap <leader>su :call SyncUploadFile()<CR>
nmap <leader>sd :call SyncDownloadFile()<CR>
