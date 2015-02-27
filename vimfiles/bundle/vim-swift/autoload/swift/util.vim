" File: swift/util.vim
" Author: Kevin Ballard
" Description: Utilities for the Swift filetype plugin
" Last Change: Feb 11, 2015

" Returns 1 if vimproc is available
function! swift#util#has_vimproc()
  if !exists('*vimproc#version')
    try
      call vimproc#version()
    catch
    endtry
  endif
  return exists('*vimproc#version')
endfunction

" Returns 1 if unite is available
function! swift#util#has_unite()
    if !exists('*unite#version')
        try
            call unite#version()
        catch
        endtry
    endif
    return exists('*unite#version')
endfunction

" Usage:
"   swift#util#system({cmd}[, {pwd}])
"   swift#util#system({args}[, {pwd}])
" Arguments:
"   {cmd} - String - A command suitable for executing.
"   {args} - List - A list of command and arguments.
"   {pwd} - String - Optional - A directory to run the command from.
" Returns:
"   A dictionary with the following keys:
"     status - The exit status.
"     output - List - The combined stdout and stderr lines.
"     stderr - List - Optional - The stderr lines. Only present if vimproc is
"                                installed.
if swift#util#has_vimproc()
  function! swift#util#system(args, ...)
    if type(a:args) == type('')
      let l:args = vimproc#parser#split_args(a:args)
    else
      let l:args = a:args
    endif
    if a:0 > 0 && !empty(a:1)
      " vimproc can't handle `cd dir && cmd` because it's not a shell, so it's
      " probably invoking /usr/bin/cd. But we can run sh, have it change
      " directories for us, and then execute the real command.
      let l:args = extend(['sh', '-c', 'cd "$1" && shift && exec "$@"', '--', a:1], l:args)
    endif
    let restore_popen2_cmd = 0
    if exists('g:vimproc#popen2_commands')
      let restore_popen2_cmd = get(g:vimproc#popen2_commands, 'sh', 0)
      if restore_popen2_cmd
        let g:vimproc#popen2_commands['sh'] = 0
      endif
    endif
    try
      let output = vimproc#system(l:args)
      let status = vimproc#get_last_status()
      let errmsg = vimproc#get_last_errmsg()
    finally
      if restore_popen2_cmd
        let g:vimproc#popen2_commands['sh'] = restore_popen2_cmd
      endif
    endtry
    let outlines = split(output, '\r*\n', 1)
    if !empty(outlines) && empty(outlines[-1])
      call remove(outlines, -1)
    endif
    let errlines = split(errmsg, '\r*\n', 1)
    if !empty(errlines) && empty(errlines[-1])
      call remove(errlines, -1)
    endif
    return { 'status': status, 'output': outlines, 'stderr': errlines }
  endfunction
else
  function! swift#util#system(args, ...)
    if type(a:args) == type([])
      let l:cmd = join(map(a:args, 'shellescape(v:val)'), ' ')
    else
      let l:cmd = a:args
    endif
    if a:0 > 0 && !empty(a:1)
      let l:cmd = 'cd ' . shellescape(a:1) . ' && ' . l:cmd
    endif
    let output = system(l:cmd)
    let status = v:shell_error
    let lines = split(output, '\r*\n', 1)
    if !empty(lines) && empty(lines[-1])
      call remove(lines, -1)
    endif
    return { 'status': status, 'output': lines }
  endfunction
endif
" vim: set et sw=2 ts=2:
