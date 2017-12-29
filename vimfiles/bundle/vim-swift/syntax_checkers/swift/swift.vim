if exists('g:loaded_syntastic_swift_swift_checker')
  finish
endif
let g:loaded_syntastic_swift_swift_checker = 1

if !exists('g:syntastic_swift_swift_executable')
  let g:syntastic_swift_swift_executable = 'swift'
endif

if !exists('g:syntastic_swift_swift_arguments')
  let g:syntastic_swift_swift_arguments = expand('%:p')
endif
let s:save_cpo = &cpo

set cpo&vim

function! SyntaxCheckers_swift_swift_IsAvailable() dict
  if !executable(self.getExec())
    return 0
  endif
  return 1
endfunction

function! SyntaxCheckers_swift_swift_GetLocList() dict
  let makeprg = self.makeprgBuild({
        \ 'args': g:syntastic_swift_swift_arguments,
        \ 'fname': '' })

  let errorformat =
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m,' .
        \ '%f:%l: %trror: %m,' .
        \ '%f:%l: %tarning: %m'

  let env = {
        \ 'SCRIPT_INPUT_FILE_COUNT': 1,
        \ 'SCRIPT_INPUT_FILE_0': expand('%:p'),
        \ }

  return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'swift',
    \ 'exec': g:syntastic_swift_swift_executable,
    \ 'name': 'swift' })

let &cpo = s:save_cpo
unlet s:save_cpo
