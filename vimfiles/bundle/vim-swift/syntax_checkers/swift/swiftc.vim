" File: syntax_checkers/swift/swiftc.vim
" Author: Kevin Ballard
" Description: Syntastic checker for Swift
" Last Change: Feb 17, 2015

if exists("g:loaded_syntastic_swift_swiftc_checker")
    finish
endif
let g:loaded_syntastic_swift_swiftc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_swift_swiftc_IsAvailable() dict
    let exec = self.getExec()
    if exec ==? 'swiftc'
        call system(swift#swiftc('-find'))
        return v:shell_error == 0
    endif
    return executable(exec)
endfunction

function! SyntaxCheckers_swift_swiftc_GetLocList() dict
    let platformInfo = swift#platform#getPlatformInfo(swift#platform#detect())
    if empty(platformInfo)
        return []
    endif
    let args = swift#platform#argsForPlatformInfo(platformInfo)

    let exec = self.getExec()
    if exec ==? 'swiftc'
        let exe = swift#swiftc()
    else
        let exe = syntastic#util#shescape(exec)
    endif

    let makeprg = self.makeprgBuild({
                \ 'exe': exe,
                \ 'args_before': args,
                \ 'args_after': '-parse'})

    let errorformat =
                \ '%E%f:%l:%c: error: %m,' .
                \ '%W%f:%l:%c: warning: %m,' .
                \ '%Z%\s%#^~%#,' .
                \ '%-G%.%#'

    return SyntasticMake({
                \ 'makeprg': makeprg,
                \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
            \ 'filetype': 'swift',
            \ 'name': 'swiftc'})

let &cpo = s:save_cpo
unlet s:save_cpo
