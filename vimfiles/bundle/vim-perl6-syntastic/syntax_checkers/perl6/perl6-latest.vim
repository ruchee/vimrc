"============================================================================
"File:        perl6latest.vim
"Description: Syntax checking plugin for syntastic.vim. This plugin tracks
"             the latest rekudo version and is more up to date than
"             the (future) perl6 support in syntastic core (by the same
"             authors).
"Maintainer:  Claudio Ramirez <pub.claudio at gmail dot com>,
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" Security:
"
" This checker runs 'perl6 -c' against your file, which in turn executes
" any BEGIN, and CHECK blocks in your file. This is probably fine if you 
" wrote the file yourself, but it can be a problem if you're trying to 
" check third party files. If you are 100% willing to let Vim run the code 
" in your file, set g:syntastic_enable_perl6latest_checker to 1 in your vimrc 
" to enable this
" checker:
"
"   let g:syntastic_enable_perl6latest_checker = 1
"
" References:
"
" - https://docs.perl6.org/programs/00-running

" Initialization 
"
if exists('g:loaded_syntastic_perl6_perl6latest_checker')
    finish
endif
let g:loaded_syntastic_perl6_perl6latest_checker = 1

if !exists('g:syntastic_perl6_lib_path')
    let g:syntastic_perl6_lib_path = []
endif

" Add support for perl6 filetype
if exists('g:syntastic_extra_filetypes')
    call add(g:syntastic_extra_filetypes, 'perl6')
else
    let g:syntastic_extra_filetypes = ['perl6']
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_perl6_perl6latest_GetLocList() dict
    " Read lib path from .vimrc
    if type(g:syntastic_perl6_lib_path) == type('')
        call syntastic#log#oneTimeWarn(
                    \'variable g:syntastic_perl6_lib_path should be a list')
        let includes = split(g:syntastic_perl6_lib_path, ',')
    else
        let includes = copy(syntastic#util#var('perl6_lib_path', []))
    endif
    " support for PERL6LIB environment variable
    if $PERL6LIB !=# ''
        let includes += split($PERL6LIB, ':')
    endif
    call map(includes, '"-I" . v:val')
    let errorformat =
        \ '%f:%l:%c:%m,' .
        \ ':%l:%c:%m'

    let makeprg = self.makeprgBuild({ 'args_before': ['-c'] + includes })

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': { 'RAKUDO_ERROR_COLOR': '0' },
        \ 'defaults': { 'bufnr': bufnr(''), 'type': 'E' },
        \ 'returns': [0, 1],
        \ 'Preprocess': 'Perl6LatestPreprocess' })
endfunction

function! SyntaxCheckers_perl6_perl6latest_IsAvailable() dict
    " Set the perl6 executable from .vimrc
    if exists('g:syntastic_perl6_interpreter')
        let g:syntastic_perl6_perl6latest_exec = g:syntastic_perl6_interpreter
    endif

    if exists('g:syntastic_perl6_perl6latest_exec')
        silent! call syntastic#util#system(
                    \g:syntastic_perl6_perl6latest_exec . ' -e ' .
                    \syntastic#util#shescape('exit(0)'))
        return v:shell_error == 0
    else
        " Default to perl6
        if !executable(self.getExec())                                              
            return 0                                                                
        else
            return 1
        endif 
    endif
endfunction

function! Perl6LatestPreprocess(errors) abort
    let out = []
    let fname = ''
    let line = 0
    let column = 0
    let msg = ''
    if a:errors[0] == 'Syntax OK'
        return out
    endif

    for e in a:errors
        if e =~# '\m^\s*$'
            continue
        endif

        if e =~# '\m^Error while '
            if msg !=# ''
                call add(out, join([fname, line, column, msg], ':'))
            endif

            call add(out, ':0:0:' . e)
            let fname = ''
            let line = 0
            let column = 0
            let msg = ''
        elseif e =~# '\m^===SORRY!=== Error while compiling\s'
            if msg !=# ''
                call add(out, join([fname, line, column, msg], ':'))
            endif

            let fname = matchstr(e, '\m^===SORRY!=== Error while compiling\s\zs.*')
            let line = 0
            let column = 0
            let msg = ''
        elseif e =~# '\m^at line \d\+$'
            let line = matchstr(e, '\m^at line \zs\d\+')
        elseif e =~# '\m used at line \d\+'
            let parts = matchlist(e, '\v^\s*(\S+) used at line (\d+)')
            if len(parts) >= 3
                let [what, line] = parts[1:2]
                let msg .= ' ' . what
            endif
        elseif e =~# '\m^at .*:\d\+$'
            let parts = matchlist(e, '\v^at\s+(.*)\:(\d+)$')
            if len(parts) >= 3
                let [fname, line] = parts[1:2]
            endif
        elseif e =~# '\m^Could not find .* at line \d\+ in:'
            let line = matchstr(e, '\m^Could not find .* at line \zs\d\+')
        elseif e =~# '^\m------> \(<BOL>\)\=.\{-}<HERE>'
            let str = matchstr(e, '^\m------> \(<BOL>\)\=\zs.\{-}\ze<HERE>')
            let str = has('iconv') && &encoding !=# '' && &encoding !=# 'utf-8' ? iconv(str, 'utf-8', &encoding) : str
            if Strwidth(str) < 40
                let column = strlen(str) + 1
            endif
        else
            let e = substitute(e, '\m^\s\+', '', '')
            let msg .= (msg !=# '' ? ' ' : '') . e
        endif
    endfor

    if msg !=# ''
        call add(out, join([fname, line, column, msg], ':'))
    endif

    return syntastic#util#unique(out)
endfunction

function! SyntaxCheckers_perl6_perl6latest_GetHighlightRegex(item)
	" Default (catches also '^Can only use'
    let term = matchstr(a:item['text'], '\m''\zs.\{-}\ze''')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
	"Undeclare routines and names
    let term = matchstr(a:item['text'], '\m^Undeclared .\+:\W\zs\S\+\ze')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
	"Not found modules
    let term = matchstr(a:item['text'], '\mCould not find \zs.\{-}\ze at')
    return term !=# '' ? '\V' . escape(term, '\') : ''
endfunction

" Copied from syntastic's util.vim
" strwidth() was added in Vim 7.3; if it doesn't exist, we use strlen()         
" and hope for the best :)                                                      
let s:_width = function(exists('*strwidth') ? 'strwidth' : 'strlen')            
lockvar s:_width                                                                
function! Strwidth(str) abort
    return s:_width(a:str)                                                      
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'perl6',
    \ 'name': 'perl6latest',
    \ 'exec': 'perl6',
    \ 'enable': 'enable_perl6latest_checker'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
