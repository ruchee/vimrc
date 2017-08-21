scriptencoding utf-8


if exists("g:loaded_nvim_nim")
    finish
endif
let g:loaded_nvim_nim = 1

let s:save_cpo = &cpo
set cpo&vim

function! CheckDependency(command)
    if !executable(a:command)
        echoerr "Not found: " . a:command
        finish
    endif
    return a:command
endfunction


" FIXME
function! FindNimbleModulesPath()
    return "~/.nimble/pkgs/"
endfunction


" FIXME
function! FindNimModulesPath()
    return "/usr/lib/nim/"
endfunction

if exists("g:ycm_semantic_triggers")
    let g:ycm_semantic_triggers["nim"] = ['.']
endif

if !exists("g:nvim_nim_enable_async")
    let g:nvim_nim_enable_async = has("nvim")
endif

if !exists("g:nvim_nim_exec_nim")
    let g:nvim_nim_exec_nim = CheckDependency("nim")
endif

if !exists("g:nvim_nim_exec_nimble")
    let g:nvim_nim_exec_nimble = CheckDependency("nimble")
endif

if !exists("g:nvim_nim_exec_nimsuggest")
    let g:nvim_nim_exec_nimsuggest = CheckDependency("nimsuggest")
endif

if !exists("g:nvim_nim_exec_bash")
    let g:nvim_nim_exec_bash = CheckDependency("bash")
endif

if !exists("g:nvim_nim_deps_nim")
    let g:nvim_nim_deps_nim = FindNimModulesPath()
endif

if !exists("g:nvim_nim_deps_nimble")
    let g:nvim_nim_deps_nimble = FindNimbleModulesPath()
endif

if !exists("g:nvim_nim_outline_track_symbol")
    let g:nvim_nim_outline_track_symbol = 1
endif

if !exists("g:nvim_nim_highlighter_enable")
    let g:nvim_nim_highlighter_enable = 0
endif

if !exists("g:nvim_nim_highlight_builtin")
    let g:nvim_nim_highlight_builtin = 1
endif

if !exists("g:nvim_nim_outline_buffer")
    let g:nvim_nim_outline_buffer = 1
endif

if !exists("g:nvim_nim_outline_buffer_width")
    let g:nvim_nim_outline_buffer_width = 30
endif

if !exists("g:nvim_nim_repl_height")
    let g:nvim_nim_repl_height = 14
endif

if !exists("g:nvim_nim_repl_vsplit")
    let g:nvim_nim_repl_vsplit = 0
endif

if !exists("g:nvim_nim_enable_default_binds")
    let g:nvim_nim_enable_default_binds = 1
endif

if !exists("g:nvim_nim_enable_custom_textobjects")
    let g:nvim_nim_enable_custom_textobjects = 1
endif

if !exists("g:nvim_nim_highlighter_enable")
    let g:nvim_nim_highlighter_enable = 0
endif

if !exists("g:nvim_nim_highlight_builtin")
    let g:nvim_nim_highlight_builtin = 1
endif

if !exists("g:nvim_nim_highlighter_semantics")
    " let g:nvim_nim_highlighter_semantics     = ["skConst", "skForVar", "skGlobalVar", "skGlobalLet", "skLet", "skModule", "skParam", "skTemp", "skVar"]
    let g:nvim_nim_highlighter_semantics     = []
endif

call highlighter#select_highlights(["skProc", "skTemplate", "skType", "skMacro", "skMethod", "skField", "skForVar", "skIterator"])

au BufNewFile,BufRead *.nim setlocal filetype=nim
au BufNewFile,BufRead *.nims setlocal filetype=nims
au BufNewFile,BufRead *.nimble setlocal filetype=nims

let &cpo = s:save_cpo
unlet s:save_cpo
