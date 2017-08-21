scriptencoding utf-8


let s:save_cpo = &cpo
set cpo&vim


function! omni#item(parsed)
    return {
                \ 'word': a:parsed.lname,
                \ 'kind': a:parsed.kindshort . " » " . util#SignatureStr(a:parsed.type),
                \ 'info': a:parsed.doc,
                \ 'menu': a:parsed.module,
                \ }
endfunction

function! omni#item_module(name, file, type)
    return {
                \ 'word': a:name,
                \ 'kind': a:type,
                \ 'info': a:file,
                \ 'menu': "module",
                \ }
endfunction

" TODO: Refactor combine (1)
function! omni#nimsuggest(file, l, c)
    let completions = []
    let tempfile = util#WriteMemfile()
    let query = "sug " . a:file . ";" . tempfile . ":" . a:l . ":" . a:c
    let jobcmdstr = g:nvim_nim_exec_nimsuggest . " --threads:on --colors:off --compileOnly --experimental --v2 --stdin " . a:file
    let fullcmd = 'echo -e "' . query . '"|' . jobcmdstr
    let completions_raw = util#FilterCompletions(split(system(fullcmd), "\n"))

    for line in completions_raw
        let parsed = util#ParseV2(line)
        call add(completions, omni#item(parsed))
    endfor

    return completions
endfunction

function! omni#modulesuggest(file, l, c)
    let modules = modules#FindGlobalImports()
    let completions = []
    for module in sort(keys(modules))
        call add(completions, omni#item_module(module, modules[module], "G"))
    endfor
    return completions
endfunction

function! s:findStart()
    let pos = col(".")
    let cline = getline(".")

    while pos > 1
        let ch = char2nr(cline[pos - 2])
        if !((48 <= ch && ch <= 57)
                    \ || (65 <= ch && ch <= 90)
                    \ || (97 <= ch && ch <= 122)
                    \ || ch == 95)
            break
        endif
        let pos = pos - 1
    endwhile

    return pos - 1
endfunction

function! omni#nim(findstart, base)
    if a:findstart && empty(a:base)
        return s:findStart()
    endif

    let completions = []
    let file = expand("%:p")
    let l = line(".")
    let c = col(".")

    let [istart, iend] = modules#ImportLineRange()
    if istart != 0 && istart <= l && l < iend
        let completions = omni#modulesuggest(file, l, c)
    else
        let completions = omni#nimsuggest(file, l, c)
    endif

    return {
                \ 'words': completions,
                \ 'refresh': 'always' }
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
