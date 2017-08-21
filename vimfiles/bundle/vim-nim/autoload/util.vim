scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


let s:idtypes = {
            \ 'skProc':         ["p", "Function"],
            \ 'skTemplate':     ["t", "Template"],
            \ 'skType':         ["T", "Type"],
            \ 'skMacro':        ["M", "Macro"],
            \ 'skMethod':       ["m", "Method"],
            \ 'skField':        ["field", "Field"],
            \ 'skAlias':        ["a", "Alias"],
            \ 'skConditional':  ["c", "Conditional"],
            \ 'skConst':        ["C", "Constant"],
            \ 'skConverter':    ["c", "Converter"],
            \ 'skDynLib':       ["d", "Dynamic library"],
            \ 'skEnumField':    ["e", "Enum field"],
            \ 'skForVar':       ["l", "Loop variable"],
            \ 'skGenericParam': ["g", "Generic parameter"],
            \ 'skGlobalVar':    ["g", "Global variable"],
            \ 'skGlobalLet':    ["g", "Global constant"],
            \ 'skIterator':     ["i", "Iterator"],
            \ 'skLabel':        ["l", "Label"],
            \ 'skLet':          ["r", "Runtime constant"],
            \ 'skModule':       ["m", "Module"],
            \ 'skPackage':      ["p", "Package"],
            \ 'skParam':        ["p", "Parameter"],
            \ 'skResult':       ["r", "Result"],
            \ 'skStub':         ["s", "Stub"],
            \ 'skTemp':         ["t", "Temporary"],
            \ 'skUnknown':      ["u", "Unknown"],
            \ 'skVar':          ["v", "Variable"],
            \ }


function! util#FirstNonEmpty(lines)
    for line in a:lines
        if len(line) > 0
            return line
        endif
    endfor
endfunction


function! util#FirstNonEmpty(lines)
    for line in a:lines
        if len(line) > 0
            return line
        endif
    endfor
endfunction


function! util#MemFilePath(file)
    if !exists("b:nvim_tmpfile")
        let b:nvim_tmpfile = tempname()
    endif
    return b:nvim_tmpfile
endfunction


" Writes current buffer into a memfile
function! util#WriteMemfile()
    let memfile = util#MemFilePath(expand("%:p"))
    call writefile(getline(1, '$'), memfile)
    return memfile
endfunction


function! util#JumpToWindow(window, line, col)
    execute ":" . a:window . "wincmd w"
    execute ":" . a:line
    execute ":norm " . (a:col) . "|"
endfunction


function! util#JumpToLocation(file, line, col)
    if expand("%:p") != a:file
        execute ":e! " . a:file
    endif
    execute ":" . a:line
    execute ":norm " . (a:col) . "|"
endfunction


function! util#JumpFromQuickfix(shouldReturn)
    let [file, location, _] = split(getline(line(".")), "|")
    let [l, c] = split(location, " col ")
    wincmd p
    call util#JumpToLocation(file, l, c)
    if a:shouldReturn
        norm zt
        wincmd p
    endif
endfunction


function! util#StartQuery()
    " echohl Comment | echo "..."
endfunction


function! s:GetModule(path)
    return a:path[0]
endfunction


function! util#ParseV1(line)
    let res = split(a:line, "	")
    let path = split(res[2], "\\.")
    let result = {
                \ "ctype":     res[0],
                \ "kind":      res[1],
                \ "symbol":    res[2],
                \ "name":      res[3],
                \ "file":      res[4],
                \ "line":      res[5],
                \ "col":       res[6],
                \ "doc":       res[7],
                \ "module":    s:GetModule(path),
                \ "location":  join(path[0:-2], "."),
                \ "lname":     path[-1],
                \ "kindstr":   s:idtypes[res[1]][1],
                \ "kindshort": s:idtypes[res[1]][0],
                \ }
    return result
endfunction


function! util#ParseV2(line)
    let res = split(a:line, "	")
    let path = split(res[2], "\\.")
    let result = {
                \ "ctype": res[0],
                \ "kind": res[1],
                \ "symbol": res[2],
                \ "type": res[3],
                \ "file": res[4],
                \ "line": res[5],
                \ "col": res[6],
                \ "doc": res[7],
                \ "module": s:GetModule(path),
                \ "location": join(path[0:-2], "."),
                \ "name": path[-1],
                \ "lname": path[-1],
                \ "kindstr": s:idtypes[res[1]][1],
                \ "kindshort": s:idtypes[res[1]][0],
                \ }
    " \ "quality": res[8],
    return result
endfunction


function! util#FilterCompletionLine(line)
    let result = a:line =~ "^\\(def\\|sug\\|use\\|con\\|outline\\|highlight\\)\\t"
    return result
endfunction


function! util#FilterCompletions(lines)
    let result = []
    for line in a:lines
        if util#FilterCompletionLine(line)
            call add(result, line)
        endif
    endfor
    return result
endfunction


let s:nesting_chars = ['(', '[', '{']
let s:unnesting_chars = [')', ']', '}']
let s:strip_regex = '\v^\s*(.{-})\s*$'

function! util#ParseSignature(input)
    let pstart = stridx(a:input, "(") + 1
    let pend = strridx(a:input, ")")
    let parameters = strpart(a:input, pstart, pend - pstart)
    let parameters_end = len(parameters) - 1

    let depth = 0
    let tsep = -1
    let result = {'params': [], 'reval': '' }

    " Parameters
    let idx = 0
    for s:char in split(parameters, '\zs')
        if index(s:nesting_chars, s:char) >= 0
            let depth += 1
        elseif index(s:unnesting_chars, s:char) >= 0
            let depth -= 1
        endif

        if depth == 0
            if s:char == ':'
                let tsep = idx
            elseif  s:char == ',' || s:char == ';'
                call add(result.params, substitute(strpart(parameters, tsep + 1, idx - tsep - 1), s:strip_regex, '\1', ''))
                let tsep = -1
            endif
        endif

        let idx += 1
    endfor

    if tsep != -1
        call add(result.params, substitute(strpart(parameters, tsep + 1, parameters_end + 1), s:strip_regex, '\1', ''))
    endif

    " Return value
    let rstart = stridx(a:input, ":", pend + 1)
    let rend = stridx(a:input, "{\.", pend + 1)
    if rstart != -1 && rstart < rend
        let reval = strpart(a:input, rstart + 1)
        if rend != -1
            let reval = strpart(reval, 0, rend - rstart - 1)
        endif
        let result.reval = substitute(reval, s:strip_regex, '\1', '')
    endif

    return result
endfunction

function! util#SignatureStr(input)
    let tinfo = util#ParseSignature(a:input)
    return join(tinfo.params, " -> ") . (tinfo.reval != "" ? (" => " . tinfo.reval) : "")
endfunction

function! util#djb(str)
    let hash = 0
    for s:char in split(a:str, '\zs')
        let hash = (hash * 11) + char2nr(s:char)
    endfor
    return float2nr(hash)
endfunction

function! util#open_module_doc(module, symbol)
    call system("$BROWSER " . "http://nim-lang.org/docs/" . a:module . ".html#" . a:symbol)
endfunction

function! util#SelectNimProc(inner)
    let curline = nextnonblank(line("."))
    if curline == 0
        let curline = prevnonblank(line("."))
    endif

    let curindent = indent(curline)
    let lastline = line("$")
    let sl = curline

    function! s:IsProcStart(l)
        return getline(a:l) =~ "\s*\\(proc\\|template\\|macro\\|func\\|method\\)\\s\\+\\w\\+("
    endfunction

    while sl > 0
        if s:IsProcStart(sl) && (curindent == -1 || curline == sl || indent(sl) < curindent)
            break
        endif
        let sl -= 1
    endwhile

    if !s:IsProcStart(sl)
        return
    endif

    call cursor(sl, 0)
    normal! 0V
    let el = sl + 1

    while el < line("$") - 1 && (getline(el) =~ "^\\s*$" || indent(el) > indent(sl))
        let el += 1
    endwhile

    let el -= 1
    if a:inner
        while getline(el) =~ "^\\s*$"
            let el -= 1
        endwhile
    endif

    call cursor(el, 0)
    normal! $
    if a:inner
        normal! h
    endif
endfunction


function! util#goto_file()
    let module_loc = modules#moduleLocation(expand("<cword>"))
endfunction


try
    call operator#user#define('nimrepl_send', 'NimReplSend')
    function! NimReplSend(motion_wiseness)
        let start = getpos("'[")
        let stop = getpos("']")
        let start_line = start[1]
        let stop_line = stop[1]
        let str = join(getline(start_line, stop_line), "\n")
        call features#repl#send(str)
    endfunction
catch
endtry


let &cpo = s:save_cpo
unlet s:save_cpo
