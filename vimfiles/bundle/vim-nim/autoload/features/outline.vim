scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim


function! features#outline#renderable(parsed)
    return {
                \ 'line': a:parsed.line,
                \ 'col': a:parsed.col,
                \ 'name': a:parsed.lname,
                \ 'kind': a:parsed.kind }
endfunction

let s:window = -1
let s:goto_table = {}
let s:current_buffer = -1
let s:buffermap = {}
let s:groups = {}
let s:group_order = ["Types", "Routines", "Constants", "Globals", "Imports"]
let s:symbols = {
            \ 'skProc':         "proc",
            \ 'skTemplate':     "template",
            \ 'skType':         "",
            \ 'skMacro':        "macro",
            \ 'skMethod':       "function",
            \ 'skField':        "field",
            \ 'skAlias':        "alias",
            \ 'skConst':        "constant",
            \ 'skConverter':    "converter",
            \ 'skDynLib':       "dynlib",
            \ 'skEnumField':    "enum",
            \ 'skGlobalVar':    "var",
            \ 'skGlobalLet':    "let",
            \ 'skIterator':     "iterator",
            \ 'skLabel':        "label",
            \ 'skLet':          "constant",
            \ 'skModule':       "module",
            \ 'skPackage':      "package",
            \ }

            " \ 'skField':     "Fields",
let s:group_aliases = {
            \ 'skType':      "Types",
            \ 'skProc':      "Routines",
            \ 'skTemplate':  "Routines",
            \ 'skMacro':     "Routines",
            \ 'skMethod':    "Routines",
            \ 'skConverter': "Routines",
            \ 'skIterator':  "Routines",
            \ 'skConst':     "Constants",
            \ 'skLet':       "Constants",
            \ 'skGlobalVar': "Globals",
            \ 'skGlobalLet': "Globals",
            \ 'skDynLib':    "Imports",
            \ 'skModule':    "Imports",
            \ 'skPackage':   "Imports",
            \ }

function! s:CreateSymbolRow(symbol, active)
    if g:nvim_nim_outline_track_symbol && a:active
        let result = " «» " . a:symbol.name
    else
        let result = "  » " . a:symbol.name
    endif
    if len(s:symbols[a:symbol.kind]) > 0
        let result .= " (" . s:symbols[a:symbol.kind] . ")"
    endif
    return result
endfunction

function! s:FindClosest()
    if len(s:goto_table) == 0
        return 
    endif

    let bline = line(".")
    let closest = 1
    for l in sort(map(keys(s:buffermap), 'str2nr(v:val)'), "n")
        if l <= bline
            let closest = l
        else
            return closest
        endif
    endfor
    return 0
endfunction

function! s:ConfigureOutlineBuffer()
    if s:IsOpen()
        return
    endif

    let s:window = winnr()
    vsplit __nim_outline__
    setlocal filetype=nimoutline
    setlocal buftype=nofile
    setlocal nonumber
    setlocal nowrap
    exec "silent vertical resize " . g:nvim_nim_outline_buffer_width
    setlocal wfw
    nnoremap <buffer><silent> <return> :call features#outline#JumpToSymbol(0)<cr>
    nnoremap <buffer><silent> o        :call features#outline#JumpToSymbol(1)<cr>
endfunction

function! features#outline#JumpToSymbol(stay)
    if !s:IsFocused()
        return
    endif

    let l = line(".")
    if !has_key(s:goto_table, l)
        return
    endif

    let [jl, jc] = s:goto_table[l]
    call util#JumpToWindow(s:window, jl, jc)
    normal! ^

    if a:stay
        normal! zt
        wincmd p
    endif

    call features#outline#run(0)
endfunction

function! s:UpdateOutline(groups)
    let s:goto_table = {}
    let s:buffermap = {}
    let s:groups = a:groups
    call s:ConfigureOutlineBuffer()
    call s:RenderOutline()
endfunction

function! s:RenderOutline()
    let wasFocused = s:IsFocused()
    let closest = 0

    if g:nvim_nim_outline_track_symbol
        let closest = s:FindClosest()
    endif

    call s:Focus()
    if !s:IsFocused()
        return
    endif

    let l = line(".")
    let c = line(".")
    let w0 = line("w0")

    exec "silent vertical resize " . g:nvim_nim_outline_buffer_width

    let rlines = []

    for groupname in s:group_order
        if len(s:groups[groupname]) == 0
            continue
        endif

        call add(rlines, groupname)

        for symbol in s:groups[groupname]
            let s:goto_table[len(rlines) + 1] = [symbol.line, symbol.col]
            if g:nvim_nim_outline_track_symbol
                let s:buffermap[symbol.line] = len(rlines) + 1
            endif
            call add(rlines, s:CreateSymbolRow(symbol, !wasFocused && closest == symbol.line))
        endfor
        call add(rlines, "")
    endfor

    let idx = 1
    for line in rlines
        call setline(idx, line)
        let idx += 1
    endfor

    exec ":" . len(rlines)
    normal! dG

    if !wasFocused && g:nvim_nim_outline_track_symbol && closest != 0 && has_key(s:buffermap, closest)
        call cursor(s:buffermap[closest], 2)
        normal zz
        normal ^
    else
        call cursor(w0, 1)
        normal zt
        call cursor(l, 2)
    endif

    if !wasFocused
        wincmd p
    endif
endfunction

function! s:Window()
    return bufwinnr("__nim_outline__")
endfunction

function! s:IsOpen()
    return s:Window() != -1
endfunction

function! s:IsFocused()
    return s:IsOpen() && s:Window() == winnr()
endfunction

function! s:Focus()
    if s:IsOpen()
        exec ":" . s:Window() . "wincmd w"
    endif
endfunction

function! features#outline#render()
    call s:RenderOutline()
endfunction

let s:OutlineImpl = {}
function! s:OutlineImpl.run(data)
    let s:OutlineImpl.cache = a:data
    let s:groups = {
                \ "Types":     [],
                \ "Routines": [],
                \ "Fields":    [],
                \ "Constants": [],
                \ "Globals":   [],
                \ "Imports":   [],
                \ }

    for line in a:data.lines
        let p = util#ParseV2(line)
        if has_key(s:group_aliases, p.kind)
            let renderable = features#outline#renderable(p)
            call add(s:groups[s:group_aliases[p.kind]], renderable)
        endif
    endfor

    call s:UpdateOutline(s:groups)
endfunction


function! s:BufferModified()
    return buffer_number(".") != s:current_buffer || getbufvar(buffer_number("."), "&mod")
endfunction


function! features#outline#run(isUpdating)
    if !a:isUpdating || s:IsOpen()
        " if s:BufferModified()
        let s:current_buffer = winnr()
        call suggest#New("outline", !g:nvim_nim_enable_async, 1, s:OutlineImpl)
        " else
            " call s:OutlineImpl.run(s:OutlineImpl.cache)
        " endif
    endif
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
