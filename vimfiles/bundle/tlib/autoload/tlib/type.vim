" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-30.
" @Last Change: 2015-12-04.
" @Revision:    44


" Enable type assertiona via |:Tlibtype|.
function! tlib#type#Enable() abort "{{{3
    " :nodoc:
    command! -nargs=+ Tlibtype call tlib#type#Check(expand('<sfile>'), [<f-args>], [<args>])
endf


" Disable type assertiona via |:Tlibtype|.
function! tlib#type#Disable() abort "{{{3
    " :nodoc:
    command! -nargs=+ Tlibtype :
endf


function! tlib#type#IsNumber(expr)
    return tlib#type#Is(a:expr, 0)
endf


function! tlib#type#IsString(expr)
    return tlib#type#Is(a:expr, 1)
endf


function! tlib#type#IsFuncref(expr)
    return tlib#type#Is(a:expr, 2)
endf


function! tlib#type#IsList(expr)
    return tlib#type#Is(a:expr, 3)
endf


function! tlib#type#IsDictionary(expr)
    return tlib#type#Is(a:expr, 4)
endf


function! tlib#type#Is(val, type) abort "{{{3
    if has_key(s:schemas, a:type)
        return tlib#type#Has(a:val, a:type)
    else
        if type(a:type) == 0
            let type = a:type
        elseif a:type =~? '^n\%[umber]'
            let type = 0
        elseif a:type =~? '^s\%[tring]'
            let type = 1
        elseif a:type =~? '^fu\%[ncref]'
            let type = 2
        elseif a:type =~? '^l\%[ist]'
            let type = 3
        elseif a:type =~? '^d\%[ictionary]'
            let type = 4
        elseif a:type =~? '^fl\%[oat]'
            let type = 5
        else
            throw 'tlib#type#Is: Unknown type: ' a:type
        endif
        " TLogVAR a:val, a:type, type, type(a:val), type(a:val) == a:type
        return type(a:val) == type
    endif
endf


function! tlib#type#Are(vals, type) abort "{{{3
    return tlib#assert#Map(a:vals, 'tlib#type#Is(v:val,'. string(a:type) .')')
endf


let s:schemas = {}


function! tlib#type#Define(name, schema) abort "{{{3
    let s:schemas[a:name] = deepcopy(a:schema)
endf


function! tlib#type#Has(val, schema) abort "{{{3
    " TLogVAR type(a:val), type(a:schema)
    if !tlib#type#IsDictionary(a:val)
        " TLogVAR 'not a dictionary', a:val
        return 0
    endif
    if tlib#type#IsString(a:schema)
        " TLogVAR a:schema
        let schema = copy(s:schemas[a:schema])
    else
        let schema = copy(a:schema)
    endif
    if tlib#type#IsDictionary(schema)
        return tlib#assert#All(map(schema, 'has_key(a:val, v:key) && tlib#type#Is(a:val[v:key], v:val)'))
    else
        " TLogVAR keys(a:val), schema
        return tlib#assert#All(map(schema, 'has_key(a:val, v:val)'))
    endif
endf


function! tlib#type#Have(vals, schema) abort "{{{3
    return tlib#assert#Map(a:vals, 'tlib#type#Has(v:val,'. string(a:schema) .')')
endf


function! tlib#type#Check(caller, names, vals) abort "{{{3
    " TLogVAR a:names, a:vals, len(a:names)
    for i in range(0, len(a:names) - 1, 2)
        let val = a:vals[i]
        let type = a:vals[i + 1]
        " TLogVAR i, val, type
        if !tlib#type#Is(val, type)
            let name = matchstr(a:names[i], '^''\zs.\{-}\ze'',\?$')
            throw 'tlib#type#Check: Type mismatch: '. name .':'. a:vals[i + 1]
        endif
    endfor
endf

