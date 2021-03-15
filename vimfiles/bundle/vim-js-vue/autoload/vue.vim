" Since vue#Log and vue#GetConfig are always called 
" in syntax and indent files,
" this file will be sourced when opening the first vue file
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetConfig(name, default)
  let name = 'g:vim_vue_plugin_'.a:name
  return exists(name) ? eval(name) : a:default
endfunction

let s:name = 'vim-vue-plugin'
let s:load_full_syntax = s:GetConfig("load_full_syntax", 0)
let s:debug = s:GetConfig("debug", 0)

function! vue#Log(msg)
  if s:debug
    echom '['.s:name.']['.v:lnum.'] '.a:msg
  endif
endfunction

function! vue#GetConfig(name, default)
  return s:GetConfig(a:name, a:default)
endfunction

if exists('##CursorMoved') && exists('*OnChangeVueSubtype')
  augroup vim_vue_plugin
    autocmd!
    autocmd CursorMoved,CursorMovedI,WinEnter *.vue,*.wpy
          \ call s:CheckSubtype()
  augroup END

  let s:subtype = ''
  function! s:CheckSubtype()
    let subtype = GetVueSubtype()

    if s:subtype != subtype
      call OnChangeVueSubtype(subtype)
      let s:subtype = subtype
    endif
  endfunction
endif

function! s:SynsEOL(lnum)
  let lnum = prevnonblank(a:lnum)
  let cnum = strlen(getline(lnum))
  return map(synstack(lnum, cnum), 'synIDattr(v:val, "name")')
endfunction

function! GetVueSubtype()
  let lnum = line('.')
  let cursyns = s:SynsEOL(lnum)
  let syn = !empty(cursyns) ? get(cursyns, 0, '') : ''

  let subtype = matchstr(syn, '\w\+\zeVue')
  if subtype =~ 'css\w\+'
    let subtype = subtype[3:]
  endif
  let subtype = tolower(subtype)
  return subtype
endfunction

function! GetVueTag(...)
  let lnum = a:0 > 0 ? a:1 : line('.')
  let cursyns = s:SynsEOL(lnum)
  let syn = get(cursyns, 0, '')

  if syn =~ 'VueTemplate'
    let tag = 'template'
  elseif syn =~ 'VueScript'
    let tag = 'script'
  elseif syn =~ 'VueStyle'
    let tag = 'style'
  else
    let tag = ''
  endif

  return tag
endfunction

function! vue#LoadSyntax(group, type)
  if s:load_full_syntax
    call vue#LoadFullSyntax(a:group, a:type)
  else
    call vue#LoadDefaultSyntax(a:group, a:type)
  endif
endfunction

function! vue#LoadDefaultSyntax(group, type)
  unlet! b:current_syntax
  let syntaxPaths = ['$VIMRUNTIME', '$VIM/vimfiles', '$HOME/.vim']
  for path in syntaxPaths
    let file = expand(path).'/syntax/'.a:type.'.vim'
    if filereadable(file)
      execute 'syntax include '.a:group.' '.file
    endif
  endfor
endfunction

" Load all syntax files in 'runtimepath'
" Useful if there is no default syntax file provided by vim
function! vue#LoadFullSyntax(group, type)
  call s:SetCurrentSyntax(a:type)
  execute 'syntax include '.a:group.' syntax/'.a:type.'.vim'
endfunction

" Settings to avoid syntax overload
function! s:SetCurrentSyntax(type)
  if a:type == 'coffee'
    syntax cluster coffeeJS contains=@htmlJavaScript

    " Avoid overload of `javascript.vim`
    let b:current_syntax = 'vue'
  else
    unlet! b:current_syntax
  endif
endfunction
"}}}
