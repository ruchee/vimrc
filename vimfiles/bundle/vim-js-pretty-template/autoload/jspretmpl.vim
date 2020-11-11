"============================================================================
" FILE: autoload/jspre.vim
" AUTHOR: Quramy <yosuke.kurami@gmail.com>
"============================================================================

scriptencoding utf-8

let s:save_cpo = &cpo
set cpo&vim

"#### TemplateSyntax

function! s:tmplSyntaxGroup(filetype)
  let ft = toupper(a:filetype)
  return 'JavaScriptPrettyTemplateCodeGroup'.ft
endfunction

function! s:tmplSyntaxRegion(filetype)
  let ft = toupper(a:filetype)
  return 'JavaScriptPrettyCodeRegion'.ft
endfunction

function! jspretmpl#loadOtherSyntax(filetype)
  let group = s:tmplSyntaxGroup(a:filetype)

  " syntax save
  if exists('b:current_syntax')
    let s:current_syntax = b:current_syntax
    unlet b:current_syntax
  endif

  execute 'syntax include @'.group.' syntax/'.a:filetype.'.vim'

  " syntax restore
  if exists('s:current_syntax')
    let b:current_syntax=s:current_syntax
  else
    unlet b:current_syntax
  endif

  return group
endfunction

function! jspretmpl#applySyntax(filetype, startCondition)
  let group = s:tmplSyntaxGroup(a:filetype)
  let region = s:tmplSyntaxRegion(a:filetype)
  let b:jspre_current_ft = a:filetype
  if &ft == 'javascript' || &ft == 'typescript' || &ft == 'jsx' || &ft == 'javascript.jsx'
    if strlen(a:startCondition)
      let regexp_start = a:startCondition
    else
      let regexp_start = '\w*`'
    endif
    let regexp_skip = '\(\\`\|\${[^}]*`\|`\(.*\(\${\)\@!.*\)*}\)'
    let regexp_end = '`'
    let group_def = 'start="'.regexp_start.'" skip="'.regexp_skip.'" end="'.regexp_end.'"'
    execute 'syntax region '.region.' matchgroup=EcmaScriptTemplateStrings '.group_def.' keepend contains=@'.group.' containedin=jsTaggedTemplate'
  elseif &ft == 'coffee' || &ft == 'dart'
    let regexp_start = '"""'
    let regexp_end = '"""'
    let group_def = 'start=+'.regexp_start.'+ end=+'.regexp_end.'+'
    execute 'syntax region '.region.' matchgroup=CoffeeScriptTemplateStringsDouble '.group_def.' keepend contains=@'.group

    let regexp_start = "'''"
    let regexp_end = "'''"
    let group_def = 'start=+'.regexp_start.'+ end=+'.regexp_end.'+'
    execute 'syntax region '.region.' matchgroup=CoffeeScriptTemplateStringsSingle '.group_def.' keepend contains=@'.group
  else
    return
  endif

endfunction

let s:rule_map = {}
function! jspretmpl#addRule(filetype, startCondition)
  if !strlen(a:startCondition)
    return
  endif
  let s:rule_map[a:filetype] = a:startCondition
endfunction

function! jspretmpl#register_tag(tagname, filetype)
  call jspretmpl#addRule(a:filetype, a:tagname.'`')
endfunction

function! jspretmpl#loadAndApply()
  for k in keys(s:rule_map)
    call jspretmpl#loadOtherSyntax(k)
    call jspretmpl#applySyntax(k, s:rule_map[k])
  endfor
endfunction

function! jspretmpl#clear()
  if !exists('b:jspre_current_ft')
    return
  endif
  execute 'syntax clear '.s:tmplSyntaxRegion(b:jspre_current_ft)
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
