"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim indent file
"
" Language: Vue
" Maintainer: leafOfTree <leafvocation@gmail.com>
"
" CREDITS: Inspired by mxw/vim-jsx.
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists("b:did_indent")
  finish
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Config {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:use_pug = vue#GetConfig("use_pug", 0)
let s:use_sass = vue#GetConfig("use_sass", 0)
let s:use_scss = vue#GetConfig("use_scss", 0)
let s:use_stylus = vue#GetConfig("use_stylus", 0)
let s:use_coffee = vue#GetConfig("use_coffee", 0)
let s:use_typescript = vue#GetConfig("use_typescript", 0)
let s:has_init_indent = vue#GetConfig("has_init_indent",
      \ expand("%:e") == 'wpy' ? 1 : 0)
let s:custom_blocks = vue#GetConfig("custom_blocks", {})
let s:use_custom_blocks = !empty(s:custom_blocks)
"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Variables {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Let <template> handled by HTML
let s:vue_tag_start = '\v^\s*\<(script|style)' 
let s:vue_tag_end = '\v^\s*\<\/(script|style)'
let s:template_tag = '\v^\s*\<\/?template'
let s:empty_tagname = '(area|base|br|col|embed|hr|input|img|keygen|link|meta|param|source|track|wbr)'
let s:empty_tag = '\v\<'.s:empty_tagname.'[^/]*\>' 
let s:empty_tag_start = '\v\<'.s:empty_tagname.'[^\>]*$' 
let s:empty_tag_end = '\v^\s*[^\<\>\/]*\/?\>\s*' 
let s:tag_start = '\v^\s*\<\w*'
let s:tag_end = '\v^\s*\/?\>\s*' " />
let s:full_tag_end = '\v^\s*\<\/' " </...>

"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Load indent method {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Save shiftwidth
let s:sw = &sw

" Use lib/indent/ files for compatibility
unlet! b:did_indent
runtime lib/indent/xml.vim

unlet! b:did_indent
runtime lib/indent/css.vim

" Use normal indent files
unlet! b:did_indent
runtime! indent/javascript.vim
let b:javascript_indentexpr = &indentexpr

if s:use_custom_blocks
  unlet! b:did_indent
  runtime indent/vue-custom-blocks.vim
  let s:vue_custom_blocks_tag = '<\/\?'.join(keys(s:custom_blocks), '\|')
endif

if s:use_pug
  unlet! b:did_indent
  let s:save_formatoptions = &formatoptions
  runtime! indent/pug.vim
  let &formatoptions = s:save_formatoptions
endif

if s:use_sass
  unlet! b:did_indent
  runtime! indent/sass.vim
endif

if s:use_scss
  unlet! b:did_indent
  runtime! indent/scss.vim
endif

if s:use_stylus
  unlet! b:did_indent
  runtime! indent/stylus.vim
endif

if s:use_coffee
  unlet! b:did_indent
  runtime! indent/coffee.vim
endif

if s:use_typescript
  unlet! b:did_indent
  runtime! indent/typescript.vim
endif

" Recover shiftwidth
let &sw = s:sw
"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Settings {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" JavaScript indentkeys
setlocal indentkeys=0{,0},0),0],0\,,!^F,o,O,e,:
" XML indentkeys
setlocal indentkeys+=*<Return>,<>>,<<>,/
setlocal indentexpr=GetVueIndent()
"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Functions {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! GetVueIndent()
  let ind = s:GetIndentBySyntax()
  let ind = s:AdjustIndent(ind)
  call vue#Log('indent: '.ind)
  return ind
endfunction

function! s:GetIndentBySyntax()
  let prevlnum = prevnonblank(v:lnum - 1)
  let prevline = getline(prevlnum)
  let curline = getline(v:lnum)
  let cursyn = get(s:SynsEOL(v:lnum), 0, '')

  if s:SynHTML(cursyn)
    call vue#Log('syntax: html')
    let ind = s:GetHTMLIndent(prevlnum, prevline, curline)
  elseif s:SynPug(cursyn)
    call vue#Log('syntax: pug')
    let ind = GetPugIndent()
  elseif s:SynCoffee(cursyn)
    call vue#Log('syntax: coffee')
    let ind = GetCoffeeIndent(v:lnum)
  elseif s:SynTypeScript(cursyn)
    call vue#Log('syntax: typescript')
    let ind = GetTypescriptIndent()
  elseif s:SynSASS(cursyn)
    call vue#Log('syntax: sass')
    let ind = GetSassIndent()
  elseif s:SynSCSS(cursyn)
    call vue#Log('syntax: scss')
    if exists('*GetSCSSIndent')
      call vue#Log('indent: scss')
      let ind = GetSCSSIndent()
    else
      call vue#Log('indent: css')
      let ind = GetCSSIndent()
    endif
  elseif s:SynStylus(cursyn)
    call vue#Log('syntax: stylus')
    let ind = GetStylusIndent()
  elseif s:SynStyle(cursyn)
    call vue#Log('syntax: css')
    let ind = GetCSSIndent()
  elseif s:use_custom_blocks && s:SynCustomBlocks(cursyn)
    call vue#Log('syntax: custom blocks')
    let ind = GetVueCustomBlocksIndent(cursyn)
  else
    " Default to JavaScript indent
    call vue#Log('syntax: javascript')
    if len(b:javascript_indentexpr)
      let ind = eval(b:javascript_indentexpr)
    else
      let ind = cindent(v:lnum)
    endif
  endif
  return ind
endfunction

function! s:AdjustIndent(ind)
  let ind = a:ind
  let prevline = getline(prevnonblank(v:lnum - 1))
  let curline = getline(v:lnum)
  let cursyn = get(s:SynsEOL(v:lnum), 0, '')

  if curline =~? s:vue_tag_start 
        \ || curline =~? s:vue_tag_end 
        \ || prevline =~? s:vue_tag_end
        \ || (curline =~ s:template_tag && s:SynPug(cursyn))
    call vue#Log('current line is vue tag or previous line is vue end tag')
    call vue#Log(', or current line is pug template tag')
    let ind = 0
  elseif s:has_init_indent && ind < 1 && s:SynVueScriptOrStyle(cursyn) 
      call vue#Log('add initial indent')
      let ind = &sw
  elseif getline(s:PrevNonBlacnkNonComment(v:lnum)) =~? s:vue_tag_start
    call vue#Log('previous line is vue tag start')
    let ind = 0
  elseif s:use_custom_blocks && curline =~ s:vue_custom_blocks_tag
    call vue#Log('current line is vue custom blocks tag')
    let ind = 0
  endif

  return ind
endfunction

function! s:GetHTMLIndent(prevlnum, prevline, curline)
  let prevlnum = a:prevlnum
  let prevline = a:prevline
  let curline = a:curline

  let ind = XmlIndentGet(v:lnum, 0)
  if prevline =~? s:empty_tag
    call vue#Log('previous line is an empty tag')
    let ind = ind - &sw
  endif

  " Align '/>' and '>' with '<' for multiline tags.
  if curline =~? s:tag_end 
    let ind = ind - &sw
  endif
  " Then correct the indentation of any element following '/>' or '>'.
  if prevline =~? s:tag_end
    let ind = ind + &sw

    " Decrease indent if prevlines are a multiline empty tag
    let [start, end] = s:PrevMultilineEmptyTag(v:lnum)
    if end == prevlnum
      call vue#Log('previous line is a multiline empty tag')
      if curline =~? s:full_tag_end 
        let ind = indent(v:lnum - 1) - &sw
      else
        let ind = indent(v:lnum - 1)
      endif
    endif
  endif
  return ind
endfunction

function! s:SynsEOL(lnum)
  let lnum = prevnonblank(a:lnum)
  let col = strlen(getline(lnum))
  return map(synstack(lnum, col), 'synIDattr(v:val, "name")')
endfunction

function! s:SynHTML(syn)
  return a:syn ==? 'htmlVueTemplate'
endfunction

function! s:SynPug(syn)
  return a:syn ==? 'pugVueTemplate'
endfunction

function! s:SynCoffee(syn)
  return a:syn ==? 'coffeeVueScript'
endfunction

function! s:SynTypeScript(syn)
  return a:syn ==? 'typescriptVueScript'
endfunction

function! s:SynSASS(syn)
  return a:syn ==? 'sassVueStyle'
endfunction

function! s:SynSCSS(syn)
  return a:syn ==? 'cssScssVueStyle'
endfunction

function! s:SynStylus(syn)
  return a:syn ==? 'cssStylusVueStyle'
endfunction

function! s:SynStyle(syn)
  return a:syn =~? 'VueStyle'
endfunction

function! s:SynCustomBlocks(syn)
  return a:syn =~? 'Block'
endfunction

function! s:SynVueScriptOrStyle(syn)
  return a:syn =~? '\v(VueStyle)|(VueScript)'
endfunction

function! s:PrevMultilineEmptyTag(lnum)
  let lnum = a:lnum - 1
  let lnums = [0, 0]
  while lnum > 0
    let line = getline(lnum)
    if line =~? s:empty_tag_end
      let lnums[1] = lnum
    endif

    if line =~? s:tag_start
      if line =~? s:empty_tag_start
        let lnums[0] = lnum
        return lnums
      else
        return [0, 0]
      endif
    endif

    let lnum = lnum - 1
  endwhile
endfunction

function! s:PrevNonBlacnkNonComment(lnum)
  let curline = getline(a:lnum)
  let cursyns = s:SynsEOL(a:lnum)
  let cursyn = get(cursyns, 1, '')
  if cursyn =~? 'comment' && !empty(curline)
    return prevnonblank(a:lnum - 1)
  endif

  let lnum = a:lnum - 1
  let prevlnum = prevnonblank(lnum)
  let prevsyns = s:SynsEOL(prevlnum)
  let prevsyn = get(prevsyns, 1, '')
  while prevsyn =~? 'comment' && lnum > 1
    let lnum = lnum - 1
    let prevlnum = prevnonblank(lnum)
    let prevsyns = s:SynsEOL(prevlnum)
    let prevsyn = get(prevsyns, 1, '')
  endwhile
  return prevlnum
endfunction
"}}}

let b:did_indent = 1
" vim: fdm=marker
