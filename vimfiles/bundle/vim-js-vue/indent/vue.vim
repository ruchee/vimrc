" Language: Vue
" Maintainer: leaf <https://github.com/leafOfTree>
" Credits: Inspired by mxw/vim-jsx.

if exists('b:did_indent') | finish |endif

let s:test = exists('g:vim_vue_plugin_test')

function! s:Init()
  """ Configs
  let s:config = vue#GetConfig('config', {})
  let s:config_syntax = s:config.syntax
  let s:initial_indent = s:config.initial_indent

  """ Variables
  let s:indent = {}
  let s:block_tag = '<\/\?\('.join(keys(s:config_syntax), '\|').'\)'

  " To adjust HTML
  let s:empty_tagname = '(area|base|br|col|embed|hr|input|img|'
        \.'keygen|link|meta|param|source|track|wbr)'
  let s:empty_tag = '\v\<'.s:empty_tagname.'.*(/)@<!\>'
  let s:empty_tag_start = '\v\<'.s:empty_tagname.'[^>]*$'
  let s:empty_tag_end = '\v^\s*[^<>/]*\/?\>\s*'
  let s:tag_start = '\v^\s*\<\w*'   " <
  let s:tag_end = '\v^\s*\/?\>\s*'  " />
  let s:full_tag_end = '\v^\s*\<\/' " </...>
  let s:ternary_q = '^\s\+?'
  let s:ternary_e = '^\s\+:.*,\s*$'
endfunction

function! s:SetVueIndent()
  """ Settings
  " JavaScript indentkeys
  setlocal indentkeys=0{,0},0),0],0\,,!^F,o,O,e,:
  " XML indentkeys
  setlocal indentkeys+=*<Return>,<>>,<<>,/
  setlocal indentexpr=GetVueIndent()
endfunction

function! s:GetIndentFile(syntax)
  let syntax = a:syntax
  " lib/indent/* files are perferred for better indent result
  " from previous version Vim
  if syntax == 'html'
    let file = 'lib/indent/xml.vim'
  elseif syntax == 'css'
    let file = 'lib/indent/css.vim'
  elseif syntax == 'javascript'
    let file = 'lib/indent/typescript.vim'
  else
    let file = 'indent/'.syntax.'.vim'
  endif
  return file
endfunction

function! s:SetSyntaxIndentExpr(syntax_list)
  let saved_shiftwidth = &shiftwidth
  let saved_formatoptions = &formatoptions

  for syntax in a:syntax_list
    unlet! b:did_indent
    let &l:indentexpr = ''
    execute 'runtime '.s:GetIndentFile(syntax)
    let s:indent[syntax] = &l:indentexpr
  endfor

  let &shiftwidth = saved_shiftwidth
  let &formatoptions = saved_formatoptions
endfunction

function! s:GetBlockIndent(syntax)
  let syntax = a:syntax
  let indentexpr = get(s:indent, syntax) 
  if !empty(indentexpr)
    let ind = eval(indentexpr)
  else
    call vue#LogWithLnum('indentexpr not found for '.syntax.', use cindent')
    let ind = cindent(v:lnum)
  endif
  return ind
endfunction

function! s:GetIndentByContext(tag, syntax)
  let ind = -1
  let prevline = getline(s:PrevNonBlankNonComment(v:lnum))
  let curline = getline(v:lnum)

  " When not in <template>, set block tags indent to 0
  if a:tag != 'template'
    if curline =~ s:block_tag || prevline =~ s:block_tag
      let ind = 0
    endif
  else
    " When 'pug' syntax in <template>, set block tags indent to 0
    if a:syntax == 'pug'
      if curline =~ s:block_tag
        let ind = 0
      endif
    endif
  endif

  return ind
endfunction

function! s:PrevNonBlankNonComment(lnum)
  let lnum = a:lnum - 1
  let prevlnum = prevnonblank(lnum)
  let prevsyn = vue#SyntaxSecondAtEnd(prevlnum)
  while prevsyn =~? 'comment' && lnum > 1
    let lnum = lnum - 1
    let prevlnum = prevnonblank(lnum)
    let prevsyn = vue#SyntaxSecondAtEnd(prevlnum)
  endwhile
  return prevlnum
endfunction

function! s:AdjustBlockIndent(syntax, ind)
  let block = a:block
  let syntax = a:syntax
  let ind = a:ind

  if syntax == 'html'
    let ind = s:AdjustHTMLIndent(ind)
  elseif syntax == 'javascript'
    let ind = s:AdjustJavaScriptIndent(ind)
  elseif syntax == 'css'
    let ind = s:AdjustCSSIndent(ind)
  endif

  return ind
endfunction

function! s:CheckInitialIndent(tag, syntax, ind)
  let ind = a:ind
  let add = 0

  if ind == 0 && getline(v:lnum) !~ s:block_tag
    let add = vue#IncludeOrEqual(s:initial_indent, a:tag.'.'.a:syntax)
          \ || vue#IncludeOrEqual(s:initial_indent, a:tag)
          \ || vue#IncludeOrEqual(s:initial_indent, a:syntax)
  endif
  if add
    call vue#LogWithLnum('add initial indent')
    let ind = &sw
  endif
  return ind
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

function! s:AdjustHTMLIndent(ind)
  let ind = a:ind
  let prevlnum = prevnonblank(v:lnum - 1)
  let prevline = getline(prevlnum)
  let curline = getline(v:lnum)

  if prevline =~? s:empty_tag
    call vue#LogWithLnum('previous line is an empty tag')
    let ind = ind - &sw
  endif

  " Align '/>' and '>' with '<'
  if curline =~? s:tag_end 
    let ind = ind - &sw
  endif
  " Then correct the indentation of any element following '/>' or '>'.
  if prevline =~? s:tag_end
    let ind = ind + &sw

    " Decrease indent if prevlines are a multiline empty tag
    let [start, end] = s:PrevMultilineEmptyTag(v:lnum)
    if prevlnum == end
      call vue#LogWithLnum('previous line is a multiline empty tag')
      let ind = indent(v:lnum - 1)
      if curline =~? s:full_tag_end 
        let ind = ind - &sw
      endif
    endif
  endif

  " Multiline array/object in attribute like v-*="[
  "   ...
  " ]
  if prevline =~ '[[{]\s*$'
    call vue#LogWithLnum('previous line is an open bracket')
    let ind = indent(prevlnum) + &sw
  endif
  if curline =~ '^\s*[]}][^"]*"\?\s*$'
    call vue#LogWithLnum('current line is a closing bracket')
    let ind = indent(prevlnum) - &sw
  endif

  " Multiline ternary 'a ? b : c' in attribute
  if curline =~ s:ternary_q
    call vue#LogWithLnum('current line is ?...')
    let ind = indent(prevlnum) + &sw
  endif
  if curline =~ s:ternary_e && prevline =~ s:ternary_q
    call vue#LogWithLnum('current line is :...')
    let ind = indent(prevlnum)
  endif
  if prevline =~ s:ternary_e
    call vue#LogWithLnum('prevline line is :...')
    let ind = indent(prevlnum) - &sw
  endif

  " Angle bracket in attribute, like v-if="isEnabled('item.<name>')"
  if prevline =~ '="[^"]*<[^"]*>[^"]*"'
    call vue#LogWithLnum('prevline line is angle bracket in attribute')
    let ind = ind - &sw
  endif

  return ind
endfunction

function! s:AdjustJavaScriptIndent(ind)
  let ind = a:ind
  let prevlnum = prevnonblank(v:lnum - 1)
  let prevline = getline(prevlnum)
  let curline = getline(v:lnum)

  if prevline =~ '^\s*\w.*$' && curline =~ '^\s*\.'
    call vue#LogWithLnum('current line is the first chain call')
    let ind = indent(prevlnum) + &sw
  endif

  if prevline =~ '\s=>\s.*,\s*$' && curline !~ '^\s*[]}])\?,\?\s*$'
    call vue#LogWithLnum('previous line is arrow function property')
    let ind = indent(prevlnum)
  endif
  if prevline =~ '\s||\s*$'
    call vue#LogWithLnum('previous line ends with "||"')
    let ind = indent(prevlnum) + &sw
  endif
  return ind
endfunction

function! s:AdjustCSSIndent(ind)
  let ind = a:ind
  let prevlnum = prevnonblank(v:lnum - 1)
  let prevline = getline(prevlnum)
  let curline = getline(v:lnum)

  if prevline =~ ':\s.*,\s*$'
    call vue#LogWithLnum('previous line is css function')
    let ind = indent(prevlnum) + &sw
  endif
  if curline =~ '^\s*);\?\s*$'
    call vue#LogWithLnum('curline is closing round bracket')
    let ind = indent(prevlnum) - &sw
  endif
  return ind
endfunction

function! GetVueIndent()
  let tag = vue#GetBlockTag(v:lnum)
  let syntax = vue#GetBlockSyntax(v:lnum)
  let ind = s:GetIndentByContext(tag, syntax)
  if ind == -1
    let ind = s:GetBlockIndent(syntax)
    let ind = s:AdjustBlockIndent(syntax, ind)
    call vue#LogWithLnum('syntax '.syntax.', ind '.ind)
  else
    call vue#LogWithLnum('context, ind '.ind)
  endif

  let ind = s:CheckInitialIndent(tag, syntax, ind)
  return ind
endfunction

function! VimVuePluginIndentMain(...)
  call s:Init()
  let syntax_list = vue#GetSyntaxList(s:config_syntax)
  call s:SetSyntaxIndentExpr(syntax_list)
  call s:SetVueIndent()
endfunction

let s:timer = exists('*timer_start') && !exists('SessionLoad') && !s:test
if s:timer
  call timer_start(200, 'VimVuePluginIndentMain')
else
  call VimVuePluginIndentMain()
endif

let b:did_indent = 1
