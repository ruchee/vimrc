if exists("b:did_indent")
  finish
endif

let s:custom_blocks = vue#GetConfig("custom_blocks", {})
let s:indent = {}

function! s:GetSyntaxList()
  let syntax_list = []
  for syntax in values(s:custom_blocks)
    let type = type(syntax)
    if type == v:t_string
      if !count(syntax_list, syntax)
        call add(syntax_list, syntax)
      endif
    elseif type == v:t_list && len(syntax)
      for syn in syntax
        if !count(syntax_list, syn)
          call add(syntax_list, syn)
        endif
      endfor
    else
      echoerr '[vim-vue-plugin] custom_blocks value type'
            \.' must be either string or list'
    endif
  endfor
  return syntax_list
endfunction

function! s:GetIndentExpr(syntax_list)
  for syntax in a:syntax_list
    unlet! b:did_indent
    execute 'runtime indent/'.syntax.'.vim'
    let s:indent[syntax] = &l:indentexpr
  endfor
endfunction

function! GetVueCustomBlocksIndent(syn)
  let syntax = matchstr(a:syn, '^\l\+')
  call vue#Log('custom block syntax: '.syntax)
  let ind = eval(s:indent[syntax])
  return ind
endfunction

call s:GetIndentExpr(s:GetSyntaxList())
