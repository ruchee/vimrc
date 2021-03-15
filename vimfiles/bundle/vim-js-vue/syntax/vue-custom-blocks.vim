let s:custom_blocks = vue#GetConfig("custom_blocks", {})

if empty(s:custom_blocks) 
  finish
endif

function! s:LoadSyntax()
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
  for syntax in syntax_list
    let syntaxGroup = '@'.syntax
    call vue#LoadFullSyntax(syntaxGroup, syntax)
  endfor
endfunction

function! s:SetSyntax(block, syntax, lang)
  let block = a:block
  let syntax = a:syntax
  let lang = a:lang

  let region_name = syntax.toupper(block[0]).block[1:].'Block'
  let syntax_lang = lang ? 'lang=["'']'.syntax.'["''][^>]*' : ''
  let start = '<'.block.'[^>]*'.syntax_lang.'>'
  let end = '</'.block.'>'
  let syntaxGroup = '@'.syntax

  execute 'syntax region '.region_name.' fold matchgroup=vueTag'
        \.' start=+'.start.'+'
        \.' end=+'.end.'+'
        \.' keepend contains='.syntaxGroup
endfunction

function! s:Highlight()
  for [block, syntax] in items(s:custom_blocks)
    let type = type(syntax)
    if type == v:t_string
      call s:SetSyntax(block, syntax, 0)
    elseif type == v:t_list && len(syntax)
      call s:SetSyntax(block, syntax[0], 0)
      for syn in syntax
        call s:SetSyntax(block, syn, 1)
      endfor
    endif
  endfor
endfunction

call s:LoadSyntax()
call s:Highlight()
