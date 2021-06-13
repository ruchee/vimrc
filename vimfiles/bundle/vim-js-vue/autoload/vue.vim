" Since vue#Log and vue#GetConfig are always called 
" in syntax and indent files,
" this file will be sourced on opening the first vue file

function! s:GetConfig(name, default)
  let name = 'g:vim_vue_plugin_'.a:name
  let value =  exists(name) ? eval(name) : a:default

  if a:name == 'config'
    let value = s:MergeUserConfigIntoDefault(value)
  endif

  return value
endfunction

function! s:MergeUserConfigIntoDefault(user)
  let default = { 
        \'syntax': {
        \   'template': ['html'],
        \   'script': ['javascript'],
        \   'style': ['css'],
        \},
        \'full_syntax': [],
        \'initial_indent': [],
        \'attribute': 0,
        \'keyword': 0,
        \'foldexpr': 0,
        \'debug': 0,
        \}

  let user = a:user
  for key in keys(default)
    if has_key(user, key)
      let default[key] = user[key]
    endif
  endfor

  " For backwards compatibility with 'init_indent'
  if !has_key(user, 'initial_indent')
    if has_key(user, 'init_indent')
          \ ? user.init_indent
          \ : expand('%:e') == 'wpy'
      let default.initial_indent = ['script', 'style']
    endif
  endif

  return default
endfunction

function! s:CheckVersion()
  if !exists('g:vim_vue_plugin_config')
    let prev_configs = [
          \'g:vim_vue_plugin_load_full_syntax',
          \'g:vim_vue_plugin_use_pug',
          \'g:vim_vue_plugin_use_coffee',
          \'g:vim_vue_plugin_use_typescript',
          \'g:vim_vue_plugin_use_less',
          \'g:vim_vue_plugin_use_sass',
          \'g:vim_vue_plugin_use_scss',
          \'g:vim_vue_plugin_use_stylus',
          \'g:vim_vue_plugin_has_init_indent',
          \'g:vim_vue_plugin_highlight_vue_attr',
          \'g:vim_vue_plugin_highlight_vue_keyword',
          \'g:vim_vue_plugin_use_foldexpr',
          \'g:vim_vue_plugin_custom_blocks',
          \'g:vim_vue_plugin_debug',
          \]
    let has_prev_config = 0
    for config in prev_configs
      if exists(config)
        let has_prev_config = 1
        break
      endif
    endfor

    if has_prev_config
      let message =  'Hey, it seems that you just upgraded. Please use `g:vim_vue_plugin_config` to replace previous configs'
      let message2 = 'For details, please check README.md ## Configuration or https://github.com/leafOfTree/vim-vue-plugin'
      echom '['.s:name.'] '.message
      echom '['.s:name.'] '.message2
    endif
  endif
endfunction

function! s:Main()
  let s:name = 'vim-vue-plugin'
  let s:config = s:GetConfig('config', {})
  let s:full_syntax = s:config.full_syntax
  let s:debug = s:config.debug

  call s:CheckVersion()
endfunction

function! vue#Log(msg)
  if s:debug
    echom '['.s:name.'] '.a:msg
  endif
endfunction

function! vue#LogWithLnum(msg)
  if s:debug
    echom '['.s:name.']['.v:lnum.'] '.a:msg
  endif
endfunction

function! vue#Warn(msg)
  if s:debug
    echohl WarningMsg
    echom '['.s:name.'] '.a:msg
    echohl None
  endif
endfunction

function! vue#GetConfig(name, default)
  return s:GetConfig(a:name, a:default)
endfunction

if exists('##CursorMoved') && (exists('*OnChangeVueSyntax') || exists('*OnChangeVueSubtype'))
  augroup vim_vue_plugin
    autocmd!
    autocmd CursorMoved,CursorMovedI,WinEnter *.vue,*.wpy
          \ call s:CheckSyntax()
  augroup END

  if exists('*OnChangeVueSyntax')
    let s:OnChangeListener = function('OnChangeVueSyntax')
  else
    let s:OnChangeListener = function('OnChangeVueSubtype')
  endif

  let s:syntax = ''
  function! s:CheckSyntax()
    let syntax = GetVueSyntax()

    if s:syntax != syntax
      call s:OnChangeListener(syntax)
      let s:syntax = syntax
    endif
  endfunction
endif

function! s:SyntaxListAtEnd(lnum)
  let plnum = prevnonblank(a:lnum)
  let col = strlen(getline(plnum))
  return map(synstack(plnum, col), 'synIDattr(v:val, "name")')
endfunction

function! s:SyntaxAtEnd(lnum)
  let syns = s:SyntaxListAtEnd(a:lnum)
  let syntax_name = empty(syns) ? '' : get(syns, 0, '')
  let syntax_name = s:RemoveCssPrefix(syntax_name)
  return syntax_name
endfunction

function! vue#SyntaxSecondAtEnd(lnum)
  let syns = s:SyntaxListAtEnd(a:lnum)
  return get(syns, 1, '')
endfunction

function! s:GetBlockTag(lnum)
  let syntax_name = s:SyntaxAtEnd(a:lnum)
  let tag = matchstr(syntax_name, '\(\u\U\+\)\+\zeBlock')
  let tag = substitute(tag, '\U\zs\(\u\)', "-\\1", 'g')
  let tag = tolower(tag)
  return tag
endfunction

let s:style_with_css_prefix = ['scss', 'less', 'stylus']

" Adjust syntax name to support emmet-vim by adding css prefix
function! vue#AlterSyntaxForEmmetVim(name, syntax)
  let name = a:name
  if count(s:style_with_css_prefix, a:syntax)
    let name = 'css'.toupper(name[0]).name[1:]
  endif
  return name
endfunction

" Remove css prefix
function! s:RemoveCssPrefix(syntax_name)
  let syntax_name = a:syntax_name
  let syntax = matchstr(syntax_name, '^\U\+')
  if syntax == 'css'
    let next_syntax = tolower(matchstr(syntax_name, '^\U\+\zs\u\U\+'))
    if count(s:style_with_css_prefix, next_syntax)
      let syntax_name = matchstr(syntax_name, '^\U\+\zs.*')
      let syntax_name = tolower(syntax_name[0]).syntax_name[1:]
    endif
  endif
  return syntax_name
endfunction

function! s:GetBlockSyntax(lnum)
  let syntax_name = s:SyntaxAtEnd(a:lnum)
  let syntax = matchstr(syntax_name, '^\U\+')
  return syntax
endfunction

function! vue#GetBlockTag(lnum)
  return s:GetBlockTag(a:lnum)
endfunction

function! vue#GetBlockSyntax(lnum)
  return s:GetBlockSyntax(a:lnum)
endfunction

function! GetVueSubtype()
  let syntax = vue#GetBlockSyntax(line('.'))
  return syntax
endfunction

function! GetVueSyntax()
  let syntax = vue#GetBlockSyntax(line('.'))
  return syntax
endfunction

function! GetVueTag(...)
  let lnum = a:0 > 0 ? a:1 : line('.')
  return vue#GetBlockTag(lnum)
endfunction

function! vue#LoadSyntax(group, syntax)
  let group = a:group
  let syntax = a:syntax
  if s:IncludeOrEqual(s:full_syntax, syntax)
    call vue#LoadFullSyntax(group, syntax)
  else
    let loaded = vue#LoadDefaultSyntax(group, syntax)
    if !loaded
      call vue#LoadFullSyntax(group, syntax)
    endif
  endif
endfunction

function! vue#LoadDefaultSyntax(group, syntax)
  unlet! b:current_syntax
  let loaded = 0
  let syntax_paths = ['$VIMRUNTIME', '$VIM/vimfiles', '$HOME/.vim']
  for path in syntax_paths
    let file = expand(path).'/syntax/'.a:syntax.'.vim'
    if filereadable(file)
      let loaded = 1
      execute 'syntax include '.a:group.' '.file
    endif
  endfor
  if loaded
    call vue#Log(a:syntax.': laod default')
  else
    call vue#Warn(a:syntax.': syntax not found in '.string(syntax_paths))
    call vue#Warn(a:syntax.': load full instead')
  endif
  return loaded
endfunction

" Load all syntax files in 'runtimepath'
" Useful if there is no default syntax file provided by vim
function! vue#LoadFullSyntax(group, syntax)
  call vue#Log(a:syntax.': load full')
  call s:SetCurrentSyntax(a:syntax)
  execute 'syntax include '.a:group.' syntax/'.a:syntax.'.vim'
endfunction

" Settings to avoid syntax overload
function! s:SetCurrentSyntax(type)
  if a:type == 'coffee'
    " Avoid `syntax/javascript.vim` in kchmck/vim-coffee-script
    let b:current_syntax = 'vue'
    syntax cluster coffeeJS contains=@javascript,@htmlJavaScript
  else
    unlet! b:current_syntax
  endif
endfunction

function! vue#GetSyntaxList(config_syntax)
  let syntax_list = []
  for syntax in values(a:config_syntax)
    let type = type(syntax)
    if type == v:t_string
      if !count(syntax_list, syntax)
        call add(syntax_list, syntax)
      endif
    elseif type == v:t_list
      for syn in syntax
        if !count(syntax_list, syn)
          call add(syntax_list, syn)
        endif
      endfor
    else
      echoerr '[vim-vue-plugin] syntax value type'
            \.' must be either string or list'
    endif
  endfor

  call s:ModifySyntaxOrder(syntax_list)

  return syntax_list
endfunction

function! s:IncludeOrEqual(listOrString, item)
  let listOrString = a:listOrString
  let item = a:item
  let type = type(listOrString)
  return (type == v:t_list && count(listOrString, item))
        \ || (type == v:t_string && listOrString == item)
endfunction

function! vue#IncludeOrEqual(listOrString, item)
  return s:IncludeOrEqual(a:listOrString, a:item)
endfunction

function! s:ModifySyntaxOrder(syntax_list)
  let syntax_list = a:syntax_list

  " Move basic syntax to the end of the list, so we can check
  " if they are already loaded by other syntax.
  " Order matters
  let load_last = ['html', 'javascript', 'css']
  for syntax in load_last
    let idx = index(syntax_list, syntax)
    if idx >= 0
      call remove(syntax_list, idx)
      call add(syntax_list, syntax)
    endif
  endfor
endfunction

call s:Main()
