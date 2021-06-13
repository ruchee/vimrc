" Language: Vue
" Maintainer: leaf <https://github.com/leafOfTree>
" Credits: Inspired by mxw/vim-jsx.

if !exists('main_syntax')
  if exists('b:current_syntax') && b:current_syntax == 'vue'
    finish
  endif
  let main_syntax = 'vue'
endif

" <sfile> is replaced with the file name of the sourced file
let s:patch_path = expand('<sfile>:p:h').'/patch'
let s:test = exists('g:vim_vue_plugin_test')

function! s:Init()
  """ Configs
  let s:config = vue#GetConfig('config', {})
  let s:config_syntax = s:config.syntax

  " For advanced users, it can be used to avoid overload
  let b:current_loading_main_syntax = 'vue'
endfunction

function! s:GetGroupNameForLoading(syntax)
  return '@'.a:syntax
endfunction

" Extend group name as
" html defines group @htmlJavaScript and @htmlCss.
" coffee defines group @coffeeJS.
function! s:GetGroupNameForHighlight(syntax)
  let syntax = a:syntax
  let name = '@'.a:syntax
  if syntax == 'javascript'
    let name = '@javascript,@htmlJavaScript,@coffeeJS'
  elseif syntax == 'css'
    let name = '@css,@htmlCss'
  endif
  return name
endfunction

function! s:GetSynatxName(block, syntax)
  let block = a:block
  let syntax = a:syntax
  let name = syntax.toupper(block[0]).block[1:].'Block'
  let name = substitute(name, '-\(.\)', "\\U\\1", 'g')
  let name = vue#AlterSyntaxForEmmetVim(name, syntax)
  return name
endfunction

function! s:LoadSyntaxList(syntax_list)
  for syntax in a:syntax_list
    let loaded = s:BeforeLoadSyntax(syntax)
    if !loaded
      let syntax_group = s:GetGroupNameForLoading(syntax)
      call vue#LoadSyntax(syntax_group, syntax)
    endif
    call s:AfterLoadSyntax(syntax)
  endfor
endfunction

" For specific syntax, we need to handle it specially
function! s:BeforeLoadSyntax(syntax)
  let syntax = a:syntax

  " Avoid overload if group already exists
  let loaded = 0
  if syntax == 'javascript'
    let loaded = hlexists('javaScriptComment')
  elseif syntax == 'css'
    let loaded = hlexists('cssTagName')
  endif
  return loaded
endfunction

function! s:AfterLoadSyntax(syntax)
  let syntax = a:syntax
  call s:LoadPatchSyntax(syntax)
  call s:LoadStyleAfterSyntax(syntax)
endfunction

function! s:LoadPatchSyntax(syntax)
  let file = s:patch_path.'/'.a:syntax.'.vim'
  if filereadable(file)
    execute 'syntax include '.file
  endif
endfunction

function! s:LoadStyleAfterSyntax(syntax)
  let syntax = a:syntax
  if count(['scss', 'sass', 'less', 'stylus'], syntax) == 1
    execute 'runtime! after/syntax/'.syntax.'.vim'
  endif
endfunction

function! s:GetSyntaxLangName(syntax)
  let syntax = a:syntax
  if syntax == 'typescript'
    let syntax = 'ts'
  endif
  return syntax
endfunction

function! s:SetSyntax(block, syntax, has_lang)
  let block = a:block
  let syntax = a:syntax
  let has_lang = a:has_lang

  let name = s:GetSynatxName(block, syntax)
  if has_lang
    let lang_name = s:GetSyntaxLangName(syntax)
    let lang = 'lang=["'']'.lang_name.'["''][^>]*'
  else
    let lang = ''
  endif

  let start = '^<'.block.'[^>]*'.lang.'>'
  let end_tag = '</'.block.'>'
  let end = '^'.end_tag
  let syntax_group = s:GetGroupNameForHighlight(syntax)

  execute 'syntax region '.name.' fold '
        \.' start=+'.start.'+'
        \.' end=+'.end.'+'
        \.' keepend contains='.syntax_group.', vueTag'

  execute 'syntax sync match vueSync groupthere '.name.' +'.start.'+'
  execute 'syntax sync match vueSync groupthere NONE +'.end.'+'

  " Support block like <script src="...">...</script>
  let oneline = start.'.*'.end_tag
  execute 'syntax match '.name.' fold '
        \.' +'.oneline.'+'
        \.' keepend contains='.syntax_group.', vueTag, vueTagOneline'
endfunction

function! s:SetBlockSyntax(config_syntax)
  syntax sync clear

  for [block, syntax] in items(a:config_syntax)
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

function! s:HighlightVueTag()
  syntax region vueTag fold
        \ start=+^<[^/]+ end=+>+ skip=+></+
        \ contained contains=htmlTagN,htmlString,htmlArg
  syntax region vueTag
        \ start=+^</+ end=+>+
        \ contained contains=htmlTagN,htmlString,htmlArg
  syntax region vueTagOneline
        \ start=+</+ end=+>$+
        \ contained contains=htmlTagN,htmlString,htmlArg
  highlight default link vueTag htmlTag
  highlight default link vueTagOneline htmlTag
endfunction

function! s:SetIsKeyword()
  if has("patch-7.4-1142")
    if has("win32")
      syntax iskeyword @,48-57,_,128-167,224-235,$,-
    else
      syntax iskeyword @,48-57,_,192-255,$,-
    endif
  else
    setlocal iskeyword+=-
  endif
endfunction

function! VimVuePluginSyntaxMain(...)
  call s:Init()
  let syntax_list = vue#GetSyntaxList(s:config_syntax)
  call s:LoadSyntaxList(syntax_list)
  call s:SetBlockSyntax(s:config_syntax)
  call s:SetIsKeyword()
  call s:HighlightVueTag()
endfunction

let s:timer = exists('*timer_start') && !exists('SessionLoad') && !s:test
if s:timer
  call timer_start(1, 'VimVuePluginSyntaxMain')
else
  call VimVuePluginSyntaxMain()
endif

let b:current_syntax = 'vue'
if main_syntax == 'vue'
  unlet main_syntax
endif
