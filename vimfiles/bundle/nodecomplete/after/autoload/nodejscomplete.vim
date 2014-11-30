" Vim completion script
" Language:	Javascript(node)
" Maintainer:	Lin Zhang ( myhere.2009 AT gmail DOT com )
" Last Change:	2012-8-18 1:32:00

" save current dir
let s:nodejs_doc_file = expand('<sfile>:p:h') . '/nodejs-doc.vim'

let s:js_varname_reg = '[$a-zA-Z_][$a-zA-Z0-9_]*'

let s:js_obj_declare_type = {
  \ 'global': 0,
  \ 'require': 1,
  \ 'constructor': 2
  \ }

" settings
" default setting
let s:nodejs_complete_config = {
  \  'js_compl_fn': 'javascriptcomplete#CompleteJS',
  \  'max_node_compl_len': 15
  \}
if exists('g:nodejs_complete_config') && type(g:nodejs_complete_config) == type({})
  let g:nodejs_complete_config = extend(s:nodejs_complete_config, g:nodejs_complete_config)
else
  let g:nodejs_complete_config = s:nodejs_complete_config
endif
unlet s:nodejs_complete_config

function! nodejscomplete#CompleteJS(findstart, base)"{{{
  if a:findstart
    try
      let JS_compl_fn = function(g:nodejs_complete_config.js_compl_fn)
      let start = call(JS_compl_fn, [a:findstart, a:base])
    catch /.*/
      echo '!!!!!!!!!!function [' . g:nodejs_complete_config.js_compl_fn . '] is not exists!'
    endtry

    "Decho 'start: ' . start
    " str[start: end] end 为负数时从末尾截取
    if start - 1 < 0
      let b:nodecompl_context = ''
    else
      let line = getline('.')
      let b:nodecompl_context = line[:start-1]
    endif
    return start
  else
    let posi = getpos('.')
    let result = s:getNodeComplete(a:base, b:nodecompl_context)
    " the function above will move the cursor
    " so we restore the cursor position
    " JS_compl_fn below may rely on the cursor position
    call setpos('.', posi)

    "Decho 'nodecomplete: ' . string(result)
    unlet b:nodecompl_context

    let nodejs_compl = result.complete
    " limit nodejs complete count
    if g:nodejs_complete_config.max_node_compl_len != 0
      let nodejs_compl = nodejs_compl[0 : g:nodejs_complete_config.max_node_compl_len - 1]
    endif

    if result.continue
      try 
        let JS_compl_fn = function(g:nodejs_complete_config.js_compl_fn)
        let js_compl = call(JS_compl_fn, [a:findstart, a:base])
      catch /.*/
        echo '!!!!!!!!!!function [' . g:nodejs_complete_config.js_compl_fn . '] is not exists!'
      endtry

      "Decho 'js_compl: ' . string(js_compl)

      return nodejs_compl + js_compl
    else
      return nodejs_compl
    endif
  endif
endfunction"}}}

" get complete
function! s:getNodeComplete(base, context)"{{{
  "Decho 'base: ' . a:base
  "Decho 'context: ' . a:context

  " TODO: 排除 module.property.h 情况
  let mod_reg = '\(' . s:js_varname_reg . '\)\s*\(\.\|\[\s*["'']\?\)\s*$'
  let matched = matchlist(a:context, mod_reg)
  "Decho 'mod_reg: ' . mod_reg

  " 模块属性补全
  if len(matched) > 0
    let var_name = matched[1]
    let operator = matched[2]
    let position = [line('.'), len(a:context) - len(matched[0])]
    "Decho 'var_name: ' . var_name . ' ; operator: ' . operator
    let declare_info = s:getObjDeclareInfo(var_name, position)
    "Decho 'mod_info: ' . string(declare_info) . '; compl_prefix: ' . a:base

    let compl_list = s:getObjectComplete(declare_info.type, declare_info.value,
                                        \ a:base, operator)

    let ret = {
      \ 'complete': compl_list
      \ }
    if len(compl_list) == 0
      let ret.continue = 1
    else
      let ret.continue = 0
    endif
  " 全局补全
  else
    "Decho 'var complete'
    let ret = {
      \ 'continue': 1,
      \ 'complete': s:getVariableComplete(a:context, a:base)
      \ }
  endif

  return ret
endfunction"}}}

function! s:getObjDeclareInfo(var_name, position)"{{{
  let position = s:fixPosition(a:position)
  "Decho 'position: ' . string(position)

  if position[0] <= 0
    return {
      \ 'type': s:js_obj_declare_type.global,
      \ 'value': a:var_name
      \}
  endif

  let decl_stmt_prefix_reg = '\<' . a:var_name . '\_s*=\_s*'
  " search backward, don't move the cursor, don't wrap
  call cursor(position[0], position[1])
  let begin_position = searchpos(decl_stmt_prefix_reg, 'bnW')
  if begin_position[0] == 0
    return {
      \ 'type': s:js_obj_declare_type.global,
      \ 'value': a:var_name
      \}
  endif

  " make sure it's not in comments...
  if !s:isDeclaration(begin_position)
    return s:getObjDeclareInfo(a:var_name, begin_position)
  endif

  let lines = s:getLinesInRange(begin_position, position)
  "Decho 'lines: ' . string(lines)
  let code = join(lines, "\n")

  " require
  let require_stmt_reg = decl_stmt_prefix_reg .
                         \ 'require\_s*(\_s*\([''"]\)\zs[^)''"]\+\ze\1\_s*)'
  let matched = matchstr(code, require_stmt_reg)
  if len(matched)
    return {
      \ 'type': s:js_obj_declare_type.require,
      \ 'value': matched
      \}
  endif

  " new
  let new_stmt_reg = decl_stmt_prefix_reg .
                    \ 'new\_s\+\zs' . s:js_varname_reg . '\%(\_s*\.\_s*' .
                    \  s:js_varname_reg . '\)*\ze'
  let matched = matchstr(code, new_stmt_reg)
  if len(matched)
    let parts = split(matched, '\.')
    return {
      \ 'type': s:js_obj_declare_type.constructor,
      \ 'value': [s:getObjDeclareInfo(parts[0], begin_position), join(parts[1:], '.')]
      \}
  endif

  " new
  " var emitter = new (require('events')).EventEmitter;
  let new_stmt_reg = decl_stmt_prefix_reg .
                    \ 'new\_s\+' .
                    \ '(\_s*require\_s*(\([''"]\)\(' . s:js_varname_reg . '\)\1\_s*)\_s*)' .
                    \ '\_s*' .
                    \ '\(\%(\.' . s:js_varname_reg . '\)\+\)'

  let matchedList = matchlist(code, new_stmt_reg)
  if (len(matchedList))
    "Decho 'new stmt: ' . string(matchedList)
    let props = [matchedList[3][1:]] + matchedList[4:]
    "Decho 'props: ' . string(props)
    return {
      \ 'type': s:js_obj_declare_type.constructor,
      \ 'value': [
      \   {
      \     'type': s:js_obj_declare_type.require,
      \     'value': matchedList[2]
      \   },
      \   join(props, '')
      \ ]
      \}
  endif


  " assign
  let assign_stmt_reg = decl_stmt_prefix_reg . '\zs' . s:js_varname_reg . '\ze'
  let matched = matchstr(code, assign_stmt_reg)
  if len(matched)
    return s:getObjDeclareInfo(a:var_name, begin_position)
  endif

  " continure to search backward
  return s:getObjDeclareInfo(a:var_name, begin_position)
endfunction"}}}

function! s:isDeclaration(position)"{{{
  let [line_num, col_num] = a:position
  " syntaxName @see: $VIMRUNTIME/syntax/javascript.vim
  let syntaxName = synIDattr(synID(line_num, col_num, 0), 'name')
  if syntaxName =~ '^javaScript\%(Comment\|LineComment\|String\|RegexpString\)'
    return 0
  else
    return 1
  endif
endfunction"}}}

" only complete nodejs's module info
function! s:getObjectComplete(type, mod_name, prop_name, operator)"{{{
  " new
  if a:type == s:js_obj_declare_type.constructor
    let list = s:getConstructedObjectComplete(a:mod_name)
  " require and global
  else
    let list = s:getNodeDocList(a:type, a:mod_name, 'props')
  endif

  if !len(list)
    return list
  else
    " no prop_name suplied
    if (len(a:prop_name) == 0)
      let ret = list
    else
      let ret = s:smartFilter(list, 'v:val["word"]', a:prop_name)
    endif

    let [prefix, suffix] = ['', '']
    let matched = matchlist(a:operator, '\[\s*\(["'']\)\?')
    "Decho 'operator_matched: ' . string(matched)
    if len(matched)
      if len(matched[1])
        let [prefix, suffix] = ['', matched[1] . ']']
      else
        let [prefix, suffix] = ['''', ''']']
      endif
    endif

    for item in ret
      let item.word = prefix . item.word . suffix
    endfor
    call s:addFunctionParen(ret)

    return ret
  endif
endfunction"}}}

function! s:getVariableComplete(context, var_name)"{{{
  "Decho 'var_name: ' . a:var_name

  " complete require's arguments
  let matched = matchlist(a:context, 'require\s*(\s*\%(\([''"]\)\(\.\{1,2}.*\)\=\)\=$')
  if (len(matched) > 0)
    "Decho 'require complete: ' . string(matched)

    if (len(matched[2]) > 0)          " complete -> require('./
      let mod_names = s:getModuleInCurrentDir(a:context, a:var_name, matched)
    else
      let mod_names = s:getModuleNames()

      if (len(matched[1]) == 0)     " complete -> require(
        call map(mod_names, '"''" . v:val . "'')"')
      elseif (len(a:var_name) == 0) " complete -> require('
        call map(mod_names, 'v:val . "' . escape(matched[1], '"') . ')"')
      else                          " complete -> require('ti
        let mod_names = filter(mod_names, 'v:val =~# "^' . a:var_name . '"')
        call map(mod_names, 'v:val . "' . escape(matched[1], '"') . ')"')
      endif
    endif

    return mod_names
  endif

  " complete global variables
  let vars = []
  if (len(a:var_name) == 0)
    return vars
  endif

  call s:loadNodeDocData()

  if (has_key(g:nodejs_complete_data, 'vars'))
    let vars = deepcopy(g:nodejs_complete_data.vars)
  endif

  let ret = s:smartFilter(vars, 'v:val["word"]', a:var_name)

  call s:addFunctionParen(ret)

  return ret
endfunction"}}}

function! s:getModuleInCurrentDir(context, var_name, matched)"{{{
  let mod_names = []
  let path = a:matched[2] . a:var_name

  " typed as require('..
  " complete as require('../
  " cause the latter one is more common
  let compl_prefix = ''
  if (path =~# '\.\.$')
    let compl_prefix = '/'
    let path = path . compl_prefix
  endif

  "Decho 'path: ' . path

  let current_dir = expand('%:p:h')
  let glob_path = current_dir . '/' . path . '*'
  let files = s:fuzglob(glob_path)
  "Decho 'glob: ' . glob_path
  "Decho 'current dir files: ' . string(files)
  for file in files
    " not '.' and '..'
    if ((isdirectory(file) ) || file =~? '\.json$\|\.js$') 
      let mod_file = file
      " directory
      if (file !~? '\.json$\|\.js$')
        let mod_file = mod_file . '/'
      endif

      " get complete word
      let mod_file = substitute(mod_file, '\', '/', 'g')
      let start = len(glob_path) - 1 " substract character '*'
      let compl_infix = strpart(mod_file, start)
      "Decho 'idx: ' . start
      "Decho 'compl_infix: ' . compl_infix
      "Decho 'relative file: ' . mod_file

      let mod_name = compl_prefix . a:var_name . compl_infix
      " file module, not a directory
      if (compl_infix !~# '/$')
        let mod_name = mod_name . a:matched[1] . ')'
      endif

      "Decho 'mod_name: ' . mod_name
      call add(mod_names, mod_name)
    endif
  endfor

  "Decho 'relative path: ' . path

  return mod_names
endfunction"}}}

function! s:getModuleNames()"{{{
  call s:loadNodeDocData()

  let mod_names = []

  " build-in module name
  if (has_key(g:nodejs_complete_data, 'modules'))
    let mod_names = keys(g:nodejs_complete_data.modules)
  endif


  " find module in 'module_dir' folder
  if (!exists('b:npm_module_names'))
    let current_dir = expand('%:p:h')

    let b:npm_module_names = s:getModuleNamesInNode_modulesFolder(current_dir)
  endif

  let mod_names = mod_names + b:npm_module_names

  return sort(mod_names)
endfunction"}}}

function! s:getModuleNamesInNode_modulesFolder(current_dir)"{{{
  " ensure platform coincidence
  let base_dir = substitute(a:current_dir, '\', '/', 'g')
  "Decho 'base_dir: ' . base_dir

  let ret = []

  let parts = split(base_dir, '/', 1)
  "Decho 'parts: ' . string(parts)
  let idx = 0
  let len = len(parts)
  let sub_parts = []
  while idx < len
    let sub_parts = add(sub_parts, parts[idx])
    let module_dir = join(sub_parts, '/') . '/node_modules'
    "Decho 'directory: ' . module_dir

    if (isdirectory(module_dir))
      let files = s:fuzglob(module_dir . '/*')
      "Decho 'node_module files: ' . string(files)
      for file in files
        if (isdirectory(file) || file =~? '\.json$\|\.js$')
          let mod_name = matchstr(file, '[^/\\]\+$')
          let ret = add(ret, mod_name)
        endif
      endfor
    endif

    let idx = idx + 1
  endwhile

  "Decho 'npm modules: ' . string(ret)

  return ret
endfunction"}}}

function! s:getConstructedObjectComplete(constructor_info)"{{{
  "Decho 'getConstructedObjectComplete, constructor_info: ' . string(a:constructor_info)

  let ret = []

  let [declare_info, class_name] = a:constructor_info
  let mod_name = declare_info.value
  " global
  if declare_info.type == s:js_obj_declare_type.global
    " Buffer
    if class_name == ''
      let class_name = '.self'
    endif
    " global.Buffer
    if mod_name == 'global'
      let mod_name = class_name
      let class_name = '.self'
    endif
  endif

  " global or require
  if declare_info.type == s:js_obj_declare_type.global ||
    \ declare_info.type == s:js_obj_declare_type.require

    let ret = s:getNodeDocList(declare_info.type, mod_name, 'classes', class_name)
  endif

  return ret
endfunction"}}}

function! s:addFunctionParen(compl_list)"{{{
  for item in a:compl_list
    if type(item) == 4
      if item.kind == 'f'
        let item.word = item.word . '('
      endif
    endif 
  endfor

  return a:compl_list
endfunction"}}}

function! s:loadNodeDocData()"{{{
  " load node module data
  if (!exists('g:nodejs_complete_data'))
    " load data from external file
    let filename = s:nodejs_doc_file
    "Decho 'filename: ' . filename
    if (filereadable(filename))
      execute 'so ' . filename
      "Decho string(g:nodejs_complete_data)
    else
      "Decho 'not readable: ' . filename
    endif
  endif
endfunction"}}}

" get infomation from g:nodejs_complete_data
" @param mod_type {Enum}
" @param mod_name {String}
" @param type {Enum} 'props' | 'classes'
" @param {String} if type == 'classes', then it exists and is class_name
"                 else do not exist
function! s:getNodeDocList(mod_type, mod_name, type, ...)"{{{
  call s:loadNodeDocData()

  if a:mod_type == s:js_obj_declare_type.require
    let type = 'modules'
  else
    let type = 'globals'
  endif

  if (has_key(g:nodejs_complete_data[type], a:mod_name))
    let mod = g:nodejs_complete_data[type][a:mod_name]
  else
    let mod = {}
  endif

  " class
  if a:0 != 0
    let class_name = a:1
    if (has_key(mod, a:type))
      let classes = mod[a:type]
    else
      let classes = {}
    endif

    if (has_key(classes, a:1))
      let ret = classes[class_name]
    else
      let ret = []
    endif
  " property
  else
    if (has_key(mod, a:type))
      let ret = mod[a:type]
    else
      let ret = []
    endif
  endif

  return deepcopy(ret)
endfunction"}}}

" copied from FuzzyFinder/autoload/fuf.vim
" returns list of paths.
" An argument for glob() is normalized in order to avoid a bug on Windows.
function! s:fuzglob(expr)"{{{
  " Substitutes "\", because on Windows, "**\" doesn't include ".\",
  " but "**/" include "./". I don't know why.
  return split(glob(substitute(a:expr, '\', '/', 'g')), "\n")
endfunction"}}}

" when x <= 0, return [0, 0]
" when y <= 0, move to previous line end
function! s:fixPosition(position)"{{{
  let [x, y] = a:position

  if x <= 0
    return [0, 0]
  endif

  if y <= 0
    let x -= 1
    let y = len(getline(x))

    return s:fixPosition([x, y])
  endif

  return [x, y]
endfunction"}}}

" return a List contains every line
function! s:getLinesInRange(begin_position, end_position)"{{{
  let [begin_x, begin_y] = a:begin_position
  let [end_x, end_y] = a:end_position

  let lines = []
  if begin_x == end_x
    let line = getline(begin_x)
    call add(lines, line[begin_y - 1 : end_y - 1])
  else
    let line = getline(begin_x)
    call add(lines, line[begin_y - 1 :])

    let x = begin_x + 1
    while x < end_x
      let line = getline(x)
      call add(lines, line)
      let x += 1
    endwhile

    let line = getline(end_x)
    call add(lines, line[: end_y - 1])
  endif

  return lines
endfunction"}}}

" filter items with exact match at first
function! s:smartFilter(items, str, keyword)"{{{
  let items = filter(a:items, a:str . ' =~ "' . a:keyword . '"')
  let [exact_ret, fuzzy_ret] = [[], []]
  for item in items
    if item.word =~ '^' . a:keyword
      call add(exact_ret, item)
    else
      call add(fuzzy_ret, item)
    endif
  endfor

  return exact_ret + fuzzy_ret
endfunction"}}}

"
" use plugin Decho(https://github.com/vim-scripts/Decho) for debug
"
" turn off debug mode
" :%s;^\(\s*\)\(Decho\);\1"\2;g | :w | so %
"
" turn on debug mode
" :%s;^\(\s*\)"\(Decho\);\1\2;g | :w | so %
"


" vim:set foldmethod=marker:
