" bundler.vim - Support for Ruby's Bundler
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.1

if exists('g:loaded_bundler') || &cp || v:version < 700
  finish
endif
let g:loaded_bundler = 1

if !exists('g:dispatch_compilers')
  let g:dispatch_compilers = {}
endif
let g:dispatch_compilers['bundle exec'] = ''

" Section: Utility

function! s:function(name) abort
  return function(substitute(a:name,'^s:',matchstr(expand('<sfile>'), '<SNR>\d\+_'),''))
endfunction

function! s:sub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'')
endfunction

function! s:gsub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'g')
endfunction

function! s:shellesc(arg) abort
  if a:arg =~# '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  else
    return shellescape(a:arg)
  endif
endfunction

function! s:fnameescape(file) abort
  if exists('*fnameescape')
    return fnameescape(a:file)
  else
    return escape(a:file," \t\n*?[{`$\\%#'\"|!<")
  endif
endfunction

function! s:shellslash(path) abort
  if exists('+shellslash') && !&shellslash
    return s:gsub(a:path,'\\','/')
  else
    return a:path
  endif
endfunction

function! s:fcall(fn, path, ...) abort
  let ns = matchstr(a:path, '^\a\a\+\ze:')
  if len(ns) && exists('*' . ns . '#' . a:fn)
    return call(ns . '#' . a:fn, [a:path] + a:000)
  else
    return call(a:fn, [a:path] + a:000)
  endif
endfunction

function! s:filereadable(path) abort
  return s:fcall('filereadable', a:path)
endfunction

function! s:completion_filter(results,A) abort
  let results = sort(copy(a:results))
  call filter(results,'v:val !~# "\\~$"')
  let filtered = filter(copy(results),'v:val[0:strlen(a:A)-1] ==# a:A')
  if !empty(filtered) | return filtered | endif
  let regex = s:gsub(a:A,'[^/:]','[&].*')
  let filtered = filter(copy(results),'v:val =~# "^".regex')
  if !empty(filtered) | return filtered | endif
  let filtered = filter(copy(results),'"/".v:val =~# "[/:]".regex')
  if !empty(filtered) | return filtered | endif
  let regex = s:gsub(a:A,'.','[&].*')
  let filtered = filter(copy(results),'"/".v:val =~# regex')
  return filtered
endfunction

function! s:throw(string) abort
  let v:errmsg = 'bundler: '.a:string
  throw v:errmsg
endfunction

function! s:warn(str) abort
  echohl WarningMsg
  echomsg a:str
  echohl None
  let v:warningmsg = a:str
endfunction

function! s:add_methods(namespace, method_names) abort
  for name in a:method_names
    let s:{a:namespace}_prototype[name] = s:function('s:'.a:namespace.'_'.name)
  endfor
endfunction

let s:commands = []
function! s:command(definition) abort
  let s:commands += [a:definition]
endfunction

function! s:define_commands() abort
  for command in s:commands
    exe 'command! -buffer '.command
  endfor
endfunction

augroup bundler_utility
  autocmd!
  autocmd User Bundler call s:define_commands()
augroup END

let s:abstract_prototype = {}

" Section: Syntax highlighting

function! s:syntaxfile() abort
  syntax keyword rubyGemfileMethod gemspec gem source path git group platform platforms env ruby git_source
  hi def link rubyGemfileMethod rubyInclude
endfunction

function! s:syntaxlock() abort
  setlocal iskeyword+=-,.
  syn match gemfilelockHeading  '^[[:upper:] ]\+$'
  syn match gemfilelockKey      '^\s\+\zs\S\+:'he=e-1 skipwhite nextgroup=gemfilelockRevision
  syn match gemfilelockKey      'remote:'he=e-1 skipwhite nextgroup=gemfilelockRemote
  syn match gemfilelockRemote   '\S\+' contained
  syn match gemfilelockRevision '[[:alnum:]._-]\+$' contained
  syn match gemfilelockGem      '^\s\+\zs[[:alnum:]._-]\+\%([ !]\|$\)\@=' contains=gemfilelockFound,gemfilelockMissing skipwhite nextgroup=gemfilelockVersions,gemfilelockBang
  syn match gemfilelockVersions '([^()]*)' contained contains=gemfilelockVersion
  syn match gemfilelockVersion  '[^,()]*' contained
  syn match gemfilelockBang     '!' contained
  if !empty(bundler#project())
    exe 'syn match gemfilelockFound "\<\%(bundler\|' . join(keys(s:project().paths()), '\|') . '\)\>" contained'
    exe 'syn match gemfilelockMissing "\<\%(' . join(filter(keys(s:project().versions()), '!has_key(s:project().paths(), v:val)'), '\|') . '\)\>" contained'
  else
    exe 'syn match gemfilelockFound "\<\%(\S*\)\>" contained'
  endif
  syn match gemfilelockHeading  '^PLATFORMS$' nextgroup=gemfilelockPlatform skipnl skipwhite
  syn match gemfilelockPlatform '^  \zs[[:alnum:]._-]\+$' contained nextgroup=gemfilelockPlatform skipnl skipwhite

  hi def link gemfilelockHeading  PreProc
  hi def link gemfilelockPlatform Typedef
  hi def link gemfilelockKey      Identifier
  hi def link gemfilelockRemote   String
  hi def link gemfilelockRevision Number
  hi def link gemfilelockFound    Statement
  hi def link gemfilelockMissing  Error
  hi def link gemfilelockVersion  Type
  hi def link gemfilelockBang     Special
endfunction

function! s:setuplock() abort
  setlocal includeexpr=get(bundler#project().gems(),v:fname,v:fname)
  setlocal suffixesadd=/
  cnoremap <buffer><expr> <Plug><cfile> get(bundler#project().gems(),expand("<cfile>"),"\022\006")
  let pattern = '^$\|Rails'
  if mapcheck('gf', 'n') =~# pattern
    nnoremap <silent><buffer> gf         :Bopen    <C-R><C-F><CR>
  endif
  if mapcheck('<C-W>f', 'n') =~# pattern
    nnoremap <silent><buffer> <C-W>f     :Bsplit   <C-R><C-F><CR>
  endif
  if mapcheck('<C-W><C-F>', 'n') =~# pattern
    nnoremap <silent><buffer> <C-W><C-F> :Bsplit   <C-R><C-F><CR>
  endif
  if mapcheck('<C-W>gf', 'n') =~# pattern
    nnoremap <silent><buffer> <C-W>gf    :Btabedit <C-R><C-F><CR>
  endif
endfunction

augroup bundler_syntax
  autocmd!
  autocmd BufNewFile,BufRead */.bundle/config set filetype=yaml
  autocmd BufNewFile,BufRead Gemfile,gems.rb
        \ if &filetype !=# 'ruby' | setf ruby | endif
  autocmd Syntax ruby
        \ if expand('<afile>:t') ==? 'gemfile' | call s:syntaxfile() | endif
  autocmd BufNewFile,BufRead [Gg]emfile.lock,gems.locked setf gemfilelock
  autocmd FileType gemfilelock set suffixesadd=.rb
  autocmd Syntax gemfilelock call s:syntaxlock()
  autocmd FileType gemfilelock    call s:setuplock()
  autocmd User Rails/Gemfile.lock,Rails/gems.locked call s:setuplock()
augroup END

" Section: Initialization

function! s:FindBundlerLock(path) abort
  let path = s:shellslash(a:path)
  let fn = fnamemodify(path,':s?[\/]$??')
  let ofn = ""
  let nfn = fn
  while fn !=# ofn && fn !=# '.'
    if s:filereadable(fn . '/Gemfile.lock') && s:filereadable(fn . '/Gemfile')
      return s:sub(fnamemodify(fn,':p'), '[\\/]=$', '/Gemfile.lock')
    elseif s:filereadable(fn . '/gems.locked') && s:filereadable(fn . '/gems.rb')
      return s:sub(fnamemodify(fn,':p'), '[\\/]=$', '/gems.locked')
    endif
    let ofn = fn
    let fn = fnamemodify(ofn,':h')
  endwhile
  return ''
endfunction

function! s:Detect(path) abort
  if !exists('b:bundler_lock')
    let lock = s:FindBundlerLock(a:path)
    if !empty(lock)
      let b:bundler_lock = lock
      unlet! b:bundler_gem
    elseif !empty(getbufvar('#', 'bundler_lock'))
      let lock = getbufvar('#', 'bundler_lock')
      for [gem, path] in items(s:project(lock).paths())
        if strpart(a:path, 0, len(path)) ==# path
          let b:bundler_lock = lock
          let b:bundler_gem = gem
          break
        endif
      endfor
    endif
  endif
  return exists('b:bundler_lock')
endfunction

function! s:Setup(path) abort
  if s:Detect(a:path)
    silent doautocmd User Bundler
  endif
endfunction

function! bundler#manifest_task(lnum) abort
  let gem = matchstr(getline(a:lnum), '^ *gem *(\= *[''"]\zs[^''\"]\+\ze[''"]')
  return len(gem) ? 'update ' . gem : 'install'
endfunction

function! s:ProjectionistDetect() abort
  if s:Detect(get(g:, 'projectionist_file', '')) && !exists('b:bundler_gem')
    let dir = fnamemodify(b:bundler_lock, ':h')
    call projectionist#append(dir, {
          \ '*': s:filereadable(dir . '/config/environment.rb') ? {} :
          \ {'console': 'bundle console'},
          \ 'Gemfile': {'dispatch': 'bundle %:s/.*/\=bundler#manifest_task(exists(''l#'') ? l# : 0)/ --gemfile={file}', 'alternate': 'Gemfile.lock'},
          \ 'gems.rb': {'dispatch': 'bundle %:s/.*/\=bundler#manifest_task(exists(''l#'') ? l# : 0)/ --gemfile={file}', 'alternate': 'gems.locked'},
          \ 'gems.locked': {'alternate': 'gems.rb'},
          \ 'Gemfile.lock': {'alternate': 'Gemfile'}})
    for projections in bundler#project().projections_list()
      call projectionist#append(fnamemodify(b:bundler_lock, ':h'), projections)
    endfor
  endif
endfunction

augroup bundler
  autocmd!
  autocmd FileType               * call s:Setup(expand('<afile>:p'))
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call s:Setup(expand('<afile>:p')) |
        \ endif
  autocmd User ProjectionistDetect call s:ProjectionistDetect()
  autocmd User ProjectionistActivate
        \ if exists('b:bundler_lock') && !exists(':Bopen') |
        \   silent doautocmd User Bundler |
        \ endif
augroup END

" Section: Project

let s:project_prototype = {}
let s:projects = {}

function! bundler#project(...) abort
  if !a:0
    let lock = !empty(get(b:, 'bundler_lock', '')) ? b:bundler_lock : s:FindBundlerLock(expand('%:p'))
  elseif s:filereadable(a:1 . '/Gemfile.lock')
    let lock = a:1 . '/Gemfile.lock'
  elseif s:filereadable(a:1 . '/gems.locked')
    let lock = a:1 . '/gems.locked'
  elseif s:filereadable(a:1)
    let lock = a:1
  else
    let lock = ''
  endif
  if !empty(lock)
    if has_key(s:projects, lock)
      let project = get(s:projects, lock)
    else
      let project = {'root': fnamemodify(lock, ':h'), '_lock': lock}
      let s:projects[lock] = project
    endif
    return extend(extend(project,s:project_prototype,'keep'),s:abstract_prototype,'keep')
  endif
  return {}
endfunction

function! s:project(...) abort
  let project = a:0 ? bundler#project(a:1) : bundler#project()
  if empty(project)
    call s:throw('not a Bundler project: '.(a:0 ? a:1 : expand('%')))
  else
    return project
  endif
endfunction

function! s:project_real(...) dict abort
  let path = join([self.root]+a:000,'/')
  let pre = substitute(matchstr(path, '^\a\a\+\ze:'), '^\a', '\u&', 'g')
  if len(pre) && exists('*' . pre . 'Real')
    return call(pre . 'Real', [path])
  elseif len(pre) && exists('*' . pre . 'Path')
    return call(pre . 'Path', [path])
  else
    return resolve(path)
  endif
endfunction

function! s:project_lock() dict abort
  return self._lock
endfunction

function! s:project_manifest() dict abort
  return substitute(substitute(self._lock, '\.locked$', '.rb', ''), '\.lock$', '', '')
endfunction

call s:add_methods('project',['real', 'lock', 'manifest'])

function! s:project_locked() dict abort
  let lock_file = self.lock()
  let time = s:fcall('getftime', lock_file)
  if time != -1 && time != get(self,'_lock_time',-1)
    let self._locked = {'git': [], 'gem': [], 'path': []}
    let self._versions = {}
    let self._dependencies = {}
    let section = ''
    let conflict = 0

    for line in s:fcall('readfile', lock_file)
      if line =~# '^[=|]'
        let conflict = 1
        continue
      elseif line =~# '^>'
        let conflict = 0
        continue
      elseif line =~# '^<' || conflict
        continue
      elseif line =~# '^\S'
        let section = tr(tolower(line), ' ', '_')
        let properties = {'versions': {}}
        if type(get(self._locked, section)) ==# type([])
          call extend(self._locked[section], [properties])
        endif
      elseif line =~# '^  \w\+: '
        let properties[matchstr(line, '\w\+')] = matchstr(line, ': \zs.*')
      elseif line =~# '^    [a-zA-Z0-9._-]\+\s\+(\d\+'
        let name = split(line, ' ')[0]
        let ver = substitute(line, '.*(\|).*', '', 'g')
        let properties.versions[name] = ver
        let self._versions[name] = ver
        let self._dependencies[name] = []
      elseif line =~# '^      [a-zA-Z0-9._-]\+\s\+('
        let dep = split(line, ' ')[0]
        call add(self._dependencies[name], dep)
      elseif line =~# '^   \S' && !has_key(self._locked, section)
        let self._locked[section] = line[3:-1]
      endif
    endfor
    let self._lock_time = time
  endif
  return get(self, '_locked', {})
endfunction

function! s:project_paths(...) dict abort
  call self.locked()
  let time = get(self, '_lock_time', -1)
  if a:0 && a:1 ==# 'refresh' || time != -1 && time != get(self, '_path_time', -1)
    let paths = {}

    let chdir = exists("*haslocaldir") && haslocaldir() ? "lchdir" : "chdir"
    let cwd = getcwd()

    " Explicitly setting $PATH means /etc/zshenv on OS X can't touch it.
    if executable('env')
      let prefix = 'env PATH='.s:shellesc($PATH).' '
    else
      let prefix = ''
    endif

    let gem_paths = []
    if exists('$GEM_PATH')
      let gem_paths = split($GEM_PATH, has('win32') ? ';' : ':')
    endif

    try
      exe chdir s:fnameescape(self.real())

      if empty(gem_paths)
        let gem_paths = split(system(prefix.'ruby -rrbconfig -rrubygems -e '.s:shellesc('print(([RbConfig::CONFIG["ruby_version"]] + Gem.path).join(%(;)))')), ';')

        let abi_version = empty(gem_paths) ? '' : remove(gem_paths, 0)
      else
        let abi_version = system(prefix.'ruby -rrbconfig -e '.s:shellesc('print RbConfig::CONFIG["ruby_version"]'))
      endif

      exe chdir s:fnameescape(cwd)
    finally
      exe chdir s:fnameescape(cwd)
    endtry

    for config in [expand('~/.bundle/config'), self.real('.bundle/config')]
      if filereadable(config)
        let body = join(readfile(config), "\n")
        let bundle_path = matchstr(body, "\\C\\<BUNDLE_PATH: [\"']\\=\\zs[^\n'\"]*")
        if !empty(bundle_path)
          let gem_paths = [self.real(bundle_path, 'ruby', abi_version), self.real(bundle_path)]
        endif
      endif
    endfor

    call map(gem_paths, 'resolve(v:val)')

    for source in self._locked.git
      let basename = matchstr(source.remote, '.*/\zs.\{-\}\ze\%(\.git\)\=$') .
            \ '-' . source.revision[0:11]
      for [name, ver] in items(source.versions)
        for path in map(copy(gem_paths), 'v:val . "/bundler/gems"') +
              \ [expand('~/.bundle/ruby/') . abi_version]
          let dir = path . '/' . basename
          if isdirectory(dir)
            let files = split(glob(dir . '/*/' . name . '.gemspec'), "\n")
            if empty(files)
              let paths[name] = dir
            else
              let paths[name] = files[0][0 : -10-strlen(name)]
            endif
            break
          endif
        endfor
      endfor
    endfor

    for source in self._locked.path
      for [name, ver] in items(source.versions)
        if source.remote =~# '^\~/'
          let local = expand(source.remote)
        elseif source.remote !~# '^/'
          let local = simplify(self.real(source.remote))
        else
          let local = source.remote
        endif
        let files = split(glob(local . '/*/' . name . '.gemspec'), "\n")
        if empty(files)
          let paths[name] = local
        else
          let paths[name] = files[0][0 : -10-strlen(name)]
        endif
      endfor
    endfor

    for source in self._locked.gem
      for [name, ver] in items(source.versions)
        for path in gem_paths
          let dir = path . '/gems/' . name . '-' . ver
          if isdirectory(dir)
            let paths[name] = dir
            break
          endif
        endfor
        if !has_key(paths, name)
          for path in gem_paths
            let dir = glob(path . '/gems/' . name . '-' . ver . '-*')
            if isdirectory(dir)
              let paths[name] = dir
              break
            endif
          endfor
        endif
      endfor
    endfor

    if has_key(self, '_projections_list')
      call remove(self, '_projections_list')
    endif
    let self._path_time = time
    let self._paths = paths
    let self._sorted = sort(values(paths))
    let index = index(self._sorted, fnamemodify(self.lock(), ':h'))
    if index > 0
      call insert(self._sorted, remove(self._sorted,index))
    endif
    if len(self._sorted) > 1 && filereadable(self._sorted[1] . '/lib/tags') &&
          \ !filereadable(self._sorted[1] . '/tags')
      let self._tags = 'lib/tags'
    endif
    call self.alter_buffer_paths()
    return paths
  endif
  return get(self,'_paths',{})
endfunction

function! s:project_sorted() dict abort
  call self.paths()
  return get(self, '_sorted', [])
endfunction

function! s:project_tags() dict abort
  call self.paths()
  return get(self, '_tags', [])
endfunction

function! s:project_gems() dict abort
  return self.paths()
endfunction

function! s:project_versions() dict abort
  call self.locked()
  return get(self, '_versions', {})
endfunction

function! s:project_has(gem) dict abort
  call self.locked()
  return has_key(self.versions(), a:gem)
endfunction

function! s:project_projections_list() dict abort
  call self.paths()
  if !has_key(self, '_projections_list')
    let self._projections_list = []
    let list = self._projections_list
    if !empty(get(g:, 'gem_projections', {}))
      for name in keys(self.versions())
        if has_key(g:gem_projections, name)
          call add(list, g:gem_projections[name])
        endif
      endfor
    endif
    for path in self.sorted()
      if filereadable(path . '/lib/projections.json')
        call add(list, projectionist#json_parse(readfile(path . '/lib/projections.json')))
      endif
    endfor
  endif
  return self._projections_list
endfunction

function! s:project_dependencies(gem, ...) dict abort
  let deps = a:0 ? a:1 : {}
  let paths = a:0 > 1 ? a:2 : self.paths()
  for dep in get(self._dependencies, a:gem, [])
    if !has_key(deps, dep) && has_key(paths, dep)
      let deps[dep] = paths[dep]
      call self.dependencies(dep, deps, paths)
    endif
  endfor
  return deps
endfunction

call s:add_methods('project', ['locked', 'gems', 'paths', 'sorted', 'versions', 'has', 'dependencies', 'projections_list'])

" Section: Buffer

let s:buffer_prototype = {}

function! s:buffer(...) abort
  let buffer = {'#': bufnr(a:0 ? a:1 : '%')}
  call extend(extend(buffer,s:buffer_prototype,'keep'),s:abstract_prototype,'keep')
  if !empty(buffer.getvar('bundler_lock'))
    return buffer
  endif
  call s:throw('not a Bundler project: '.(a:0 ? a:1 : expand('%')))
endfunction

function! bundler#buffer(...) abort
  return s:buffer(a:0 ? a:1 : '%')
endfunction

function! s:buffer_getvar(var) dict abort
  return getbufvar(self['#'],a:var)
endfunction

function! s:buffer_setvar(var,value) dict abort
  return setbufvar(self['#'],a:var,a:value)
endfunction

function! s:buffer_project() dict abort
  return s:project(self.getvar('bundler_lock'))
endfunction

call s:add_methods('buffer',['getvar','setvar','project'])

" Section: Bundle

function! s:push_chdir() abort
  if !exists("s:command_stack") | let s:command_stack = [] | endif
  let chdir = exists("*haslocaldir") && haslocaldir() ? "lchdir " : "chdir "
  call add(s:command_stack,chdir.s:fnameescape(getcwd()))
  exe chdir.'`=s:project().real()`'
endfunction

function! s:pop_command() abort
  if exists("s:command_stack") && len(s:command_stack) > 0
    exe remove(s:command_stack,-1)
  endif
endfunction

function! s:Bundle(bang,arg) abort
  let old_makeprg = &l:makeprg
  let old_errorformat = &l:errorformat
  let old_compiler = get(b:, 'current_compiler', '')
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  try
    execute cd fnameescape(s:project().real())
    if a:arg =~# '^\s*console\>'
      let arg = substitute(a:arg, '^\s*console\s*', '', '')
      if exists(':Start') > 1
        execute 'Start'.a:bang '-title=' .
              \ escape(fnamemodify(s:project().real(), ':t'), ' ') .
              \ '\ console bundle console' arg
      else
        execute '!bundle console' arg
      endif
    else
      compiler bundler
      execute 'make! '.a:arg
      if a:bang ==# ''
        return 'if !empty(getqflist()) | cfirst | endif'
      else
        return ''
      endif
    endif
  finally
    let &l:errorformat = old_errorformat
    let &l:makeprg = old_makeprg
    let b:current_compiler = old_compiler
    if empty(b:current_compiler)
      unlet b:current_compiler
    endif
    execute cd fnameescape(cwd)
  endtry
endfunction

function! s:BundleComplete(A, L, P) abort
  return bundler#complete(a:A, a:L, a:P, bundler#project())
endfunction

function! bundler#complete(A, L, P, ...) abort
  let project = a:0 ? a:1 : bundler#project(getcwd())
  if !empty(project) && a:L =~# '\s\+\%(show\|update\) '
    return s:completion_filter(keys(project.paths()), a:A)
  endif
  return s:completion_filter(['install','update','exec','package','config','check','list','show','outdated','console','viz','benchmark'], a:A)
endfunction

call s:command("-bar -bang -nargs=? -complete=customlist,s:BundleComplete Bundle :execute s:Bundle('<bang>',<q-args>)")

function! s:IsBundlerMake() abort
  return &makeprg =~# '^bundle' && exists('b:bundler_lock')
endfunction

function! s:QuickFixCmdPreMake() abort
  if !s:IsBundlerMake()
    return
  endif
  call s:push_chdir()
endfunction

function! s:QuickFixCmdPostMake() abort
  if !s:IsBundlerMake()
    return
  endif
  call s:pop_command()
  call s:project().paths('refresh')
endfunction

augroup bundler_command
  autocmd QuickFixCmdPre *make* call s:QuickFixCmdPreMake()
  autocmd QuickFixCmdPost *make* call s:QuickFixCmdPostMake()
  autocmd User Bundler
        \ if exists(':Console') < 2 |
        \   exe "command! -buffer -bar -bang -nargs=* Console :Bundle<bang> console <args>" |
        \ endif
augroup END

" Section: Bopen

function! s:Open(cmd,gem,lcd) abort
  if a:gem ==# '' && a:lcd
    return a:cmd.' '.fnameescape(s:project().manifest())
  elseif a:gem ==# ''
    return a:cmd.' '.fnameescape(s:project().lock())
  else
    if !has_key(s:project().paths(), a:gem)
      call s:project().paths('refresh')
    endif
    if !has_key(s:project().paths(), a:gem)
      if has_key(s:project().versions(), a:gem)
        let v:errmsg = "Gem \"".a:gem."\" is in bundle but not installed"
      else
        let v:errmsg = "Gem \"".a:gem."\" is not in bundle"
      endif
      return 'echoerr v:errmsg'
    endif
    let path = fnameescape(s:project().paths()[a:gem])
    let exec = a:cmd.' '.path
    if a:cmd =~# '^pedit' && a:lcd
      let exec .= '|wincmd P|lcd '.path.'|wincmd p'
    elseif a:lcd
      let exec .= '|lcd '.path
    endif
    return exec
  endif
endfunction

function! s:OpenComplete(A,L,P) abort
  return s:completion_filter(keys(s:project().paths()),a:A)
endfunction

call s:command("-bar -bang -nargs=? -complete=customlist,s:OpenComplete Bopen :execute s:Open('edit<bang>',<q-args>,1)")
call s:command("-bar -bang -nargs=? -complete=customlist,s:OpenComplete Bedit :execute s:Open('edit<bang>',<q-args>,0)")
call s:command("-bar -bang -nargs=? -complete=customlist,s:OpenComplete Bsplit :execute s:Open('split',<q-args>,<bang>1)")
call s:command("-bar -bang -nargs=? -complete=customlist,s:OpenComplete Bvsplit :execute s:Open('vsplit',<q-args>,<bang>1)")
call s:command("-bar -bang -nargs=? -complete=customlist,s:OpenComplete Btabedit :execute s:Open('tabedit',<q-args>,<bang>1)")
call s:command("-bar -bang -nargs=? -complete=customlist,s:OpenComplete Bpedit :execute s:Open('pedit',<q-args>,<bang>1)")

" Section: Paths

function! s:build_path_option(paths,suffix) abort
  return join(map(copy(a:paths),'",".escape(s:shellslash(v:val."/".a:suffix),", ")'),'')
endfunction

function! s:buffer_alter_paths() dict abort
  if self.getvar('&suffixesadd') =~# '\.rb\>'
    let gem = self.getvar('bundler_gem')
    if empty(gem)
      let new = self.project().sorted()
    else
      let new = sort(values(self.project().dependencies(gem)))
    endif
    let old = type(self.getvar('bundler_paths')) == type([]) ? self.getvar('bundler_paths') : []
    for [option, suffix] in
          \ [['tags', get(self.project(), '_tags', 'tags')], ['path', 'lib']]
      let value = self.getvar('&'.option)
      let tail = matchstr(value, '\%(,\.\)\=\%(,,\)\=$')
      let value = strpart(value, 0, len(value) - len(tail))
      if !empty(old)
        let drop = s:build_path_option(old, suffix)
        let index = stridx(value, drop)
        if index > 0
          let value = value[0:index-1] . value[index+strlen(drop):-1]
        endif
      endif
      call self.setvar('&'.option,value.s:build_path_option(new, suffix) . tail)
    endfor
    call self.setvar('bundler_paths',new)
  endif
endfunction

call s:add_methods('buffer',['alter_paths'])

function! s:project_alter_buffer_paths() dict abort
  for bufnr in range(1,bufnr('$'))
    if getbufvar(bufnr,'bundler_lock') ==# self.lock()
      let vim_parsing_quirk = s:buffer(bufnr).alter_paths()
    endif
    if getbufvar(bufnr, '&syntax') ==# 'gemfilelock'
      call setbufvar(bufnr, '&syntax', 'gemfilelock')
    endif
  endfor
endfunction

call s:add_methods('project',['alter_buffer_paths'])

augroup bundler_path
  autocmd!
  autocmd User Bundler call s:buffer().alter_paths()
augroup END

" vim:set sw=2 sts=2:
