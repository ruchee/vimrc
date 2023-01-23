" bundler.vim - Support for Ruby's Bundler
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.1

if exists('g:loaded_bundler') || &cp || v:version < 704
  finish
endif
let g:loaded_bundler = 1

if !exists('g:dispatch_compilers')
  let g:dispatch_compilers = {}
endif
let g:dispatch_compilers['bundle exec'] = ''

" Section: Utility

function! s:function(name) abort
  return function(substitute(a:name,'^s:',matchstr(expand('<sfile>'), '.*\zs<SNR>\d\+_'),''))
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

function! s:shellslash(path) abort
  if exists('+shellslash')
    return tr(a:path, '\', '/')
  else
    return a:path
  endif
endfunction

function! s:Absolute(...) abort
  let path = s:shellslash(a:0 ? a:1 : @%)
  if !a:0 && &buftype =~# '^\%(nofile\|acwrite\)' && path =~# '^\a\+:\|^/'
    return path
  elseif !a:0 && &buftype !~# '^\%(nowrite\)\=$'
    return ''
  endif
  if path !~# '^\a\+:\|^/\|^$'
    let path = s:shellslash(getcwd()) . '/' . path
  endif
  return path
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

let s:abstract_prototype = {}

" Section: Syntax highlighting

function! s:syntaxfile() abort
  syntax keyword rubyGemfileMethod gemspec gem source path git group platform platforms env ruby git_source
  hi def link rubyGemfileMethod rubyInclude
endfunction

function! s:syntaxlock() abort
  setlocal iskeyword+=-,.
  syn match gemfilelockHeading  '^[[:upper:] ]\+$'
  syn match gemfilelockKey      '^\%(  \)\+\zs\S\+:'he=e-1 skipwhite nextgroup=gemfilelockRevision
  syn match gemfilelockKey      'remote:'he=e-1 skipwhite nextgroup=gemfilelockRemote
  syn match gemfilelockRemote   '\S\+' contained
  syn match gemfilelockRevision '[[:alnum:]._-]\+$' contained
  syn match gemfilelockGem      '^\%(  \)\+\zs[[:alnum:]._-]\+\%([ !]\|$\)\@=' contains=gemfilelockFound,gemfilelockMissing skipwhite nextgroup=gemfilelockVersions,gemfilelockBang
  syn match gemfilelockVersions '([^()]*)' contained contains=gemfilelockVersion
  syn match gemfilelockVersion  '[^,()]*' contained
  syn match gemfilelockBang     '!' contained
  if !empty(bundler#project())
    let gem_paths = bundler#GemPaths()
    exe 'syn match gemfilelockFound "\<\%(' . join(['bundler'] + keys(gem_paths), '\|') . '\)\>" contained'
    let missing_gems = filter(keys(bundler#GemVersions()), '!has_key(gem_paths, v:val)')
    if !empty(missing_gems)
      exe 'syn match gemfilelockMissing "\<\%(' . escape(join(missing_gems, '\|'), '.') . '\)\>" contained'
    endif
  else
    syn match gemfilelockFound "\<\%(\k\+\)\>" contained
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
  setlocal includeexpr=get(bundler#GemPaths(),v:fname,v:fname)
  setlocal suffixesadd=/
  cnoremap <buffer><expr> <Plug><cfile> get(bundler#GemPaths(),expand("<cfile>"),"\022\006")
  let pattern = '^$\|Rails'
  if mapcheck('gf', 'n') =~# pattern
    nnoremap <silent><buffer> gf         :<C-U>Bundle :edit <C-R><C-F><CR>
  endif
  if mapcheck('<C-W>f', 'n') =~# pattern
    nnoremap <silent><buffer> <C-W><C-F> :<C-U> exe s:Bundle('', &sb ? 'belowright' : 'aboveleft', expand('<cfile>'))<CR>
  endif
  if mapcheck('<C-W><C-F>', 'n') =~# pattern
    nnoremap <silent><buffer> <C-W><C-F> :<C-U> exe s:Bundle('', &sb ? 'belowright' : 'aboveleft', expand('<cfile>'))<CR>
  endif
  if mapcheck('<C-W>gf', 'n') =~# pattern
    nnoremap <silent><buffer> <C-W>gf    :<C-U> exe s:Bundle('', 'tab', expand('<cfile>'))<CR>
  endif
endfunction

augroup bundler_syntax
  autocmd!
  autocmd BufNewFile,BufRead */.bundle/config set filetype=yaml
  autocmd BufNewFile,BufRead Gemfile
        \ if &filetype !=# 'ruby' | setf ruby | endif
  autocmd Syntax ruby
        \ if expand('<afile>:t') ==? 'gemfile' | call s:syntaxfile() | endif
  autocmd BufNewFile,BufRead [Gg]emfile.lock setf gemfilelock
  autocmd Syntax gemfilelock call s:syntaxlock()
  autocmd FileType gemfilelock    call s:setuplock()
  autocmd User Rails/Gemfile.lock call s:setuplock()
augroup END

" Section: Initialization

function! s:FindBundlerLock(path) abort
  let fn = substitute(s:Absolute(a:path), '/$', '', '')
  let ofn = ""
  let nfn = fn
  while fn !=# ofn && fn !=# '.'
    if s:filereadable(fn . '/Gemfile.lock') && s:filereadable(fn . '/Gemfile')
      return fn . '/Gemfile.lock'
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

function! s:Setup() abort
  let path = s:Absolute()
  if s:Detect(path)
    silent doautocmd User Bundler
  endif
endfunction

function! bundler#ManifestTask(lnum) abort
  if &filetype ==# 'gemfilelock'
    let gem = matchstr(getline(a:lnum), '^ \{4,\}\zs\S\+\ze (.*)$')
  else
    let gem = matchstr(getline(a:lnum), '^ *gem *(\= *[''"]\zs[^''\"]\+\ze[''"]')
  endif
  return len(gem) ? 'update ' . gem : 'install'
endfunction

function! s:ProjectionistDetect() abort
  if s:Detect(get(g:, 'projectionist_file', '')) && !exists('b:bundler_gem')
    let dir = fnamemodify(b:bundler_lock, ':h')
    let dispatch = 'bundle %:s/.*/\=bundler#ManifestTask(exists(''l#'') ? l# : 0)/ --gemfile={project}/Gemfile'
    call projectionist#append(dir, {
          \ '*': s:filereadable(dir . '/config/environment.rb') ? {} :
          \ {'console': 'bundle console'},
          \ 'Gemfile': {'dispatch': dispatch, 'alternate': 'Gemfile.lock'},
          \ 'Gemfile.lock': {'dispatch': dispatch, 'alternate': 'Gemfile'}})
    for projections in bundler#project().projections_list()
      call projectionist#append(fnamemodify(b:bundler_lock, ':h'), copy(projections))
    endfor
  endif
endfunction

augroup bundler
  autocmd!
  autocmd FileType               * call s:Setup()
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call s:Setup() |
        \ endif
  autocmd User ProjectionistDetect call s:ProjectionistDetect()
augroup END

" Section: Project

let s:project_prototype = {}
let s:projects = {}

function! bundler#project(...) abort
  if !a:0 || a:1 is# 0
    let lock = !empty(get(b:, 'bundler_lock', '')) ? b:bundler_lock : s:FindBundlerLock(s:Absolute())
  elseif type(a:1) == type(0)
    let lock = getbufvar(a:1, 'bundler_lock')
    if empty(lock)
      let lock = s:FindBundlerLock(bufname(a:1))
    endif
  elseif type(a:1) == type('') && s:filereadable(a:1 . '/Gemfile.lock')
    let lock = substitute(s:Absolute(a:1), '/$', '', '') . '/Gemfile.lock'
  elseif type(a:1) == type('') && s:filereadable(a:1)
    let lock = s:Absolute(a:1)
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
  if a:0 && a:1 =~# '^/\|^\a\+:'
    let path = join(a:000, '/')
  else
    let path = join([self.root]+a:000,'/')
  endif
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
    let locked = {'git': [], 'gem': [], 'path': []}
    let versions = {}
    let dependencies = {}
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
        if type(get(locked, section)) ==# type([])
          call extend(locked[section], [properties])
        endif
      elseif line =~# '^  \w\+: '
        let properties[matchstr(line, '\w\+')] = matchstr(line, ': \zs.*')
      elseif line =~# '^    [a-zA-Z0-9._-]\+\s\+(\d\+'
        let name = split(line, ' ')[0]
        let ver = substitute(line, '.*(\|).*', '', 'g')
        let properties.versions[name] = ver
        let versions[name] = ver
        let dependencies[name] = []
      elseif line =~# '^      [a-zA-Z0-9._-]\+\s\+('
        let dep = split(line, ' ')[0]
        call add(dependencies[name], dep)
      elseif line =~# '^   \S' && !has_key(locked, section)
        let locked[section] = line[3:-1]
      endif
    endfor
    lockvar! locked versions dependencies
    let self._locked = locked
    let self._versions = versions
    let self._dependencies = dependencies
    let self._lock_time = time
  endif
  return get(self, '_locked', {})
endfunction

function! s:project_paths(...) dict abort
  call self.locked()
  let time = get(self, '_lock_time', -1)
  if a:0 && a:1 ==# 'refresh' || time != -1 && time != get(self, '_path_time', -1)
    let paths = {}

    let chdir = haslocaldir() ? 'lcd' : exists(':tcd') && haslocaldir(-1) ? 'tcd' : 'cd'
    let cwd = getcwd()

    let gem_paths = []
    if exists('$GEM_PATH')
      let gem_paths = split($GEM_PATH, has('win32') ? ';' : ':')
    endif

    " Explicitly setting $PATH means /etc/zshenv on OS X can't touch it.
    if executable('env')
      let prefix = 'env PATH='.s:shellesc($PATH) . ' JRUBY_OPTS='.s:shellesc('--dev') . ' '
    else
      let prefix = ''
    endif

    try
      exe chdir fnameescape(self.real())

      if empty(gem_paths)
        let gem_paths = split(system(prefix.'ruby -rrbconfig -rrubygems -e '.s:shellesc('print(([RbConfig::CONFIG["ruby_version"]] + Gem.path).join(%(;)))')), ';')

        let abi_version = empty(gem_paths) ? '' : remove(gem_paths, 0)
      else
        let abi_version = system(prefix.'ruby -rrbconfig --disable-rubygems -e '.s:shellesc('print RbConfig::CONFIG["ruby_version"]'))
      endif

      exe chdir fnameescape(cwd)
    finally
      exe chdir fnameescape(cwd)
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
    let sorted = sort(values(paths))
    let index = index(sorted, fnamemodify(self.lock(), ':h'))
    if index > 0
      call insert(sorted, remove(sorted,index))
    endif
    lockvar! paths sorted
    let self._path_time = time
    let self._paths = paths
    let self._sorted = sorted
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
  if has_key(self, '_projections_list')
    return self._projections_list
  endif
  let list = []
  let gem_projections = type(get(g:, 'gem_projections')) == type({}) ? g:gem_projections : {}
  for name in empty(gem_projections) ? [] : keys(self.versions())
    if type(get(gem_projections, name)) ==# type({})
      call add(list, gem_projections[name])
    elseif type(get(gem_projections, name)) ==# type('') && !empty(gem_projections[name])
      let file = gem_projections[name]
      if file !~# '^\a\+:\|^/'
        if !has_key(self.paths(), name)
          continue
        endif
        let file = self.paths()[name] . '/' . file
      endif
      if file =~# '/$'
        let file .= 'projections.json'
      endif
      if filereadable(file)
        call add(list, file)
      endif
    endif
  endfor
  lockvar! list
  let self._projections_list = list
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

function! bundler#GemPaths(...) abort
  let project = call('bundler#project', a:000)
  return empty(project) ? {} : call(project.paths, a:000[1:-1], project)
endfunction

function! bundler#GemVersions(...) abort
  let project = call('bundler#project', a:000)
  return empty(project) ? {} : project.versions()
endfunction

" Section: Bundle

function! s:push_chdir() abort
  if !exists("s:command_stack") | let s:command_stack = [] | endif
  let chdir = haslocaldir() ? 'lcd' : exists(':tcd') && haslocaldir(-1) ? 'tcd' : 'cd'
  call add(s:command_stack,chdir . fnameescape(getcwd()))
  exe chdir fnameescape(s:project().real())
endfunction

function! s:pop_command() abort
  if exists("s:command_stack") && len(s:command_stack) > 0
    exe remove(s:command_stack,-1)
  endif
endfunction

function! s:Bundle(bang, mods, arg) abort
  let mods = a:mods ==# '<mods>' ? '' : a:mods
  if a:arg =~# '^open\>'
    if mods !~# '\<\%(aboveleft\|belowright\|leftabove\|rightbelow\|topleft\|botright\|tab\)\>'
      let mods .= ' ' . get(g:, 'bundler_open_default_modifier', 'tab')
    endif
    return s:Open(mods . ' split', matchstr(a:arg, '\s\+["'']\=\zs.*[^"'']'), a:bang !=# '!')
  endif
  let delegate = matchstr(a:arg, '^:\zs\%(e\%[dit]\|v\=sp\%[lit]\|tabe\%[dit]\|ped\%[it]\|s\=find\=\|tabf\%[ind]\|dr\%[op]\|s\=view\=\|[tl]\=cd\|[tl]\=chd\%[ir]\)\>!\=')
  if len(delegate)
    return s:Open(mods . ' ' . delegate, matchstr(a:arg, '\s\+\zs.*'), 0)
  endif
  if a:arg =~# '^\%(init\|gem\)\>'
    let project = {}
  else
    let project = bundler#project()
    if empty(project)
      return 'echoerr ' . string('Bundler manifest not found')
    endif
  endif
  let old_makeprg = &l:makeprg
  let old_errorformat = &l:errorformat
  let old_compiler = get(b:, 'current_compiler', '')
  let cd = haslocaldir() ? 'lcd' : exists(':tcd') && haslocaldir(-1) ? 'tcd' : 'cd'
  let cwd = getcwd()
  try
    if !empty(project)
      execute cd fnameescape(project.real())
    endif
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
  return bundler#Complete(a:A, a:L, a:P, bundler#project())
endfunction

let s:completions = {
      \ 'install': '',
      \ 'update': 'gem',
      \ 'package': '',
      \ 'exec': '',
      \ 'config': '',
      \ 'add': 'gem',
      \ 'binstubs': 'gem',
      \ 'check': '',
      \ 'show': 'gem',
      \ 'outdated': 'gem',
      \ 'open': 'gem',
      \ 'viz': '',
      \ 'init': '',
      \ 'gem': 'dir',
      \ 'platform': '',
      \ 'clean': '',
      \ 'doctor': '',
      \ 'remove': 'gem',
      \ }

function! bundler#Complete(A, L, P, ...) abort
  let project = a:0 ? a:1 : bundler#project(getcwd())
  let cmd = matchstr(a:L, '\C\u\w*[! ] *\zs\S\+\ze ')
  if empty(cmd)
    return s:completion_filter(keys(s:completions), a:A)
  elseif get(s:completions, cmd, '') is# 'gem' || cmd =~# '^:'
    return s:completion_filter(empty(project) ? [] : keys(project.paths()), a:A)
  elseif cmd ==# 'exec'
    return getcompletion(a:A, a:L =~# '\u\w*[! ] *\zs\S\+ \+\S*$' ? 'shellcmd' : 'file')
  elseif has_key(s:completions, cmd)
    return getcompletion(a:A, s:completions[cmd])
  else
    return []
  endif
endfunction

command! -bar -bang -nargs=? -complete=customlist,s:BundleComplete Bundle exe s:Bundle('<bang>', '<mods>', <q-args>)

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
augroup END

" Section: Bopen

function! s:Open(cmd,gem,lcd) abort
  if empty(bundler#project())
    return 'echoerr ' . string('Bundler manifest not found')
  endif
  let exec = substitute(a:cmd, '^<mods> ', '', '')
  let gem = matchstr(a:gem, '^[^/]*')
  if empty(gem) && a:cmd =~# '\<[tl]\=ch\=d'
    return exec . ' ' . fnameescape(s:project().real())
  elseif empty(gem) && a:lcd
    return exec . ' ' . fnameescape(s:project().manifest())
  elseif empty(gem)
    return exec . ' ' . fnameescape(s:project().lock())
  else
    if !has_key(s:project().paths(), gem)
      call s:project().paths('refresh')
    endif
    if !has_key(s:project().paths(), gem)
      if has_key(s:project().versions(), gem)
        let v:errmsg = "Gem \"".gem."\" is in bundle but not installed"
      else
        let v:errmsg = "Gem \"".gem."\" is not in bundle"
      endif
      return 'echoerr v:errmsg'
    endif
    let gempath = fnameescape(s:project().paths()[gem])
    let path = gempath . fnameescape(matchstr(a:gem, '/.*'))
    if !a:lcd
      let exec .= ' ' . path
    elseif a:cmd =~# '\<pe'
      let exec .= ' +lcd\ ' . gempath . ' ' . path
    else
      let exec .= ' ' . path . '|lcd ' . gempath
    endif
    return exec
  endif
endfunction

function! s:OpenComplete(A,L,P) abort
  let project = bundler#project()
  if empty(project)
    return []
  endif
  return s:completion_filter(keys(project.paths()), a:A)
endfunction

command! -bar -bang -nargs=? -complete=customlist,s:OpenComplete Bopen    exe s:Open('<mods> edit<bang>', <q-args>,1)
command! -bar -bang -nargs=? -complete=customlist,s:OpenComplete Bedit    exe s:Open('<mods> edit<bang>', <q-args>,0)
command! -bar -bang -nargs=? -complete=customlist,s:OpenComplete Bsplit   exe s:Open('<mods> split', <q-args>, <bang>1)
command! -bar -bang -nargs=? -complete=customlist,s:OpenComplete Bvsplit  exe s:Open('<mods> vsplit', <q-args>, <bang>1)
command! -bar -bang -nargs=? -complete=customlist,s:OpenComplete Btabedit exe s:Open('<mods> tabedit', <q-args>, <bang>1)
command! -bar -bang -nargs=? -complete=customlist,s:OpenComplete Bpedit   exe s:Open('<mods> pedit', <q-args>, <bang>1)

" Section: Paths

function! s:build_path_option(paths,suffix) abort
  return join(map(copy(a:paths),'",".escape(s:shellslash(v:val."/".a:suffix),", ")'),'')
endfunction

function! s:AlterPaths(project, buf) abort
  if !empty(a:project)
    let gem = getbufvar(a:buf, 'bundler_gem')
    if empty(gem)
      let new = a:project.sorted()
    else
      let new = sort(values(a:project.dependencies(gem)))
    endif
    let old = type(getbufvar(a:buf, 'bundler_paths')) == type([]) ? getbufvar(a:buf, 'bundler_paths') : []
    for [option, suffix] in
          \ [['tags', get(a:project, '_tags', 'tags')], ['path', 'lib']]
      let value = getbufvar(a:buf, '&'.option)
      let tail = matchstr(value, '\%(,\.\)\=\%(,,\)\=$')
      let value = strpart(value, 0, len(value) - len(tail))
      if !empty(old)
        let drop = s:build_path_option(old, suffix)
        let index = stridx(value, drop)
        if index > 0
          let value = value[0:index-1] . value[index+strlen(drop):-1]
        endif
      endif
      call setbufvar(a:buf, '&'.option, value.s:build_path_option(new, suffix) . tail)
    endfor
    call setbufvar(a:buf, 'bundler_paths', new)
  endif
endfunction

function! s:project_alter_buffer_paths() dict abort
  for bufnr in range(1,bufnr('$'))
    if getbufvar(bufnr, 'bundler_lock') ==# self.lock() && getbufvar(bufnr, '&suffixesadd') =~# '\.rb\>'
      call s:AlterPaths(self, bufnr)
    endif
    if getbufvar(bufnr, '&syntax') ==# 'gemfilelock'
      call setbufvar(bufnr, '&syntax', 'gemfilelock')
    endif
  endfor
endfunction

call s:add_methods('project',['alter_buffer_paths'])

augroup bundler_path
  autocmd!
  autocmd User Bundler
        \ if &suffixesadd =~# '\.rb\>' |
        \   call s:AlterPaths(bundler#project(), bufnr('')) |
        \ endif
augroup END

" vim:set sw=2 sts=2:
