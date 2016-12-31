" rake.vim - It's like rails.vim without the rails
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      2.0
" GetLatestVimScripts: 3669 1 :AutoInstall: rake.vim

if exists('g:loaded_rake') || &cp || v:version < 700
  finish
endif
let g:loaded_rake = 1

if !exists('g:dispatch_compilers')
  let g:dispatch_compilers = {}
endif
let g:dispatch_compilers['bundle exec'] = ''
let g:dispatch_compilers['ruby bin/rake'] = 'rake'
let g:dispatch_compilers['ruby -Itest'] = 'rubyunit'

" Utility {{{1

function! s:function(name) abort
  return function(substitute(a:name,'^s:',matchstr(expand('<sfile>'), '<SNR>\d\+_'),''))
endfunction

function! s:shellslash(path) abort
  if exists('+shellslash') && !&shellslash
    return substitute(a:path, '\\', '/', 'g')
  else
    return a:path
  endif
endfunction

function! s:throw(string) abort
  let v:errmsg = 'rake: '.a:string
  throw v:errmsg
endfunction

function! s:add_methods(namespace, method_names) abort
  for name in a:method_names
    let s:{a:namespace}_prototype[name] = s:function('s:'.a:namespace.'_'.name)
  endfor
endfunction

let s:abstract_prototype = {}

" }}}1
" Initialization {{{1

function! s:find_root(path) abort
  let root = s:shellslash(simplify(fnamemodify(a:path, ':p:s?[\/]$??')))
  for p in [$GEM_HOME] + split($GEM_PATH,':')
    if p !=# '' && s:shellslash(p.'/gems/') ==# (root)[0 : strlen(p)+5]
      return simplify(s:shellslash(p.'/gems/')).matchstr(root[strlen(p)+6:-1],'[^\\/]*')
    endif
  endfor
  let previous = ''
  while root !=# previous && root !=# '/'
    if filereadable(root.'/Rakefile') || (isdirectory(root.'/lib') && filereadable(root.'/Gemfile'))
      if filereadable(root.'/config/environment.rb')
        return ''
      else
        return root
      endif
    elseif root =~# '[\/]gems[\/][0-9.]\+[\/]gems[\/][[:alnum:]._-]\+$'
      return root
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
  return ''
endfunction

function! s:Detect(path) abort
  if !exists('b:rake_root')
    let dir = s:find_root(a:path)
    if dir !=# ''
      let b:rake_root = dir
    endif
  endif
endfunction

function! s:Setup(path) abort
  call s:Detect(a:path)
  if exists('b:rake_root')
    silent doautocmd User Rake
  endif
endfunction

augroup rake
  autocmd!
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call s:Setup(expand('<amatch>:p')) |
        \ endif
  autocmd FileType * call s:Setup(expand('%:p'))
  autocmd User NERDTreeInit,NERDTreeNewRoot call s:Setup(b:NERDTreeRoot.path.str())
  autocmd VimEnter * if expand('<amatch>')==''|call s:Setup(getcwd())|endif
augroup END

" }}}1
" Projectionist {{{

let s:projections = {
      \ '*': {},
      \ 'lib/*.rb': {'type': 'lib', 'alternate': [
      \   'test/{}_test.rb', 'test/lib/{}_test.rb', 'test/unit/{}_test.rb',
      \   'test/{dirname}/test_{basename}.rb',
      \   'spec/{}_spec.rb', 'spec/lib/{}_spec.rb', 'spec/unit/{}_spec.rb']},
      \ 'test/test_helper.rb': {'type': 'test'},
      \ 'test/*_test.rb': {
      \   'type': 'test',
      \   'alternate': 'lib/{}.rb'},
      \ 'test/lib/*_test.rb': {'alternate': 'lib/{}.rb'},
      \ 'test/unit/*_test.rb': {'alternate': 'lib/{}.rb'},
      \ 'test/**/test_*.rb': {
      \   'type': 'test',
      \   'alternate': 'lib/{}.rb'},
      \ 'spec/spec_helper.rb': {'type': 'spec'},
      \ 'spec/*_spec.rb': {
      \   'type': 'spec',
      \   'alternate': 'lib/{}.rb'},
      \ 'spec/lib/*_spec.rb': {'alternate': 'lib/{}.rb'},
      \ 'spec/unit/*_spec.rb': {'alternate': 'lib/{}.rb'},
      \ 'rakelib/*.rake': {'type': 'task'},
      \ 'Rakefile': {'type': 'task'}}

function! s:binstub(root, cmd) abort
  if !has('win32') && a:root !~# '\s' && executable(a:root.'/bin/'.a:cmd)
    return ['bin/'.a:cmd]
  elseif filereadable(a:root.'/Gemfile')
    return ['bundle', 'exec', a:cmd]
  else
    return [a:cmd]
  endif
endfunction

function! s:ProjectionistDetect() abort
  call s:Detect(get(g:, 'projectionist_file', ''))
  if exists('b:rake_root')
    let projections = deepcopy(s:projections)
    if isdirectory(b:rake_root.'/test')
      let test = 1
    endif
    if isdirectory(b:rake_root.'/spec')
      let spec = 1
    endif
    let projections['*'].make = split(s:project().makeprg())
    let projections['Rakefile'].dispatch = projections['*'].make
    let projections['rakelib/*.rake'].dispatch = projections['*'].make + ['{}']
    let ruby = s:binstub(b:rake_root, 'ruby')
    if ruby ==# ['ruby']
      let projections['test/*.rb'] = {'dispatch': ruby + ['-Itest', '-Ilib', '{file}']}
    else
      let projections['test/*.rb'] = {'dispatch': ruby + ['-Itest', '{file}']}
    endif
    let projections['spec/*_spec.rb'].dispatch = s:binstub(b:rake_root, 'rspec') + ['{file}']
    call filter(projections['lib/*.rb'].alternate, 'exists(v:val[0:3])')
    call filter(projections, 'v:key[4] !=# "/" || exists(v:key[0:3])')
    let gemspec = fnamemodify(get(split(glob(b:rake_root.'/*.gemspec'), "\n"), 0, 'Gemfile'), ':t')
    let projections[gemspec] = {'type': 'lib'}
    if gemspec !=# 'Gemfile'
      let projections[gemspec].dispatch = ['gem', 'build', '{file}']
    endif
    call projectionist#append(b:rake_root, projections)
    let secondary = {
          \ 'test/*_test.rb': exists('test') ? {'type': 'spec'} : {},
          \ 'spec/*_spec.rb': exists('spec') ? {'type': 'test'} : {}}
    call filter(secondary, '!empty(v:val)')
    if !empty(secondary)
      call projectionist#append(b:rake_root, secondary)
    endif
  endif
endfunction

augroup rake_projectionist
  autocmd!
  autocmd User ProjectionistDetect call s:ProjectionistDetect()
augroup END

" }}}1
" Project {{{1

let s:project_prototype = {}
let s:projects = {}

function! rake#project(...) abort
  let dir = a:0 ? a:1 : (exists('b:rake_root') && b:rake_root !=# '' ? b:rake_root : s:find_root(expand('%:p')))
  if dir !=# ''
    if has_key(s:projects, dir)
      let project = get(s:projects, dir)
    else
      let project = {'_root': dir}
      let s:projects[dir] = project
    endif
    return extend(extend(project, s:project_prototype, 'keep'), s:abstract_prototype, 'keep')
  endif
  return {}
endfunction

function! s:project(...) abort
  let project = call('rake#project', a:000)
  if !empty(project)
    return project
  endif
  call s:throw('not a rake project: '.expand('%:p'))
endfunction

function! s:project_path(...) dict abort
  return join([self._root]+a:000,'/')
endfunction

function! s:project_ruby_include_path() dict abort
  if !has_key(self, '_ruby_include_path')
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    let cwd = getcwd()
    try
      execute cd fnameescape(self.path())
      let self._ruby_include_path = system('ruby -rrbconfig -e "print RbConfig::CONFIG[\"rubyhdrdir\"] || RbConfig::CONFIG[\"topdir\"]"')
    finally
      execute cd fnameescape(cwd)
    endtry
  endif
  return self._ruby_include_path
endfunction

call s:add_methods('project',['path','ruby_include_path'])

" }}}1
" Rake {{{1

function! s:project_makeprg() dict abort
  if executable(self.path('bin/rake'))
    return 'bin/rake'
  elseif filereadable(self.path('bin/rake'))
    return 'ruby bin/rake'
  elseif filereadable(self.path('Gemfile'))
    return 'bundle exec rake'
  else
    return 'rake'
  endif
endfunction

call s:add_methods('project', ['makeprg'])

function! s:Rake(bang, arg) abort
  let old_makeprg = &l:makeprg
  let old_errorformat = &l:errorformat
  let old_compiler = get(b:, 'current_compiler', '')
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  try
    execute cd fnameescape(s:project().path())
    if !empty(findfile('compiler/rake.vim', escape(&rtp, ' ')))
      compiler rake
    else
      let &l:errorformat = '%+I%.%#'
      let b:current_compiler = 'rake'
    endif
    let &l:makeprg = s:project().makeprg()
    if exists(':Make') == 2
      execute 'Make'.a:bang.' '.a:arg
    else
      execute 'make! '.a:arg
      if a:bang !=# '!'
        return 'cwindow'
      endif
    endif
    return ''
  finally
    let &l:errorformat = old_errorformat
    let &l:makeprg = old_makeprg
    let b:current_compiler = old_compiler
    if empty(old_compiler)
      unlet! b:current_compiler
    endif
    execute cd fnameescape(cwd)
  endtry
endfunction

function! s:RakeComplete(A, L, P, ...) abort
  let project = a:0 ? a:1 : s:project()
  return projectionist#completion_filter(project.tasks(), a:A, ':')
endfunction

function! CompilerComplete_rake(A, L, P) abort
  let path = findfile('Rakefile', escape(getcwd(), ' ,;').';')
  if empty(path)
    return []
  endif
  let path = fnamemodify(path, ':p:h')
  if path ==# get(b:, 'rails_root', 'x') && exists('*rails#complete_rake')
    return rails#complete_rake(a:A, a:L, a:P)
  else
    return s:RakeComplete(a:A, a:L, a:P, s:project(path))
  endif
endfunction

function! s:project_tasks() dict abort
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  try
    execute cd fnameescape(self.path())
    let lines = split(system(self.makeprg() . ' -T'), "\n")
  finally
    execute cd fnameescape(cwd)
  endtry
  if v:shell_error != 0
    return []
  endif
  call map(lines,'matchstr(v:val,"^rake\\s\\+\\zs\\S*")')
  call filter(lines,'v:val != ""')
  return lines
endfunction

call s:add_methods('project', ['tasks'])

function! s:define_rake() abort
  command! -buffer -bar -bang -nargs=? -complete=customlist,s:RakeComplete Rake
        \ execute s:Rake('<bang>',<q-args>)
endfunction

augroup rake_command
  autocmd!
  autocmd User Rake call s:define_rake()
augroup END

" }}}1
" Path {{{1

if !exists('g:did_load_ftplugin')
  filetype plugin on
endif

augroup rake_path
  autocmd!
  autocmd User Rake
        \ if &suffixesadd =~# '\.rb\>' && stridx(&path, escape(s:project().path('lib'),', ')) < 0 |
        \   let &l:path = escape(s:project().path('lib'),', ')
        \     . ',' . escape(s:project().path('ext'),', ') . ',' . &path |
        \ endif
  autocmd User Rake
        \ if &filetype ==# 'c' || &filetype ==# 'cpp' |
        \   let &l:path = &path . ',' . escape(s:project().ruby_include_path(),', ') |
        \   let &l:tags = &tags . ',' . escape(s:project().ruby_include_path().'/tags',', ') |
        \ endif
augroup END

" }}}1

" vim:set sw=2 sts=2:
