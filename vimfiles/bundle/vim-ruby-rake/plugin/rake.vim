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

function! s:fcall(fn, path, ...) abort
  let ns = matchstr(a:path, '^\a\a\+\ze:')
  if len(ns) && exists('*' . ns . '#' . a:fn)
    return call(ns . '#' . a:fn, [a:path] + a:000)
  else
    return call(a:fn, [a:path] + a:000)
  endif
endfunction

function! s:real(file) abort
  let pre = substitute(matchstr(a:file, '^\a\a\+\ze:'), '^.', '\u&', '')
  if empty(pre)
    let path = a:file
  elseif exists('*' . pre . 'Real')
    let path = {pre}Real(a:file)
  elseif exists('*' . pre . 'Path')
    let path = {pre}Path(a:file)
  else
    return ''
  endif
  return exists('+shellslash') && !&shellslash ? tr(path, '/', '\') : path
endfunction

function! s:has(root, file) abort
  return s:fcall(a:file =~# '/$' ? 'isdirectory' : 'filereadable', a:root . '/' . a:file)
endfunction

function! s:find_root(path) abort
  let root = s:shellslash(fnamemodify(a:path, ':p:s?[\/]$??'))
  let previous = ''
  while root !=# previous && root !~# '^\%(\a\+:\)\=/*$\|^\.$'
    if s:has(root, 'Rakefile') || (s:has(root, 'lib/') && s:has(root, 'Gemfile'))
      if s:has(root, 'config/environment.rb') && s:has(root, 'app/')
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
    return 'bin/'.a:cmd
  elseif filereadable(a:root.'/Gemfile')
    return 'bundle exec '.a:cmd
  else
    return a:cmd
  endif
endfunction

function! s:ProjectionistDetect() abort
  call s:Detect(get(g:, 'projectionist_file', ''))
  if exists('b:rake_root')
    let projections = deepcopy(s:projections)
    let test = s:has(b:rake_root, 'test/')
    let spec = s:has(b:rake_root, 'spec/')
    let real_root = s:real(b:rake_root)
    if len(real_root)
      let projections['*'].make = s:project().makeprg()
      let projections['Rakefile'].dispatch = projections['*'].make
      let projections['rakelib/*.rake'].dispatch = projections['*'].make . ' {}'
      let ruby = s:binstub(real_root, 'ruby')
      if ruby ==# 'ruby'
        let projections['test/*.rb'] = {'dispatch': ruby . ' -Itest -Ilib {file}'}
      else
        let projections['test/*.rb'] = {'dispatch': ruby . ' -Itest {file}'}
      endif
      let projections['spec/*_spec.rb'].dispatch =
            \ s:binstub(real_root, 'rspec') . ' {file}%:s/.*/\=exists("l#") ? ":".l# : " "/{vim|nothing}'
    endif
    call filter(projections['lib/*.rb'].alternate, 'get(l:, v:val[0:3])')
    call filter(projections, 'v:key[4] !=# "/" || get(l:, v:key[0:3])')
    let gemspec = fnamemodify(get(split(s:fcall('glob', b:rake_root.'/*.gemspec'), "\n"), 0, 'Gemfile'), ':t')
    let projections[gemspec] = {'type': 'lib'}
    if gemspec !=# 'Gemfile'
      let projections[gemspec].dispatch = 'gem build {file}'
    endif
    call projectionist#append(b:rake_root, projections)
    let secondary = {
          \ 'test/*_test.rb': test ? {'type': 'spec'} : {},
          \ 'spec/*_spec.rb': extend(
          \ len(real_root) ? {"dispatch": s:binstub(real_root, 'rspec') . ' {file}'} : {},
          \ spec ? {'type': 'test'} : {})}
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

function! s:project_real(...) dict abort
  return s:real(join([self._root]+a:000,'/'))
endfunction

function! s:project_ruby_include_path() dict abort
  if !has_key(self, '_ruby_include_path') && len(self.real())
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    let cwd = getcwd()
    try
      execute cd fnameescape(self.real())
      let self._ruby_include_path = system('ruby -rrbconfig -e ' . shellescape(
            \ 'print RbConfig::CONFIG["rubyhdrdir"] || RbConfig::CONFIG["topdir"]'))
    finally
      execute cd fnameescape(cwd)
    endtry
  endif
  return get(self, '_ruby_include_path', '')
endfunction

call s:add_methods('project',['path','real','ruby_include_path'])

" }}}1
" Rake {{{1

function! s:project_makeprg() dict abort
  if executable(self.real('bin/rake'))
    return 'bin/rake'
  elseif filereadable(self.real('bin/rake'))
    return 'ruby bin/rake'
  elseif filereadable(self.real('Gemfile'))
    return 'bundle exec rake'
  elseif len(self.real())
    return 'rake'
  else
    return ''
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
    execute cd fnameescape(s:project().real())
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
  let root = a:0 ? a:1 : b:rake_root
  return projectionist#completion_filter(s:Tasks(root), a:A, ':')
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
    return s:RakeComplete(a:A, a:L, a:P, path)
  endif
endfunction

function! s:Tasks(...) abort
  let project = s:project(a:0 ? a:1 : b:rake_root)
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  try
    execute cd fnameescape(project.real())
    let lines = split(system(project.makeprg() . ' -T'), "\n")
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

function! s:define_rake() abort
  command! -buffer -bar -bang -nargs=? -complete=customlist,s:RakeComplete Rake
        \ execute s:Rake('<bang>',<q-args>)
endfunction

augroup rake_command
  autocmd!
  autocmd User Rake if len(s:project().makeprg()) | call s:define_rake() | endif
augroup END

" }}}1
" Path {{{1

if !exists('g:did_load_ftplugin')
  filetype plugin on
endif

function! s:path_addition(file) abort
  return escape(substitute(s:project().path(a:file), '^\a\a\+:', '+&', ''), ', ')
endfunction

augroup rake_path
  autocmd!
  autocmd User Rake
        \ if &suffixesadd =~# '\.rb\>' && stridx(&path, s:path_addition('lib')) < 0 |
        \   let &l:path = s:path_addition('lib')
        \     . ',' . s:path_addition('ext') . ',' . &path |
        \ endif
  autocmd User Rake
        \ if len(s:project().ruby_include_path()) && &filetype =~# '^c\%(pp\)\=$' |
        \   let &l:path = &path . ',' . escape(s:project().ruby_include_path(),', ') |
        \   let &l:tags = &tags . ',' . escape(s:project().ruby_include_path().'/tags',', ') |
        \ endif
augroup END

" }}}1

" vim:set sw=2 sts=2:
