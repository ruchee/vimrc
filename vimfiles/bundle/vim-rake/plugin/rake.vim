" rake.vim - It's like rails.vim without the rails
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.2
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

function! s:sub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'')
endfunction

function! s:gsub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'g')
endfunction

function! s:shellesc(arg) abort
  if a:arg =~ '^[A-Za-z0-9_/.-]\+$'
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

function! s:shellslash(path)
  if exists('+shellslash') && !&shellslash
    return s:gsub(a:path,'\\','/')
  else
    return a:path
  endif
endfunction

function! s:fuzzyglob(arg)
  return s:gsub(s:gsub(a:arg,'[^/.]','[&]*'),'%(/|^)\.@!|\.','&*')
endfunction

function! s:completion_filter(results,A)
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
  let v:errmsg = 'rake: '.a:string
  throw v:errmsg
endfunction

function! s:warn(str)
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
function! s:command(definition, ...) abort
  let s:commands += [[a:definition, a:0]]
endfunction

function! s:define_commands()
  for [command, concede] in s:commands
    if !concede || !exists('g:loaded_projectionist')
      exe 'command! -buffer '.command
    endif
  endfor
endfunction

augroup rake_utility
  autocmd!
  autocmd User Rake call s:define_commands()
augroup END

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
      \ 'lib/*.rb': {'command': 'lib', 'alternate': [
      \   'test/{}_test.rb', 'test/lib/{}_test.rb', 'test/unit/{}_test.rb',
      \   'spec/{}_spec.rb', 'spec/lib/{}_spec.rb', 'spec/unit/{}_spec.rb']},
      \ 'test/test_helper.rb': {'command': 'test'},
      \ 'test/*_test.rb': {
      \   'command': 'test',
      \   'alternate': 'lib/{}.rb'},
      \ 'test/lib/*_test.rb': {'alternate': 'lib/{}.rb'},
      \ 'test/unit/*_test.rb': {'alternate': 'lib/{}.rb'},
      \ 'spec/spec_helper.rb': {'command': 'spec'},
      \ 'spec/*_spec.rb': {
      \   'command': 'spec',
      \   'alternate': 'lib/{}.rb'},
      \ 'spec/lib/*_spec.rb': {'alternate': 'lib/{}.rb'},
      \ 'spec/unit/*_spec.rb': {'alternate': 'lib/{}.rb'},
      \ 'rakelib/*.rake': {'command': 'task'},
      \ 'Rakefile': {'command': 'task'}}

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
    let projections['test/*.rb'] = {'dispatch': s:binstub(b:rake_root, 'ruby') + ['-Itest', '{file}']}
    let projections['spec/*_spec.rb'].dispatch = s:binstub(b:rake_root, 'rspec') + ['{file}']
    call filter(projections['lib/*.rb'].alternate, 'exists(v:val[0:3])')
    call filter(projections, 'v:key[4] !=# "/" || exists(v:key[0:3])')
    let gemspec = fnamemodify(get(split(glob(b:rake_root.'/*.gemspec'), "\n"), 0, 'Gemfile'), ':t')
    let projections[gemspec] = {'command': 'lib'}
    if gemspec !=# 'Gemfile'
      let projections[gemspec].dispatch = ['gem', 'build', '{file}']
    endif
    call projectionist#append(b:rake_root, projections)
    call projectionist#append(b:rake_root, {
          \ 'test/*_test.rb': {'command': 'spec'},
          \ 'spec/*_spec.rb': {'command': 'test'}})
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

function! s:project(...)
  let project = call('rake#project', a:000)
  if !empty(project)
    return project
  endif
  call s:throw('not a rake project: '.expand('%:p'))
endfunction

function! s:project_path(...) dict abort
  return join([self._root]+a:000,'/')
endfunction

call s:add_methods('project',['path'])

function! s:project_dirglob(base) dict abort
  let base = s:sub(a:base,'^/','')
  let matches = split(glob(self.path(s:gsub(base,'/','*&').'*/')),"\n")
  call map(matches,'v:val[ strlen(self.path())+(a:base !~ "^/") : -1 ]')
  return matches
endfunction

function! s:project_has_file(file) dict
  return filereadable(self.path(a:file))
endfunction

function! s:project_has_directory(file) dict
  return isdirectory(self.path(a:file))
endfunction

function! s:project_first_file(...) dict abort
  for file in a:000
    if s:project().has_file(file)
      return file
    endif
  endfor
  for file in a:000
    if s:project().has_directory(matchstr(file,'^[^/]*'))
      return file
    endif
  endfor
  return a:000[0]
endfunction

call s:add_methods('project',['dirglob','has_file','has_directory','first_file'])

" }}}1
" Buffer {{{1

let s:buffer_prototype = {}

function! s:buffer(...) abort
  let buffer = {'#': bufnr(a:0 ? a:1 : '%')}
  call extend(extend(buffer,s:buffer_prototype,'keep'),s:abstract_prototype,'keep')
  if buffer.getvar('rake_root') !=# ''
    return buffer
  endif
  call s:throw('not a rake project: '.expand('%:p'))
endfunction

function! rake#buffer(...) abort
  return s:buffer(a:0 ? a:1 : '%')
endfunction

function! s:buffer_getvar(var) dict abort
  return getbufvar(self['#'],a:var)
endfunction

function! s:buffer_setvar(var,value) dict abort
  return setbufvar(self['#'],a:var,a:value)
endfunction

function! s:buffer_getline(lnum) dict abort
  return getbufline(self['#'],a:lnum)[0]
endfunction

function! s:buffer_project() dict abort
  return s:project(self.getvar('rake_root'))
endfunction

function! s:buffer_name() dict abort
  return self.path()[strlen(self.project().path())+1 : -1]
endfunction

function! s:buffer_path() dict abort
  let bufname = bufname(self['#'])
  return s:shellslash(bufname == '' ? '' : fnamemodify(bufname,':p'))
endfunction

function! s:buffer_relative() dict abort
  return self.name()
endfunction

function! s:buffer_absolute() dict abort
  return self.path()
endfunction

call s:add_methods('buffer',['getvar','setvar','getline','project','name','path','relative','absolute'])

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
  if exists('*projectionist#completion_filter')
    return projectionist#completion_filter(project.tasks(), a:A, ':')
  else
    return s:completion_filter(project.tasks(), a:A)
  endif
endfunction

function! CompilerComplete_rake(A, L, P)
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

call s:command("-bar -bang -nargs=? -complete=customlist,s:RakeComplete Rake :execute s:Rake('<bang>',<q-args>)")

" }}}1
" Cd, Lcd {{{1

function! s:DirComplete(A,L,P) abort
  return s:project().dirglob(a:A)
endfunction

if exists('g:rake_legacy')
  call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Rcd  :Cd<bang> <args>")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Rlcd :Lcd<bang> <args>")
else
  call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Rcd  echoerr ':Rcd is deprecated. Use :Cd or let g:rake_legacy = 1 in vimrc'")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Rlcd echoerr ':Rlcd is deprecated. Use :Lcd or let g:rake_legacy = 1 in vimrc'")
endif
call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Cd   :cd<bang>  `=s:project().path(<q-args>)`", 1)
call s:command("-bar -bang -nargs=? -complete=customlist,s:DirComplete Lcd  :lcd<bang> `=s:project().path(<q-args>)`", 1)

" }}}1
" A {{{1

function! s:buffer_related() dict abort
  if self.name() =~# '^lib/'
    let bare = s:sub(self.name()[4:-1],'\.rb$','')
    return s:project().first_file(
          \'test/'.bare.'_test.rb',
          \'spec/'.bare.'_spec.rb',
          \'test/lib/'.bare.'_test.rb',
          \'spec/lib/'.bare.'_spec.rb',
          \'test/unit/'.bare.'_test.rb',
          \'spec/unit/'.bare.'_spec.rb')
  elseif self.name() =~# '^\(test\|spec\)/.*_\1\.rb$'
    return s:project().first_file(
      \'lib/'.self.name()[5:-9].'.rb',
      \self.name()[5:-9].'.rb')
  elseif self.name() ==# 'Gemfile'
    return 'Gemfile.lock'
  elseif self.name() ==# 'Gemfile.lock'
    return 'Gemfile'
  endif
  return ''
endfunction

call s:add_methods('buffer',['related'])

function! s:project_relglob(path,glob,...) dict
  if exists("+shellslash") && ! &shellslash
    let old_ss = &shellslash
  endif
  try
    let &shellslash = 1
    let path = a:path
    if path !~ '^/' && path !~ '^\w:'
      let path = self.path(path)
    endif
    let suffix = a:0 ? a:1 : ''
    let full_paths = split(glob(path.a:glob.suffix),"\n")
    let relative_paths = []
    for entry in full_paths
      if suffix == '' && isdirectory(entry) && entry !~ '/$'
        let entry .= '/'
      endif
      let relative_paths += [entry[strlen(path) : -strlen(suffix)-1]]
    endfor
    return relative_paths
  finally
    if exists("old_ss")
      let &shellslash = old_ss
    endif
  endtry
endfunction

call s:add_methods('project',['relglob'])

function! s:R(cmd,bang,...) abort
  let cmds = {'E': 'edit', 'S': 'split', 'V': 'vsplit', 'T': 'tabedit', 'D': 'read'}
  let cmd = cmds[a:cmd] . a:bang
  try
    if a:0
      let goal = s:project().path(a:1)
    else
      let related = s:buffer().related()
      if related == ''
        call s:throw('no related file')
      else
        let goal = s:project().path(related)
      endif
    endif
    if goal =~# '[#:]\d\+$'
      let cmd .= ' +'.matchstr(goal,'\d\+$')
      let goal = matchstr(goal,'.*\ze[:#].*$')
    elseif goal =~ '[#:]\w\+[?!=]\=$'
      let cmd .= ' +/^\\s*def\\s\\+'.matchstr(goal,'[:#]\zs.\{-\}$')
      let goal = matchstr(goal,'.*\ze[:#].*$')
    endif
    let parent = fnamemodify(goal,':h')
    if !isdirectory(parent)
      if a:bang ==# '!' && isdirectory(fnamemodify(parent,':h'))
        call mkdir(parent)
      endif
      call s:throw('No such directory: '.parent)
    endif
    return cmd.' '.s:fnameescape(goal)
    return ''
  catch /^rake:/
    return 'echoerr v:errmsg'
  endtry
endfunction

function! s:RComplete(A,L,P) abort
  return s:completion_filter(s:project().relglob('',s:fuzzyglob(a:A).'*'),a:A)
endfunction

if exists('g:rake_legacy')
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete R  :A<bang> <args>|call s:warn(':R is deprecated. Use :A')")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RS :AS<bang> <args>|call s:warn(':RS is deprecated. Use :AS')")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RV :AV<bang> <args>|call s:warn(':RV is deprecated. Use :AV')")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RT :AT<bang> <args>|call s:warn(':RT is deprecated. Use :AT')")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RD :AD<bang> <args>|call s:warn(':RT is deprecated. Use :AD')")
else
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete R  echoerr ':R is deprecated. Use :A or let g:rake_legacy = 1 in vimrc'")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RS echoerr ':RS is deprecated. Use :A or let g:rake_legacy = 1 in vimrc'")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RV echoerr ':RV is deprecated. Use :A or let g:rake_legacy = 1 in vimrc'")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RT echoerr ':RT is deprecated. Use :A or let g:rake_legacy = 1 in vimrc'")
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete RD echoerr ':RD is deprecated. Use :A or let g:rake_legacy = 1 in vimrc'")
endif

if !exists('g:loaded_projectionist')
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete A  :execute s:R('E','<bang>',<f-args>)", 1)
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete AE :execute s:R('E','<bang>',<f-args>)", 1)
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete AS :execute s:R('S','<bang>',<f-args>)", 1)
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete AV :execute s:R('V','<bang>',<f-args>)", 1)
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete AT :execute s:R('T','<bang>',<f-args>)", 1)
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete AD :execute s:R('D','<bang>',<f-args>)", 1)
  call s:command("-bar -bang -nargs=? -complete=customlist,s:RComplete AR :execute s:R('D','<bang>',<f-args>)", 1)
endif

" }}}1
" Elib, etc. {{{1

function! s:navcommand(name) abort
  for type in ['E', 'S', 'V', 'T', 'D']
    if !exists('g:loaded_projectionist')
      call s:command("-bar -bang -nargs=? -complete=customlist,s:R".a:name."Complete ".type.a:name." :execute s:Edit('".type."','<bang>',s:R".a:name."(matchstr(<q-args>,'[^:#]*')).matchstr(<q-args>,'[:#].*'))", 1)
    endif
    if exists('g:rake_legacy')
      call s:command("-bar -bang -nargs=? -complete=customlist,s:R".a:name."Complete R".type.a:name.' '.type.a:name.'<bang> <args>|call s:warn(":R'.type.a:name.' is deprecated. Use :'.type.a:name.'")')
    else
      call s:command("-bar -bang -nargs=? -complete=customlist,s:R".a:name."Complete R".type.a:name." echoerr ':R".type.a:name." is deprecated. Use :".type.a:name." or let g:rake_legacy = 1 in vimrc'")
    endif
  endfor
  if exists('g:rake_legacy')
    call s:command("-bar -bang -nargs=? -complete=customlist,s:R".a:name."Complete R".a:name.' E'.a:name.'<bang> <args>|call s:warn(":R'.a:name.' is deprecated. Use :E'.a:name.'")')
  else
    call s:command("-bar -bang -nargs=? -complete=customlist,s:R".a:name."Complete R".a:name." echoerr ':R".a:name." is deprecated. Use :E".a:name." or let g:rake_legacy = 1 in vimrc'")
  endif
endfunction

function! s:Edit(cmd,bang,file)
  return s:R(a:cmd == '' ? 'E' : a:cmd, a:bang, a:file)
endfunction

function! s:Rlib(file)
  if a:file ==# ''
    return get(s:project().relglob('','*.gemspec'),0,'Gemfile')
  elseif a:file =~# '/$'
    return 'lib/'.a:file
  else
    return 'lib/'.a:file.'.rb'
  endif
endfunction

function! s:RlibComplete(A,L,P)
  return s:completion_filter(s:project().relglob('lib/','**/*','.rb'),a:A)
endfunction

function! s:first_file(choices)
  return call(s:project().first_file,a:choices,s:project())
endfunction

function! s:Rtestorspec(order,file)
  if a:file ==# ''
    return s:first_file(map(copy(a:order),'v:val."/".v:val."_helper.rb"'))
  elseif a:file =~# '/$'
    return s:first_file(map(copy(a:order),'v:val."/".a:file."/"'))
  elseif a:file ==# '.'
    return s:first_file(map(copy(a:order),'v:val."/"'))
  else
    return s:first_file(map(copy(a:order),'v:val."/".a:file."_".v:val.".rb"'))
  endif
endfunction

function! s:Rtest(...)
  return call('s:Rtestorspec',[['test', 'spec']] + a:000)
endfunction

function! s:RtestComplete(A,L,P)
  return s:completion_filter(s:project().relglob('test/','**/*','_test.rb')+s:project().relglob('spec/','**/*','_spec.rb'),a:A)
endfunction

function! s:Rspec(...)
  return call('s:Rtestorspec',[['spec', 'test']] + a:000)
endfunction

function! s:RspecComplete(A,L,P)
  return s:completion_filter(s:project().relglob('spec/','**/*','_spec.rb')+s:project().relglob('test/','**/*','_test.rb'),a:A)
endfunction

function! s:Rtask(file)
  if a:file ==# ''
    return 'Rakefile'
  elseif a:file =~# '/$'
    return 'rakelib/'.a:file
  else
    return 'rakelib/'.a:file.'.rake'
  endif
endfunction

function! s:RtaskComplete(A,L,P)
  return s:completion_filter(s:project().relglob('rakelib/','**/*','.rake'),a:A)
endfunction

call s:navcommand('lib')
call s:navcommand('test')
call s:navcommand('spec')
call s:navcommand('task')

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
augroup END

" }}}1

" vim:set sw=2 sts=2:
