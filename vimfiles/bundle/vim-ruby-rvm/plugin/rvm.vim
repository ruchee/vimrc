" rvm.vim - Switch Ruby versions from inside Vim
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.0

if exists('g:loaded_rvm') || v:version < 700 || &cp
  finish
endif

if !exists('$rvm_path') && isdirectory(expand('~/.rvm'))
  let $rvm_path = expand('~/.rvm')
  let $PATH = $rvm_path . '/bin' . ':' . $PATH
endif

if !exists('$rvm_path')
  finish
endif

let g:loaded_rvm = 1

" Utility {{{1

function! s:shellesc(arg) abort
  if a:arg =~ '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  else
    return shellescape(a:arg)
  endif
endfunction

" }}}1
" :Rvm {{{1

function! rvm#buffer_path_identifier(...)
  let name = bufname(a:0 ? a:1 : '%')
  if name ==# ''
    let path = '.'
  elseif isdirectory(name)
    let path = name
  elseif !isdirectory(resolve(fnamemodify(name, ":p:h")))
    let path = '.'
  else
    let path = fnamemodify(name, ':h')
  endif

  let path_identifier = system('rvm tools path-identifier '.s:shellesc(path))
  return split(path_identifier)[-1]
endfunction

function! s:Rvm(bang,...) abort
  let path = split($PATH,':')
  call filter(path, 'v:val[0:strlen($rvm_path)] !=# $rvm_path."/"')

  if a:0 && a:0 < 3 && a:1 ==# 'use'
    let use = 1
    let args = a:000[1:-1]
  else
    let use = 0
    let args = copy(a:000)
  endif

  if len(args) > 1 || (len(args) == 1 && args[0] !~ '^\%(@\|\d\|default\|j\=ruby\|goruby\|rbx\|ree\|kiji\|maglev\|ironruby\|system\)' && !use)
    return '!rvm '.join(map(copy(a:000), 's:shellesc(v:val)'), ' ')
  elseif !empty(args) && args[-1] ==# 'system'
    let desired = 'system'
  elseif !empty(args)
    let desired = system('rvm tools strings '.s:shellesc(args[0]))[0:-2]
  elseif use || !exists('b:rvm_string')
    let desired = rvm#buffer_path_identifier()
  else
    let desired = b:rvm_string
  endif

  if desired ==# 'system'
    let $RUBY_VERSION = ''
    let $MY_RUBY_HOME = ''
    let $IRBRC = expand('~/.irbrc')
    let $PATH = join([$rvm_path.'/bin'] + path,':')
    let $GEM_HOME = system('env -i PATH="'.$PATH.'" ruby -rubygems -e "print Gem.dir"')
    let $GEM_PATH = system('env -i PATH="'.$PATH.'" ruby -rubygems -e "print Gem.path.join(%{:})"')
    if use
      return 'echomsg "Using system ruby"'
    else
      return ''
    endif
  endif

  let ver = matchstr(desired,'[^@]*')
  let gemset = matchstr(desired,'@.*')
  if ver ==# ''
    return 'echoerr "Ruby version not found"'
  endif
  if !isdirectory($rvm_path . '/rubies/' . ver)
    if $rvm_install_on_use_flag
      execute 'Rvm install '.ver
    else
      return 'echoerr "Ruby version not installed: :Rvm install ".'.string(ver)
    endif
  endif
  let b:rvm_string = desired

  let $RUBY_VERSION = ver
  let $GEM_HOME = $rvm_path . '/gems/' . $RUBY_VERSION . gemset
  let $MY_RUBY_HOME = $rvm_path . '/rubies/' . $RUBY_VERSION
  let $IRBRC = $MY_RUBY_HOME . '/.irbrc'

  let gemsets = [$GEM_HOME, $rvm_path . '/gems/' . $RUBY_VERSION . '@global']

  let $GEM_PATH = join(gemsets, ':')
  let $PATH = join(
        \ map(gemsets,'v:val."/bin"') +
        \ [$MY_RUBY_HOME.'/bin'] +
        \ [$rvm_path.'/bin'] +
        \ path, ':')
  if use
    return 'echomsg "Using " . $GEM_HOME'
  else
    return ''
  endif
endfunction

function! s:Complete(A,L,P)
  if a:A =~# '@'
    let requested = matchstr(a:A,'^[^@]*')
    let desired = system('rvm tools strings '.s:shellesc(requested))[0:-2]
    let all = split(glob($rvm_path.'/gems/'.desired.'@*'),"\n")
    call map(all,"v:val[strlen($rvm_path)+6:-1]")
    call map(all,'substitute(v:val,"^[^@]*",requested,"")')
  else
    let all = split(glob($rvm_path.'/rubies/*'),"\n")
    call map(all,"v:val[strlen($rvm_path)+8:-1]")
    if a:A !~# '^r'
      call map(all,'substitute(v:val,"^ruby-\\ze\\d","","")')
    endif
  endif
  return join(all,"\n")
endfunction

command! -bar -nargs=* -complete=custom,s:Complete Rvm :execute s:Rvm(<bang>0,<f-args>)

" }}}1
" Statusline {{{1

function! rvm#string()
  return matchstr($GEM_HOME,'[^/]*$')
endfunction

function! rvm#statusline()
  return substitute('['.rvm#string().']','^\[\]$','','')
endfunction

function! rvm#statusline_ft_ruby()
  if &filetype ==# 'ruby'
    return rvm#statusline()
  else
    return ''
  endif
endfunction

" }}}1
" Load path {{{1

function! rvm#ruby_version_paths() abort
  let dict = {}
  for entry in split(glob("$rvm_path/rubies/ruby-*"))
    let ver = matchstr(entry, '/rubies/ruby-\zs.*')
    let paths = ver =~# '^1.[0-8]' ? ['.'] : []
    let paths += split($RUBYLIB, ':')
    let site_ruby_arch = glob(entry . '/lib/ruby/site_ruby/*.*/*-*')
    if empty(site_ruby_arch) || site_ruby_arch =~# "\n"
      continue
    endif
    let arch = fnamemodify(site_ruby_arch, ':t')
    let minor = fnamemodify(site_ruby_arch, ':h:t')
    let paths += [
          \ entry . '/lib/ruby/site_ruby/' . minor,
          \ entry . '/lib/ruby/site_ruby/' . minor . '/' . arch,
          \ entry . '/lib/ruby/site_ruby',
          \ entry . '/lib/ruby/vendor_ruby/' . minor,
          \ entry . '/lib/ruby/vendor_ruby/' . minor . '/' . arch,
          \ entry . '/lib/ruby/vendor_ruby',
          \ entry . '/lib/ruby/' . minor,
          \ entry . '/lib/ruby/' . minor . '/' . arch]
    let dict[ver] = paths
  endfor
  return dict
endfunction

if !exists('g:ruby_version_paths')
  let g:ruby_version_paths = {}
endif

call extend(g:ruby_version_paths, rvm#ruby_version_paths(), 'keep')

" }}}1

" vim:set sw=2 sts=2:
