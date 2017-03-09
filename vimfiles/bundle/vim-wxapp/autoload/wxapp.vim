
" generate folder and related files
function! wxapp#generate(folder, name) abort
  let g:name = a:name
  if a:name !~ '^\(\w\|-\)\+$'
    echohl Error | echon '文件名非法' | echohl None
    return
  endif
  let path = simplify(getcwd().'/'.a:folder.'/'.a:name)
  if isdirectory(path) || filereadable(path)
    echohl Error | echon '目录已存在' | echohl None
  elseif exists('*mkdir')
    call mkdir(path, 'p')
    call s:system('touch '.path.'/'.a:name.'.wxml')
    call s:system('touch '.path.'/'.a:name.'.wxss')
    call s:system('touch '.path.'/'.a:name.'.js')
    execute 'edit '.path.'/'.a:name.'.js'
    execute 'belowright vs'.path.'/'.a:name.'.wxss'
    execute 'split '.path.'/'.a:name.'.wxml'
    execute 'wincmd h'
  endif
endfunction

function! wxapp#start() abort
  if !has("mac") | return | endif
  call s:osascript(
    \'tell application "wechatwebdevtools"',
    \  'activate',
    \'end tell')
endfunction

function! s:osascript(...) abort
  let args = join(map(copy(a:000), '" -e ".shellescape(v:val)'), '')
  call  s:system('osascript'. args)
  return !v:shell_error
endfunction

function! s:system(cmd)
  let output = system(a:cmd)
  if v:shell_error
    if output =~ '2700'
      echohl Error | echon '请先启动微信开发者工具' | echohl None
      return
    else
      echohl Error | echon output | echohl None
      return
    endif
  endif
  return output
endfunction
