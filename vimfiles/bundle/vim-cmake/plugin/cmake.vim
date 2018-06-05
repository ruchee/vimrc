" cmake.vim - Vim plugin to make working with CMake a little nicer
" Maintainer:   Dirk Van Haerenborgh <http://vhdirk.github.com/>
" Version:      0.2

let s:cmake_plugin_version = '0.2'

if exists("loaded_cmake_plugin")
  finish
endif

" We set this variable here even though the plugin may not actually be loaded
" because the executable is not found. Otherwise the error message will be
" displayed more than once.
let loaded_cmake_plugin = 1

" Set option defaults
if !exists("g:cmake_export_compile_commands")
  let g:cmake_export_compile_commands = 0
endif
if !exists("g:cmake_ycm_symlinks")
  let g:cmake_ycm_symlinks = 0
endif

if !executable("cmake")
  echoerr "vim-cmake requires cmake executable. Please make sure it is installed and on PATH."
  finish
endif

function! s:find_build_dir()
  " Do not overwrite already found build_dir, may be set explicitly
  " by user.
  if exists("b:build_dir") && b:build_dir != ""
    return 1
  endif

  let g:cmake_build_dir = get(g:, 'cmake_build_dir', 'build')
  let b:build_dir = finddir(g:cmake_build_dir, ';')

  if b:build_dir == ""
    " Find build directory in path of current file
    let b:build_dir = finddir(g:cmake_build_dir, s:fnameescape(expand("%:p:h")) . ';')
  endif

  if b:build_dir != ""
    " expand() would expand "" to working directory, but we need
    " this as an indicator that build was not found
    let b:build_dir = fnamemodify(b:build_dir, ':p')
    echom "Found cmake build directory: " . s:fnameescape(b:build_dir)
    return 1
  else
    echom "Unable to find cmake build directory."
    return 0
  endif

endfunction

" Configure the cmake project in the currently set build dir.
"
" This will override any of the following variables if the
" corresponding vim variable is set:
"   * CMAKE_INSTALL_PREFIX
"   * CMAKE_BUILD_TYPE
"   * CMAKE_BUILD_SHARED_LIBS
" If the project is not configured already, the following variables will be set
" whenever the corresponding vim variable for the following is set:
"   * CMAKE_CXX_COMPILER
"   * CMAKE_C_COMPILER
"   * The generator (-G)
function! s:cmake_configure()
  exec 'cd' s:fnameescape(b:build_dir)

  let l:argument = []
  " Only change values of variables, if project is not configured
  " already, otherwise we overwrite existing configuration.
  let l:configured = filereadable("CMakeCache.txt")

  if !l:configured
    if exists("g:cmake_project_generator")
        let l:argument += [ "-G \"" . g:cmake_project_generator . "\"" ]
    endif
    if exists("g:cmake_cxx_compiler")
        let l:argument += [ "-DCMAKE_CXX_COMPILER:FILEPATH="     . g:cmake_cxx_compiler ]
    endif
    if exists("g:cmake_c_compiler")
        let l:argument += [ "-DCMAKE_C_COMPILER:FILEPATH="       . g:cmake_c_compiler ]
    endif

    if exists("g:cmake_usr_args")
      let l:argument+= [ g:cmake_usr_args ]
    endif
  endif

  if exists("g:cmake_install_prefix")
    let l:argument += [ "-DCMAKE_INSTALL_PREFIX:FILEPATH="  . g:cmake_install_prefix ]
  endif
  if exists("g:cmake_build_type" )
    let l:argument += [ "-DCMAKE_BUILD_TYPE:STRING="         . g:cmake_build_type ]
  endif
  if exists("g:cmake_build_shared_libs")
    let l:argument += [ "-DBUILD_SHARED_LIBS:BOOL="          . g:cmake_build_shared_libs ]
  endif
  if g:cmake_export_compile_commands
    let l:argument += [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" ]
  endif

  let l:argumentstr = join(l:argument, " ")

  let s:cmd = 'cmake .. '. l:argumentstr . " " . join(a:000)
  echo s:cmd
  if exists(":AsyncRun")
    execute 'copen'
    execute 'AsyncRun ' . s:cmd
    execute 'wincmd p'
  else
    silent let s:res = system(s:cmd)
    silent echo s:res
  endif

  " Create symbolic link to compilation database for use with YouCompleteMe
  if g:cmake_ycm_symlinks && filereadable("compile_commands.json")
    if has("win32")
      exec "mklink" "../compile_commands.json" "compile_commands.json"
    else
      silent echo system("ln -s " . s:fnameescape(b:build_dir) ."/compile_commands.json ../compile_commands.json")
    endif
    echom "Created symlink to compilation database"
  endif

  exec 'cd -'
endfunction

" Utility function
" Thanks to tpope/vim-fugitive
function! s:fnameescape(file) abort
  if exists('*fnameescape')
    return fnameescape(a:file)
  else
    return escape(a:file," \t\n*?[{`$\\%#'\"|!<")
  endif
endfunction

" Public Interface:
command! -nargs=? CMake call s:cmake(<f-args>)
command! CMakeClean call s:cmakeclean()
command! CMakeFindBuildDir call s:cmake_find_build_dir()

function! s:cmake_find_build_dir()
  unlet! b:build_dir
  call s:find_build_dir()
endfunction

function! s:cmake(...)
  if !s:find_build_dir()
    return
  endif

  let &makeprg = 'cmake --build ' . shellescape(b:build_dir) . ' --target'
  call s:cmake_configure()
endfunction

function! s:cmakeclean()
  if !s:find_build_dir()
    return
  endif

  silent echo system("rm -r '" . b:build_dir. "'/*")
  echom "Build directory has been cleaned."
endfunction

