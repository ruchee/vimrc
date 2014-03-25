" Vim compiler file
" Compiler:	Microsoft MSBuild Runner
" Maintainer:	Kian Ryan (kian@orangetentacle.co.uk)
" Last Change:	2012 Sep 22

if exists("current_compiler")
  finish
endif
let current_compiler = "msbuild"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet errorformat&
CompilerSet errorformat+=%f(%l\\,%v):\ %t%*[^:]:\ %m,
            \%trror%*[^:]:\ %m,
            \%tarning%*[^:]:\ %m


if exists("g:net_build_file")
    let s:build_file = g:net_build_file
else
    let s:build_file = cs#find_net_solution_file()
endif

execute 'CompilerSet makeprg=' . cs#get_net_compiler("msbuild.exe") . "\\ " 
            \ . "/nologo\\ /clp:Verbosity=quiet\\ /property:GenerateFullPaths=true\\ "
            \ . s:build_file



