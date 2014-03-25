" Vim compiler file
" Compiler:             Microsoft Visual Studio C#
" Maintainer:           Kian Ryan (kian@orangetentacle.co.uk)
" Previous Maintainer:  Zhou YiChao (broken.zhou@gmail.com)
" Last Change:          2012 Sep 22	

if exists("current_compiler")
  finish
endif
let current_compiler = "cs"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet errorformat&
CompilerSet errorformat+=%f(%l\\,%v):\ %t%*[^:]:\ %m,
            \%trror%*[^:]:\ %m,
            \%tarning%*[^:]:\ %m

execute 'CompilerSet makeprg=' . cs#get_net_compiler("csc.exe") . "\\ %"
