" Vim ftplugin file
" Language: Ruby Signature (RBS) <github.com/ruby/rbs>
" Author: Jeffrey Crochet <jlcrochet91@pm.me>
" URL: https://github.com/jlcrochet/vim-rbs

if get(b:, 'did_ftplugin')
  finish
endif

let b:did_ftplugin = 1

setlocal
      \ shiftwidth=2
      \ comments=:#
      \ commentstring=#\ %s
      \ suffixesadd=.rbs

let b:undo_ftplugin = "setlocal shiftwidth< comments< commentstring< suffixesadd<"

" matchit.vim
if get(g:, "loaded_matchit")
  let b:match_words = '\<\%(class\|module\|interface\)\>:\<end\>'
  let b:match_skip = 'S:^rbsDefine$'

  let b:undo_ftplugin ..= " | unlet b:match_words b:match_skip"
endif
