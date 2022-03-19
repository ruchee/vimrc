" Vim ftplugin file
" Language: Ruby Signature (RBS) <github.com/ruby/rbs>
" Author: Jeffrey Crochet <jlcrochet@pm.me>
" URL: https://github.com/jlcrochet/vim-rbs

if get(b:, 'did_ftplugin')
  finish
endif

let b:did_ftplugin = 1

setlocal shiftwidth=2
setlocal comments=:#
setlocal commentstring=#\ %s
setlocal suffixesadd=.rbs

if get(g:, "rbs_fold")
  setlocal foldmethod=syntax
endif

" matchit.vim
let b:match_words = '\<\%(class\|module\|interface\)\>:\<end\>'
let b:match_skip = 'S:^rbsDefine$'
