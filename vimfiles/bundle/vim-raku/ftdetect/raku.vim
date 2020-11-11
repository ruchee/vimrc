" whenever a named file is created, writen or read,
" set raku filetype if the extension is one of those:
" https://github.com/Raku/problem-solving/blob/master/solutions/language/Path-to-Raku.md#extensions

autocmd BufNewFile,BufWritePost,BufReadPost
\ *.pm6,*.p6,*.t6,*.pod6,*.raku,*.rakumod,*.rakudoc,*.rakutest
\ set filetype=raku

" whenever a named file is written or read,
" set raku filetype if there is a shebang with raku in it

autocmd BufWritePost,BufReadPost *
\ if getline(1) =~ '^#!.*raku' |
\    setf raku |
\ endif

