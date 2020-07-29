"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim ftdetect file
" Language: TSX (Typescript)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd FileType typescript.tsx setlocal commentstring={/*\ %s\ */}
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx
