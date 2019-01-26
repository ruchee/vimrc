if has('autocmd')
    au BufNewFile,BufRead *.swig set ft=swig syntax=swig | runtime! ftplugin/swig.vim ftplugin/swig*.vim ftplugin/swig/*.vim
endif
