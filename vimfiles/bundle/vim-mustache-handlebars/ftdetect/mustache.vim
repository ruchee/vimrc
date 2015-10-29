if has("autocmd")
  au  BufNewFile,BufRead *.mustache,*.hogan,*.hulk,*.hjs set filetype=mustache syntax=mustache | runtime! ftplugin/mustache.vim ftplugin/mustache*.vim ftplugin/mustache/*.vim
  au  BufNewFile,BufRead *.handlebars,*.hbs set filetype=handlebars syntax=mustache | runtime! ftplugin/mustache.vim ftplugin/mustache*.vim ftplugin/mustache/*.vim
endif
