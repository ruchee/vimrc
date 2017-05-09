if has("autocmd")
  au  BufNewFile,BufRead *.html,*.spacebars,*.mustache,*.handlebars,*.hbs,*.hogan,*.hulk,*.hjs set filetype=html syntax=spacebars | runtime! ftplugin/spacebars.vim ftplugin/spacebars*.vim ftplugin/spacebars/*.vim
endif
