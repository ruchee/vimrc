" Set filetype for ~/.kube/config
autocmd BufRead,BufNewFile */.kube/config set filetype=yaml

autocmd BufRead,BufNewFile */templates/*.yaml,*/templates/*.tpl set filetype=yaml.gotexttmpl
