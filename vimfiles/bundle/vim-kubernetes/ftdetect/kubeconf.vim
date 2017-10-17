" Set filetype for ~/.kube/config
autocmd BufRead,BufNewFile */.kube/config set filetype=yaml

autocmd BufRead,BufNewFile */templates/*.yaml set filetype=yaml.gotexttmpl
