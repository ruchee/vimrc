" Set filetype for ~/.kube/config
autocmd BufRead,BufNewFile */.kube/config set filetype=yaml

autocmd BufRead,BufNewFile */templates/*.yaml,*/templates/*.tpl set filetype=yaml.gotexttmpl

" Detect kubectl get X -oyaml | vim (no file)
function s:DetectKubernetes() abort
  if did_filetype() || &ft != ''
    return
  endif
  let l:first_line = getline(1)
  if l:first_line =~# '^\(kind\|apiVersion\): '
    set filetype=yaml
  endif
endfunction
autocmd BufNewFile,BufRead,BufEnter * call s:DetectKubernetes()
