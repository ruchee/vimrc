" Language: Jenkinsfile
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License

function! ctrlp#funky#ft#Jenkinsfile#filters()
  let pat = '\m\C^[\t ]*'
  let pat .= '\('
  let pat .= 'agent\|parameters[\t ]*{\|environment\|post\|stage[\t ]*([^)]*)'
  let pat .= '\)'

  let filters = [
        \ { 'pattern': pat,
        \   'formatter': ['^[\t ]*', '', ''] }
  \ ]

  call extend(filters, ctrlp#funky#ft#groovy#filters())

  return filters
endfunction

" vim: ft=vim
