" Language: Apache Groovy
" Author: Tacahiroy <tacahiroy@gmail.com>
" License: The MIT License

function! ctrlp#funky#ft#groovy#filters()
  let p = '\m\C^[\t ]*'
  let p .= '\(def\)[\t ]\+\w\+([^)]*)[\t ]*\n*{'

  let filters = [
        \ { 'pattern': p,
        \   'formatter': ['^[\t ]*', '', ''] }
  \ ]
  return filters
endfunction
