" License: shell script (zsh)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#zsh#filters()
  " The zsh is really tolerant of the function definition
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*\%(function[\t ]\)\?\%([-/+_a-zA-Z0-9]\+\)\?[\t ]*\%(()\)\?[\t ]*{',
        \   'formatter' : [] },
        \ { 'pattern': '\m\C^[\t ]*\%(function[\t ]*\)\@<=\%([-/+_a-zA-Z0-9]\+[\t ]*\)\(()\)\?[\t ]*{',
        \   'formatter' : [] }
  \ ]

  return filters
endfunction

function! ctrlp#funky#ft#zsh#is_kinda_sh()
  return 1
endfunction
