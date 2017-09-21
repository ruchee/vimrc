" Language: Vim script (vim)
" Author: Takahiro Yoshihara
" License: The MIT License

function! ctrlp#funky#ft#vim#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*fu\(n\|nc\|nct\|ncti\|nctio\|nction\)\?!\?[\t ]\+\S\+',
        \   'formatter': ['\m\C^[\t ]\+', '', ''] }
  \ ]
  return filters
endfunction
