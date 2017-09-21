" Language: coffee-script
" Author: kirstein
" License: The MIT License

function! ctrlp#funky#ft#coffee#filters()
  let filters = [
        \ { 'pattern'   : '\v\C\w.+\s*\=\s*(-|\=)\>',
        \   'formatter' : ['\v(^\s*)|((-|\=)\>.*\ze \t#)', '', 'g']
        \ },
        \ { 'pattern'   : '\v\C\w.+\s*\=\s*\(.*\)\s*(-|\=)\>',
        \   'formatter' : ['\v(^\s*)|((-|\=)\>.*\ze \t#)', '', 'g']
        \ },
        \ { 'pattern'   : '\v\C\s*\w+\s*:\s*\(.*\)\s*(-|\=)\>',
        \   'formatter' : ['\v(^\s*)|((-|\=)\>.*\ze \t#)', '', 'g']
        \ },
        \ { 'pattern'   : '\v\C\s*\w+\s*:\s*(-|\=)\>',
        \   'formatter' : ['\v(^\s*)|((-|\=)\>.*\ze \t#)', '', 'g']
        \ }
  \ ]

  return filters
endfunction
