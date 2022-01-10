" Some useful resources:
" Highlighting: https://github.com/dominikduda/vim_current_word/blob/master/autoload/vim_current_word.vim

let s:matches = [] 

if !hlexists("RescriptHighlight")
  " TODO: make colors configurable
  hi link RescriptHighlight IncSearch
endif

function! rescript#highlight#HighlightWord(pos)
  "let id = matchadd('RescriptHighlight', '\S*\%#\S*', 5)
  let id = matchaddpos('RescriptHighlight', a:pos)
  let s:matches = add(s:matches, id)
  augroup RescriptHighlighting
    au!
    autocmd InsertEnter <buffer> call rescript#highlight#StopHighlighting()
    autocmd BufWinLeave <buffer> call rescript#highlight#StopHighlighting()
  augroup END
endfunction

function! rescript#highlight#StopHighlighting()
  for item in s:matches
    try
      call matchdelete(item)
    catch /.*/
      " don't really care about highlighting errors
    endtry
  endfor
  let s:matches = []

  augroup RescriptHighlighting
    au!
  augroup END
endfunction
