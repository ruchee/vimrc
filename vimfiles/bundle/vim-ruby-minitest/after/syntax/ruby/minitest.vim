let s:fold = get(b:, 'ruby_minitest_fold', get(g:, 'ruby_minitest_fold'))
let s:i = ''

if !has('folding') || empty(s:fold)
  syntax keyword rubyTestMethod
        \ describe
        \ it
  finish
else
  let s:fold = ' fold'
endif

" Regions for `describe ... end` blocks,
" named with `rubyMinitestDescribeBlock` and so on.
for s:i in ['describe', 'it']
  let s:group_name = 'rubyMinitest' . substitute(s:i, '^.', '\u\0', '') . 'Block'
  execute 'syntax region ' . s:group_name . ' matchgroup=rubyControl ' .
        \ 'start="^\z(\s*\)' . s:i . '\>[?!]\@!" ' .
        \ 'end="^\z1end\>" ' .
        \ 'contains=ALLBUT,@rubyNotTop' .
        \ s:fold
endfor
unlet s:i
