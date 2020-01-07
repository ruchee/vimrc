" 
let g:racket_hash_lang_regexp = '^#lang\s\+\([^][)(}{[:space:]]\+\)'

" Tries to detect filetype from #lang line; defaults to ft=racket.
function! RacketDetectHashLang()
  let old_ft = &filetype

  let matches = matchlist(getline(1), g:racket_hash_lang_regexp)
  if ! empty(matches)
    let &l:filetype = matches[1]
  endif

  if &filetype == old_ft
    set filetype=racket
  endif
endfunction

au BufRead,BufNewFile *.rkt,*.rktl call RacketDetectHashLang()
