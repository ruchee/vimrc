"
"
"
if exists("b:did_ftplugin") | finish | endif

runtime! ftplugin/html.vim ftplugin/html_*.vim ftplugin/html/*.vim

let b:did_ftplugin = 1

if exists("loaded_matchit")
  if exists('smarty_left_delimiter')
    let b:smarty_left_delimiter = smarty_left_delimiter
  else
    let b:smarty_left_delimiter = '{'
  endif

  let s:block_tags = [
        \ [ "block", "/block" ],
        \ [ "capture", "/capture" ],
        \ [ "for", "forelse", "/for"],
        \ [ "foreach", "foreachelse", "/foreach"],
        \ [ "function", "/function"],
        \ [ "if", "elseif", "else", "/if" ],
        \ [ "literal", "/literal" ],
        \ [ "nocache", "/nocache" ],
        \ [ "section", "sectionelse", "/section" ],
        \ [ "strip", "/strip" ],
        \ [ "while", "/while" ],
        \ ]

  function! s:smarty3_match_worlds()
    let words = ""

    for pair in s:block_tags
      if words != ""
        let words .= ","
      endif

      let isfirst = 1
      for atag in pair
        if isfirst
          let isfirst = 0
        else
          let words .= ':'
        endif
        let words .= b:smarty_left_delimiter . '\s*' . atag . '\>'
      endfor
    endfor
    return words
  endfunction

  let b:match_words .= "," . s:smarty3_match_worlds()
endif

" vim: ts=8 sts=2 sw=2 expandtab
