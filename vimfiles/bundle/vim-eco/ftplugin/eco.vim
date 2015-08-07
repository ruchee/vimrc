" Language:    Eco
" Maintainer:  Andrew Radev <andrey.radev@gmail.com>
" URL:         http://github.com/AndrewRadev/vim-eco

if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/html.vim ftplugin/html_*.vim ftplugin/html/*.vim
let b:did_ftplugin = 1

" Matchit support, adding to the html one
let s:match_words =
      \ '\<\%(if\|unless\|switch\|while\|until\|for\)\>=\@!' .
      \ ':' .
      \ '\<\%(else\|elsif\|when\|break\|continue\)\>' .
      \ ':' .
      \ '\<end\>' .
      \ ',{:},\[:\],(:)'

if exists('b:match_words')
  let b:match_words = b:match_words . ',' .s:match_words
else
  let b:match_words = s:match_words
endif

let b:match_skip =
      \ "synIDattr(synID(line('.'),col('.'),0),'name') =~ '" .
      \ "\\<coffee\\%(String\\|BlockComment\\|Heredoc\\|Comment\\)\\|".
      \ "\\<ecoComment\\>".
      \ "'"

" surround.vim additions
"
" Usage:
"   yss% : Surrounds current line with <% %>
"   yss= : Surrounds current line with <%- %>
"   yss# : Surrounds current line with <%# %>
"   And so on, see http://www.vim.org/scripts/script.php?script_id=1697 for
"   more usage info.
"
" Note: Extracted from rails.vim (thanks tpope):
" https://github.com/tpope/vim-rails/blob/master/autoload/rails.vim#L4506
function! s:buffer_getvar(varname)
  return getbufvar(bufnr('%'),a:varname)
endfunction

function! s:buffer_setvar(varname, val)
  return setbufvar(bufnr('%'),a:varname,a:val)
endfunction

if exists("g:loaded_surround")
  if s:buffer_getvar('surround_37') == '' || s:buffer_getvar('surround_37') == "<% \r %>" " %
    call s:buffer_setvar('surround_37', "<% \r %>")
  endif
  if s:buffer_getvar('surround_61') == '' " =
    call s:buffer_setvar('surround_61', "<%= \r %>")
  endif
  if s:buffer_getvar("surround_35") == '' " #
    call s:buffer_setvar('surround_35', "<%# \r %>")
  endif
endif

" Text object for embedded code (<%= ... %>)
" For instance, given the following code:
"
"   <%- foo("bar") %>
"
" Typing ci= would leave only the following:
"
"   <%-  %>
"
" And place the cursor in insert mode in the empty space left over.
"
" Define a text object for embedded code (<%= ... %>)
onoremap <buffer> a= :<c-u>call <SID>EcoTextObject('a')<cr>
xnoremap <buffer> a= :<c-u>call <SID>EcoTextObject('a')<cr>
onoremap <buffer> i= :<c-u>call <SID>EcoTextObject('i')<cr>
xnoremap <buffer> i= :<c-u>call <SID>EcoTextObject('i')<cr>
function! s:EcoTextObject(mode)
  if search('<%.*\%#.*%>', 'n') <= 0
    return
  endif

  if a:mode == 'i'
    let [start_flags, end_flags] = ['be', '']
  else " a:mode == 'a'
    let [start_flags, end_flags] = ['b', 'e']
  endif

  call search('<%[=-]\?\s*.', start_flags, line('.'))
  let start = col('.') - 1
  call search('.\s*%>', end_flags, line('.'))
  let end = col('.') - 1

  let interval = end - start

  if start == 0
    exe 'normal! 0v'.interval.'l'
  else
    exe 'normal! 0'.start.'lv'.interval.'l'
  endif
endfunction
