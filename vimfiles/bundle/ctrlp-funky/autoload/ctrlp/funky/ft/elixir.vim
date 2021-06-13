" Language: elixir
" Author: Takahiro Yoshihara, Jagtesh Chadha, Hasitha Pathiraja
" License: The MIT License

function! ctrlp#funky#ft#elixir#filters()
  let filters = [
        \ { 'pattern': '\m\C^[\t ]*\(def\|defp\|defmacro\|describe\|test\)[\t ]\+\S\+\_.*do[\t ]*$',
        \   'formatter': []}
  \ ]
  return filters
endfunction
