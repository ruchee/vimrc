" Language: Haskell
" Author: zaynetro
" License: The MIT License

" Matches only functions that are defined from the beginning of the line
" and have a type signature.
"
" Ex:
" ```
" someFunc :: [Integer] -> [Integer]
" someFunc = fmap addOne
"   where
"     addOne :: Integer -> Integer
"     addOne = (+1)
" ```
"
" In this example only someFunc will be matched. The issue with matching
" local functions is that the same syntax is used for record data type
" entries.

function! ctrlp#funky#ft#haskell#filters()
  let filters = [
        \ { 'pattern': '^[^ ,]\+ *::.*$',
        \   'formatter': [] }
  \ ]
  return filters
endfunction
