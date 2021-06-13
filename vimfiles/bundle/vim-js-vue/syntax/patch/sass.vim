silent! syntax clear sassDefinition
syntax region sassDefinition matchgroup=cssBraces 
      \ contains=@SassSyntax,sassDefinition 
      \ contained containedin=sassVueStyle
      \ start="{" end="}" 

" Extend to highlight all numbers in expression
syntax match cssValueNumber
      \ /\W\zs\d\+\(\.\d\+\)\?%\?\ze\W/
      \ contained containedin=sassDefinition
