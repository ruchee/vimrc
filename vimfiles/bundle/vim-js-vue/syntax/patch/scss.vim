" If not loading https://github.com/cakebaker/scss-syntax.vim
if !hlexists('scssNestedProperty')
  silent! syntax clear scssDefinition
  syntax region cssScssDefinition transparent matchgroup=cssBraces 
        \ contains=@ScssSyntax,cssScssDefinition
        \ contained containedin=cssScssVueStyle
        \ start="{" end="}" 

  " Extend to highlight all numbers in expression
  syntax match cssValueNumber
        \ /\W\zs\d\+\(\.\d\+\)\?%\?\ze\W/
        \ contained containedin=cssScssDefinition
endif
