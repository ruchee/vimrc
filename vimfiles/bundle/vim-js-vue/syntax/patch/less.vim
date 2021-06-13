" Use emmet-vim css type.
silent! syntax clear lessDefinition
syntax region cssLessDefinition matchgroup=cssBraces 
      \ contains=@LessSyntax,cssLessDefinition 
      \ contained containedin=cssLessVueStyle
      \ start="{" end="}" 
