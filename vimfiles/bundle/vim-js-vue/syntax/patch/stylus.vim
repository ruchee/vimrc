silent! syntax clear stylusDefinition
syntax region cssStylusDefinition matchgroup=cssBraces 
      \ contains=@StylusSyntax,cssStylusDefinition
      \ contained containedin=cssStylusVueStyle
      \ start="{" end="}" 
