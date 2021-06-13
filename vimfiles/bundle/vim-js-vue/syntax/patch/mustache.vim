" Patch for https://github.com/mustache/vim-mustache-handlebars
" Use containedin=mustacheTemplateBlock in place of containedin=Top

syntax clear mustacheBlockComment
syntax region mustacheBlockComment start=/{{!--/rs=s+2 skip=/{{.\{-}}}/ end=/--}}/re=e-2 contains=Todo contained extend containedin=mustacheTemplateBlock,@mustacheInside,@htmlMustacheContainer

syntax clear mustacheComment
syntax region mustacheComment      start=/{{!/rs=s+2   skip=/{{.\{-}}}/ end=/}}/re=e-2   contains=Todo contained containedin=mustacheTemplateBlock,@mustacheInside,@htmlMustacheContainer

syntax clear mustacheAngleComponent
syntax region mustacheAngleComponent start=/<\/\?[[:upper:]]/ end=/>/ keepend containedin=mustacheTemplateBlock,@htmlMustacheContainer

syntax clear mustacheHbsComponent
syntax region mustacheHbsComponent start=/{{[^!][$#^/]\?/ end=/}}}\?/ keepend containedin=mustacheTemplateBlock,@htmlMustacheContainer
