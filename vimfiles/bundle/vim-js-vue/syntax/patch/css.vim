" Use a different name in order to avoid css syntax interference
silent! syntax clear cssUnitDecorators
syntax match cssUnitDecorators2 
      \ /\(#\|-\|+\|%\|mm\|cm\|in\|pt\|pc\|em\|ex\|px\|ch\|rem\|vh\|vw\|vmin\|vmax\|dpi\|dppx\|dpcm\|Hz\|kHz\|s\|ms\|deg\|grad\|rad\)\ze\(;\|$\)/
      \ contained
      \ containedin=cssAttrRegion,sassCssAttribute,lessCssAttribute,stylusCssAttribute

silent! syntax clear cssKeyFrameProp
syn match cssKeyFrameProp2 /\d*%\|from\|to/ 
      \ contained nextgroup=cssDefinition
      \ containedin=cssAttrRegion,sassCssAttribute,lessCssAttribute,stylusCssAttribute

highlight default link cssUnitDecorators2 Number
highlight default link cssKeyFrameProp2 Constant
