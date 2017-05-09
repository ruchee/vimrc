" Spacebars, Mustache & Handlebars syntax
" Language:	Spacebars, Mustache, Handlebars
" Maintainer: Slava Kim <slava@meteor.com>
" Original Maintainer:	Juvenn Woo <machese@gmail.com>
" References:
"   [Mustache](http://github.com/defunkt/mustache)
"   [Handlebars](https://github.com/wycats/handlebars.js)
"   [ctemplate](http://code.google.com/p/google-ctemplate/)
"   [ctemplate doc](http://google-ctemplate.googlecode.com/svn/trunk/doc/howto.html)
"   [et](http://www.ivan.fomichev.name/2008/05/erlang-template-engine-prototype.html)
"   [Spacebars](https://github.com/meteor/meteor/tree/devel/packages/spacebars)


" Read the HTML syntax to start with
if version < 600
  so <sfile>:p:h/html.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
endif

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Standard HiLink will not work with included syntax files
if version < 508
  command! -nargs=+ HtmlHiLink hi link <args>
else
  command! -nargs=+ HtmlHiLink hi def link <args>
endif

syntax match spacebarsError '}}}\?'
syntax match spacebarsInsideError '{{[{#<>=!\/]\?'
syntax region spacebarsInside start=/{{/ end=/}}}\?/ keepend containedin=TOP,@htmlSpacebarsContainer
syntax match spacebarsOperators '=\|\.\|/' contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax region spacebarsSection start='{{[#^/]'lc=2 end='[} ]'me=e-1 contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax region spacebarsPartial start=/{{[<>]\s*/lc=2 end=+[} ]+me=e-1 contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax region spacebarsMarkerSet start=/{{=/lc=2 end=/=}}/me=e-2 contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax match spacebarsHandlebars '{{\|}}' contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax match spacebarsUnescape '{{{\|}}}' contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax match spacebarsConditionals '\([/#]\(if\>\|unless\>\)\|\<else\>\)' contained containedin=spacebarsInside
syntax match spacebarsHelpers '[/#]\(with\|each\)' contained containedin=spacebarsSection
syntax region spacebarsComment start=/{{!/rs=s+2 end=/}}/re=e-2 contains=Todo contained containedin=spacebarsInside,@htmlSpacebarsContainer
syntax region spacebarsBlockComment start=/{{!--/rs=s+2 end=/--}}/re=e-2 contains=Todo
syntax region spacebarsQString start=/'/ skip=/\\'/ end=/'/ contained containedin=spacebarsInside
syntax region spacebarsDQString start=/"/ skip=/\\"/ end=/"/ contained containedin=spacebarsInside

syntax region   spacebarsHelperRegion
      \ start=+{{#\z\([^ /!?<>"'{}]\+\)+
      \ skip=+{{!\_.\{-}}}+
      \ end=+{{/\z1\_\s\{-}}}+
      \ fold
      \ contains=TOP,@htmlSpacebarsContainer,spacebarsError,spacebarsOperators,spacebarsSection,spacebarsPartial,spacebarsMarkerSet,spacebarsHandlebars,spacebarsUnescape,spacebarsConditionals,spacebarsHelpers,spacebarsComment,spacebarsBlockComment,spacebarsQString,spacebarsDQString,spacebarsHelperRegion
      \ keepend
      \ extend


" Clustering
syntax cluster htmlSpacebarsContainer add=htmlHead,htmlTitle,htmlString,htmlH1,htmlH2,htmlH3,htmlH4,htmlH5,htmlH6,htmlLink,htmlBold,htmlUnderline,htmlItalic,htmlValue


" Hilighting
" spacebarsInside hilighted as Number, which is rarely used in html
" you might like change it to Function or Identifier
HtmlHiLink spacebarsInside Number
HtmlHiLink spacebarsVariable Number
HtmlHiLink spacebarsVariableUnescape Number
HtmlHiLink spacebarsPartial PreProc
HtmlHiLink spacebarsSection PreProc
HtmlHiLink spacebarsMarkerSet Number

HtmlHiLink spacebarsComment Comment
HtmlHiLink spacebarsBlockComment Comment
HtmlHiLink spacebarsError Error
HtmlHiLink spacebarsInsideError Error

HtmlHiLink spacebarsHandlebars Special
HtmlHiLink spacebarsUnescape Identifier
HtmlHiLink spacebarsOperators Operator
HtmlHiLink spacebarsConditionals Conditional
HtmlHiLink spacebarsHelpers Repeat
HtmlHiLink spacebarsQString String
HtmlHiLink spacebarsDQString String

syn region spacebarsScriptTemplate start=+<script [^>]*type *=[^>]*text/\(spacebars\|x-handlebars-template\)[^>]*>+
      \                       end=+</script>+me=s-1 keepend
      \                       contains=spacebarsInside,@htmlSpacebarsContainer,htmlTag,htmlEndTag,htmlTagName,htmlSpecialChar

let b:current_syntax = "spacebars"
delcommand HtmlHiLink
