"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Config {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:config = vue#GetConfig('config', {})
let s:keyword = s:config.keyword
let s:initial_indent = s:config.initial_indent

let s:enable_initial_indent = count(s:initial_indent, 'script')
      \ || count(s:initial_indent, 'javascript')
      \ || count(s:initial_indent, 'script.javascript')
"}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Syntax highlight {{{
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Number with minus
syntax match javaScriptNumber '\v<-?\d+L?>|0[xX][0-9a-fA-F]+>' 
      \ containedin=@javascript display
highlight link javaScriptNumber Constant

" Vue keywords
if !s:keyword | finish | endif

let s:vue_keywords = 'name parent functional delimiters comments components directives filters extends mixins inheritAttrs model props propsData data computed watch methods template render renderError inject provide beforeCreate created beforeMount mounted beforeUpdate updated activated deactivated beforeDestroy destroyed setup beforeUnmount unmounted errorCaptured renderTracked renderTriggered'

let s:indent = &sw * (1 + s:enable_initial_indent)
let s:keywords_regexp = '\v^\s{'.s:indent.'}(async )?<('
      \.join(split(s:vue_keywords, ' '), '|')
      \.')\ze'

" Currently support https://github.com/pangloss/vim-javascript
let s:useJavaScriptPlugin = hlexists('jsAsyncKeyword')
let s:containedin = s:useJavaScriptPlugin
      \? 'jsObject,jsFuncBlock,@jsExpression' 
      \: 'javascriptScriptBlock' 
let s:contains = s:useJavaScriptPlugin
      \? 'jsAsyncKeyword' 
      \: 'javaScriptReserved' 
let s:match_option = 
      \' containedin='.s:containedin
      \.' contains='.s:contains
      \.' skipwhite skipempty'

execute 'syntax match vueObjectKey display /'
      \.s:keywords_regexp
      \.'\s*:/'
      \.s:match_option
      \.' nextgroup=jsObjectValue'

execute 'syntax match vueObjectFuncName display /'
      \.s:keywords_regexp
      \.'\_s*\(/'
      \.s:match_option
      \.' nextgroup=jsFuncArgs'

execute 'syntax match vueObjectFuncKey display /'
      \.s:keywords_regexp
      \.'\s*:\s*function>/'
      \.s:match_option
      \.' nextgroup=jsFuncArgs'

let s:vue3_keywords = 'ref reactive toRefs watch computed'.
      \' onBeforeMount onMounted onBeforeUpdate onUpdated onBeforeUnmount'.
      \' onUnmounted onErrorCaptured onRenderTracked onRenderTriggered'.
      \' getCurrentInstance'
let s:vue3_keywords_regexp = '\v<('
      \.join(split(s:vue3_keywords, ' '), '|')
      \.')\ze'

execute 'syntax match vue3Keyword display /'
      \.s:vue3_keywords_regexp
      \.'\_s*\(/'
      \.s:match_option

highlight default link vueObjectKey vueObjectKeyword
highlight default link vueObjectFuncName vueObjectKeyword
highlight default link vue3Keyword vueObjectKeyword
highlight default link vueObjectFuncKey vueObjectKeyword
highlight default link vueObjectKeyword Type
"}}}
" vim: fdm=marker
