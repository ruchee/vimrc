" this configuration is used for providing a sandbox
" environment for development

let &runtimepath.=','.escape(expand('<sfile>:p:h'), '\,')

set nocompatible
set backspace=indent,eol,start
set autoread

filetype plugin indent on
syntax on

set ai
set si
set sw=4 sts=4 ts=4
set et

set laststatus=2
set statusline=%f
set statusline+=%y

set nowrap

" hit F10 to identify the highlight group under the cursor
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
            \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

let g:blade_custom_directives = ['datetime', 'namespaced::directive']
let g:blade_custom_directives_pairs = {'cache': 'endcache'}
