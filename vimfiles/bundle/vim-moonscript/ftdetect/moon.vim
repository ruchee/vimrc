" Language:    MoonScript
" Maintainer:  leafo <leafot@gmail.com>
" Based On:    CoffeeScript by Mick Koch <kchmck@gmail.com>
" URL:         http://github.com/leafo/moonscript-vim
" License:     WTFPL

autocmd BufNewFile,BufRead *.moon set filetype=moon

function! s:DetectMoon()
    if getline(1) =~ '^#!.*\<moon\>'
        set filetype=moon
    endif
endfunction

autocmd BufNewFile,BufRead * call s:DetectMoon()
