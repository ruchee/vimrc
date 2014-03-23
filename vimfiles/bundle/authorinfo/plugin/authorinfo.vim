"=============================================================================
"  Author:          dantezhu - http://www.vimer.cn
"  Email:           zny2008@gmail.com
"  FileName:        authorinfo.vim
"  Description:     
"  Version:         1.5
"  LastChange:      2011-02-23 16:42:42
"  History:         support bash's #!xxx
"                   fix bug for NerdComment's <leader>
"=============================================================================
if exists('g:loaded_authorinfo')
    finish
endif
let g:loaded_authorinfo= 1

if exists("mapleader")
    let s:t_mapleader = mapleader
elseif exists("g:mapleader")
    let s:t_mapleader = g:mapleader
else
    let s:t_mapleader = '\'
endif

function! g:CheckFileType(type)
    let t_filetypes = split(&filetype,'\.')
    if index(t_filetypes,a:type)>=0
        return 1
    else
        return 0
    endif
endfunction
function s:DetectFirstLine()
    "跳转到指定区域的第一行，开始操作
    exe 'normal '.1.'G'
    " 以下用于声明需要在载入模板时跳过的语句，已增加和修改D/Coffee/Lua/Moon/Perl/Blade/Ruby/Racket/newLisp/Clojure/Node/Elixir/Groovy的声明
    " Ruchee 2012-04-13、2012-10-19、2013-01-30、2013-03-20、2013-04-02、2013-04-12、2013-04-28、2013-05-09、2013-07-19、2013-10-20
    " 2014-03-07、2014-03-09
    let arrData = [
                \['d',['^#!.*$']],
                \['coffee',['^#!.*$']],
                \['html',['^<!.*']],
                \['lua',['^#!.*$']],
                \['moon',['^#!.*$']],
                \['racket',['^#!.*$']],
                \['newlisp',['^#!.*$']],
                \['clojure',['^#!.*$']],
                \['perl',['^#!.*$', '^use.*$']],
                \['php',['^<?.*']],
                \['blade',['^<?.*']],
                \['python',['^#!.*$','^#.*coding:.*$']],
                \['ruby',['^#!.*$','^#.*encoding:.*$']],
                \['node',['^#!.*$']],
                \['elixir',['^#!.*$']],
                \['groovy',['^#!.*$']],
                \['sh',['^#!.*$']]
                \]
    let oldNum = line('.')
    while 1
        let line = getline('.')
        let findMatch = 0
        for [t,v] in arrData
            if g:CheckFileType(t)
                for it in v
                    if line =~ it
                        let findMatch = 1
                        break
                    endif
                endfor
            endif
        endfor
        if findMatch != 1
            break
        endif
        normal j
        "到了最后一行了，所以直接o就可以了
        if oldNum == line('.')
            normal o
            return
        endif
        let oldNum = line('.')
    endwhile
    normal O
endfunction

" 以下两函数是用来在Python文件最前面用文档字符串包围作者信息，已注销，不需要
" Ruchee 2012-04-13
function s:BeforeTitle()
    "let arrData = [['python',""]]
    "for [t,v] in arrData
    "    if g:CheckFileType(t)
    "        call setline('.',v)
    "        normal o
    "        break
    "    endif
    "endfor
endfunction
function s:AfterTitle()
    "let arrData = [['python',""]]
    "for [t,v] in arrData
    "    if g:CheckFileType(t)
    "        normal o
    "        call setline('.',v)
    "        normal k
    "        break
    "    endif
    "endfor
endfunction
function s:AddTitle()
    "检查开始插入作者信息的行
    call s:DetectFirstLine()
    "判断是否支持多行注释
    let hasMul = 0
    let preChar = ''
    let noTypeChar = ''

    call setline('.','test mul')
    let oldline = getline('.')
    exec 'normal '.s:t_mapleader.'cm'
    let newline = getline('.')
    if oldline != newline
        let hasMul = 1
        let preChar = '#'
    else
        exec 'normal '.s:t_mapleader.'cl'
        let newline = getline('.')
        if oldline == newline
            let hasMul = -1
            let noTypeChar = '#'
        endif
    endif

    "在第一行之前做的事情
    call s:BeforeTitle()

    let firstLine = line('.')
    call setline('.',noTypeChar.'===================================================================')
    normal o
    call setline('.',noTypeChar.preChar.'    FileName: '.expand("%:t"))
    normal o
    call setline('.',noTypeChar.preChar.'      Author: '.g:vimrc_author)
    normal o
    call setline('.',noTypeChar.preChar.'       Email: '.g:vimrc_email)
    " normal o
    " call setline('.',noTypeChar.preChar.'     WebSite: '.g:vimrc_homepage)
    normal o
    call setline('.',noTypeChar.preChar.'  CreateTime: '.strftime("%Y-%m-%d %H:%M"))
    normal o
    call setline('.',noTypeChar.'===================================================================')
    let lastLine = line('.')
    let gotoLn = line('.')  " 指示光标最后停留的位置

    "在最后一行之后做的事情
    call s:AfterTitle()

    if hasMul == 1
        exe 'normal '.firstLine.'Gv'.lastLine.'G'.s:t_mapleader.'cm'
    else
        exe 'normal '.firstLine.'Gv'.lastLine.'G'.s:t_mapleader.'cl'
    endif

    exe 'normal '.gotoLn.'G'
    startinsert!
    echohl WarningMsg | echo "Succ to add the copyright." | echohl None
endf
function s:TitleDet()
    silent! normal ms
    let updated = 0
    let n = 1
    "默认为添加
    while n < 20
        let line = getline(n)
        if line =~ '^.*FileName:\S*.*$'
            let newline=substitute(line,':\(\s*\)\(\S.*$\)$',':\1'.expand("%:t"),'g')
            call setline(n,newline)
            let updated = 1
        endif
        if line =~ '^.*LastChange:\S*.*$'
            let newline=substitute(line,':\(\s*\)\(\S.*$\)$',':\1'.strftime("%Y-%m-%d %H:%M:%S"),'g')
            call setline(n,newline)
            let updated = 1
        endif
        let n = n + 1
    endwhile
    if updated == 1
        silent! normal 's
        echohl WarningMsg | echo "Succ to update the copyright." | echohl None
        return
    endif
    call s:AddTitle()
endfunction
command! -nargs=0 AuthorInfoDetect :call s:TitleDet()
