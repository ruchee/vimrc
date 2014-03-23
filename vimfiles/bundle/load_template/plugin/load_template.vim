"=============================================================================
"  Author:          DanteZhu - http://www.vimer.cn
"  Email:           dantezhu@vip.qq.com
"  FileName:        load_template.vim
"  Version:         1.0
"  LastChange:      2010-02-01 09:22:35
"  Description:     自动载入各种模板
"					1）下载load_template.vim插件，并放在plugin的目录下
"					2）在vimrc文件中指定你的模板目录，来指定插件读取模板的目录。
"					如在下载包中已经带了一个template的目录，里面按照文件扩展名、文件类型，
"					分别建立了目录，你可以将这个template文件夹复制到C:/，那么就可以指定
"					let g:template_path = 'C:\template\'
"					注意，最后的'\'请务必不要忘记（在linux下也是一样要加 '/' ）
"					3）新建/打开一个test.cpp，执行
"					:LoadTemplate
"					试试看，是否有类似模板列表出来啦~
"					Template filelist:
"					0		:		cpp/main.cpp
"					1		:		cpp/singleton.h
"					please select:
"					4）你甚至可以自己制作模板文件~~只需要参考已经存在的模板文件就行啦~~
"					支持vim脚本的哦
"
"  					欢迎来 http://www.vimer.cn来分享你的模板文件，或者直接给我
"  					发送邮件zny2008@gmail.com/dantezhu@vip.qq.com
"  History:
"=============================================================================
command! -nargs=? LoadTemplate call LoadTemplate(<f-args>)

"类似宏定义{{{
if(has("win32") || has("win95") || has("win64") || has("win16"))
	if !exists('g:vimrc_iswindows')
		let g:vimrc_iswindows=1
	endif
	if !exists('g:vimrc_splitstr')
		let g:vimrc_splitstr='\'
	endif
	if !exists('g:template_path')
		let g:template_path=$VIM.'\vimfiles\template\'
	endif
else
	if !exists('g:vimrc_iswindows')
		let g:vimrc_iswindows=0
	endif
	if !exists('g:vimrc_splitstr')
		let g:vimrc_splitstr='/'
	endif
	if !exists('g:template_path')
		let g:template_path=$HOME.'/.vim/template/'
	endif
endif
"是否需要在新buf里操作
let s:template_neednew = 'NEEDNEW'
let s:template_sourcefile = '~'.g:vimrc_splitstr.'template_source.vim'
let s:template_cursor = 'TEMPLATE_CURSOR'
"}}}

"对外的主函数
function LoadTemplate(...) "{{{
	let type = (a:0 ? a:1 : '')

	"获取扩展名或者类型名{{{
	let extension = expand ("%:e")
	let ftype = &filetype

	if type == "" && extension == "" && ftype == ""
		echohl WarningMsg | echo "No extension or filetype!" | echohl None
		return
	endif
	"}}}


	let templateFileList = []

	let t_path = g:template_path
	if type == ""
		call s:AddTemplateFile(templateFileList,t_path,'*.'.extension)
		call s:AddTemplateFile(templateFileList,t_path,'*.'.ftype)
		call s:AddTemplateFile(templateFileList,t_path,extension.g:vimrc_splitstr.'*')
		call s:AddTemplateFile(templateFileList,t_path,ftype.g:vimrc_splitstr.'*')
	else
		call s:AddTemplateFile(templateFileList,t_path,'*.'.type)
		call s:AddTemplateFile(templateFileList,t_path,type.g:vimrc_splitstr.'*')
	endif

	if len(templateFileList) == 0
		echohl WarningMsg | echo "No template file found!" | echohl None
		return
	endif

	"如果就一个模板，就直接载入就行
	if len(templateFileList) == 1
		call s:LoadFile(templateFileList[0])
		return
	endif

	echohl WarningMsg | echo "Template filelist:" | echohl None

	if g:vimrc_iswindows == 1
		let substr = substitute(t_path,g:vimrc_splitstr,'\\\\','g')
	else
		let substr = t_path
	endif
	for idx in range(len(templateFileList))
		let showname = substitute(templateFileList[idx],substr,'','g')
		echo idx.'	:	'.showname
	endfor

	let rangeStr = input('Please select : ')

	let selIdx = 0
	if len(rangeStr) == 0
		return
	elseif rangeStr =~ '^\d\+$'
		if rangeStr >= len(templateFileList)
			echohl WarningMsg | echo "Wrong select index!" | echohl None
			return
		else
			let selIdx = rangeStr
		endif
	else
		echohl WarningMsg | echo "Wrong input Format!" | echohl None
		return
	endif
	call s:LoadFile(templateFileList[selIdx])
endfunction
"}}}

"加载list
function s:AddTemplateFile(list,path,filter) "{{{
	let filesStr = globpath(a:path, a:filter)
	let files = split(filesStr, "\n")
	for i in files
		if isdirectory(i) || !filereadable(i)
			continue
		endif
		if count(a:list, i) == 0
			call add(a:list, i)
		endif
	endfor
endfunction
"}}}

"加载模板文件
function s:LoadFile(path) "{{{
	let preFileType = &filetype
	let line = getline('.')
	if(line =~ '^\s*$')
		let pChar = 'P'
	else
		let pChar = 'p'
	endif

	silent execute "new"
	silent execute "0r ".a:path
	silent execute "normal Gdd"

	let needNew = 0
	let i = 1
	while i <= line('$')
		let line = getline(i)
		if line =~ '\s*EXE_BEGIN_TEMPLATE.*'
			if line =~ '.*\s'.s:template_neednew.'.*'
				let needNew = 1
			else
				let needNew = 0
			endif
			break
		endif

		let i = i+1
	endwhile

	if needNew == 0
		silent execute "normal ggyG"
		silent execute "q!"
		silent execute "normal ".pChar
		call s:ExeAferLoad()
	else
		call s:ExeAferLoad()
		silent execute "normal ggyG"
		silent execute "q!"
		silent execute "normal ".pChar
	endif
	if preFileType != ''
		silent execute "setf ".preFileType
	endif

	"指定光标位置
	silent! call search(s:template_cursor)
	let w = expand("<cword>")
	if w == s:template_cursor 
		silent normal diw
		startinsert
	endif
endfunction
"}}}

"自动执行最后的脚本
function s:ExeAferLoad() "{{{
	let exeList = []

	let i = 1
	let firstLine = -1
	let lastLine = -1
	while i <= line('$')
		let line = getline(i)
		if line =~ '\s*EXE_BEGIN_TEMPLATE.*'
			let firstLine = i
		elseif line =~ '\s*EXE_END_TEMPLATE\s*'
			let lastLine = i
			break
		endif

		let i = i+1
	endwhile
	if firstLine != -1 && lastLine != -1 && firstLine != lastLine
		if ((firstLine+1)<=(lastLine-1))
			silent execute (firstLine+1).",".(lastLine-1)."w! ".s:template_sourcefile
		endif
		silent execute "normal ".firstLine."G"
		silent execute "normal ".lastLine."dG"
		if ((firstLine+1)<=(lastLine-1))
			execute "source ".s:template_sourcefile
		endif
	endif
endfunction
"}}}
