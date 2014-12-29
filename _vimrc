" -----------------   Author: Ruchee
" -----------------    Email: my@ruchee.com
" -----------------     Date: 2014-12-29 23:13
" -----------------    https://github.com/ruchee/vimrc


" 判断工作地点，根据指定路径的文件是否存在判断
" （仅为我个人所用，如无工作场所切换需求，可将本文件所有 atCompany 相关的判断语句全部去掉）
if filereadable("~/.atCompany") || filereadable("C:/atCompany.txt")
    let g:atCompany = 1
else
    let g:atCompany = 0
endif

" 针对不同的使用环境进行具体配置
if g:atCompany
    " set tags+=D:/Ruchee/Files/code/wuyun/kernel/tags
    " set tags+=D:/Ruchee/Files/code/wuyun/bg/tags
    set tags+=D:/Ruchee/Files/code/baofeng/baofeng_game/tags

    set tags+=D:/Ruchee/Files/code/self/thinkphp/tags
else
    set tags+=~/code/fms/thinkphp/tags
endif


" ---------- Ctrl系按键 ----------
"
" Ctrl + H                   --光标移当前行行首       [插入模式]
" Ctrl + J                   --光标移下一行行首       [插入模式]
" Ctrl + K                   --光标移上一行行尾       [插入模式]
" Ctrl + L                   --光标移当前行行尾       [插入模式]

" ---------- Meta系按键 ----------
"
" Alt  + H                   --光标左移一格           [插入模式] [Linux下可能失效]
" Alt  + J                   --光标下移一格           [插入模式] [Linux下可能失效]
" Alt  + K                   --光标上移一格           [插入模式] [Linux下可能失效]
" Alt  + L                   --光标右移一格           [插入模式] [Linux下可能失效]

" ---------- Leader系按键 ----------
"
" \c                         --复制至公共剪贴板       [仅选择模式]
" \a                         --复制所有至公共剪贴板   [Normal模式可用]
" \v                         --从公共剪贴板粘贴       [全模式可用]
"
" \rr                        --一键编译&&运行         [全模式可用]
" \rb                        --一键去除所有尾部空白   [全模式可用]
" \rm                        --一键去除字符         [全模式可用]
" \rt                        --一键替换全部Tab为空格  [全模式可用]
"
" \nt                        --打开NERDTree文件树窗口
" \tl                        --打开/关闭TagList/TxtBrowser窗口
" \be                        --打开BufExplorer窗口    [独立显示] [Normal模式可用]
" \bs                        --打开BufExplorer窗口    [分割显示] [Normal模式可用]
" \bv                        --打开BufExplorer窗口    [边栏显示] [Normal模式可用]
" \fe                        --打开/关闭文件编码窗口  [Normal模式可用]
" \ig                        --显示/关闭对齐线
" \bb                        --按=号对齐代码
" \bn                        --自定义对齐
" \th                        --一键生成与当前编辑文件同名的HTML文件 [不输出行号]
" \ev                        --编辑当前所使用的Vim配置文件
"
" \cc                        --添加注释               [NERD_commenter]
" \cu                        --取消注释               [NERD_commenter]
" \cm                        --添加块注释             [NERD_commenter]
" \cs                        --添加SexStyle块注释     [NERD_commenter]
"
" \php                       --一键切换到PHP语法高亮
" \js                        --一键切换到JavaScript语法高亮
" \css                       --一键切换到CSS语法高亮
" \html                      --一键切换到HTML语法高亮

" ---------- 补全命令 ----------
"
" Ctrl + P                   --缓冲区补全             [插入模式]
" Ctrl + Z                   --omni补全               [插入模式] [只支持定义了omnifunc的语言]
" Tab键                      --语法结构补全           [插入模式] [snipMate插件]
" Ctrl + Y + ,               --HTML标签补全           [插入模式] [emmet插件]

" ---------- 格式化命令 ----------
"
" ==                         --缩进当前行
" =G                         --缩进直到文件结尾
" gg=G                       --缩进整个文件
" 行号G=行号G                --缩进指定区间

" u [小写]                   --单步复原               [非插入模式]
" U [大写]                   --整行复原               [非插入模式]
" Ctrl + R                   --撤消“撤消”操作         [非插入模式]
"
" ---------- 查看命令 ----------
"
" Ctrl+G                     --显示当前文件和光标的粗略信息
" g Ctrl+G                   --显示当前文件和光标的详细信息
"
" ---------- 搜索命令 ----------
"
" #                          --向前搜索当前光标所在字符
" *                          --向后搜索当前光标所在字符
" ?                          --向前搜索
" /                          --向后搜索
"
" ---------- 跳转命令 ----------
"
" Ctrl + ]                   --转到函数定义           [ctags跳转]
" Ctrl + T                   --返回调用函数           [ctags跳转]
" g Ctrl+]                   --列出可选跳转列表       [ctags跳转]

" 0 or ^ or $                --跳至 行首 or 第一个非空字符 or 行尾
" %                          --在匹配的括号间跳跃
" { or }                     --按段落上/下跳跃
" f字符                      --跳至从当前光标开始本行第一个指定字符出现的位置
" gd                         --跳至当前光标所在单词首次出现的位置
" gf                         --打开当前光标所在的文件名，如果确实存在该文件的话
"
" [ Ctrl+D                   --跳至当前光标所在变量的首次定义位置 [从文件头部开始]
" [ Ctrl+I                   --跳至当前光标所在变量的首次出现位置 [从文件头部开始]
" [ D                        --列出当前光标所在变量的所有定义位置 [从文件头部开始]
" [ I                        --列出当前光标所在变量的所有出现位置 [从文件头部开始]
"
" ---------- 文本操作 ----------
"
" dw de d0 d^ d$ dd          --删除
" cw ce c0 c^ c$ cc          --删除并进入插入模式
" yw ye y0 y^ y$ yy          --复制
" vw ve v0 v^ v$ vv          --选中
"
" di分隔符                   --删除指定分隔符之间的内容 [不包括分隔符]
" ci分隔符                   --删除指定分隔符之间的内容并进入插入模式 [不包括分隔符]
" yi分隔符                   --复制指定分隔符之间的内容 [不包括分隔符]
" vi分隔符                   --选中指定分隔符之间的内容 [不包括分隔符]
"
" da分隔符                   --删除指定分隔符之间的内容 [包括分隔符]
" ca分隔符                   --删除指定分隔符之间的内容并进入插入模式 [包括分隔符]
" ya分隔符                   --复制指定分隔符之间的内容 [包括分隔符]
" va分隔符                   --选中指定分隔符之间的内容 [包括分隔符]
"
" Xi和Xa都可以在X后面加入一个数字，以指代所处理的括号层次
" 如 d2i( 执行的是删除当前光标外围第二层括号内的所有内容
"
" dt字符                     --删除本行内容，直到遇到第一个指定字符 [不包括该字符]
" ct字符                     --删除本行内容，直到遇到第一个指定字符并进入插入模式 [不包括该字符]
" yt字符                     --复制本行内容，直到遇到第一个指定字符 [不包括该字符]
" vt字符                     --选中本行内容，直到遇到第一个指定字符 [不包括该字符]
"
" df字符                     --删除本行内容，直到遇到第一个指定字符 [包括该字符]
" cf字符                     --删除本行内容，直到遇到第一个指定字符并进入插入模式 [包括该字符]
" yf字符                     --复制本行内容，直到遇到第一个指定字符 [包括该字符]
" vf字符                     --选中本行内容，直到遇到第一个指定字符 [包括该字符]
"
" XT 和 XF 是 Xt/Xf 的反方向操作
"
" cs'"                        --将外围的单引号变成双引号     [surround.vim插件]
" cs"<p>                      --将外围的双引号变成HTML标签对 [surround.vim插件]
" cst"                        --将外围的界定符变成双引号     [surround.vim插件]
" ds"                         --删除外围的双引号定界符       [surround.vim插件]
"
" ---------- 便捷操作 ----------
"
" Ctrl + A                   --将当前光标所在数字自增1        [仅普通模式可用]
" Ctrl + X                   --将当前光标所在数字自减1        [仅普通模式可用]
" :g/^/m0                    --将整个文件所有行排列顺序颠倒   [命令模式]
" m字符       and '字符      --标记位置 and 跳转到标记位置
" q字符 xxx q and @字符      --录制宏   and 执行宏
"
" ---------- 代码折叠 ----------
"
" zc                         --折叠
" zC                         --对所在范围内所有嵌套的折叠点进行折叠
" zo                         --展开折叠
" zO                         --对所在范围内所有嵌套的折叠点展开
" [z                         --到当前打开的折叠的开始处
" ]z                         --到当前打开的折叠的末尾处
" zj                         --向下移动到后一个折叠的开始处
" zk                         --向上移动到前一个折叠的结束处
"
" ---------- 其他常用内建命令 ------------------------------
"
" :se ff=unix                --更改文件格式，可选 unix、dos、mac
" :se ft=cpp                 --更改文件语法着色模式


" 判断操作系统类型
if(has("win32") || has("win64"))
    let g:isWIN = 1
else
    let g:isWIN = 0
endif

" 判断是否处于GUI界面
if has("gui_running")
    let g:isGUI = 1
else
    let g:isGUI = 0
endif


" 设置通用缩进策略
set shiftwidth=4
set tabstop=4

" 对部分语言设置单独的缩进
au FileType groovy,scala,clojure,scheme,racket,lisp,lua,ruby,eruby,slim,elixir,julia,dart,coffee,jade,sh set shiftwidth=2
au FileType groovy,scala,clojure,scheme,racket,lisp,lua,ruby,eruby,slim,elixir,julia,dart,coffee,jade,sh set tabstop=2

" 根据后缀名指定文件类型
au BufRead,BufNewFile *.h        setlocal ft=c
au BufRead,BufNewFile *.di       setlocal ft=d
au BufRead,BufNewFile *.cl       setlocal ft=lisp
au BufRead,BufNewFile *.phpt     setlocal ft=php
au BufRead,BufNewFile *.inc      setlocal ft=php
au BufRead,BufNewFile *.sql      setlocal ft=mysql
au BufRead,BufNewFile *.tpl      setlocal ft=smarty
au BufRead,BufNewFile *.txt      setlocal ft=txt
au BufRead,BufNewFile hosts      setlocal ft=conf
au BufRead,BufNewFile http*.conf setlocal ft=apache
au BufRead,BufNewFile *.conf     setlocal ft=nginx
au BufRead,BufNewFile *.ini      setlocal ft=dosini


" 设置着色模式和字体
if g:isWIN
    colorscheme molokai
    set guifont=Monaco:h12
else
    colorscheme molokai
    set guifont=Monaco\ 11
endif


set backspace=2              " 设置退格键可用
set autoindent               " 自动对齐
set ai!                      " 设置自动缩进
set smartindent              " 智能自动缩进
set relativenumber           " 开启相对行号
set nu!                      " 显示行号
set mouse=a                  " 启用鼠标
set ruler                    " 右下角显示光标位置的状态行
set incsearch                " 开启实时搜索功能
set hlsearch                 " 开启高亮显示结果
set nowrapscan               " 搜索到文件两端时不重新搜索
set nocompatible             " 关闭兼容模式
set hidden                   " 允许在有未保存的修改时切换缓冲区
set autochdir                " 设定文件浏览器目录为当前目录
set foldmethod=indent        " 选择代码折叠类型
set foldlevel=100            " 禁止自动折叠
set laststatus=2             " 开启状态栏信息
set cmdheight=2              " 命令行的高度，默认为1，这里设为2
set autoread                 " 当文件在外部被修改时自动更新该文件
set nobackup                 " 不生成备份文件
set noswapfile               " 不生成交换文件
set list                     " 显示特殊字符，其中Tab使用高亮~代替，尾部空白使用高亮点号代替
set listchars=tab:\~\ ,trail:.
set expandtab                " 将Tab自动转化成空格 [需要输入真正的Tab键时，使用 Ctrl+V + Tab]
"set showmatch               " 显示括号配对情况
"set nowrap                  " 设置不自动换行

syntax enable                " 打开语法高亮
syntax on                    " 开启文件类型侦测
filetype indent on           " 针对不同的文件类型采用不同的缩进格式
filetype plugin on           " 针对不同的文件类型加载对应的插件
filetype plugin indent on    " 启用自动补全


" 设置文件编码和文件格式
set fenc=utf-8
set encoding=utf-8
set fileencodings=utf-8,gbk,cp936,latin-1
set fileformat=unix
set fileformats=unix,mac,dos
if g:isWIN
    source $VIMRUNTIME/delmenu.vim
    source $VIMRUNTIME/menu.vim
    language messages zh_CN.utf-8
endif


" 使用GUI界面时的设置
if g:isGUI
    " 启动时自动最大化窗口
    if g:isWIN
        au GUIEnter * simalt ~x
    endif
    "winpos 20 20            " 指定窗口出现的位置，坐标原点在屏幕左上角
    "set lines=20 columns=90 " 指定窗口大小，lines为高度，columns为宽度
    set guioptions+=c        " 使用字符提示框
    set guioptions-=m        " 隐藏菜单栏
    set guioptions-=T        " 隐藏工具栏
    set guioptions-=L        " 隐藏左侧滚动条
    set guioptions-=r        " 隐藏右侧滚动条
    set guioptions-=b        " 隐藏底部滚动条
    set showtabline=0        " 隐藏Tab栏
    set cursorline           " 突出显示当前行
endif


" ======= 引号 && 括号自动匹配 ======= "

:inoremap ( ()<ESC>i
:inoremap ) <c-r>=ClosePair(')')<CR>
:inoremap { {}<ESC>i
:inoremap } <c-r>=ClosePair('}')<CR>
:inoremap [ []<ESC>i
:inoremap ] <c-r>=ClosePair(']')<CR>
:inoremap " ""<ESC>i
:inoremap ' ''<ESC>i
:inoremap ` ``<ESC>i

function ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endf


" 加载pathogen插件管理器
execute pathogen#infect()


" 针对部分语言加减指定字符的单词属性
au FileType clojure  set iskeyword-=.
au FileType clojure  set iskeyword-=>
au FileType perl,php set iskeyword-=.
au FileType perl,php set iskeyword-=$
au FileType perl,php set iskeyword-=-
au FileType ruby     set iskeyword+=!
au FileType ruby     set iskeyword+=?


" 针对部分语言添加字典补全
au FileType c          call AddCDict()
au FileType cpp        call AddCPPDict()
au FileType java       call AddJavaDict()
au FileType lua        call AddLuaDict()
au FileType perl       call AddPerlDict()
au FileType php        call AddPHPDict()
au FileType python     call AddPythonDict()
au FileType ruby       call AddRubyDict()
au FileType scala      call AddScalaDict()
au FileType javascript call AddJavaScriptDict()
au FileType css        call AddCSSDict()

function AddCDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/c.txt
    else
        set dict+=~/.vim/dict/c.txt
    endif
    set complete+=k
endfunction

function AddCPPDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/c.txt
        set dict+=$VIM/vimfiles/dict/cpp-stdlib.txt
        set dict+=$VIM/vimfiles/dict/cpp-boost.txt
    else
        set dict+=~/.vim/dict/c.txt
        set dict+=~/.vim/dict/cpp-stdlib.txt
        set dict+=~/.vim/dict/cpp-boost.txt
    endif
    set complete+=k
endfunction

function AddJavaDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/java.txt
    else
        set dict+=~/.vim/dict/java.txt
    endif
    set complete+=k
endfunction

function AddLuaDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/lua.txt
    else
        set dict+=~/.vim/dict/lua.txt
    endif
    set complete+=k
endfunction

function AddPerlDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/perl.txt
    else
        set dict+=~/.vim/dict/perl.txt
    endif
    set complete+=k
endfunction

function AddPHPDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/php.txt
    else
        set dict+=~/.vim/dict/php.txt
    endif
    set complete+=k
endfunction

function AddPythonDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/python.txt
    else
        set dict+=~/.vim/dict/python.txt
    endif
    set complete+=k
endfunction

function AddRubyDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/ruby.txt
    else
        set dict+=~/.vim/dict/ruby.txt
    endif
    set complete+=k
endfunction

function AddScalaDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/scala.txt
    else
        set dict+=~/.vim/dict/scala.txt
    endif
    set complete+=k
endfunction

function AddJavaScriptDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/javascript.txt
        set dict+=$VIM/vimfiles/dict/node.txt
    else
        set dict+=~/.vim/dict/javascript.txt
        set dict+=~/.vim/dict/node.txt
    endif
    set complete+=k
endfunction

function AddCSSDict()
    if g:isWIN
        set dict+=$VIM/vimfiles/dict/css.txt
    else
        set dict+=~/.vim/dict/css.txt
    endif
    set complete+=k
endfunction


" 开启部分语法高亮的非默认特性
let g:go_auto_type_info      = 0               " 关闭Go语言自动显示类型信息（默认就是关闭的，此处用于方便需要时开启）
let g:go_def_mapping_enabled = 0               " 关闭Go语言对gd的绑定
let g:go_highlight_operators = 1               " 开启Go语言操作符高亮
let g:go_highlight_functions = 1               " 开启Go语言函数名高亮
let g:go_highlight_methods   = 1               " 开启Go语言方法名高亮
let g:go_highlight_structs   = 1               " 开启Go语言结构体名高亮
let python_highlight_all     = 1               " 打开全部Python高亮

" BufExplorer         文件缓冲浏览器
let g:bufExplorerSortBy = 'name'               " 按文件名排序

" Tlist               调用TagList
let Tlist_Show_One_File        = 1             " 只显示当前文件的tags
let Tlist_Exit_OnlyWindow      = 1             " 如果Taglist窗口是最后一个窗口则退出Vim
let Tlist_Use_Right_Window     = 1             " 在右侧窗口中显示
let Tlist_File_Fold_Auto_Close = 1             " 自动折叠

" snipMate            Tab智能补全
let g:snips_author = 'Ruchee'
if g:isWIN
    let g:snippets_dir = $VIM.'/snippets/'
else
    let g:snippets_dir = '~/.vim/snippets/'
endif
let g:snipMate                             = {}
" 不使用插件自带的默认继承
let g:snipMate.no_default_aliases          = 1
" 设置补全项之间的继承关系，比如 PHP补全继承HTML的补全
let g:snipMate.scope_aliases               = {}
let g:snipMate.scope_aliases['c']          = 'cpp'
let g:snipMate.scope_aliases['scheme']     = 'racket'
let g:snipMate.scope_aliases['php']        = 'php,html'
let g:snipMate.scope_aliases['blade']      = 'blade,html'
let g:snipMate.scope_aliases['html.twig']  = 'twig,html'
let g:snipMate.scope_aliases['jinja.twig'] = 'twig,html'
let g:snipMate.scope_aliases['jinja']      = 'jinja,html'
let g:snipMate.scope_aliases['eruby']      = 'eruby,html'
let g:snipMate.scope_aliases['jst']        = 'jst,html'
let g:snipMate.scope_aliases['mustache']   = 'mustache,html'
let g:snipMate.scope_aliases['scss']       = 'scss,css'
let g:snipMate.scope_aliases['less']       = 'less,css'
let g:snipMate.scope_aliases['xhtml']      = 'html'

" NERD_commenter      注释处理插件
let NERDSpaceDelims = 1                        " 自动添加前置空格

" Indent_guides       显示对齐线
let g:indent_guides_enable_on_vim_startup = 0  " 默认关闭
let g:indent_guides_guide_size            = 1  " 指定对齐线的尺寸

" AirLine             彩色状态栏
let g:airline_theme = 'badwolf'                " 设置主题

" Syntastic           语法检查
let g:syntastic_check_on_open = 1              " 默认开启
let g:syntastic_mode_map      = {'mode': 'active',
            \'active_filetypes':  [],
            \'passive_filetypes': ['html', 'css', 'xhtml', 'go', 'groovy', 'scala', 'clojure', 'racket', 'eruby', 'slim', 'jade', 'scss', 'less']
            \}                                 " 指定不需要检查的语言 [主要是因为开启这些语言的语法检查会妨碍到正常的工作]
" 自定义编译器和编译参数
let g:syntastic_c_compiler = 'gcc'
let g:syntastic_cpp_compiler = 'g++'
let g:syntastic_c_compiler_options = '-std=c11 -Wall'
let g:syntastic_cpp_compiler_options = '-std=c++11 -Wall'

" javascript-libraries-syntax                    指定需要高亮的JS库
let g:used_javascript_libs = 'jquery,angularjs'


" ======= 自定义快捷键 ======= "

" Ctrl + H            光标移当前行行首[插入模式]、切换左窗口[Normal模式]
imap <c-h> <ESC>I
map <c-h> <c-w><c-h>

" Ctrl + J            光标移下一行行首[插入模式]、切换下窗口[Normal模式]
imap <c-j> <ESC><Down>I
map <c-j> <c-w><c-j>

" Ctrl + K            光标移上一行行尾[插入模式]、切换上窗口[Normal模式]
imap <c-k> <ESC><Up>A
map <c-k> <c-w><c-k>

" Ctrl + L            光标移当前行行尾[插入模式]、切换右窗口[Normal模式]
imap <c-l> <ESC>A
map <c-l> <c-w><c-l>

" Alt  + H            光标左移一格
imap <m-h> <Left>

" Alt  + J            光标下移一格
imap <m-j> <Down>

" Alt  + K            光标上移一格
imap <m-k> <Up>

" Alt  + L            光标右移一格
imap <m-l> <Right>

" Ctrl + Z            omni补全
imap <c-z> <c-x><c-o>

" \c                  复制至公共剪贴板
vmap <leader>c "+y

" \a                  复制所有至公共剪贴板
nmap <leader>a <ESC>ggVG"+y<ESC>

" \v                  从公共剪贴板粘贴
imap <leader>v <ESC>"+p
nmap <leader>v "+p
vmap <leader>v "+p

" \bb                 按=号对齐代码 [Tabular插件]
nmap <leader>bb :Tab /=<CR>

" \bn                 自定义对齐    [Tabular插件]
nmap <leader>bn :Tab /

" \nt                 打开NERDTree窗口，在左侧栏显示
nmap <leader>nt :NERDTree<CR>

" \tl                 打开Taglist/TxtBrowser窗口，在右侧栏显示
nmap <leader>tl :Tlist<CR><c-w><c-l>

" \fe                 打开文件编码窗口，在右侧栏显示 [FencView插件]
nmap <leader>fe :FencView<CR>

" \rb                 一键去除所有尾部空白
imap <leader>rb <ESC>:let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
nmap <leader>rb :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
vmap <leader>rb <ESC>:let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" \rm                 一键去除字符
imap <leader>rm <ESC>:%s/<c-v><c-m>//g<CR>
nmap <leader>rm :%s/<c-v><c-m>//g<CR>
vmap <leader>rm <ESC>:%s/<c-v><c-m>//g<CR>

" \rt                 一键替换全部Tab为空格
func! RemoveTabs()
    if &shiftwidth == 2
        exec "%s/	/  /g"
    elseif &shiftwidth == 4
        exec "%s/	/    /g"
    elseif &shiftwidth == 6
        exec "%s/	/      /g"
    elseif &shiftwidth == 8
        exec "%s/	/        /g"
    else
        exec "%s/	/ /g"
    end
endfunc

imap <leader>rt <ESC>:call RemoveTabs()<CR>
nmap <leader>rt :call RemoveTabs()<CR>
vmap <leader>rt <ESC>:call RemoveTabs()<CR>

" \th                 一键生成与当前编辑文件同名的HTML文件 [不输出行号]
imap <leader>th <ESC>:set nonumber<CR>:set norelativenumber<CR><ESC>:TOhtml<CR><ESC>:w %:r.html<CR><ESC>:q<CR>:set number<CR>:set relativenumber<CR>
nmap <leader>th <ESC>:set nonumber<CR>:set norelativenumber<CR><ESC>:TOhtml<CR><ESC>:w %:r.html<CR><ESC>:q<CR>:set number<CR>:set relativenumber<CR>
vmap <leader>th <ESC>:set nonumber<CR>:set norelativenumber<CR><ESC>:TOhtml<CR><ESC>:w %:r.html<CR><ESC>:q<CR>:set number<CR>:set relativenumber<CR>

" \ev                 编辑当前所使用的Vim配置文件
nmap <leader>ev <ESC>:e $MYVIMRC<CR>

" \php                一键切换到PHP语法高亮
imap <leader>php <ESC>:se ft=php<CR>li
nmap <leader>php <ESC>:se ft=php<CR>

" \js                 一键切换到JavaScript语法高亮
imap <leader>js <ESC>:se ft=javascript<CR>li
nmap <leader>js <ESC>:se ft=javascript<CR>

" \css                一键切换到CSS语法高亮
imap <leader>css <ESC>:se ft=css<CR>li
nmap <leader>css <ESC>:se ft=css<CR>

" \html               一键切换到HTML语法高亮
imap <leader>html <ESC>:se ft=html<CR>li
nmap <leader>html <ESC>:se ft=html<CR>


" ======= 编译 && 运行 && 模板 ======= "

" 编译并运行
func! Compile_Run_Code()
    exec "w"
    if &filetype == "c"
        if g:isWIN
            exec "!gcc -Wall -std=c11 -o %:r %:t && %:r.exe"
        else
            exec "!clang -Wall -std=c11 -o %:r %:t && ./%:r"
        endif
    elseif &filetype == "cpp"
        if g:isWIN
            exec "!g++ -Wall -std=c++11 -o %:r %:t && %:r.exe"
        else
            exec "!clang++ -Wall -std=c++11 -o %:r %:t && ./%:r"
        endif
    elseif &filetype == "d"
        if g:isWIN
            exec "!dmd -wi %:t && del %:r.obj && %:r.exe"
        else
            exec "!gdc -Wall -o %:r %:t && ./%:r"
        endif
    elseif &filetype == "go"
        exec "!go run %:t"
    elseif &filetype == "rust"
        if g:isWIN
            exec "!rustc %:t && %:r.exe"
        else
            exec "!rustc %:t && ./%:r"
        endif
    elseif &filetype == "java"
        exec "!javac %:t && java %:r"
    elseif &filetype == "groovy"
        exec "!groovy %:t"
    elseif &filetype == "scala"
        exec "!scala %:t"
    elseif &filetype == "clojure"
        exec "!clojure -i %:t"
    elseif &filetype == "cs"
        if g:isWIN
            exec "!csc %:t && %:r.exe"
        else
            exec "!mono-csc %:t && ./%:r"
        endif
    elseif &filetype == "fsharp"
        if g:isWIN
            exec "!fsc %:t && %:r.exe"
        else
            exec "!fsharpc %:t && ./%:r"
        endif
    elseif &filetype == "scheme"
        exec "!mit-scheme --load %:t"
    elseif &filetype == "racket"
        exec "!racket -fi %:t"
    elseif &filetype == "lisp"
        if g:isWIN
            exec "!ccl -l %:t"
        else
            exec "!clisp -i %:t"
        endif
    elseif &filetype == "ocaml"
        if g:isWIN
            exec "!ocamlc -o %:r.exe %:t && %:r.exe"
        else
            exec "!ocamlc -o %:r %:t && ./%:r"
        endif
    elseif &filetype == "haskell"
        if g:isWIN
            exec "!ghc -o %:r %:t && %:r.exe"
        else
            exec "!ghc -o %:r %:t && ./%:r"
        endif
    elseif &filetype == "lua"
        exec "!lua %:t"
    elseif &filetype == "perl"
        exec "!perl %:t"
    elseif &filetype == "php"
        exec "!php %:t"
    elseif &filetype == "python"
        exec "!python %:t"
    elseif &filetype == "ruby"
        exec "!ruby %:t"
    elseif &filetype == "elixir"
        exec "!elixir %:t"
    elseif &filetype == "julia"
        exec "!julia %:t"
    elseif &filetype == "dart"
        exec "!dart %:t"
    elseif &filetype == "r"
        exec "!Rscript %:t"
    elseif &filetype == "coffee"
        exec "!coffee %:t"
    elseif &filetype == "javascript"
        exec "!node %:t"
    elseif &filetype == "sh"
        exec "!bash %:t"
    endif
endfunc

" \rr        一键保存、编译、运行
imap <leader>rr <ESC>:call Compile_Run_Code()<CR>
nmap <leader>rr :call Compile_Run_Code()<CR>
vmap <leader>rr <ESC>:call Compile_Run_Code()<CR>
