![Vim Ruchee](https://raw.github.com/ruchee/vimrc/master/macvim.jpg "Vim Ruchee")

----
#### macOS

1. `brew cask install macvim`
2. `brew install ctags git`
3. 删除个人主目录下的 .vim 文件夹和 .vimrc 文件（如果存在的话） [ 命令为 `rm -rf ~/.vim ~/.vimrc` ]
4. 使用 Git 下载本项目，然后将项目的子目录拷贝到个人主目录下，并将 vimfiles、_vimrc 分别重命名为 .vim、.vimrc [ 命令为 `git clone https://github.com/ruchee/vimrc.git ~/vimrc`、`mv ~/vimrc/vimfiles ~/.vim`、`mv ~/vimrc/_vimrc ~/.vimrc` ]
5. 指定在终端使用 MacVim [ 命令为 `ln -s /Applications/MacVim.app/Contents/bin/* /usr/local/bin` ]

----

#### Linux

1. `sudo apt-get install vim-gtk exuberant-ctags git` [ 非 Debian/Ubuntu 系统安装命令略有不同  ]
2. 删除个人主目录下的 .vim 文件夹和 .vimrc 文件（如果存在的话） [ 命令为 `rm -rf ~/.vim ~/.vimrc` ]
3. 使用 Git 下载本项目，然后将项目的子目录拷贝到个人主目录下，并将 vimfiles、_vimrc 分别重命名为 .vim、.vimrc [ 命令为 `git clone https://github.com/ruchee/vimrc.git ~/vimrc`、`mv ~/vimrc/vimfiles ~/.vim`、`mv ~/vimrc/_vimrc ~/.vimrc`]
4. 下载 Monaco 字体，下载后使用命令 `mv monaco.ttf ~/.fonts` 完成安装 [ Monaco 字体下载地址 [https://github.com/todylu/monaco.ttf/blob/master/monaco.ttf?raw=true](https://github.com/todylu/monaco.ttf/blob/master/monaco.ttf?raw=true) ]
5. 如此这般就配置好了，尽情享受编码的乐趣吧，使用说明全部集中在 .vimrc 文件的头部，配置的后半部分是各插件的具体配置项，初学无需理会

----

#### Windows

1. 访问 [http://www.vim.org/download.php#pc](http://www.vim.org/download.php#pc) 下载最新的 gVim（如果无法访问该网站，请自行使用科学上网方法）
2. 安装 gVim 到任意目录，这儿为方便讲解，我假定你安装到了 D:\Apps\Vim
3. 将 D:\Apps\Vim\vim80 加入 path 环境变量 [ 不知何为环境变量者，请求助于搜索引擎 ]
4. 删除 Vim 安装目录下的 vimfiles 目录以及 _vimrc 文件 [ 如果你自己修改过配置，请注意备份 ]
5. 使用 Git 下载本项目，然后将项目的子目录拷贝到 Vim 安装目录下，取代已删文件的位置 [ 命令为 `git clone https://github.com/ruchee/vimrc.git` ]（也可以点击本页面的 Download ZIP 按钮下载）
6. 访问 [http://ctags.sourceforge.net](http://ctags.sourceforge.net) 下载最新的 ctags，将 ctags.exe 复制到 `D:\Apps\Vim\vim80` 目录
7. 安装 Monaco 字体，本配置默认使用该字体，可上 [https://github.com/todylu/monaco.ttf/blob/master/monaco.ttf?raw=true](https://github.com/todylu/monaco.ttf/blob/master/monaco.ttf?raw=true) 下载，下载后丢到 C:\WINDOWS\Fonts 目录即可
8. 然后。。。然后就大功告成了，接下来只需学习如何使用即可，使用说明全部集中在了 _vimrc 文件的头部，配置的后半部分是各插件的具体配置项，初学无需理会

----

#### Cygwin

1. 启动 Cygwin 安装器，选中 vim、git 为 Install 状态，然后开始安装，直至安装完成
2. 打开 Cygwin 终端，用 Git 下载本项目
3. 余下各步骤与上述 Linux 下的步骤几乎一样，这儿不再赘述
4. 字体安装这一步，因为 Cygwin 沿用的是 Windows 的字体，所以将字体文件放到 C:\WINDOWS\Fonts 下即可

----

### 注意事项

1. 如出现快捷键不响应的情况，请检查你是否开启了其他软件（比如 金山词霸 等），某些软件的快捷键有可能和 Vim 相冲突，只需修改或禁用这些软件的快捷键即可
2. 本配置默认显示的是相对行号，如不习惯，可注释掉配置中的 set relativenumber 一项，以使用绝对行号
3. 如果条件允许，最好用 universal-ctags 替代老旧的 ctags
4. 部分插件需要脚本语言的支持，如果条件允许，请安装 Lua、PHP7、Python3、Ruby 等语言的运行环境
5. 本配置集成了 vim-sync 插件，可以和远程服务器相互传输文件，使用此功能需要先配置项目的 .sync，详情请参考 [https://github.com/eshion/vim-sync/issues/10](https://github.com/eshion/vim-sync/issues/10)
6. 请尽量使用最新的 Vim 8.x 版本，低版本部分功能可能无法使用

----

### ctags 简易的使用说明，这儿以 Windows 下的 MinGW 为例

1. 首先确保系统能够找到 ctags，也就是 ctags 添加到了系统的 path 环境变量
2. 以 MinGW 为例，到编译器安装目录的 include 目录上（譬如 D:\MinGW\include ）执行命令 `ctags -R --languages=c,c++`
3. 在 _vimrc 文件中添加两行 set tags+=D:/MinGW/include/tags、set path+=D:/MinGW/include
4. 以后编辑 C/C++ 源文件时，键入一小部分字符，然后按 Ctrl+P 即可拥有简单的 C/C++ 的代码补全
5. 将光标移到某个函数名上，按 Ctrl+]，Vim 将自动跳转到该函数的定义，按 Ctrl+T 可返回跳转之前的位置

以上只是 ctags 简单的用法，更专业的介绍请 Google，另有更为强大的补全利器 [YouCompleteMe](https://github.com/Valloric/YouCompleteMe) 等你探索（因为安装需要编译，考虑到跨平台使用的便利性，本配置并未集成该插件，请自行安装）

----

本配置文件的更新以我本机的实际使用情况为准

祝使用愉快
