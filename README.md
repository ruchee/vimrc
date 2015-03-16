![Vim Ruchee](https://raw.github.com/ruchee/mysite/master/public/images/vim.png "Vim Ruchee")

----

##### 配置文件的着色版本：`http://www.ruchee.com/public/_vimrc.html`

----

## 本配置文件使用指南

----

### 使用方法

#### Windows

1. 访问 `http://www.vim.org/download.php#pc` 下载最新的 gVim [如无法访问该网站，可上 `http://vim.wendal.net/download.php#pc` 下载]
2. 安装 gVim 到任意目录，这儿为方便讲解，我假定你安装到了 `D:\Apps\Vim`
3. 将 `D:\Apps\Vim\vim74` 加入 path 环境变量 [不知何为环境变量者，请求助于搜索引擎]
4. 删除 Vim 安装目录下的 vimfiles 目录以及 _vimrc 文件 [如果你自己修改过配置，请注意备份]
5. 使用 Git 下载本项目，然后将本项目的子目录拷贝到 Vim 安装目录下，取代已删文件的位置 [命令为 `git clone https://github.com/ruchee/vimrc.git`]（也可以点击本页面的 Download ZIP 按钮下载）
6. 访问 `http://ctags.sourceforge.net` 下载最新的 ctags，将 ctags.exe 复制到 `D:\Apps\Vim\vim74` 目录 [如无法访问该网站，可上 `https://code.google.com/p/unix-cmd-win32/downloads/list` 下载]
7. 推荐安装 Monaco 字体，因为本配置默认使用该字体，可上 `https://github.com/ruchee/backup/blob/master/download/MONACO.TTF?raw=true` 下载，下载后丢到 `C:\WINDOWS\Fonts` 目录即可
8. 使用任意文本编辑器打开 _vimrc，将名字、邮箱、网址等全部替换为你自己的信息，如遇路径不同也全部替换为你本机的实际路径
9. 然后。。。然后就大功告成了，接下只需学习如何使用而已，使用说明全部集中在了 `_vimrc` 文件的头部，配置的后半部分全是各插件的具体配置项，初学无需理会

----

#### Mac OSX

1. 首先安装 `brew` 包管理器，如果已经安装则此步可跳过：`ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`
2. `brew install macvim ctags git`
3. 删除个人主目录下的 .vim 文件夹和 .vimrc 文件，如果没有则不需要执行删除动作 [使用命令 `rm -rf ~/.vim ~/.vimrc`，请注意备份]
4. 使用 Git 下载本项目，然后将本项目的子目录拷贝到个人主目录下，取代已删文件的位置，然后将 vimfiles、_vimrc 改名为 .vim、.vimrc [命令为 `git clone https://github.com/ruchee/vimrc.git`、`mv ~/vimrc/vimfiles ~/.vim`、`mv ~/vimrc/_vimrc ~/.vimrc`]（也可以点击本页面的 Download ZIP 按钮下载）
5. 使用任意文本编辑器打开 .vimrc，将名字、邮箱、网址等全部替换为你自己的信息，如遇路径不同也全部替换为你本机的实际路径
6. 将 `vim` 命令指定为使用 `macvim` [命令为 `cd /usr/local/bin && ln -s mvim vim`]

----

#### Linux [具体指 Debian、Ubuntu 及其各衍生版，譬如 LinuxMint、Kubuntu、Xubuntu 等]

1. `sudo apt-get install vim-gtk exuberant-ctags` [其他非 Debian 系的 Linux 请使用其自己的包管理器进行安装]
2. 删除个人主目录下的 .vim 文件夹和 .vimrc 文件，如果没有则不需要执行删除动作 [使用命令 `rm -rf ~/.vim ~/.vimrc`，请注意备份]
3. 使用 Git 下载本项目，然后将本项目的子目录拷贝到个人主目录下，取代已删文件的位置，然后将 vimfiles、_vimrc 改名为 .vim、.vimrc [命令为 `git clone https://github.com/ruchee/vimrc.git`、`mv ~/vimrc/vimfiles ~/.vim`、`mv ~/vimrc/_vimrc ~/.vimrc`]（也可以点击本页面的 Download ZIP 按钮下载）
4. 可上 `https://github.com/ruchee/backup/blob/master/download/MONACO.TTF?raw=true` 下载 Monaco 字体，下载后使用命令 `mv MONACO.TTF ~/.fonts` 将其丢到 `~/.fonts` 目录即可
5. 使用任意文本编辑器打开 .vimrc，将名字、邮箱、网址等全部替换为你自己的信息，如遇路径不同也全部替换为你本机的实际路径
6. 如此这般就配置好了，尽情享受编码的乐趣吧，使用说明全部集中在 .vimrc 文件的头部，配置的后半部分全是各插件的具体配置项，初学无需理会

----

#### Cygwin

1. 启动 Cygwin 安装器，选中 vim、git 为 `Install` 状态，然后开始安装，直至安装完成
2. 打开 Cygwin 终端，用 Git 下载本项目
3. 余下各步骤与上述 Linux 下的步骤几乎一样，这儿就不再赘述了
4. 字体安装这一步，Cygwin 源用的是 Windows 中的字体，所以将字体文件放到 `C:\WINDOWS\Fonts` 下即可

----

### 注意事项

1. `Windows` 下需要的软件：gvim、ctags
2. `Linux` 下需要的包文件：vim-gtk、exuberant-ctags
3. `Linux` 下必须使用 GUI 界面，否则 Meta 系按键将失效，可在 .bashrc 文件里面写入这么一行：`alias vim='gvim'`
4. 配置文件前面部分的 tags、path 路径是我本人开发所用，你可以将其删除，也可以替换成自己的工程路径 [你需要先用 ctags 生成 tags 文件]
5. 可使用这两条命令使 Linux、Cygwin 以及 Windows 共用同一套配置 [当然，这儿假设你安装的是双系统]：`ln -s your_gvim_path/vimfiles ~/.vim`、`ln -s your_gvim_path/_vimrc ~/.vimrc`
6. 此仓库包含的 snippets 补全文件只是我自己使用的那一部分，更多补全可上这下载： `https://github.com/ruchee/backup2/tree/master/snippets`
7. 如出现快捷键不响应的情况，请检查你是否开启了其他软件（比如 `金山词霸` 等），某些软件的快捷键有可能和 Vim 相冲突，只需修改或禁用这些软件的快捷键即可
8. 本配置默认显示的是相对行号，如不习惯，可注释掉配置中的 `set relativenumber` 一项，以使用绝对行号
9. 请尽量使用最新的 `Vim7.4` 版本，`7.4` 更新了一大批语法文件，部分之前需要自己下载语法插件的语言现在已经内建支持

----

### ctags简易的使用说明，这儿以Windows下的MinGW为例

1. 首先确保系统能够找到 ctags，也就是 ctags 添加到了系统的 path 环境变量
2. 以 MinGW 为例，到编译器安装目录的 include 目录上（譬如 `D:\MinGW\include` ）执行命令 `ctags -R --languages=c,c++`
3. 在 _vimrc 文件中添加两行 `set tags+=D:/MinGW/include/tags`、`set path+=D:/MinGW/include`
4. 以后编辑 C/C++ 源文件时，键入一小部分字符，然后按 `Ctrl+P` 即可拥有 C/C++ 的代码补全
5. 将光标移到某个函数名上，按 `Ctrl+]`，Vim 将自动跳转到该函数的定义，按 `Ctrl+T` 可返回跳转之前的位置

以上只是 ctags 简单的用法，更专业的介绍请 Google

----

本配置文件的更新以我本机的实际使用为准，有安装和使用上的疑问，可访问这个地址 [`https://github.com/ruchee/vimrc/issues`] 提交反馈

祝使用愉快，Thanks!

by Ruchee
