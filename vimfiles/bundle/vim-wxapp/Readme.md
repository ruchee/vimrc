# Wxapp.vim

微信小程序开发 vim 插件。

提供包含文件检测、智能补全、文档跳转、语法高亮、缩进、代码段、单词列表、语法检查等功能。

## 目录

- [安装方式](安装方式)
- [效果图](#效果图)
- [功能列表](功能列表)
- [智能补全](#智能补全)
- [目录生成](#目录生成)
- [推荐插件](#推荐插件)
- [语法检查](#语法检查)
- [待完成](#待完成)

## 安装方式


以下分别是使用 [NeoBundle](https://github.com/Shougo/neobundle.vim) [Vundle](https://github.com/VundleVim/Vundle.vim) [vim-plug](https://github.com/junegunn/vim-plug) 安装的命令：


    NeoBundle 'chemzqm/wxapp.vim'
    Plugin 'chemzqm/wxapp.vim'
    Plug 'chemzqm/wxapp.vim'


## 效果图

文件生成

* js 智能补全

![wxapp](https://cloud.githubusercontent.com/assets/251450/21974010/31568a52-dbff-11e6-994d-9ce011baa058.gif)

* 插入代码块

![two](https://cloud.githubusercontent.com/assets/251450/18817568/222c1180-8396-11e6-9bed-a175d81f201f.gif)

* 使用 [unite](https://github.com/Shougo/unite.vim) 查找并插入代码

![three](https://cloud.githubusercontent.com/assets/251450/18817569/27e7db54-8396-11e6-85e2-3f82fc07365e.gif)


## 功能列表

* [页面目录生成](#目录生成)
* wxml 和 wxss 文件检测, 代码高亮, 缩进函数 (推荐快捷键 `=at` `=a{`)
* wxml, wxss 以及 javascript dictionary 文件, 使用参考：[vim dictionary 的使用方式](https://chemzqm.me/vim-dictionary)
* wxml 和 javascript [Ultisnips](https://github.com/SirVer/ultisnips) 代码块补全
* wxml 和 wxss 的[语法检查支持](#语法检查)
* wxml wxss 以及 JavaScript omnicomplete 补全

## 智能补全

本插件提供比官方 IDE 更为完整实用的智能补全,  具体实现和使用方式如下：

* 针对 wxcss 使用的 vim runtime 自带的 css 补全，插件内已进行如下配置：

    setl omnifunc=csscomplete#CompleteCSS

* 针对 wxml 实现并且配置了函数 `wxmlcomplete#Complete`, 它能提供标签、
  属性、属性值的智能补全（官方 IDE 仅支持标签）

* 针对 javscript 本插件提供了 [tern](https://github.com/ternjs/tern)
  的插件，内含所有 wx 相关 API 的函数补全，使用前需要首先安装 
  [tern-for-vim](https://github.com/ternjs/tern_for_vim)
  插件。

  在 tern-for-vim 插件下执行：
  
        npm install https://github.com/ternjs/tern/tarball/master -S

  命令安装 tern。

  **npm 上的 tern 还不支持参数内属性补全，请安装使用 master 分支**

  将文件 `tern/wxapp.json` 拷贝到所有 小程序项目的父目录下，
  最后在小程序项目的根目录下配置文件 `.tern-project` 为：

  ``` json
  {
    "libs": [
      "browser",
      "../wxapp"
    ],
    "loadEagerly": [
    ],
    "plugins": {
      "doc_comment": {},
      "es_modules": {},
      "commonjs": {}
    }
  }
  ```
  
  * `doc_comment` 插件用于反射 jsdoc 的文档
  * `es_modules` 用于 es6 模块的路径解析
  * `commonjs` 用于 require exports 相关解析。

  即可为项目的 javscript 文件启用 `omnicomplete` 了

  安装完成后，可通过 vim 打开一个 js 文件，然后查看命令 `:echo &omnifunc`
  返回来确保 tern 的 omni 补全已启用。


## 目录生成

使用命令 `Wxgen [folder] name` 可以快速生成并打开一个页面所需的 `wxml` `wxss`
以及 `javascript` 文件，例如：

```
:Wxgen component product
```

将在 component 目录下生成 product 目录以及相关的三个文件并打开，如果命令只有一个参数则在当前目录下生成。

## 推荐插件

* [xml.vim](http://www.vim.org/scripts/script.php?script_id=1397) 用于辅助编辑 xml 文件, 包含自动添加匹配标签、快速修改/删除标签等功能。
* [emmet-vim](https://github.com/mattn/emmet-vim) 快速生成 xml 和 css,
  参考配置：

    ``` vim
      let g:user_emmet_settings = {
      \ 'wxss': {
      \   'extends': 'css',
      \ },
      \ 'wxml': {
      \   'extends': 'html',
      \   'aliases': {
      \     'div': 'view',
      \     'span': 'text',
      \   },
      \  'default_attributes': {
      \     'block': [{'wx:for-items': '{{list}}','wx:for-item': '{{item}}'}],
      \     'navigator': [{'url': '', 'redirect': 'false'}],
      \     'scroll-view': [{'bindscroll': ''}],
      \     'swiper': [{'autoplay': 'false', 'current': '0'}],
      \     'icon': [{'type': 'success', 'size': '23'}],
      \     'progress': [{'precent': '0'}],
      \     'button': [{'size': 'default'}],
      \     'checkbox-group': [{'bindchange': ''}],
      \     'checkbox': [{'value': '', 'checked': ''}],
      \     'form': [{'bindsubmit': ''}],
      \     'input': [{'type': 'text'}],
      \     'label': [{'for': ''}],
      \     'picker': [{'bindchange': ''}],
      \     'radio-group': [{'bindchange': ''}],
      \     'radio': [{'checked': ''}],
      \     'switch': [{'checked': ''}],
      \     'slider': [{'value': ''}],
      \     'action-sheet': [{'bindchange': ''}],
      \     'modal': [{'title': ''}],
      \     'loading': [{'bindchange': ''}],
      \     'toast': [{'duration': '1500'}],
      \     'audio': [{'src': ''}],
      \     'video': [{'src': ''}],
      \     'image': [{'src': '', 'mode': 'scaleToFill'}],
      \   }
      \ },
      \}
    ```
  
  如果你已经配置了变量 `g:user_emmet_settings`,  注意避免重复设置。

## 语法检查

* javascript 推荐使用 [eslint](http://eslint.org/), 然后在 `.eslintrc` 中加入

    ``` json
    "globals": {
      "App": true,
      "Page": true,
      "getApp": true,
      "wx": true,
      "getCurrentPages": true
    },
    ```
  避免小程序变量的未定义错误。

* wxss 推荐使用 [stylelint](https://github.com/stylelint/stylelint),
针对 wxss 的[参考配置](https://gist.github.com/chemzqm/7fc6144d9953f9cfa71bd18fdfcee5b6), 安装本插件后可添加配置： `let g:neomake_wxss_enabled_makers = ['stylelint']` 启用 neomake 的 wxss 的代码检测。

* wxml 推荐使用 [tidy-html5](https://github.com/htacg/tidy-html5), 可使用命令 `brew install tidy-html5` 进行安装, 安装本插件后添加配置 `let g:neomake_wxml_enabled_makers = ['tidy']` 启用 neomake 的 wxml 代码检测。

## LICENSE

Copyright 2016 chemzqm@gmail.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
