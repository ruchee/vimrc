# Wxapp.vim

微信小程序开发 vim 插件。

提供包含文件检测、语法高亮、缩进、代码片段功能。

推荐使用 [coc.nvim](https://github.com/neoclide/coc.nvim) 获得小程序 LSP 支持，例如：

* 智能补全
* 语法检查
* 文档查看

详细文档： [Coc 小程序开发支持](https://github.com/neoclide/coc.nvim/wiki/%E5%B0%8F%E7%A8%8B%E5%BA%8F%E5%BC%80%E5%8F%91%E6%94%AF%E6%8C%81)

Mac 用户推荐使用小程序 Dash 文档： [chemzqm/wx-dash](https://github.com/chemzqm/wx-dash)

## 目录

- [安装方式](安装方式)
- [效果图](#效果图)
- [功能列表](功能列表)
- [推荐插件](#推荐插件)
- [待完成](#待完成)

## 安装方式

以下分别是使用 [NeoBundle](https://github.com/Shougo/neobundle.vim) [vim-plug](https://github.com/junegunn/vim-plug) 安装的命令：


    NeoBundle 'chemzqm/wxapp.vim'
    Plug 'chemzqm/wxapp.vim'

## 功能列表

* [页面目录生成](#目录生成)
* wxml 和 wxss 文件检测, 代码高亮, 缩进函数 (推荐快捷键 `=at` `=a{`)
* wxml, wxss 以及 javascript dictionary 文件, 使用参考：[vim dictionary 的使用方式](https://chemzqm.me/vim-dictionary)
* wxml 和 javascript [Ultisnips](https://github.com/SirVer/ultisnips) 代码块补全
* wxml 和 wxss 的[语法检查支持](#语法检查)

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
