- 中文
- [English](./README.md)

[![asciicast](https://asciinema.org/a/4dzyyjymrguylqt21igxlhhqx.png)](https://asciinema.org/a/4dzyyjymrguylqt21igxlhhqx)

## 简介
PHPCD，全称 PHP Completion Daemon，译为 PHP 补全服务。PHPCD 实现了 Vim 的 omni 补全接口，提供 PHP 相关的智能补全和定义跳转服务。

PHPCD 的 VimL 部分基于[phpcomplete.vim](https://github.com/shawncplus/phpcomplete.vim)，感谢原项目贡献者的努力。原项目的 3k+ 行代码已经被裁剪为 1.5k 行，可维护性大为增强。

因为 PHPCD 利用 PHP 的[反射机制](http://php.net/manual/en/book.reflection.php)进行补全和跳转，所以 PHPCD 几乎不需要事先生成索引文件，启动速度、补全速度和跳转速度都非常快，代码也非常简洁。

~~PHPCD 目前只能配合[NeoVim](http://neovim.io/)工作，这是一个艰难的抉择。~~

##  特色
 * 快、轻、强
 * 静态调用`Class::method()`显示静态成员和方法；动态调用`$class->method()`显示非静态成员变量和方法
 * 真正识别`self`，`static`和`$this`上下文环境
 * 支持通过多种方式推断变量类型：
     - 变量类型注解 `/* @var $yourvar YourClass */`、 `/* @var YourClass $yourvar */`
     - 使用 `new` 初始化类实例 `$instance = new Class;`
     - 函数（全局函数、成员函数和匿名函数）参数的类型提示 `function (Foo $foo) { // ..  }`
     - 使用函数块注释中 `@return` 制定函数返回值的类型
 * 补全成员方法和成员属性的时候自动显示块注释
 * 支持内建类的方法、属性、常量的补全
 * 增强型定义跳转<kbd>ctrl</kbd>+<kbd>]</kbd>

## 安装指南

### 环境要求
 1. [PHP 5.3+](http://php.net/)
 2. [PCNTL](http://php.net/manual/en/book.pcntl.php) 扩展
 3. [Msgpack 0.5.7+(NeoVim)](https://github.com/msgpack/msgpack-php) 扩展或者[JSON(Vim 7.4+)](http://php.net/manual/en/intro.json.php) 扩展
 4. [Composer](https://getcomposer.org/) 支持


### 安装 PHPCD

推荐使用[Vim-Plug](https://github.com/junegunn/vim-plug/blob/master/README.md)管理 Vim 插件。

安装 Vim-Plug 后，添加：

```
Plug 'php-vim/phpcd.vim', { 'for': 'php' , 'do': 'composer update' }
```

然后执行`:PlugInstall`进行安装。

## 使用方法

首先运行 `composer install` 更新依赖并生成自动加载文件，然后打开 NeoVim。

补全按<kbd>Ctrl</kbd>+<kbd>x</kbd><kbd>Ctrl</kbd>+<kbd>o</kbd>，跳转按<kbd>ctrl</kbd>+<kbd>]</kbd>。
