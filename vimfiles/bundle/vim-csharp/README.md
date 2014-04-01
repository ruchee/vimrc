Vim-CSharp
----------

This Vim bundle extends Vim's built in support for CSharp files.

<a href="https://raw.github.com/OrangeT/vim-csharp/master/screenshot.png">
<img src="https://raw.github.com/OrangeT/vim-csharp/master/screenshot.png" width="322" height="495" />
</a><a href="https://raw.github.com/OrangeT/vim-csharp/master/screenshot2.png">
<img src="https://raw.github.com/OrangeT/vim-csharp/master/screenshot2.png" width="459" height="343" />
</a>

Installation
============

If you don't have a preferred installation method, I recommend installing [pathogen.vim](https://github.com/tpope/vim-pathogen), and then simply copy and paste:

OS X/Linux:
```
cd ~/.vim/bundle
git clone git://github.com/OrangeT/vim-csharp.git
```

Windows:
```
cd c:\users\[username]\vimfiles\bundle
git clone git://github.com/OrangeT/vim-csharp.git
```


Syntax Highlighting
===================

Existing CSharp highlighting plus:

* Support for async/await keywords.
* Support for var keyword.
* Support for LINQ keywords.
* Support for automatic properties.
* Context highlighting of Interface/Class names.
* Highlighting of Generic Types.

Interface/Class Names
=====================

Interface/Class names are aliased to the Type group, for inclusion with your existing colour schemes.  Alternatively, add specific highlighting for csIface and csClass to your colour scheme.

Compiler/MsBuild Support
========================

Executing :make will look for a .sln file in the root of the working directory.  If found, will run MSBuild against the highest available msbuild.exe available on the system.  Errors are available in the QuickFix window.

See second screenshot above for overview.

_(Note: This feature does not look for msbuild.exe in your path, but looks directly in c:/windows/Microsoft.NET for a msbuild.exe.)_

Custom Build/Solution File 
==========================

To use your own build file, call the following command:

```
:MsProjFile [path to build file]
```

Custom Framework Version
========================

Framework version can be overriden with the following:
```
:MsVersion [4, 3.5, 2, 1]
```

Passing Parameters to :make
===========================

You can pass parameters to :make which will in turn pass them to MsBuild.  This is useful for passing a variety of options, incluing a custom target for testing:

```
:MsProjFile build.proj
:make /target:Test
```

Todo
====

* Highlighting of types in attributes.
* Adding and removing classes/files from projects and solutions (hook into NERDTree?)
* Add syntax/region support for razor files.
