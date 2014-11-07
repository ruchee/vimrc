Vim-CSharp
----------

This Vim bundle extends Vim's built in support for CSharp files.

<a href="https://raw.github.com/OrangeT/vim-csharp/master/screenshot.png">
<img src="https://raw.github.com/OrangeT/vim-csharp/master/screenshot.png" width="322" height="495" />
</a><a href="https://raw.github.com/OrangeT/vim-csharp/master/screenshot2.png">
<img src="https://raw.github.com/OrangeT/vim-csharp/master/screenshot2.png" width="459" height="343" />
</a>

Compliments [Omnisharp](https://github.com/nosami/Omnisharp) rather well.

New Features
============

* Snippets for Razor files, Webforms based MVC views, Xunit and Moq.
* Added basic syntax highlighting for razor files.

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

Razor highlighting:

* Uses html formatting for ... html.
* Recognises @{} and @ blocks and formats using c-sharp highlighting.

Interface/Class Names
=====================

Interface/Class names are aliased to the Type group, for inclusion with your existing colour schemes.  Alternatively, add specific highlighting for csIface and csClass to your colour scheme.

Snippets
========

Snippets are designed to work with [vim-snipmate](https://github.com/garbas/vim-snipmate).  Snippet support is relatively new, and is currently being trialled by fire.  If you find a problem, feel free to raise an issue.

### Razor/ASPX Snippets

Razor snippets are designed to be deterministic and as productive as possible.  Your best bet is to familiarise yourself with the snippet files (snippets/cshtml.snippets, snippets/aspx.snippets).  Snippets take the form:

* Language declaration character (@ for Razor, % for Webforms)
* Initals of required control (tbf = TextBoxFor)
* Additional options (m = model, _ = html attr. collection, . = html attr. collection with class )

This means that for a "text box for" with a class attribute in razor, type @tbf. followed by your expansion key (default tab).

### Xunit, Moq Snippets

Additional snippets are provided to reduce time spent writing unit tests.

Xunit assertions are prefixed by x, followed by the test type, followed by a ! to negate.  So a does not contain assertion is created by typing xcontains! followed by the expansion key (default tab).

There are only two moq snippets, "moq" which then requests which scenario to complete to (there are many), and "it" which expands to It.IsAny<>.

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

Framework version can be overridden with the following:
```
:MsVersion [4, 3.5, 2, 1]
```

Passing Parameters to :make
===========================

You can pass parameters to :make which will in turn pass them to MsBuild.  This is useful for passing a variety of options, including a custom target for testing:

```
:MsProjFile build.proj
:make /target:Test
```

Todo
====

* Highlighting of types in attributes.
* Adding and removing classes/files from projects and solutions (hook into NERDTree?)
