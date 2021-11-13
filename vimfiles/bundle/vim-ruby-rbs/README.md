![vim-rbs_demo](demo.png)

## Introduction

This plugin adds syntax highlighting and indentation for [Ruby Signature](https://github.com/ruby/rbs) (`*.rbs`) files.

These files are included in [my own rewrite of vim-ruby](https://github.com/jlcrochet/vim-ruby), but are listed separately here for those who want to use them alongside the original [vim-ruby](https://github.com/vim-ruby/vim-ruby).

## Installation

This is a standard Vim plugin which can be installed using your plugin manager of choice. If you do not already have a plugin manager, I recommend [vim-plug](https://github.com/junegunn/vim-plug).

Alternatively, if you are using Vim 8 or NeoVim, you can install this plugin as a package; see `:h packages` for instructions.

## Configuration

#### `g:rbs_fold`

If true, module, interface, and class blocks will be folded.
