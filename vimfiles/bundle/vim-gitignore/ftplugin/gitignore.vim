" Vim indent file
" Language:	.gitignore
" Maintainer:	gisphm <phmfk@hotmail.com>
" URL:		https://github.com/gisphm/vim-gitignore

if exists('b:did_ftplugin')
    finish
endif
let b:did_ftplugin = 1

let s:save_cpo = &cpo
set cpo&vim

setlocal commentstring=#\ %s

let b:undo_ftplugin = ""
