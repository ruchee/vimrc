" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/javascript.vim ftplugin/javascript_*.vim ftplugin/javascript/*.vim

unlet! b:did_ftplugin
