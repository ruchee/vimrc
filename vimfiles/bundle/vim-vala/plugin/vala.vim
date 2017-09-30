" Insert a CCode attribute for the symbol below the cursor
" https://wiki.gnome.org/Projects/Vala/LegacyBindings
function! CCode() abort
  normal yiwO[CCode (cname = "pa")]
endfunction

" Set Vala Coding Style
" https://wiki.gnome.org/Projects/Vala/Hacking#Coding_Style
function! ValaCodingStyle() abort
  set ts=4 sts=4 sw=4 tw=0 wm=0
endfunction

command! -bar CCode call CCode()
command! -bar ValaCodingStyle call ValaCodingStyle()

augroup arrufat_vala
  " clear all autocmds defined within arrufat_vala group
  autocmd!
  autocmd FileType vala
      \ if get(g:, 'vala_syntax_folding_enabled', 1) |
      \   setlocal foldmethod=syntax |
      \ endif
augroup END
