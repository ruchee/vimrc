# tig-explorer.vim

Vim plugin to use [tig](https://github.com/jonas/tig) as a git client. Seamless switching between vim and tig with opening in the same buffer.
[![https://gyazo.com/181fef546cced7ca6dc651dff59cd1bf](https://i.gyazo.com/181fef546cced7ca6dc651dff59cd1bf.gif)](https://gyazo.com/181fef546cced7ca6dc651dff59cd1bf)

## Requirement

* tig(https://github.com/jonas/tig)

## Installation

```vim
NeoBundle     'iberianpig/tig-explorer.vim'
```

## Usage

```vim
" open tig with current file
nnoremap <Leader>t :TigOpenCurrentFile<CR>

" open tig with Project root path
nnoremap <Leader>T :TigOpenProjectRootDir<CR>

" open tig grep
nnoremap <Leader>g :TigGrep<CR>

" open tig grep with the selected word
vnoremap <Leader>g y:TigGrep<Space><C-R>"<CR>

" open tig grep with the word under the cursor
nnoremap <Leader>cg :<C-u>:TigGrep<Space><C-R><C-W><CR>

" open tig blame with current file
nnoremap <Leader>b :TigBlame<CR>
```
