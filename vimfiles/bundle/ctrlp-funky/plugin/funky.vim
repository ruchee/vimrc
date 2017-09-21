" File: plugin/funky.vim
" Description: a simple ctrlp.vim extension provides jumping to function
" Author: Takahiro Yoshihara <tacahiroy@gmail.com>
" License: The MIT License

command! -nargs=? CtrlPFunky              call ctrlp#funky#funky(<q-args>)
command! -nargs=? CtrlPFunkyMulti         call ctrlp#funky#funky(<q-args>, { 'multi_buffers': 1 })
" command! -nargs=? CtrlPFunkyDeep          call ctrlp#funky#funky(<q-args>, { 'deep': 1, 'mutli_buffers': 0 })
command! -nargs=? CtrlPFunkyClearCache    call ctrlp#funky#clear_cache(expand('%:p'))
command! -nargs=? CtrlPFunkyClearCacheAll call ctrlp#funky#clear_cache_all()
