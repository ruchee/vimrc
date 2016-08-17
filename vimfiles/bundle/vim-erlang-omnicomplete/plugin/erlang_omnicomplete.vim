if exists('g:loaded_erlang_omnicomplete')
    finish
endif

let g:loaded_erlang_omnicomplete = 1

command ErlangCompleteClearAllCache call erlang_complete#ClearAllCache()
