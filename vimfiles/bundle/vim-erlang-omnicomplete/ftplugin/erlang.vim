if exists('b:erlang_omnicomplete_loaded')
    finish
else
    let b:erlang_omnicomplete_loaded = 1
endif

setlocal omnifunc=erlang_complete#Complete
