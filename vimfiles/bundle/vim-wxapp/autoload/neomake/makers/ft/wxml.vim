function! neomake#makers#ft#wxml#tidy()
    return {
                \ 'args': ['-xml', '-e', '-q', '--gnu-emacs', 'true'],
                \ 'errorformat': '%A%f:%l:%c: Warning: %m',
                \ }
endfunction

function! neomake#makers#ft#wxml#EnabledMakers()
    return ['tidy']
endfunction
