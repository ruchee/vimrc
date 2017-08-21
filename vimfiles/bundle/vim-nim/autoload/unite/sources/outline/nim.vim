let s:Tree = unite#sources#outline#import('Tree')
let s:OutlineInfo = {
            \ 'auto-update': 0}


function! unite#sources#outline#nim#outline_info()
    return s:OutlineInfo
endfunction

function! s:OutlineInfo.extract_headings(context)
    let root = s:Tree.new()                          
    call s:Tree.append_child(root, {})
    return root
endfunction
