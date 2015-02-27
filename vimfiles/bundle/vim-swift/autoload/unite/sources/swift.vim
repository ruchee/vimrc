" File: unite/sources/swift.vim
" Author: Kevin Ballard
" Description: Unite sources file for Swift
" Last Change: Jan 13, 2015

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#swift#define() "{{{
    let sources = [s:source_device]
    if !empty(s:source_dev_dir)
        call add(sources, s:source_dev_dir)
    endif
    return sources
endfunction "}}}

" swift/device {{{

let s:source_device = {
            \ 'name': 'swift/device',
            \ 'hooks': {},
            \ 'action_table': {},
            \ 'default_action': 'set_global',
            \ 'is_grouped': 1,
            \ 'description': 'iOS Simulator devices for use with Swift'
            \}

function! s:source_device.gather_candidates(args, context) "{{{
    let devices = swift#platform#simDeviceInfo()
    if empty(devices)
        redraw
        call unite#print_source_error('Error fetching sim device info', self.name)
        return []
    endif
    return map(copy(devices), "{ 'word': v:val.name, 'group': v:val.runtime.name }")
endfunction "}}}

function! s:source_device.hooks.on_post_filter(args, context) "{{{
    let g:swift_unite_candidates = deepcopy(a:context.candidates)
    for candidate in a:context.candidates
        if get(candidate, 'is_dummy')
            " group name
            let candidate.abbr =
                        \ '-- ' . candidate.word . ' --'
        else
            let candidate.abbr =
                        \ '  ' . candidate.word
        endif
    endfor
endfunction "}}}

" Actions {{{

let s:source_device.action_table.set_buffer = {
            \ 'description': 'select the iOS Simulator device (buffer-local)',
            \ 'is_selectable': 0
            \}
function! s:source_device.action_table.set_buffer.func(candidate) "{{{
    let b:swift_device = a:candidate.word
endfunction "}}}

let s:source_device.action_table.set_global = {
            \ 'description': 'select the iOS Simulator device (global)',
            \ 'is_selectable': 0
            \}
function! s:source_device.action_table.set_global.func(candidate) "{{{
    let g:swift_device = a:candidate.word
endfunction "}}}

" }}}

" }}}
" swift/developer_dir {{{

let s:source_dev_dir = {
            \ 'name': 'swift/developer_dir',
            \ 'hooks': {},
            \ 'action_table': {},
            \ 'default_action': 'set_global',
            \ 'description': 'Xcode directories for use with Swift'
            \}

function! s:source_dev_dir.gather_candidates(args, context) "{{{
    if !swift#util#has_vimproc()
        call unite#print_source_message('vimproc plugin is not installed.', self.name)
        return []
    endif
    let cmd = swift#util#system('mdfind "kMDItemCFBundleIdentifier = com.apple.dt.Xcode"')
    if cmd.status == 0
        let result = [{'word': '', 'abbr': '(default)'}]
        call extend(result, map(cmd.output, "{ 'word': v:val }"))
        return result
    else
        call unite#print_source_error('mdfind error', self.name)
        for line in cmd.output
            call unite#print_source_error(line, self.name)
        endfor
        return []
    endif
endfunction "}}}

" Actions {{{

let s:source_dev_dir.action_table.set_buffer = {
            \ 'description': 'select the Swift developer dir (buffer-local)',
            \ 'is_selectable': 0
            \}
function! s:source_dev_dir.action_table.set_buffer.func(candidate) "{{{
    if empty(a:candidate.word)
        unlet b:swift_developer_dir
    else
        let b:swift_developer_dir = a:candidate.word
    endif
endfunction "}}}

let s:source_dev_dir.action_table.set_global = {
            \ 'description': 'select the Swift developer dir (global)',
            \ 'is_selectable': 0
            \}
function! s:source_dev_dir.action_table.set_global.func(candidate) "{{{
    if empty(a:candidate.word)
        unlet g:swift_developer_dir
    else
        let g:swift_developer_dir = a:candidate.word
    endif
endfunction "}}}

" }}}

" }}}

let &cpo = s:save_cpo
unlet s:save_cpo
