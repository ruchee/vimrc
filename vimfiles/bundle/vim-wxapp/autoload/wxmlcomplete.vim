if !exists('*json_decode')
  echohl Error | echon 'json_decode function not found, please upgrade your vim' | echohl None
  finish
endif

let b:file = expand('<sfile>:h').'/wxml.json'
let b:config_data = json_decode(join(readfile(b:file), ''))

function! wxmlcomplete#Complete(findstart, base)
  if a:findstart
    let line = getline('.')
    let start = col('.') - 1
    let curline = line('.')
    while start >= 0 && line[start - 1] =~ '\(\k\|[!:.-]\)'
      let start -= 1
    endwhile
    let before = start == 0 ? '' : line[0:start - 1]
    if start == 0 || line[start - 1] == '<' || (before =~ '^\s*$' && curline > 1 && getline(curline - 1) =~ '>\s*$')
      let b:complete_type = 'tag'
      return start
    endif
    if start > 0 && line[start - 1] == '"'
      let b:complete_type = 'value'
      let type = 'value'
      let b:complete_attr = matchstr(before, '\(\k\|:\)\+\ze="$')
    endif
    let lines = [before]
    if before !~ '<'
      while curline > 1
        let line = getline(curline - 1)
        if line =~ '<'
          let lines = [line] + lines
          break
        else
          let lines = [line] + lines
          let curline = curline - 1
        endif
      endwhile
    endif
    let str = join(lines, '')
    let b:complete_tag = matchlist(str, '<\(\k\+\)[^<]\+$')[1]
    if !exists('type')
      let b:complete_type = 'attribute'
    endif
    return start
  else
    let tag = get(b:, 'complete_tag', '')
    let type = get(b:, 'complete_type', '')
    let attr = get(b:, 'complete_attr', '')
    if type == 'tag'
      return s:filter(keys(b:config_data['tags']), a:base)
    elseif type == 'attribute' && len(tag)
      let common = get(b:config_data, 'common', [])
      let attrs = get(b:config_data['tags'], tag, [])
      return  s:filter(attrs, a:base) + s:filter(common, a:base)
    elseif type == 'value' && len(attr)
      let values = get(b:config_data['attrs'], attr, [])
      let values = get(get(b:config_data, tag, []), attr, []) + values
      return s:filter(values, a:base)
    endif
    return []
  endif
endfunction

function! s:filter(data, base)
  return filter(copy(a:data), 'stridx(v:val, "'.a:base.'") == 0')
endfunction
