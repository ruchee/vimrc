if exists('g:loaded_kube') || &cp || v:version < 700
  finish
endif
let g:loaded_kube = 1

let s:nvim = has('nvim')
let s:async = has('job') && has('channel')

let s:job_output = {}

function! kube#fileOpDoneCb(ch)
  let id = s:ch_get_id(a:ch)
  let jo = s:job_output[id]

  let sawError = 0
  if ch_status(a:ch, {'part': 'err'}) == 'buffered'
    let sawError = 1
    while ch_status(a:ch, {'part': 'err'}) == 'buffered'
      let line = ch_readraw(a:ch)
      call add(jo['lines'], line)
    endwhile
  else
    while ch_status(a:ch, {'part': 'out'}) == 'buffered'
      let line = ch_readraw(a:ch)
      call add(jo['lines'], line)
    endwhile
  endif

  let jobOp = jo['op']
  let out = join(jo['lines'], "\n")

  call s:handle_out(out, jobOp, sawError)
endfunction

function! s:handle_out(out, op, error)
  if a:error
    echom "Error encountered:\n" . a:out
  else
    if a:op == 'delete'
      echom "Successfully deleted resources:\n" . a:out
    else
      echom "Successfully applied updates:\n" . a:out
    endif
  endif
endfunction

function! s:ch_get_id(ch)
  let id = substitute(a:ch, '^channel \(\d\+\) \(open\|closed\)$', '\1', '')
endfunction

function! s:KubeFileOp(op, wholeDir)
  let cmd = 'kubectl ' . a:op . ' -f '

  let input = ""
  if a:wholeDir
    let cmd = cmd . expand('%:h:p')
  else
    " Using stdin so this can possibly be switched to buffer contents
    let cmd = cmd . '-'
    let input = join(getline(1,'$'), "\n")
  endif

  if a:op == "delete"
    let cmd = cmd . " -o name"
  endif

  if s:async && (!exists('g:kubernetes_no_async') || !g:kubernetes_no_async)
    let job = job_start(cmd, 
          \{
          \'close_cb': 'kube#fileOpDoneCb',
          \'err_io': 'out',
          \})

    let ch = job_getchannel(job)
    let id = s:ch_get_id(ch)

    let s:job_output[id] = {
          \'lines': [],
          \'op': a:op,
          \}

    call ch_sendraw(ch, input)
    call ch_close_in(ch)

    echom "called " . cmd
  else
    let out = system(cmd, input)
    call s:handle_out(out, a:op, v:shell_error)
  endif
endfunction

fun! s:KubeRecreate()
  let g:kubernetes_no_async="true"
  call s:KubeFileOp('delete', 0)
  unlet g:kubernetes_no_async
  call s:KubeFileOp('create', 0)
endf

command! KubeApply call s:KubeFileOp('apply', 0)
command! KubeDelete call s:KubeFileOp('delete', 0)
command! KubeCreate call s:KubeFileOp('create', 0)
command! KubeRecreate call s:KubeRecreate()

command! KubeApplyDir call s:KubeFileOp('apply', 1)
command! KubeDeleteDir call s:KubeFileOp('delete', 1)
