function! rpc#request(...) " {{{
	if has('nvim')
		return call('rpcrequest', a:000)
	else
		let channel = job_getchannel(a:1)
		let request = [0, 1, a:2, a:000[2:]]
		let [type, msgid, error, response] = json_decode(ch_evalraw(channel, json_encode(request)."\n"))
		if error
			throw error
		end
		return response
	end
endfunction " }}}

function! rpc#notify(...) " {{{
	if has('nvim')
		return call('rpcnotify', a:000)
	else
		let channel = job_getchannel(a:1)
		let notice = [2, a:2, a:000[2:]]
		call ch_sendraw(channel, json_encode(notice)."\n")
	end
endfunction " }}}

function! s:OnCall(status, response) " {{{
	let msg = json_decode(a:response)
	if len(msg) != 3
		return
	endif

	let [type, method, params] = msg
	if method != 'vim_command' || len(params) != 1
		return
	endif

	execute params[0]
endfunction " }}}

function! s:OnError(a, b, c) " {{{
	echo join(a:b, "\n")
endfunction

function! s:OnError2(a, b)
	echo a:b
endfunction " }}}

function! rpc#start(...) " {{{
	if has('nvim')
		return jobstart(a:000, {'rpc': v:true, 'on_stderr': function('s:OnError')})
	else
		return job_start(a:000, {'out_cb': function('s:OnCall'), 'err_cb': function('s:OnError2')})
	end
endfunction " }}}

function! rpc#stop(...) " {{{
	if has('nvim')
		return call('rpcstop', a:000)
	else
		return job_stop(a:1)
	end
endfunction " }}}

" vim: foldmethod=marker:noexpandtab:ts=2:sts=2:sw=2
