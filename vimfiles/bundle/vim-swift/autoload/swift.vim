" File: autoload/swift.vim
" Author: Kevin Ballard
" Description: Helper functions for Swift
" Last Change: Feb 10, 2015

" Run {{{1

function! swift#Run(bang, args)
	let args = s:ShellTokenize(a:args)
	if a:bang
		let idx = index(l:args, '--')
		if idx != -1
			let swift_args = idx == 0 ? [] : l:args[:idx-1]
			let args = l:args[idx+1:]
		else
			let swift_args = l:args
			let args = []
		endif
	else
		let swift_args = []
	endif

	let b:swift_last_swift_args = l:swift_args
	let b:swift_last_args = l:args

	call s:WithPath(function("s:Run"), swift_args, args, 0)
endfunction

function! s:Run(dict, swift_args, args, xctest)
	let exepath = a:dict.tmpdir.'/'.fnamemodify(a:dict.path, ':t:r')
	if has('win32')
		let exepath .= '.exe'
	endif

	let platformInfo = swift#platform#getPlatformInfo(swift#platform#detect())
	if empty(platformInfo)
		return
	endif
	let swift_args = swift#platform#argsForPlatformInfo(platformInfo)
	let sourcepath = get(a:dict, 'tmpdir_relpath', a:dict.path)
	let swift_args += [sourcepath, '-o', exepath] + a:swift_args

	let swift = swift#swiftc()

	let pwd = a:dict.istemp ? a:dict.tmpdir : ''
	let cmd = swift#util#system(swift.' '.join(map(swift_args, 'shellescape(v:val)')), pwd)
	if !empty(cmd.output)
		echohl WarningMsg
		for line in cmd.output
			echo line
		endfor
		echohl None
	endif
	if cmd.status == 0
		if a:xctest
			let path = swift#platform#xctestStringForExecutable(exepath, platformInfo)
		else
			let path = swift#platform#commandStringForExecutable(exepath, platformInfo)
		endif
		exe '!' . path . ' ' . join(map(a:args, 'shellescape(v:val)'))
	endif
endfunction

" Emit {{{1

function! swift#Emit(tab, type, bang, args)
	let args = s:ShellTokenize(a:args)
	let type = a:type
	if type ==# 'sil' && a:bang
		let type = 'silgen'
	endif
	if type == 'objc-header'
		" objc-header generation saves some secondary files
		" so we should always use the temporary directory
		let save_write = &write
		set nowrite
	endif
	try
		call s:WithPath(function("s:Emit"), a:tab, type, args)
	finally
		if exists("save_write") | let &write = save_write | endif
	endtry
endfunction

function! s:Emit(dict, tab, type, args)
	try
		let platformInfo = swift#platform#getPlatformInfo(swift#platform#detect())
		if empty(platformInfo)
			return
		endif
		let basename = fnamemodify(a:dict.path, ':t:r')
		let args = swift#platform#argsForPlatformInfo(platformInfo)
		if a:type == 'objc-header'
			" emitting the objc-header is a bit complicated
			let args += ['-emit-objc-header-path', '-', '-parse-as-library', '-emit-module']
			" for some reason, even after all that, we still need to import an
			" obj-c header before it will emit anything useful.
			let args += ['-import-objc-header', '/dev/null']
			" and finally, provide a path for the generated swiftmodule inside
			" the temporary dir, or it will put it in $PWD
			let args += ['-o', a:dict.tmpdir.'/'.basename.'.swiftmodule']
		else
			let args += ['-emit-'.a:type, '-o', '-']
		endif
		let args += a:args
		let args += ['--', get(a:dict, 'tmpdir_relpath', a:dict.path)]

		let swift = swift#swiftc()

		let pwd = a:dict.istemp ? a:dict.tmpdir : ''
		let cmd = swift#util#system(swift.' '.join(map(args, 'shellescape(v:val)')), pwd)
		if cmd.status != 0
			echohl WarningMsg
			for line in cmd.output
				echo line
			endfor
			echohl None
		else
			if a:tab
				tabnew
			else
				new
			endif
			silent put =cmd.output
			1
			d
			if a:type == 'ir'
				setl filetype=llvm
				let extension='ll'
			elseif a:type == 'assembly'
				setl filetype=asm
				let extension='s'
			elseif a:type == 'sil' || a:type == 'silgen'
				" we don't have a SIL filetype yet
				setl filetype=
				let extension='sil'
			elseif a:type == 'objc-header'
				setl filetype=objc
				let extension='h'
			endif
			setl buftype=nofile
			setl bufhidden=hide
			setl noswapfile
			if exists('l:extension')
				let suffix=1
				while 1
					let bufname = basename
					if suffix > 1 | let bufname .= ' ('.suffix.')' | endif
					let bufname .= '.'.extension
					if bufexists(bufname)
						let suffix += 1
						continue
					endif
					exe 'silent noautocmd keepalt file' fnameescape(bufname)
					break
				endwhile
			endif
		endif
	endtry
endfunction

" RunTests {{{1

function! swift#RunTests(args)
	let args = s:ShellTokenize(a:args)
	call s:WithPath(function("s:Run"), args, [], 1)
endfunction

" Version {{{1

function! swift#PrintVersion()
	let cmd = swift#swiftc() . ' -version'
	let output = system(l:cmd)
	let output = substitute(output, '\_s\+\%$', '', '')
	if v:shell_error == 0
		echo output
	else
		echohl ErrorMsg
		echo 'error running: '.l:cmd
		echohl None
		echo output
	endif
endfunction

" Utility functions {{{1

" Invokes func(dict, ...)
" Where {dict} is a dictionary with the following keys:
"   'path' - The path to the file
"   'tmpdir' - The path to a temporary directory that will be deleted when the
"              function returns.
"   'istemp' - 1 if the path is a file inside of {dict.tmpdir} or 0 otherwise.
" If {istemp} is 1 then an additional key is provided:
"   'tmpdir_relpath' - The {path} relative to the {tmpdir}.
"
" {dict.path} may be a path to a file inside of {dict.tmpdir} or it may be the
" existing path of the current buffer. If the path is inside of {dict.tmpdir}
" then it is guaranteed to have a '.swift' extension.
function! s:WithPath(func, ...)
	let buf = bufnr('')
	let saved = {}
	let dict = {}
	try
		let saved.write = &write
		set write
		let dict.path = expand('%')
		let pathisempty = empty(dict.path)

		" Always create a tmpdir in case the wrapped command wants it
		let dict.tmpdir = tempname()
		call mkdir(dict.tmpdir)

		if pathisempty || !saved.write
			let dict.istemp = 1
			" if we're doing this because of nowrite, preserve the filename
			if !pathisempty
				let filename = expand('%:t:r').".swift"
			else
				let filename = 'unnamed.swift'
			endif
			let dict.tmpdir_relpath = filename
			let dict.path = dict.tmpdir.'/'.filename

			let saved.mod = &mod
			set nomod

			try
				silent exe 'keepalt write! ' . fnameescape(dict.path)
			finally
				if pathisempty
					silent keepalt 0file
				endif
			endtry
		else
			let dict.istemp = 0
			update
		endif

		call call(a:func, [dict] + a:000)
	finally
		if bufexists(buf)
			for [opt, value] in items(saved)
				silent call setbufvar(buf, '&'.opt, value)
				unlet value " avoid variable type mismatches
			endfor
		endif
		if has_key(dict, 'tmpdir') | silent call s:RmDir(dict.tmpdir) | endif
	endtry
endfunction

function! swift#AppendCmdLine(text)
	call setcmdpos(getcmdpos())
	let cmd = getcmdline() . a:text
	return cmd
endfunction

" Tokenize the String according to shell parsing rules
function! s:ShellTokenize(text)
	let pat = '\%([^ \t\n''"]\+\|\\.\|''[^'']*\%(''\|$\)\|"\%(\\.\|[^"]\)*\%("\|$\)\)\+'
	let start = 0
	let tokens = []
	while 1
		let pos = match(a:text, pat, start)
		if l:pos == -1
			break
		endif
		let end = matchend(a:text, pat, start)
		call add(tokens, strpart(a:text, pos, end-pos))
		let start = l:end
	endwhile
	return l:tokens
endfunction

function! s:RmDir(path)
	" sanity check; make sure it's not empty, /, or $HOME
	if empty(a:path)
		echoerr 'Attempted to delete empty path'
		return 0
	elseif a:path == '/' || a:path == $HOME
		echoerr 'Attempted to delete protected path: ' . a:path
		return 0
	endif
	silent exe "!rm -rf " . shellescape(a:path)
endfunction

" Returns a string that can be passed to the shell to invoke swiftc
" Optional argument is flags to pass to xcrun.
function! swift#swiftc(...)
	let l:key = 'swift_developer_dir'
	let developer_dir = get(b:, l:key, get(w:, l:key, get(g:, l:key, '')))
	if empty(developer_dir)
		let result = ''
	else
		let result = 'env '.shellescape('DEVELOPER_DIR='.developer_dir).' '
	endif
	let result .= 'xcrun '
	if a:0 > 0
		let result .= a:1.' '
	endif
	let result .= 'swiftc'
	return result
endfunction

" }}}1

" vim: set noet sw=4 ts=4:
