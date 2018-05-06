let s:phpcd_path = expand('<sfile>:p:h:h') . '/php/main.php'

function! phpcd#CompletePHP(findstart, base) " {{{
	" we need to wait phpcd {{{
	if !exists('g:phpcd_channel_id')
		return
	endif " }}}

	if a:findstart " {{{
		unlet! b:php_menu
		" locate the start of the word
		let line = getline('.')
		let start = col('.') - 1
		let compl_begin = col('.') - 2
		while start >= 0 && line[start - 1] =~ '[\\a-zA-Z_0-9\x7f-\xff$]'
			let start -= 1
		endwhile

		" TODO 清理 phpbegin
		let phpbegin = searchpairpos('<?', '', '?>', 'bWn',
				\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string\\|comment"')
		let b:phpbegin = phpbegin
		let b:compl_context = phpcd#GetCurrentInstruction(line('.'), max([0, col('.') - 2]), phpbegin)

		if b:compl_context =~ 'new\s\+\\'
			let start = start + 1
		endif

		return start
	endif " }}}

	" If exists b:php_menu it means completion was already constructed {{{
	" we don't need to do anything more
	if exists('b:php_menu')
		return b:php_menu
	endif " }}}

	" a:base is very short - we need context {{{
	if exists('b:compl_context')
		let context = b:compl_context
		unlet! b:compl_context
		" chop of the "base" from the end of the current instruction
		if a:base != ""
			let context = substitute(context, '\s*[$a-zA-Z_0-9\x7f-\xff]*$', '', '')
		end
	else
		let context = ''
	end " }}}

	try " {{{
		let winheight = winheight(0)
		let winnr = winnr()

		if context =~? '^namespace' "{{{
			return phpcd#GetPsrNamespace()
		endif "}}}

		if context =~? '\v^((abstract|final)\s+)?(class|interface|trait)' "{{{
			return [expand('%:t:r')]
		end "}}}

		let [current_namespace, imports] = phpcd#GetCurrentNameSpace()

		if context =~# '\v^(use$|use+\s.*)' " {{{
			return rpc#request(g:phpcd_channel_id, 'classmap', a:base)
		endif " }}}

		if context =~ '\(->\|::\)$' " {{{
			let classname = phpcd#GetClassName(line('.'), context, current_namespace, imports)

			" TODO Fix it for variables with reference to $this etc.
			let public_only = (context !~# '^\(\$this\|self\|static\|parent\)')
			let is_static = 'only_nonstatic'

			if strridx(context, '::') == strlen(context) - 2 " context =~ '::$' {{{
				if stridx(context, 'parent') != 0
					let is_static = 'only_static'
				else
					let is_static = 'both'
				endif
			endif" }}}

			if get(g:, 'phpcd_disable_static_filter', 0) "{{{
					let is_static = 'both'
			endif "}}}

			return rpc#request(g:phpcd_channel_id, 'info', classname, a:base, is_static, public_only)
		elseif context =~? 'implements'
			" TODO complete class Foo implements
		elseif context =~? 'extends\s\+.\+$' && a:base == ''
			" TODO complete class Foo extends
		elseif context =~? 'extends'
			" TODO complete class Foo extends Prefix..
		elseif context =~? 'class [a-zA-Z_\x7f-\xff\\][a-zA-Z_0-9\x7f-\xff\\]*'
			" special case when you've typed the class keyword and the name too,
			" only extends and implements allowed there
			return filter(['extends', 'implements'], 'stridx(v:val, a:base) == 0')
		elseif context =~? 'new\s\+\\'
			return rpc#request(g:phpcd_channel_id, 'classes', a:base)
		endif " }}}

		if a:base =~ '^[^$]' " {{{
			return phpcd#CompleteGeneral(a:base, current_namespace, imports)
		endif " }}}
	finally
		silent! exec winnr.'resize '.winheight
	endtry " }}}
endfunction " }}}

function! phpcd#GetPsrNamespace() " {{{
	let path = expand('%:p')
	if !filereadable(path)
		return []
	endif

	return rpc#request(g:phpcd_channel_id, 'psr4ns', path)
endfunction " }}}

function! phpcd#CompleteGeneral(base, current_namespace, imports) " {{{
	let base = substitute(a:base, '^\\', '', '')
	let [pattern, namespace] = phpcd#ExpandClassName(a:base, a:current_namespace, a:imports)
	return rpc#request(g:phpcd_channel_id, 'keyword', pattern)
				\ + rpc#request(g:phpcd_channel_id, 'info', '', pattern, 'both', 1)
endfunction " }}}

function! phpcd#JumpToDefinition(mode) " {{{
	if !exists('g:phpcd_channel_id')
		return
	endif

	let [symbol, symbol_context, symbol_namespace, current_imports] = phpcd#GetCurrentSymbolWithContext()
	if symbol == ''
		return
	endif

	let [symbol_file, symbol_line, symbol_col] = phpcd#LocateSymbol(symbol, symbol_context, symbol_namespace, current_imports)
	if symbol_file == '' || symbol_file == v:false
		return
	endif

	if a:mode == 'normal'
		let edit_cmd = "e +"
	elseif a:mode == 'preview'
		let edit_cmd = "pedit +"
	else
		let edit_cmd = a:mode . " +"
	endif

	let cur_pos = getcurpos()
	let cur_pos[0] = bufnr('%')
	if !exists('s:phpcd_jump_stack')
		let s:phpcd_jump_stack = []
	endif
	call add(s:phpcd_jump_stack, cur_pos)

	if str2nr(symbol_line) > 0
		if expand('%:p') == symbol_file
			silent! execute symbol_line
		else
			silent! execute edit_cmd . symbol_line . ' ' . symbol_file
		endif
	else
		silent! execute edit_cmd . '1 ' . symbol_file
		silent! call search(symbol_line)
	endif

	silent! execute "normal! zt"
	normal! zv
	normal! zz
endfunction " }}}

function! phpcd#JumpBack() "{{{
	if !exists('s:phpcd_jump_stack') || len(s:phpcd_jump_stack) == 0
		return
	endif

	let prev_pos = s:phpcd_jump_stack[-1]
	let prev_buf = prev_pos[0]
	let prev_pos[0] = 0
	unlet s:phpcd_jump_stack[-1]
	exec 'buffer '.prev_buf
	call setpos('.', prev_pos)
endfunction "}}}

function! phpcd#GetCurrentSymbolWithContext() " {{{
	" Check if we are inside of PHP markup
	let pos = getpos('.')
	let phpbegin = searchpairpos('<?', '', '?>', 'bWn',
			\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string\\|comment"')
	let phpend = searchpairpos('<?', '', '?>', 'Wn',
			\ 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string\\|comment"')

	if (phpbegin == [0, 0] && phpend == [0, 0])
		return ['', '', '', '']
	endif

	" locate the start of the word
	let b:phpbegin = phpbegin

	let line = getline('.')
	let start = col('.') - 1
	let end = start
	if start < 0
		let start = 0
	endif
	if end < 0
		let end = 0
	endif

	while start >= 0 && line[start - 1] =~ '[\\a-zA-Z_0-9\x7f-\xff$]'
		let start -= 1
	endwhile
	while end + 1 <= len(line) && line[end + 1] =~ '[\\a-zA-Z_0-9\x7f-\xff$]'
		let end += 1
	endwhile
	let word = line[start : end]
	" trim extra non-word chars from the end line "(" that can come from a
	" function call
	let word = substitute(word, '\v\c[^\\a-zA-Z_0-9$]*$', '', '')

	let current_instruction = phpcd#GetCurrentInstruction(line('.'), max([0, col('.') - 2]), phpbegin)
	let context = substitute(current_instruction, 'clone ', '', '')
	let context = substitute(context, 'yield from', '', '')
	let context = substitute(context, 'yield ', '', '')
	let context = substitute(context, 'return ', '', '')
	let context = substitute(context, 'echo ', '', '')
	let context = substitute(context, '\s*[$a-zA-Z_0-9\\\x7f-\xff]*$', '', '')
	let context = substitute(context, '\s\+\([\-:]\)', '\1', '')

	if current_instruction[0:3] == 'use ' && word != '' && word[0] != '\'
		let class_begin = search('{', 'bnW', search('class|trait', 'bnW'))
		let class_end = searchpair('{','','}','n')
		let cline = line('.')
		if cline < class_begin || cline > class_end
			let word = '\'.word
		endif
	endif

	let [current_namespace, current_imports] = phpcd#GetCurrentNameSpace()
	if word != ''
		let [symbol, symbol_namespace] = phpcd#ExpandClassName(word, current_namespace, current_imports)
	else
		let [symbol, symbol_namespace] = [word, current_namespace]
	endif

	return [symbol, context, symbol_namespace, current_imports]
endfunction " }}}

function! phpcd#LocateSymbol(symbol, symbol_context, symbol_namespace, current_imports) " {{{
	let unknow_location = ['', '', '']

	" are we looking for a method?
	if a:symbol_context =~ '\(->\|::\)$' " {{{
		" Get name of the class
		let classname = phpcd#GetClassName(line('.'), a:symbol_context, a:symbol_namespace, a:current_imports)

		" Get location of class definition, we have to iterate through all
		if classname != ''
			let [path, line] = rpc#request(g:phpcd_channel_id, 'location', classname, a:symbol)
			return [path, line, 0]
		endif " }}}
	elseif index(['new', 'use', 'implements', 'extends'], a:symbol_context) > -1 " {{{
		let full_classname = s:GetFullName(a:symbol_namespace, a:symbol)
		let [path, line] = rpc#request(g:phpcd_channel_id, 'location', full_classname, '')
		return [path, line, 0] " }}}
	elseif a:symbol_context =~ 'function' " {{{
		" try to find interface method's implementation
		" or the subclass of abstract class
		" the var 'interface' is the interface name
		" or the abstract class name
		let interface = phpcd#GetClassName(line('.'), a:symbol_context, a:symbol_namespace, a:current_imports)
		let is_interface = 1
		if a:symbol_context =~ '^abstract'
			let is_interface = 0
		endif
		if interface != '' && exists('g:phpid_channel_id')
			let impls = rpc#request(g:phpid_channel_id, 'ls', interface, is_interface)
			let impl = phpcd#SelectOne(impls)

			if impl != ''
				let [path, line] = rpc#request(g:phpcd_channel_id, 'location', impl, a:symbol)
				return [path, line, 0]
			endif
		endif " }}}
	elseif a:symbol_context =~ '\/' " {{{
		let path = matchstr(getline('.'), '\(require\|include\)\(_once\)\?\s*__DIR__\s*\.\s*\zs.*\ze;')
		let cwd = expand('%:p:h')
		let path = cwd.substitute(path, "'", '', 'g')
		let path = fnamemodify(path, ':p:.')
		return [path, '$', 0] "}}}
	elseif a:symbol_context =~ '\v.+(\@|:)' " pattern like 'class@method' or 'class:method' {{{
		let full_classname = strpart(a:symbol_context, 1, strlen(a:symbol_context)-2)
		let [path, line] = rpc#request(g:phpcd_channel_id, 'location', full_classname, a:symbol)
		return [path, line, 0] " }}}
	else " {{{
		if a:symbol =~ '\v\C^[A-Z]'
			let [classname, namespace] = phpcd#ExpandClassName(a:symbol, a:symbol_namespace, a:current_imports)
			let full_classname = s:GetFullName(namespace, classname)
			let [path, line] = rpc#request(g:phpcd_channel_id, 'location', full_classname, '')
		else
			let [path, line] = rpc#request(g:phpcd_channel_id, 'location', '', a:symbol)
		end

		return [path, line, 0]
	endif " }}}

	return unknow_location
endfunction " }}}

function! phpcd#SelectOne(items) " {{{
	let items = a:items
	let len = len(items)
	if (len == 1)
		return items[0]
	elseif (len == 0)
		return
	endif

	let list = []
	for i in range(1, len)
		call add(list, printf("%2d %s", i, items[i - 1]))
	endfor
	let index = inputlist(list)
	if index >= 1 && index <= len
		return items[index - 1]
	endif
endfunction " }}}

function! s:getNextCharWithPos(filelines, current_pos) " {{{
	let line_no   = a:current_pos[0]
	let col_no    = a:current_pos[1]
	let last_line = a:filelines[len(a:filelines) - 1]
	let end_pos   = [len(a:filelines) - 1, strlen(last_line) - 1]
	if line_no > end_pos[0] || line_no == end_pos[0] && col_no > end_pos[1]
		return ['EOF', 'EOF']
	endif

	" we've not reached the end of the current line break
	if col_no + 1 < strlen(a:filelines[line_no])
		let col_no += 1
	else
		" we've reached the end of the current line, jump to the next
		" non-blank line (blank lines have no position where we can read from,
		" not even a whitespace. The newline char does not positionable by vim
		let line_no += 1
		while strlen(a:filelines[line_no]) == 0
			let line_no += 1
		endwhile

		let col_no = 0
	endif

	" return 'EOF' string to signal end of file, normal results only one char
	" in length
	if line_no == end_pos[0] && col_no > end_pos[1]
		return ['EOF', 'EOF']
	endif

	return [[line_no, col_no], a:filelines[line_no][col_no]]
endfunction " }}}

function! phpcd#GetCurrentInstruction(line_number, col_number, phpbegin) " {{{
	" locate the current instruction
	" up until the previous non comment or string ";"
	" or php region start (<?php or <?) without newlines
	let col_number = a:col_number
	let line_number = a:line_number
	let line = getline(a:line_number)
	let current_char = -1
	let instruction = ''
	let parent_depth = 0
	let bracket_depth = 0
	let stop_chars = [
				\ '!', '@', '%', '^', '&',
				\ '*', '/', '-', '+', '=',
				\ ':', '>', '<', '.', '?',
				\ ';', '(', '|', '['
				\ ]

	let phpbegin_length = len(matchstr(getline(a:phpbegin[0]), '\zs<?\(php\)\?\ze'))
	let phpbegin_end = [a:phpbegin[0], a:phpbegin[1] - 1 + phpbegin_length]

	" will hold the first place where a coma could have ended the match
	let first_coma_break_pos = -1
	let next_char = len(line) < col_number ? line[col_number + 1] : ''

	while !(line_number <= 1 && col_number <= 1)
		if current_char != -1
			let next_char = current_char
		endif

		let current_char = line[col_number]
		let synIDName = synIDattr(synID(line_number, col_number + 1, 0), 'name')

		if col_number - 1 == -1
			let prev_line_number = line_number - 1
			let prev_line = getline(line_number - 1)
			let prev_col_number = strlen(prev_line)
		else
			let prev_line_number = line_number
			let prev_col_number = col_number - 1
			let prev_line = line
		endif
		let prev_char = prev_line[prev_col_number]

		" skip comments
		if synIDName =~? 'comment\|phpDocTags'
			let current_char = ''
		endif

		" break on the last char of the "and" and "or" operators
		if synIDName == 'phpOperator' && (current_char == 'r' || current_char == 'd')
			break
		endif

		" break on statements as "return" or "throws"
		if synIDName == 'phpStatement' || synIDName == 'phpException'
			break
		endif

		" if the current char should be considered
		if current_char != '' && parent_depth >= 0 && bracket_depth >= 0 && synIDName !~? 'comment\|string'
			" break if we are on a "naked" stop_char (operators, colon, openparent...)
			if index(stop_chars, current_char) != -1
				let do_break = 1
				" dont break if it does look like a "->"
				if (prev_char == '-' && current_char == '>') || (current_char == '-' && next_char == '>')
					let do_break = 0
				endif
				" dont break if it does look like a "::"
				if (prev_char == ':' && current_char == ':') || (current_char == ':' && next_char == ':')
					let do_break = 0
				endif

				if do_break
					break
				endif
			endif

			" save the coma position for later use if theres a "naked" , possibly separating a parameter and it is not in a parented part
			if first_coma_break_pos == -1 && current_char == ','
				let first_coma_break_pos = len(instruction)
			endif
		endif

		" count nested darenthesis and brackets so we can tell if we need to break on a ';' or not (think of for (;;) loops)
		if synIDName =~? 'phpBraceFunc\|phpParent\|Delimiter'
			if current_char == '('
				let parent_depth += 1
			elseif current_char == ')'
				let parent_depth -= 1

			elseif current_char == '['
				let bracket_depth += 1
			elseif current_char == ']'
				let bracket_depth -= 1
			endif
		endif

		" stop collecting chars if we see a function start { (think of first line in a function)
		if (current_char == '{' || current_char == '}') && synIDName =~? 'phpBraceFunc\|phpParent\|Delimiter'
			break
		endif

		" break if we are reached the php block start (<?php or <?)
		if [line_number, col_number] == phpbegin_end
			break
		endif

		let instruction = current_char.instruction

		" step a char or a line back if we are on the first column of the line already
		let col_number -= 1
		if col_number == -1
			let line_number -= 1
			let line = getline(line_number)
			let col_number = strlen(line)
		endif
	endwhile

	" strip leading whitespace
	let instruction = substitute(instruction, '^\s\+', '', '')

	" there were a "naked" coma in the instruction
	if first_coma_break_pos != -1
		if instruction !~? '^use' && instruction !~? '^class' " use ... statements and class delcarations should not be broken up by comas
			let pos = (-1 * first_coma_break_pos) + 1
			let instruction = instruction[pos :]
		endif
	endif

	" HACK to remove one line conditionals from code like "if ($foo) echo 'bar'"
	" what the plugin really need is a proper php tokenizer
	if instruction =~? '\c^\(if\|while\|foreach\|for\)\s*('
		" clear everything up until the first (
		let instruction = substitute(instruction, '^\(if\|while\|foreach\|for\)\s*(\s*', '', '')

		" lets iterate trough the instruction until we can find the pair for the opening (
		let i = 0
		let depth = 1
		while i < len(instruction)
			if instruction[i] == '('
				let depth += 1
			endif
			if instruction[i] == ')'
				let depth -= 1
			endif
			if depth == 0
				break
			end
			let i += 1
		endwhile
		let instruction = instruction[i + 1 : len(instruction)]
	endif

	" trim whitespace from the ends
	let instruction = substitute(instruction, '\v^(^\s+)|(\s+)$', '', 'g')

	return instruction
endfunction " }}}

function! phpcd#GetCallChainReturnType(classname_candidate, class_candidate_namespace, imports, methodstack) " {{{
	" Tries to get the classname and namespace for a chained method call like:
	"	$this->foo()->bar()->baz()->

	if a:class_candidate_namespace[0] == '\'
		let imports = {}
	else
		let imports = a:imports
	endif

	let classname_candidate = a:classname_candidate " {{{
	let class_candidate_namespace = a:class_candidate_namespace
	let methodstack = a:methodstack
	let unknown_result = ''
	let prev_method_is_array = (methodstack[0] =~ '\v^[^([]+\[' ? 1 : 0)
	let classname_candidate_is_array = (classname_candidate =~ '\[\]$' ? 1 : 0) " }}}

	if prev_method_is_array " {{{
		if classname_candidate_is_array
			let classname_candidate = substitute(classname_candidate, '\[\]$', '', '')
		else
			return unknown_result
		endif
	endif " }}}

	if (len(methodstack) == 1) " {{{
		let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, class_candidate_namespace, imports)
		let return_type = s:GetFullName(class_candidate_namespace, classname_candidate)

		return return_type
	endif " }}}

	call remove(methodstack, 0)
	let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, class_candidate_namespace, imports)
	let full_classname = s:GetFullName(class_candidate_namespace, classname_candidate)

	if methodstack[0] =~ '('
		let method = matchstr(methodstack[0], '\v^\$*\zs[^[(]+\ze')
		let return_types = rpc#request(g:phpcd_channel_id, 'functype', full_classname, method, expand('%:p'))
	else
		let prop = matchstr(methodstack[0], '\v^\$*\zs[^[(]+\ze')
		let return_types = rpc#request(g:phpcd_channel_id, 'proptype', full_classname, prop, expand('%:p'))
	endif

	if len(return_types) > 0
		let return_type = phpcd#SelectOne(return_types)
		return phpcd#GetCallChainReturnType(return_type, '', imports, methodstack)
	endif

	return unknown_result
endfunction " }}}

function! phpcd#GetMethodStack(line) " {{{
	let methodstack = []
	let i = 0
	let end = len(a:line)

	let current_part = ''

	let parent_depth = 0
	let in_string = 0
	let string_start = ''

	let next_char = ''

	while i < end
		let current_char = a:line[i]
		let next_char = i + 1 < end ? a:line[i + 1] : ''
		let prev_char = i >= 1 ? a:line[i - 1] : ''
		let prev_prev_char = i >= 2 ? a:line[i - 2] : ''

		if in_string == 0 && parent_depth == 0 && ((current_char == '-' && next_char == '>') || (current_char == ':' && next_char == ':'))
			call add(methodstack, current_part)
			let current_part = ''
			let i += 2
			continue
		endif

		" if it's looks like a string
		if current_char == "'" || current_char == '"'
			" and it is not escaped
			if prev_char != '\' || (prev_char == '\' && prev_prev_char == '\')
				" and we are in a string already
				if in_string
					" and that string started with this char too
					if current_char == string_start
						" clear the string mark
						let in_string = 0
					endif
				else " ... and we are not in a string
					" set the string mark
					let in_string = 1
					let string_start = current_char
				endif
			endif
		endif

		if !in_string && a:line[i] == '('
			let parent_depth += 1
		endif
		if !in_string && a:line[i] == ')'
			let parent_depth -= 1
		endif

		let current_part .= current_char
		let i += 1
	endwhile

	" add the last remaining part, this can be an empty string and this is expected
	" the empty string represents the completion base (which happen to be an empty string)
	if current_part != ''
		call add(methodstack, current_part)
	endif

	return methodstack
endfunction " }}}

function! phpcd#GetClassName(start_line, context, current_namespace, imports) " {{{
	let class_name_pattern = '[a-zA-Z_\x7f-\xff\\][a-zA-Z_0-9\x7f-\xff\\]*' " {{{
	let function_name_pattern = '[a-zA-Z_\x7f-\xff\\][a-zA-Z_0-9\x7f-\xff\\]*'
	let function_invocation_pattern = '[a-zA-Z_\x7f-\xff\\][a-zA-Z_0-9\x7f-\xff\\]*('
	let variable_name_pattern = '\$[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*'

	let classname_candidate = ''
	let class_candidate_namespace = a:current_namespace
	let class_candidate_imports = a:imports
	let methodstack = phpcd#GetMethodStack(a:context) " }}}

	if empty(methodstack) "{{{
		return ''
	endif "}}}

	if methodstack[-1] =~# '\vmake|app|get' " {{{
		" just for laravel and container-interop
		let container_interface = matchstr(methodstack[-1], '^\(make\|app\|get\)(\zs.\+\ze::class)')
		if container_interface != ''
			let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(container_interface, a:current_namespace, a:imports)
			return s:GetFullName(class_candidate_namespace, classname_candidate)
		endif
	endif " }}}
	if a:context =~? '^\$this->' || a:context =~? '^\(self\|static\)::' || a:context =~? 'parent::' " {{{
		let i = 1
		while i < a:start_line
			let line = getline(a:start_line - i)

			" Don't complete self:: or $this if outside of a class
			" (assumes correct indenting)
			if line =~ '^}'
				return ''
			endif

			if line =~? '\v^\s*(abstract\s+|final\s+)*\s*(class|trait)\s'
				let class_name = matchstr(line, '\c\(class\|trait\)\s\+\zs'.class_name_pattern.'\ze')
				let extended_class = matchstr(line, '\cclass\s\+'.class_name_pattern.'\s\+extends\s\+\zs'.class_name_pattern.'\ze')

				let classname_candidate = a:context =~? 'parent::' ? extended_class : class_name
				if classname_candidate != ''
					return phpcd#GetCallChainReturnType(classname_candidate, class_candidate_namespace, class_candidate_imports, methodstack)
				endif
			endif

			let i += 1
		endwhile " }}}
	elseif a:context =~ 'function' " {{{
		let i = 1
		while i < a:start_line
			let line = getline(a:start_line - i)

			" Don't complete self:: or $this if outside of a class
			" (assumes correct indenting)
			if line =~ '^}'
				return ''
			endif

			if line =~? '\v^\s*interface\s'
				let class_name = matchstr(line, '\cinterface\s\+\zs'.class_name_pattern.'\ze')

				if class_name != ''
					return a:current_namespace . '\' . class_name
				endif
			endif

			if line =~? '\v^\s*abstract\s'
				let class_name = matchstr(line, '\cabstract\s\+class\s\+\zs'.class_name_pattern.'\ze')

				if class_name != ''
					return a:current_namespace . '\' . class_name
				endif
			endif

			let i += 1
		endwhile " }}}
	elseif a:context =~? '(\s*new\s\+'.class_name_pattern.'\s*\(([^)]*)\)\?)->' " {{{
		let classname_candidate = matchstr(a:context, '\cnew\s\+\zs'.class_name_pattern.'\ze')
		if classname_candidate =~? '\vstatic|self' " {{{
			let i = 1
			while i < a:start_line
				let line = getline(a:start_line - i)

				if line =~? '\v^\s*(abstract\s+|final\s+)*\s*class\s'
					let classname_candidate = matchstr(line, '\cclass\s\+\zs'.class_name_pattern.'\ze')
					break
				endif

				let i += 1
			endwhile
		end " }}}
		return phpcd#GetCallChainReturnType(classname_candidate, class_candidate_namespace, class_candidate_imports, methodstack) " }}}
	elseif get(methodstack, 0) =~# function_invocation_pattern " {{{
		let function_name = matchstr(methodstack[0], '^\s*\zs'.function_name_pattern)
		let return_types = rpc#request(g:phpcd_channel_id, 'functype', '', function_name, expand('%:p'))
		if len(return_types) > 0
			let return_type = phpcd#SelectOne(return_types)
			return phpcd#GetCallChainReturnType(return_type, '', class_candidate_imports, methodstack)
		endif " }}}
	else " {{{
		" extract the variable name from the context {{{
		let object = methodstack[0]
		let object_is_array = (object =~ '\v^[^[]+\[' ? 1 : 0)
		let object = matchstr(object, variable_name_pattern)

		let function_boundary = phpcd#GetCurrentFunctionBoundaries()
		let search_end_line = max([1, function_boundary[0][0]])
		" -1 makes us ignore the current line (where the completion was invoked
		let lines = reverse(getline(search_end_line, a:start_line - 1)) "}}}

		" check Constant lookup {{{
		let classname_candidate = matchstr(a:context, '\zs'.class_name_pattern.'\ze::')
		if classname_candidate != ''
			let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, a:current_namespace, a:imports)
			return phpcd#GetCallChainReturnType(classname_candidate, class_candidate_namespace, class_candidate_imports, methodstack)
		endif "}}}

		" scan the file backwards from the current line {{{
		let i = 1
		for line in lines
				" in file lookup for /* @var $foo Class */
				if line =~# '@var\s\+'.object.'\s\+'.class_name_pattern "{{{
					let classname_candidate = matchstr(line, '@var\s\+'.object.'\s\+\zs'.class_name_pattern.'\(\[\]\)\?')
					let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, a:current_namespace, a:imports)
					break
				endif "}}}

				" in file lookup for /* @var Class $foo */
				if line =~# '@var\s\+'.class_name_pattern.'\s\+'.object "{{{
					let classname_candidate = matchstr(line, '@var\s\+\zs'.class_name_pattern.'\(\[\]\)\?\ze'.'\s\+'.object)
					let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, a:current_namespace, a:imports)
					break
				endif "}}}

			" do in-file lookup of $var = new Class or $var = new (s|S)tatic
			if line =~# '^\s*'.object.'\s*=\s*new\s\+'.class_name_pattern && !object_is_array " {{{
				let classname_candidate = matchstr(line, object.'\c\s*=\s*new\s*\zs'.class_name_pattern.'\ze')
				if classname_candidate =~? '\vstatic|self' " {{{
					let nsuse = rpc#request(g:phpcd_channel_id, 'nsuse', expand('%:p'))
					let classname_candidate = nsuse.class
				end " }}}
				let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, a:current_namespace, a:imports)
				break
			endif " }}}

			if line =~# '^\s*'.object.'\s*=\s*\(require\|include\).*' && !object_is_array " {{{
				let path = matchstr(line, '\(require\|include\)\(_once\)\?\s*__DIR__\s*\.\s*\zs.*\ze;')
				let cwd = expand('%:p:h')
				let path = cwd.substitute(path, "'", '', 'g')
				let path = fnamemodify(path, ':p:.')

				silent! below 1sp
				exec 'e +$ ' . path
				call search(';')
				let [symbol, context, namespace, imports] = phpcd#GetCurrentSymbolWithContext()
				let classname = phpcd#GetClassName(line('.'), symbol.'->', namespace, imports)
				q
				return classname
			endif " }}}

			" lambda declaration line
			if line =~? 'function\s*(.\{-\}\s*'.object " {{{
				" skip function () use($object)
				if line =~? 'use\s*(.\{-\}'.object.'\>'
					let i += 1
					continue
				endif

				" search for type hinted arguments
				let classname_candidate = matchstr(line, '\c\zs'.class_name_pattern.'\ze\s\+'.object.'\>')
				if classname_candidate
					if classname_candidate[0] == '\'
						return classname_candidate
					endif
					let [classname_candidate, class_candidate_namespace] = phpcd#ExpandClassName(classname_candidate, a:current_namespace, a:imports)
					break
				endif
			endif " }}}

			" function declaration line
			if line =~? 'function\s\+'.function_name_pattern.'\s*(.\{-\}\s*'.object " {{{
				let nsuse = rpc#request(g:phpcd_channel_id, 'nsuse', expand('%:p'))

				let line_no = a:start_line - i
				if line_no > nsuse.start_line && line_no < nsuse.end_line
					let classname = nsuse.namespace.'\'.nsuse.class
				else
					let classname = ''
				endif

				let funcname = matchstr(line, '\cfunction\s\+\zs'.function_name_pattern.'\ze')
				let argtypes = rpc#request(g:phpcd_channel_id, 'argtype', classname, funcname, object, expand('%:p'))

				let classname_candidate = phpcd#SelectOne(argtypes)
				break
			endif " }}}

			" assignment for the variable in question with a variable on the right hand side
			if line =~# '^\s*'.object.'\s*=&\?\s*\(clone\s\+\)\?\s*'.variable_name_pattern.';' " {{{
				let c = matchstr(a:context, variable_name_pattern.'\zs.\+')
				let classname_candidate = phpcd#GetTypeAt(a:start_line - i, c)
				break
			endif " }}}

			" assignment for the variable in question with function chains on the right hand side
			if line =~? '^\s*' . object . '\s*=.*);\?$' " {{{
				let classname_candidate = phpcd#GetCallChainReturnTypeAt(a:start_line - i)
				break
			endif " }}}

			if line =~? object.'\s*=\s*'.variable_name_pattern.'[' " {{{
				let sub_context = matchstr(line, '=\s*\zs'.variable_name_pattern.'\ze[')
				let prev_class = phpcd#GetClassName(a:start_line - i, sub_context, a:current_namespace, a:imports)

				let [classname_candidate, class_candidate_namespace] = s:getArrayType(prev_class)
				break
			endif " }}}

			" foreach with the variable in question
			if line =~? 'foreach\s*(.\{-}\s\+'.object.'\s*)' " {{{
				let sub_context = matchstr(line, 'foreach\s*(\s*\zs.\{-}\ze\s\+as')
				let prev_class = phpcd#GetClassName(a:start_line - i, sub_context, a:current_namespace, a:imports)

				let [classname_candidate, class_candidate_namespace] = s:getArrayType(prev_class)
				if classname_candidate != ''
					break
				else
					let i += 1
					continue
				endif
			endif " }}}

			" catch clause with the variable in question
			if line =~? 'catch\s*(\zs'.class_name_pattern.'\ze\s\+'.object " {{{
				let classname_candidate = matchstr(line, 'catch\s*(\zs'.class_name_pattern.'\ze\s\+'.object)
				break
			endif " }}}

			let i += 1
		endfor " }}}

		if classname_candidate != '' " {{{
			return phpcd#GetCallChainReturnType(classname_candidate, class_candidate_namespace, class_candidate_imports, methodstack)
		endif " }}}
	endif " }}}
endfunction " }}}

function s:getArrayType(prev_class) " {{{
	" the iterated expression should return an array type
	if a:prev_class =~ '\[\]$' " {{{
		let prev_class = matchstr(a:prev_class, '\v^[^[]+')
	else
		return ['', '']
	endif " }}}

	if stridx(prev_class, '\') != -1 " {{{
		let classname_parts = split(prev_class, '\\\+')
		let classname_candidate = classname_parts[-1]
		let class_candidate_namespace = join(classname_parts[0:-2], '\')
	else
		let classname_candidate = prev_class
		let class_candidate_namespace = '\'
	endif " }}}

	return [classname_candidate, class_candidate_namespace]
endfunction " }}}

function! phpcd#UpdateIndex() " {{{
	if !exists('g:phpid_channel_id')
		return
	endif

	let g:phpcd_need_update = 0
	let nsuse = rpc#request(g:phpcd_channel_id, 'nsuse', expand('%:p'))

	if !nsuse.class
		return
	endif

	let classname = nsuse.namespace . '\' . nsuse.class
	return rpc#notify(g:phpid_channel_id, 'update', classname)
endfunction " }}}

function! phpcd#Index() "{{{
	if !exists('g:phpid_channel_id')
		return
	endif

	call rpc#notify(g:phpid_channel_id, 'index')
endfunction " }}}

function! phpcd#GetCurrentNameSpace() " {{{
	let nsuse = rpc#request(g:phpcd_channel_id, 'nsuse', expand('%:p'))

	return [nsuse.namespace, nsuse.imports]
endfunction " }}}

function! phpcd#GetCurrentFunctionBoundaries() " {{{
	let old_cursor_pos = [line('.'), col('.')]
	let current_line_no = old_cursor_pos[0]
	let function_pattern = '\<function\s\+[^(]\+('

	let func_start_pos = searchpos(function_pattern, 'Wbc')
	if func_start_pos == [0, 0]
		call cursor(old_cursor_pos[0], old_cursor_pos[1])
		return 0
	endif

	" get the line where the function declaration actually started
	call search('\cfunction\_.\{-}(\_.\{-})\_.\{-}{', 'Wce')

	" get the position of the function block's closing "}"
	let func_end_pos = searchpairpos('{', '', '}', 'W', 'synIDattr(synID(line("."), col("."), 0), "name") =~? "string\\|comment"')
	if func_end_pos == [0, 0]
		" there is a function start but no end found, assume that we are in a
		" function but the user did not typed the closing "}" yet and the
		" function runs to the end of the file
		let func_end_pos = [line('$'), len(getline(line('$')))]
	endif

	" Decho func_start_pos[0].' <= '.current_line_no.' && '.current_line_no.' <= '.func_end_pos[0]
	if func_start_pos[0] <= current_line_no && current_line_no <= func_end_pos[0]
		call cursor(old_cursor_pos[0], old_cursor_pos[1])
		return [func_start_pos, func_end_pos]
	endif

	call cursor(old_cursor_pos[0], old_cursor_pos[1])
	return 0
endfunction " }}}

function! phpcd#ExpandClassName(classname, current_namespace, imports) " {{{
	if a:classname[0] == '\'
		let last_slash_pos = strridx(a:classname, '\')
		if last_slash_pos <= 0
			return [a:classname[1:], '\']
		else
			return [a:classname[last_slash_pos+1:], a:classname[:last_slash_pos-1]]
		endif
	endif

	let parts = split(a:classname, '\\\+')
	if has_key(a:imports, parts[0])
		let parts[0] = a:imports[parts[0]]
	else
		call insert(parts, a:current_namespace, 0)
	endif

	if len(parts) == 1
		let parts = split(parts[0], '\\\+')
	endif
	let classname = parts[-1]
	let namespace = join(parts[0:-2], '\')
	return [classname, namespace]
endfunction " }}}

function! phpcd#GetCallChainReturnTypeAt(line) " {{{
	silent! below 1sp
	let classname = ''
	try
		exec 'normal! ' . a:line . 'G'
		call search(';')
		let [_, context, namespace, imports] = phpcd#GetCurrentSymbolWithContext()
		let classname = phpcd#GetClassName(line('.'), context, namespace, imports)
	finally
		q
	endtry
	if classname[0] != '\'
		let classname = '\'.classname
	endif
	return classname
endfunction " }}}

function! phpcd#GetTypeAt(line, context) " {{{
	silent! below 1sp
	let classname = ''
	try
		exec 'normal! ' . a:line . 'G'
		call search(';')
		normal beh
		let [_, context, namespace, imports] = phpcd#GetCurrentSymbolWithContext()
		let instruction = phpcd#GetCurrentInstruction(line('.'), col('.'), [0,0])
		let context = instruction.a:context
		let classname = phpcd#GetClassName(line('.'), context, namespace, imports)
	finally
		q
	endtry
	return classname
endfunction " }}}

function! s:GetFullName(namespace, classname) " {{{
	if a:namespace == '\'
		let full_classname = a:classname
	else
		let full_classname = a:namespace . '\' . a:classname
	endif

	return full_classname
endfunction " }}}

function! phpcd#GetRoot() " {{{
	let pwd = expand("%:p:h")

	if pwd[0] != '/' " for editing non exists dir file
		let pwd = getcwd()
	endif

	let root = pwd

	if g:phpcd_root != '/' && stridx(root, g:phpcd_root) == 0
		return g:phpcd_root
	endif

	while root != "/"
		if (filereadable(root.'/.phpcd.vim'))
			return root
		endif
		let root = fnamemodify(root, ":h")
	endwhile

	let root = pwd
	while root != "/"
		if (filereadable(root . "/vendor/autoload.php"))
			return root
		endif
		let root = fnamemodify(root, ":h")
	endwhile

	return root
endfunction " }}}

function! phpcd#OpenFileNoAutoRestart() " {{{
	if g:phpcd_root == '/'
		call s:init()
	endif

	call s:startChannel()
endfunction " }}}

function! phpcd#EnterBufferWithAutoRestart() " {{{
	if phpcd#GetRoot() != g:phpcd_root
		call s:init()
	endif

	call s:startChannel()
endfunction " }}}

function! s:init() " {{{
	let g:phpcd_root = phpcd#GetRoot()
	let phpcd_vim = g:phpcd_root.'/.phpcd.vim'
	if filereadable(phpcd_vim)
		exec 'source '.phpcd_vim
	endif

	if exists('g:phpcd_channel_id')
		call rpc#stop(g:phpcd_channel_id)

		unlet g:phpcd_channel_id
		if exists('g:phpid_channel_id')
			unlet g:phpid_channel_id
		endif
	endif
endfunction " }}}

function! s:startChannel() " {{{
	if has('nvim')
		let messenger = 'msgpack'
	else
		let messenger = 'json'
	endif

	if !exists('g:phpcd_channel_id')
		let g:php_autoload_path = g:phpcd_root.'/'.g:phpcd_autoload_path
		let g:phpcd_channel_id = rpc#start(g:phpcd_php_cli_executable,
					\ s:phpcd_path, g:phpcd_root, messenger, g:php_autoload_path,
					\ g:phpcd_disable_modifier)

		if g:phpcd_root != '/'
			let g:phpid_channel_id = g:phpcd_channel_id
		endif
	endif
endfunction " }}}

" vim: foldmethod=marker:noexpandtab:ts=2:sts=2:sw=2
