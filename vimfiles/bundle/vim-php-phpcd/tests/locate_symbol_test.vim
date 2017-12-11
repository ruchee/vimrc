" Code dealing with test buffers is placed in 'try' blocks, and wiping of test
" buffers is done in 'finally' blocks, to ensure that the buffers are wiped
" even if an exception is thrown from the test code.  Unfortunately this
" prevents Vimunit from showing the correct line numbers for failed
" assertions; try to compensate by having a message for each assertion.

let s:path_base = expand('%:p:h') . '/fixtures/PHPCD/'
let s:asame_path = s:path_base . 'SameName/A/Same.php'
let s:bsame_path = s:path_base . 'SameName/B/Same.php'
let s:supersame_path = s:path_base . 'SameName/A/SuperSame.php'
let s:expect_locate_path = s:path_base . 'B/C/ExpectLocate.php'

let s:asame_locations = {
	\'class_start': {'line': 4, 'col': 1},
	\'property': {'line': 10, 'col': 13},
	\'abstract_method_impl': {'line': 12, 'col': 14},
	\'simple_method': {'line': 16, 'col': 14},
\}

let s:bsame_locations = {
	\'class_start': {'line': 4, 'col': 1},
	\'property': {'line': 8, 'col': 13},
	\'simple_method': {'line': 10, 'col': 14},
\}

let s:supersame_locations = {
	\'static_method': {'line': 9, 'col': 28},
	\'abstract_method': {'line': 18, 'col': 23},
\}

let s:expect_locate_locations = {
	\'new_asame_class': {'line': 11, 'col': 18},
	\'new_bsame_class': {'line': 12, 'col': 18},
	\'asame_property_use': {'line': 14, 'col': 13},
	\'asame_method_use': {'line': 17, 'col': 13},
	\'asame_class_use': {'line': 21, 'col': 9},
	\'asame_const_use': {'line': 21, 'col': 15},
	\'bsame_const_use': {'line': 22, 'col': 16},
	\'super_method_use': {'line': 24, 'col': 15},
\}

let s:asame_info = {'path': s:asame_path, 'locations': s:asame_locations}
let s:bsame_info = {'path': s:bsame_path, 'locations': s:bsame_locations}

function! TestCaseLocateNewASameMid()
	call s:checkLocateClass(s:expect_locate_locations.new_asame_class, 1, 'new', s:asame_info)
endfunction

function! TestCaseLocateNewBSameMid()
	call s:checkLocateClass(s:expect_locate_locations.new_bsame_class, 1, 'new', s:bsame_info)
endfunction

function! TestCaseLocateASameProperty()
	call s:checkLocateProperty(s:expect_locate_locations.asame_property_use, '$a->', s:asame_info)
endfunction

function! TestCaseLocateASameMethodStart()
	let cursor_location = s:expect_locate_locations.asame_method_use
	call s:checkLocateBasic(cursor_location, s:asame_path, '$a->', s:asame_locations.simple_method.line)
endfunction

function! TestCaseLocateASameMethodMid()
	let cursor_location = s:offsetColumn(s:expect_locate_locations.asame_method_use, 1)
	call s:checkLocateBasic(cursor_location, s:asame_path, '$a->', s:asame_locations.simple_method.line)
endfunction

function! TestCaseLocateASameClassStart()
	call s:checkLocateClass(s:expect_locate_locations.asame_class_use, 0, '', s:asame_info)
endfunction

function! TestCaseLocateASameClassMid()
	call s:checkLocateClass(s:expect_locate_locations.asame_class_use, 1, '', s:asame_info)
endfunction

function! TestCaseLocateASameConst()
	let cursor_location = s:expect_locate_locations.asame_const_use
	call s:checkLocateBasic(cursor_location, s:asame_path, 'Same::', 'const CA')
endfunction

function! TestCaseLocateBSameConst()
	let cursor_location = s:expect_locate_locations.bsame_const_use
	call s:checkLocateBasic(cursor_location, s:bsame_path, 'BSame::', 'const CA')
endfunction

function! TestCaseLocateSuperMethod()
	let cursor_location = s:expect_locate_locations.super_method_use
	call s:checkLocateBasic(cursor_location, s:supersame_path, 'Same::', s:supersame_locations.static_method.line)
endfunction

function! TestCaseLocateImplMid()
	call s:checkLocateAbstractMethodImpl(1)
endfunction

function! s:checkLocateClass(
	\base_cursor_location,
	\column_offset,
	\expected_symbol_context,
	\expected_class_info)

	call s:checkLocateBasic(
		\s:offsetColumn(a:base_cursor_location, a:column_offset),
		\a:expected_class_info.path,
		\a:expected_symbol_context,
		\a:expected_class_info.locations.class_start.line)
endfunction

function! s:offsetColumn(location, offset)
	return {'line': a:location.line, 'col': a:location.col + a:offset}
endfunction

function! s:checkLocateProperty(cursor_location, expected_symbol_context, expected_class_info)
	call s:checkLocateBasic(
		\a:cursor_location,
		\a:expected_class_info.path,
		\a:expected_symbol_context,
		\a:expected_class_info.locations.property.line)
endfunction

function! s:checkLocateBasic(
	\cursor_location,
	\expected_path,
	\expected_symbol_context,
	\expected_line_or_const)

	call s:checkLocate(
		\a:cursor_location.line,
		\a:cursor_location.col,
		\a:expected_path,
		\a:expected_symbol_context,
		\a:expected_line_or_const,
		\0,
		\s:expect_locate_path)
endfunction

function! s:checkLocateNonexistentMethod(
	\cursor_line,
	\cursor_column,
	\expected_symbol_context)

	call s:checkLocate(
		\a:cursor_line,
		\a:cursor_column,
		\'',
		\a:expected_symbol_context,
		\'',
		\'',
		\s:expect_locate_path)
endfunction

function! s:checkLocateAbstractMethodImpl(column_offset)
	let base_cursor_location = s:supersame_locations.abstract_method
	call s:checkLocate(
		\base_cursor_location.line,
		\base_cursor_location.col + a:column_offset,
		\s:asame_path,
		\'abstract function',
		\s:asame_locations.abstract_method_impl.line,
		\0,
		\s:supersame_path)
endfunction

function! s:checkLocate(
	\cursor_line,
	\cursor_column,
	\expected_path,
	\expected_symbol_context,
	\expected_line_or_const,
	\expected_column,
	\path_to_edit)

	below 1new
	exec ':silent! edit ' . a:path_to_edit

	try
		call cursor(a:cursor_line, a:cursor_column)

		let [
			\symbol,
			\symbol_context,
			\symbol_namespace,
			\current_imports
		\] = phpcd#GetCurrentSymbolWithContext()

		call VUAssertEquals(
			\a:expected_symbol_context,
			\symbol_context,
			\'symbol_context should be "' . a:expected_symbol_context . '"')

		let [
			\symbol_file,
			\symbol_line,
			\symbol_col
		\] = phpcd#LocateSymbol(symbol, symbol_context, symbol_namespace, current_imports)

		call s:checkPath(a:expected_path, symbol_file)

		call VUAssertEquals(
			\a:expected_line_or_const,
			\symbol_line,
			\'expect line or const: ' . a:expected_line_or_const)

		call VUAssertEquals(a:expected_column, symbol_col, 'expect column ' . a:expected_column)
	finally
		silent! bw! %
	endtry
endfunction

function! s:checkPath(expected_path, symbol_file)
	if empty(a:expected_path)
		let expected_path_message = 'expect path to be empty'
	else
		let expected_path_message = 'expected path: ' . a:expected_path
	endif

	call VUAssertEquals(a:expected_path, a:symbol_file, expected_path_message)
endfunction
