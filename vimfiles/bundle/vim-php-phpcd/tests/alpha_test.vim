" Code dealing with test buffers is placed in 'try' blocks, and wiping of test
" buffers is done in 'finally' blocks, to ensure that the buffers are wiped
" even if an exception is thrown from the test code.  Unfortunately this
" prevents Vimunit from showing the correct line numbers for failed
" assertions; try to compensate by having a message for each assertion.

fun! TestCase_expect_public_property()
	let path =  expand('%:p:h')."/".'fixtures/PHPCD/B/C/ExpectPublicVariable.php'
	below 1new
	exe ":silent! edit ".path

	try
		call cursor(11, 18)
		let start = phpcd#CompletePHP(1, '')
		call VUAssertEquals(16, start, 'completion start column should be 16')
		let base = strpart(getline('.'), start, 1)
		call VUAssertEquals('p', base, 'completion base should be "p"')
		let res = phpcd#CompletePHP(0, base)
		" res == [{'word': 'pubvar', 'info': '', 'kind': 'p', 'abbr': '  - pubvar', 'icase': 1}]

		call VUAssertEquals(v:t_list, type(res), 'result should be a List')
		call VUAssertTrue(len(res) > 0, 'result List should not be empty')
		call VUAssertEquals(
			\v:t_dict,
			\type(res[0]),
			\'0th element of result List should be a Dictionary')
		call VUAssertTrue(
			\has_key(res[0], 'word'),
			\'the Dictionary from the result should have the key "word"')
		call VUAssertEquals(
			\'pubvar',
			\res[0].word,
			\'completion word should be "pubvar"')

	finally
		silent! bw! %
	endtry
endf

fun! TestCase_expect_constant()
	let path =  expand('%:p:h')."/".'fixtures/PHPCD/B/C/ExpectClassConstantOnly.php'
	below 1new
	exe ":silent! edit ".path

	try
		call cursor(9, 27)
		let start = phpcd#CompletePHP(1, '')
		call VUAssertEquals(24, start, 'completion start column should be 24')
		let base = strpart(getline('.'), start, 1)
		call VUAssertEquals('i', base, 'completion base should be "i"')
		let res = phpcd#CompletePHP(0, base)

		" this test with fixtures
		" intentionally exposes completion of static variables
		" even without leading dollar sign
		for item in res
			call VUAssertEquals(
				\'d',
				\item.kind,
				\'expected item.kind to be "d"')
		endfor
	finally
		silent! bw! %
	endtry
endf
