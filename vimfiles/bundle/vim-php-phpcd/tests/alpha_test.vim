fun! TestCase_expect_public_property()
	let path =  expand('%:p:h')."/".'fixtures/PHPCD/B/C/ExpectPublicVariable.php'
	below 1new
	exe ":silent! edit ".path

	call cursor(11, 18)
	let start = phpcd#CompletePHP(1, '')
	let base = strpart(getline("."), start)
	let res = phpcd#CompletePHP(0, base)
	" res == [{'word': 'pubvar', 'info': '', 'kind': 'p', 'abbr': '  - pubvar', 'icase': 1}]

	call VUAssertEquals(
		\'pubvar',
		\res[0].word)

	silent! bw! %
endf

fun! TestCase_expect_constant()
	let path =  expand('%:p:h')."/".'fixtures/PHPCD/B/C/ExpectClassConstantOnly.php'
	below 1new
	exe ":silent! edit ".path

	call cursor(9, 27)
	let start = phpcd#CompletePHP(1, '')
	let base = strpart(getline("."), start)
	let res = phpcd#CompletePHP(0, base)

	" this test with fixtures
	" intentionally exposes completion of static variables
	" even without leading dollar sign
	for item in res
		call VUAssertEquals(
		\'d',
		\item.kind)
	endfor
	silent! bw! %
endf
