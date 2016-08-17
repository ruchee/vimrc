source helper.vim

let tmpfile = 'untrackedFileWithinRepo.tmp'
call system('touch '.tmpfile)
execute 'edit '.tmpfile
normal ggo*
doautocmd CursorHold
call DumpSigns('untrackedFileWithinRepo')

call system('rm '.tmpfile)
