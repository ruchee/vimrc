" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-09-17.
" @Last Change: 2010-09-17.
" @Revision:    16

SpecBegin 'title': 'tlib#date'

Should be equal tlib#date#Parse('2000-1-0', 1), [2000, 1, 0]
Should be equal tlib#date#Parse('2000-1-2'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000-01-02'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000-10-20'), [2000, 10, 20]

Should be equal tlib#date#Parse('00-1-0', 1), [2000, 1, 0]
Should be equal tlib#date#Parse('00-1-2'), [2000, 1, 2]
Should be equal tlib#date#Parse('00-01-02'), [2000, 1, 2]
Should be equal tlib#date#Parse('00-10-20'), [2000, 10, 20]

Should be equal tlib#date#Parse('2000/2/1'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000/02/01'), [2000, 1, 2]
Should be equal tlib#date#Parse('2000/20/10'), [2000, 10, 20]

Should be equal tlib#date#Parse('00/2/1'), [2000, 1, 2]
Should be equal tlib#date#Parse('00/02/01'), [2000, 1, 2]
Should be equal tlib#date#Parse('00/20/10'), [2000, 10, 20]

Should be equal tlib#date#Parse('2.1.2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('2. 1. 2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('02.01.2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('02. 01. 2000'), [2000, 1, 2]
Should be equal tlib#date#Parse('20.10.2000'), [2000, 10, 20]
Should be equal tlib#date#Parse('20. 10. 2000'), [2000, 10, 20]

Should throw exception "tlib#date#Parse('2000-14-2')", 'TLib: Invalid date'
Should throw exception "tlib#date#Parse('2000-011-02')", 'TLib: Invalid date'
Should throw exception "tlib#date#Parse('2000-10-40')", 'TLib: Invalid date'
Should throw exception "tlib#date#Parse('2000-10-0')", 'TLib: Invalid date'

