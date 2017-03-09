" This script detects filetype 'llvm' from the content of file.
" See :help new-filetype-scripts

if did_filetype()
    finish
endif

if line('$') > 3
    if getline(1) =~# '^; ModuleID = ' && getline(2) =~# '^target datalayout = ' && getline(3) =~# '^target triple = '
        setfiletype llvm
    endif
endif
