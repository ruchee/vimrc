if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

setlocal autoindent nosmartindent

let b:undo_indent = "setl autoindent< smartindent<"
