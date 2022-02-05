" Moon Colorscheme for GUI

let s:foreground  = "FFFFFF"
let s:background = "2B2B2B"
let s:background_light  = "3C3C3C"

let s:builtin  = "F69385"
let s:string   = "F1BF8E"
let s:proper   = "99CBCA"
let s:bool     = "B3EFE5"
let s:func     = "B0D89C"
let s:punct    = "F277A1"
let s:keyword  = "BB84B4"
let s:comment  = "929292"
let s:number   = "9D8FF2"

set background=dark
hi clear
syntax reset

let g:colors_name = "moon"

if has("gui_running")
	fun <SID>X(group, fg, bg, attr)
		if a:fg != ""
			exec "hi " . a:group . " guifg=#" . a:fg
		endif
		if a:bg != ""
			exec "hi " . a:group . " guibg=#" . a:bg
		endif
		if a:attr != ""
			exec "hi " . a:group . " gui=" . a:attr
		endif
	endfun

	call <SID>X("Normal", s:foreground, s:background, "")

	call <SID>X("Identifier", s:builtin, "", "")
	call <SID>X("Function", s:func, "", "")

	call <SID>X("Keyword", s:keyword, "", "")
	call <SID>X("Conditional", s:keyword, "", "")
	call <SID>X("Repeat", s:keyword, "", "")
	call <SID>X("Statement", s:keyword, "", "")

	call <SID>X("Comment", s:comment, "", "")
	call <SID>X("Todo", s:comment, "", "")

	call <SID>X("Operator", s:punct, "", "")
	call <SID>X("PreProc", s:punct, "", "")

	call <SID>X("Boolean", s:bool, "", "")

	call <SID>X("Type", s:proper, "", "")
	call <SID>X("Structure", s:proper, "", "")
	call <SID>X("Constant", s:proper, "", "")

	call <SID>X("String", s:string, "", "")
	call <SID>X("Number", s:number, "", "")
	call <SID>X("Float", s:number, "", "")

	call <SID>X("LineNr", s:background_light, "", "")
	call <SID>X("StatusLine", s:foreground, s:background_light, "")

	" TODO:
	" call <SID>X("Error", "", "", "")
	" call <SID>X("SpecialChar", "", "", "")
endif
