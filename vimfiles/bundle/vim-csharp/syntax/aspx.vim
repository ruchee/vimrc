" Vim syntax file
" Language:     ASP.NET pages with C#
" Maintainer:   Stacey Abshire <vim.3.sabshire@xoxy.net>
" Last Change:  $Id: aspnet.vim,v 1.0 2004/04/02 02:00:03 sabshire Exp $
" Filenames:    *.aspx,*.ascx,*.asmx
"
" CREDITS: Mark Feeney for the original.  I tweaked it to correctly
"          handle code in a DataBind region.
"

" Quit when a syntax file was already loaded
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'aspnet'
endif

syn include @aspnetAddCS syntax/cs.vim
unlet b:current_syntax

"

  runtime! syntax/html.vim
unlet b:current_syntax

syn cluster htmlPreProc add=aspnetServerScript,aspnetSpecialTag,aspDataBindRegion,aspnetDataBindInString

" Handles <%# %> tags within a string
syn match aspnetDataBindInString /".*<%#.*%>.*"/

" ASP.NET Tags
" EXAMPLE:
" <%@ Page Langauge="C#" %>
"
syn region aspnetSpecialTag contained keepend extend matchgroup=Function start=+<%+ end=+%>+me=e-1 contains=aspnetDirective,aspnetAttribute,aspnetSpecialTagPunct,aspnetDataBindRegion,@aspnetAddCS 
syn match aspnetSpecialTagPunct contained       +[@"'=]+

" Data binding syntax
" EXAMPLE:
" <%# Container.DataItem %>
syn region aspnetDataBindRegion keepend contained matchgroup=Function start=+<%#+ end=+%>+ contains=@aspnetAddCS,aspnetDataBindRegionPunct
syn match aspnetDataBindRegion contained +#[^"']\+\.*+ contains=@aspnetAddCS,aspnetDataBindRegionPunct display
syn match aspnetDataBindRegionPunct contained +#+

" @ directives
" NOTE: we skip over highlighting the @ and use aspnetSpecialTagPunct to give it a
"       different colour.
syn match aspnetDirective contained     +@\s*\(Page\|Control\|Import\|Implements\|Register\|Assembly\|OutputCache\|Reference\)+hs=s+1 contains=aspnetSpecialTagPunct
syn keyword aspnetAttribute contained   AspCompat AutoEventWireup Assembly Buffer
syn keyword aspnetAttribute contained   ClassName ClientTarget CodePage CompilerOptions ContentType Culture
syn keyword aspnetAttribute contained   Debug Description EnableSessionState EnableViewState EnableViewStateMac
syn keyword aspnetAttribute contained   ErrorPage Explicit Inherits Language LCID ResponseEncoding Src
syn keyword aspnetAttribute contained   SmartNavigation Strict Trace TraceMode Transaction UICulture WarningLevel
syn keyword aspnetAttribute contained   Namespace Interface TagPrefix TagName Duration Location
syn keyword aspnetAttribute contained   VaryByCustom VaryByHeader VaryByParam VaryByControl Control

" ASP.NET server side script blocks.
" EXAMPLE: <script runat="server"> ... </script>
" FIXME:   This is a pretty skety region here -- I can't seem to get the end
"          </script> tag to be part of the region... annoying.
syn region aspnetServerScript matchgroup=Special start=+<script[^>]*runat="\=server"\=[^>]*>+ end=+</script>+ contained contains=@aspnetAddCS fold

" ASP standard server controls
" EXAMPLE: <asp:TextBox id="t1" runat="server"/>
" NOTE:    I've set this up to only work if runat="server" is present since I
"          always forget to put that in, and then wonder why things don't
"          work.
syn match aspnetServerControl contained +asp:\w\+\(.*runat="\=server"\=\)\@=+ contains=aspnetServerControlName,aspnetServerControlPunct
syn match aspnetServerControl contained +/asp:\w\++ contains=aspnetServerControlName,aspnetServerControlPunct
syn match aspnetServerControlPunct contained +:+
syn keyword aspnetServerControlName contained Literal PlaceHolder Xml AdRotator Button Calendar CheckBox
syn keyword aspnetServerControlName contained CheckBoxList DataGrid DataList DropDownList HyperLink Image
syn keyword aspnetServerControlName contained ImageButton Label LinkButton ListBox Panel PlaceHolder RadioButton
syn keyword aspnetServerControlName contained RadioButtonList Repeater Table TableCell TableRow TextBox
syn keyword aspnetServerControlArg contained runat id OnSelectedIndexChanged AutoPostBack EnableViewState

" Add these new custom tags to the rest of the HTML markup rules
syn cluster htmlTagNameCluster add=aspnetServerControl
syn cluster htmlArgCluster add=aspnetServerControlArg

"
" Link the highlighting up
"
hi def link aspnetSpecialTagPunct       Delimiter
hi def link aspnetDataBindRegionPunct   Delimiter
hi def link aspnetDirective             Statement
hi def link aspnetAttribute             Type
hi def link aspnetServerControl         PreProc
hi def link aspnetServerControlName     PreProc
hi def link aspnetServerControlPunct    Delimiter
hi def link aspnetServerControlArg      Type

let b:current_syntax = "aspnet"

