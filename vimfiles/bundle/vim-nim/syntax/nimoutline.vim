scriptencoding utf-8


syntax keyword nimoTypes      Types
syntax keyword nimoCallables  Routines
syntax keyword nimoConstants  Constants
syntax keyword nimoGlobals    Globals
syntax keyword nimoImports    Imports

syntax match nimoSeparator  "[»()<>\[\]]"
syntax match nimoActive     "«»\s.*$"
syntax match nimoDetail     "(\(\w\+\))"

hi link nimoConstants  Constant
hi link nimoConverters Function
hi link nimoFields     Type
hi link nimoGlobals    Constant
hi link nimoInclude    Include
hi link nimoIterators  Function
hi link nimoMacros     Macro
hi link nimoMethods    Function
hi link nimoModules    Include
hi link nimoPackages   Include
hi link nimoProcs      Function
hi link nimoTemplates  Function
hi link nimoTypes      Type
hi link nimoSeparator  Comment
hi link nimoActive     Typedef

hi link nimoTypes      Type
hi link nimoCallables  Function
hi link nimoConstants  Constant
hi link nimoGlobals    Identifier
hi link nimoImports    Include
hi link nimoDetail     Identifier
