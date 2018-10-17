" Vim omni completion file
" Language:     Erlang
" Author:       Oscar Hellström <oscar@oscarh.net>
" Contributors: kTT (http://github.com/kTT)
"               Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               Eduardo Lopez (http://github.com/tapichu)
" License:      Vim license
" Version:      2012/01/14

" Completion program path
let s:erlang_complete_file = expand('<sfile>:p:h') . '/erlang_complete.erl'

" Returns whether "substring" is a prefix of "string".
function s:starts_with(string, substring)
    let string_start = strpart(a:string, 0, len(a:substring))
    return string_start ==# a:substring
endfunction

" If we are running in Cygwin, the path needs to be converted.
" See: https://github.com/vim-erlang/vim-erlang-omnicomplete/issues/21
if has('win32') == 0 && s:starts_with(system('uname'), 'CYGWIN')
    " Cygwin system. Now check if erlang is Windows or cygwin (currently only
    " Windows is possible)
    let cygwin_base_path = system('cygpath -w /')
    if !s:starts_with(s:erlang_complete_file, cygwin_base_path)
        " Windows, as expected
        let s:erlang_complete_file = system('cygpath -w ' . s:erlang_complete_file)
    endif
endif

au BufWritePre *.erl :call erlang_complete#ClearOneCache(expand('%:t:r'))

if !exists('g:erlang_completion_cache')
    let g:erlang_completion_cache = 1
endif

" Modules cache used to speed up the completion.
"
" This dictionary contains completion items that represent functions exported
" from this module. Each value in this dictionary is a list of completion
" items. A completion item is itself a dictionary (see ":help complete-items"
" for the format of the completion item dictionaries).
"
" Type: module -> [complete_item]
"
" Example: {'mymod': [{'word': 'myfun(',  # text inserted during the completion
"                      'abbr': 'myfun/1', # text displayed in the popup menu
"                      'kind': 'f',       # this is a function
"                      'dup': 1},         # 'word' values might be duplicates
"                     {'word': 'myfun(',
"                      'abbr': 'myfun/2',
"                      'kind': 'f',
"                      'dup': 1} ,
"                     {'word': 'myfun_with_type_spec(',
"                      'abbr': 'myfun_with_type_spec(A, B)',
"                      'kind': 'f',
"                      'dup': 1}],
"           'myothermod': [{'word': 'myotherfun(',
"                           'abbr': 'myotherfun/2',
"                           'kind': 'f',
"                           'dup': 1}]}
let s:modules_cache = {}

" Patterns for completions
let s:erlang_local_func_beg    = '\(\<[0-9A-Za-z_-]*\|\s*\)$'
let s:erlang_external_func_beg = '\<[0-9A-Za-z_-]\+:[0-9A-Za-z_-]*$'
let s:erlang_blank_line        = '^\s*\(%.*\)\?$'

if !exists('g:erlang_completion_preview_help')
    let g:erlang_completion_preview_help = 1
end

" Return the informational line displayed at the end of the preview window.
function s:get_preview_line()
    if g:erlang_completion_preview_help == 1
        return "\n\nClose preview window: CTRL-W z in normal mode." .
             \ "\nDisable preview window: :set cot-=preview." .
             \ "\nDon't show this message: :let g:erlang_completion_preview_help = 0."
    else
        return ""
    end
endfunction

" Main function for completion.
"
" - If findstart = 1, then the function must return the column where the base
"   (the word to be completed) starts.
" - If findstart = 0, then a:base is the word to be completed, and the
"   function must return a list with the possible completions.
"
" See ":help complete-functions" for the exact specification of this function.
function erlang_complete#Complete(findstart, base)
    let lnum = line('.')
    let column = col('.')
    let line = strpart(getline('.'), 0, column - 1)

    " 1) If the char to the left of us is not the part of a function call, the
    " user probably wants to type a local function, a module or a BIF
    if line[column - 2] !~ '[0-9A-Za-z:_-]'
        if a:findstart
            return column
        else
            return s:ErlangFindLocalFunc(a:base)
        endif
    endif

    " 2) Function in external module
    if line =~ s:erlang_external_func_beg
        let delimiter = match(line, ':[0-9A-Za-z_-]*$') + 1
        if a:findstart
            return delimiter
        else
            let module = matchstr(line[:-2], '\<\k*\>$')
            return s:ErlangFindExternalFunc(module, a:base)
        endif
    endif

    " 3) Local function
    if line =~ s:erlang_local_func_beg
        let funcstart = match(line, ':\@<![0-9A-Za-z_-]*$')
        if a:findstart
            return funcstart
        else
            return s:ErlangFindLocalFunc(a:base)
        endif
    endif

    " 4) Unhandled situation
    if a:findstart
        return -1
    else
        return []
    endif
endfunction

" Find the next non-blank line
function s:ErlangFindNextNonBlank(lnum)
    let lnum = nextnonblank(a:lnum + 1)
    let line = getline(lnum)

    while line =~ s:erlang_blank_line && 0 != lnum
        let lnum = nextnonblank(lnum + 1)
        let line = getline(lnum)
    endwhile

    return lnum
endfunction

" Find external function names
"
" Parameters:
"
" - module: the module being edited.
" - base: the word to be completed.
function s:ErlangFindExternalFunc(module, base)

    " If the module is cached, load its functions
    if has_key(s:modules_cache, a:module)
        let compl_words = []
        let module_cache = get(s:modules_cache, a:module)
        for compl_item in module_cache
            " If a:base is a prefix of compl_item, add compl_item to the list
            " of possible completions
            if match(compl_item.word, a:base) == 0
                call add(compl_words, compl_item)
            endif
        endfor

        return compl_words
    endif

    let compl_words = []
    let functions = system('escript ' . fnameescape(s:erlang_complete_file) .
                          \' list-functions ' . fnameescape(a:module) .
                          \' --basedir ' .  fnameescape(expand('%:p:h')))
    " We iterate on functions in the given module that start with `base` and
    " add them to the completion list.
    for function_spec in split(functions, '\n')
        " - When the function doesn't have a type spec, its function_spec
        "   will be e.g. "f/2".
        " - When the function has a type spec, its function_spec will be e.g.
        "   "f(A, B)".
        if match(function_spec, a:base) == 0
            let function_name = matchstr(function_spec, a:base . '\w*')
            " See the documentation of the completion items in `:help
            " complete-items`.
            let compl_item = {'word': function_name . '(',
                             \'abbr': function_spec,
                             \'info': function_spec . s:get_preview_line(),
                             \'kind': 'f',
                             \'dup': 1}
            call add(compl_words, compl_item)

            " Populate the cache only when iterating over all the
            " module functions (i.e. no prefix for the completion)
            if g:erlang_completion_cache && a:base == ''
                if !has_key(s:modules_cache, a:module)
                    let s:modules_cache[a:module] = [compl_item]
                else
                    let module_cache = get(s:modules_cache, a:module)
                    let s:modules_cache[a:module] =
                      \ add(module_cache, compl_item)
                endif
            endif

            " The user entered some text, so stop the completion
            if complete_check()
                " The module couldn't be entirely cached
                if has_key(s:modules_cache, a:module)
                    call remove(s:modules_cache, a:module)
                endif
                break
            endif
        endif
    endfor

    return compl_words
endfunction

" Find local function names, BIFs and modules.
"
" This function is called when a word (without a ":") needs to be completed,
" such as base = "lis". This could be a local function call (e.g.
" "list_things"), a BIF ("list_to_binary") or a module ("lists").
"
" Parameter:
"
" - base: the word to be completed.
function s:ErlangFindLocalFunc(base)
    " Begin at line 1
    let lnum = s:ErlangFindNextNonBlank(1)

    if "" == a:base
        let base = '^\w' " Used to match against word symbol
    else
        let base = '^' . a:base
    endif

    " Find local functions that start with `base`.
    let compl_words = []
    while 0 != lnum && !complete_check()
        let line = getline(lnum)
        let function_name = matchstr(line, base . '[0-9A-Za-z_-]\+(\@=')
        if function_name != ""
            " We found such a local function.
            call add(compl_words, {'word': function_name . '(',
                                  \'abbr': function_name,
                                  \'info': function_name . s:get_preview_line(),
                                  \'kind': 'f'})
        endif
        let lnum = s:ErlangFindNextNonBlank(lnum)
    endwhile

    if "" == a:base
        let base = ''
    else
        let base = '^' . a:base
    endif

    " Find BIFs that start with `base`.
    for bif_line in s:auto_imported_bifs
        if bif_line =~# base
            let bif_name = substitute(bif_line, '(.*', '(', '')
            call add(compl_words, {'word': bif_name,
                                  \'abbr': bif_line,
                                  \'info': bif_line . s:get_preview_line(),
                                  \'kind': 'f'})
        endif
    endfor

    " Find modules that start with `base`.
    let modules = system('escript ' . fnameescape(s:erlang_complete_file) .
                        \' list-modules ' .
                        \' --basedir ' . fnameescape(expand('%:p:h')))
    for module in split(modules, '\n')
        if module =~# base
            call add(compl_words, {'word': module . ':',
                                  \'abbr': module,
                                  \'info': module . s:get_preview_line(),
                                  \'kind': 'm'})
        endif
    endfor

    return compl_words
endfunction

function erlang_complete#ClearAllCache()
    let s:modules_cache = {}
endfunction

function erlang_complete#ClearOneCache(mod)
    if has_key(s:modules_cache, a:mod)
        call remove(s:modules_cache, a:mod)
    endif
endfunc

" This list comes from http://www.erlang.org/doc/man/erlang.html (minor
" modifications have been performed).
let s:auto_imported_bifs = [
    \ 'abs(Number) -> number()',
    \ 'apply(Fun, Args) -> term()',
    \ 'apply(Module, Function, Args) -> term()',
    \ 'atom_to_binary(Atom, Encoding) -> binary()',
    \ 'atom_to_list(Atom) -> string()',
    \ 'binary_part(Subject, PosLen) -> binary()',
    \ 'binary_part(Subject, Start, Length) -> binary()',
    \ 'binary_to_atom(Binary, Encoding) -> atom()',
    \ 'binary_to_existing_atom(Binary, Encoding) -> atom()',
    \ 'binary_to_float(Binary) -> float()',
    \ 'binary_to_integer(Binary) -> integer()',
    \ 'binary_to_integer(Binary, Base) -> integer()',
    \ 'binary_to_list(Binary) -> [byte()]',
    \ 'binary_to_list(Binary, Start, Stop) -> [byte()]',
    \ 'bitstring_to_list(Bitstring) -> [byte() | bitstring()]',
    \ 'binary_to_term(Binary) -> term()',
    \ 'binary_to_term(Binary, Opts) -> term()',
    \ 'bit_size(Bitstring) -> integer() >= 0',
    \ 'byte_size(Bitstring) -> integer() >= 0',
    \ 'check_old_code(Module) -> boolean()',
    \ 'check_process_code(Pid, Module) -> boolean()',
    \ 'date() -> Date',
    \ 'delete_module(Module) -> true | undefined',
    \ 'demonitor(MonitorRef) -> true',
    \ 'demonitor(MonitorRef, OptionList) -> boolean()',
    \ 'disconnect_node(Node) -> boolean() | ignored',
    \ 'element(N, Tuple) -> term()',
    \ 'erase() -> [{Key, Val}]',
    \ 'erase(Key) -> Val | undefined',
    \ 'error(Reason) -> no_return()',
    \ 'error(Reason, Args) -> no_return()',
    \ 'exit(Reason) -> no_return()',
    \ 'exit(Pid, Reason) -> true',
    \ 'float(Number) -> float()',
    \ 'float_to_binary(Float) -> binary()',
    \ 'float_to_binary(Float, Options) -> binary()',
    \ 'float_to_list(Float) -> string()',
    \ 'float_to_list(Float, Options) -> string()',
    \ 'garbage_collect() -> true',
    \ 'garbage_collect(Pid) -> boolean()',
    \ 'get() -> [{Key, Val}]',
    \ 'get(Key) -> Val | undefined',
    \ 'get_keys(Val) -> [Key]',
    \ 'group_leader() -> pid()',
    \ 'group_leader(GroupLeader, Pid) -> true',
    \ 'halt() -> no_return()',
    \ 'halt(Status) -> no_return()',
    \ 'halt(Status, Options) -> no_return()',
    \ 'hd(List) -> term()',
    \ 'integer_to_binary(Integer) -> binary()',
    \ 'integer_to_binary(Integer, Base) -> binary()',
    \ 'integer_to_list(Integer) -> string()',
    \ 'integer_to_list(Integer, Base) -> string()',
    \ 'iolist_to_binary(IoListOrBinary) -> binary()',
    \ 'iolist_size(Item) -> integer() >= 0',
    \ 'is_alive() -> boolean()',
    \ 'is_atom(Term) -> boolean()',
    \ 'is_binary(Term) -> boolean()',
    \ 'is_bitstring(Term) -> boolean()',
    \ 'is_boolean(Term) -> boolean()',
    \ 'is_float(Term) -> boolean()',
    \ 'is_function(Term) -> boolean()',
    \ 'is_function(Term, Arity) -> boolean()',
    \ 'is_integer(Term) -> boolean()',
    \ 'is_list(Term) -> boolean()',
    \ 'is_number(Term) -> boolean()',
    \ 'is_pid(Term) -> boolean()',
    \ 'is_port(Term) -> boolean()',
    \ 'is_process_alive(Pid) -> boolean()',
    \ 'is_record(Term, RecordTag) -> boolean()',
    \ 'is_record(Term, RecordTag, Size) -> boolean()',
    \ 'is_reference(Term) -> boolean()',
    \ 'is_tuple(Term) -> boolean()',
    \ 'length(List) -> integer() >= 0',
    \ 'link(PidOrPort) -> true',
    \ 'list_to_atom(String) -> atom()',
    \ 'list_to_binary(IoList) -> binary()',
    \ 'list_to_bitstring(BitstringList) -> bitstring()',
    \ 'list_to_existing_atom(String) -> atom()',
    \ 'list_to_float(String) -> float()',
    \ 'list_to_integer(String) -> integer()',
    \ 'list_to_integer(String, Base) -> integer()',
    \ 'list_to_pid(String) -> pid()',
    \ 'list_to_tuple(List) -> tuple()',
    \ 'load_module(Module, Binary) -> {module, Module} | {error, Reason}',
    \ 'make_ref() -> reference()',
    \ 'max(Term1, Term2) -> Maximum',
    \ 'min(Term1, Term2) -> Minimum',
    \ 'module_loaded(Module) -> boolean()',
    \ 'monitor(Type, Item) -> MonitorRef',
    \ 'monitor_node(Node, Flag) -> true',
    \ 'node() -> Node',
    \ 'node(Arg) -> Node',
    \ 'nodes() -> Nodes',
    \ 'nodes(Arg) -> Nodes',
    \ 'now() -> Timestamp',
    \ 'open_port(PortName, PortSettings) -> port()',
    \ 'pid_to_list(Pid) -> string()',
    \ 'port_close(Port) -> true',
    \ 'port_command(Port, Data) -> true',
    \ 'port_command(Port, Data, OptionList) -> boolean()',
    \ 'port_connect(Port, Pid) -> true',
    \ 'port_control(Port, Operation, Data) -> iodata() | binary()',
    \ 'pre_loaded() -> [module()]',
    \ 'process_flag(Flag :: trap_exit, Boolean) -> OldBoolean',
    \ 'process_flag(Pid, Flag, Value) -> OldValue',
    \ 'process_info(Pid) -> Info',
    \ 'process_info(Pid, Item) -> InfoTuple | [] | undefined',
    \ 'process_info(Pid, ItemList) -> InfoTupleList | [] | undefined',
    \ 'processes() -> [pid()]',
    \ 'purge_module(Module) -> true',
    \ 'put(Key, Val) -> term()',
    \ 'register(RegName, PidOrPort) -> true',
    \ 'registered() -> [RegName]',
    \ 'round(Number) -> integer()',
    \ 'self() -> pid()',
    \ 'setelement(Index, Tuple1, Value) -> Tuple2',
    \ 'size(Item) -> integer() >= 0',
    \ 'spawn(Fun) -> pid()',
    \ 'spawn(Node, Fun) -> pid()',
    \ 'spawn(Module, Function, Args) -> pid()',
    \ 'spawn(Node, Module, Function, Args) -> pid()',
    \ 'spawn_link(Fun) -> pid()',
    \ 'spawn_link(Node, Fun) -> pid()',
    \ 'spawn_link(Module, Function, Args) -> pid()',
    \ 'spawn_link(Node, Module, Function, Args) -> pid()',
    \ 'spawn_monitor(Fun) -> {pid(), reference()}',
    \ 'spawn_monitor(Module, Function, Args) -> {pid(), reference()}',
    \ 'spawn_opt(Fun, Options) -> pid() | {pid(), reference()}',
    \ 'spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()}',
    \ 'spawn_opt(Module, Function, Args, Options) -> pid() | {pid(), reference()}',
    \ 'spawn_opt(Node, Module, Function, Args, Options) -> pid() | {pid(), reference()}',
    \ 'split_binary(Bin, Pos) -> {binary(), binary()}',
    \ 'statistics(Item :: context_switches) -> {ContextSwitches, 0}',
    \ 'term_to_binary(Term) -> ext_binary()',
    \ 'term_to_binary(Term, Options) -> ext_binary()',
    \ 'throw(Any) -> no_return()',
    \ 'time() -> Time',
    \ 'tl(List) -> term()',
    \ 'trunc(Number) -> integer()',
    \ 'tuple_size(Tuple) -> integer() >= 0',
    \ 'tuple_to_list(Tuple) -> [term()]',
    \ 'unlink(Id) -> true',
    \ 'unregister(RegName) -> true',
    \ 'whereis(RegName) -> pid() | port() | undefined']
