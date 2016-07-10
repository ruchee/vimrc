%%% Vim syntax highlight example file
%%%
%%% Language:   Erlang (http://www.erlang.org)
%%% Author:     Csaba Hoch <csaba.hoch@gmail.com>
%%% License:    Vim license (http://vimdoc.sourceforge.net/htmldoc/uganda.html)
%%% URL:        https://github.com/hcs42/vim-erlang

%%% The organization of this file follows the Erlang Reference Manual:
%%%
%%% http://erlang.org/doc/reference_manual/users_guide.html

%%% ===========================================================================
%%% 1 Introduction
%%% ===========================================================================

%%% ===========================================================================
%%% 1.5 Reserved Words
%%% ===========================================================================

reserved_words() ->
    after and andalso band begin bnot bor bsl bsr bxor case catch cond div end
    fun if let not of or orelse receive rem try when xor.

%%% ===========================================================================
%%% 1.6 Character Set
%%% ===========================================================================

string_characters() ->
    " ¿ ÀÖ × ØÞ ßö ÷ øÿ".

variable_characters() ->
    ÀÖØÞßöøÿ.

%%% ===========================================================================
%%% 2 Data Types
%%% ===========================================================================

%%% ===========================================================================
%%% 2.2 Number
%%% ===========================================================================

number_examples() ->
    42,
    $A,
    $\n,
    2#101,
    2#102, % bad
    8#17,
    8#19, % bad
    16#ff,
    16#fg, % bad
    17#fg,
    2.3,
    2.3e3,
    2e3, % bad
    2.3e-3,
    2.3e1.2. % bad

number_with_dollar() ->
    $\b,
    $\d,
    $\e,
    $\f,
    $\n,
    $\r,
    $\s,
    $\t,
    $\v,
    $\a,
    $\c,
    $\1,
    $\11,
    $\111,
    $\1111, % The last digit is not part of the modifier
    $\88, % "8" is not an octal digit
    $\x1, % Incorrect
    $\x11,
    $\xaF,
    $\x111, % The last digit is not part of the modifier
    $\x{1},
    $\x{abcDEF},
    $\^a, $\^z,
    $\^A, $\^Z,
    $\',
    $\",
    $\\.

%%% ===========================================================================
%%% 2.3 Atom
%%% ===========================================================================

atom_examples() ->
    '',
    hello,
    phone_number,
    hello@you,
    'Monday',
    'phone number',
    case@case.

%%% ===========================================================================
%%% 2.9 Tuple
%%% 2.10 List
%%% ===========================================================================

tuple_list() ->
    {}, {A}, {A, B}
    [], [A], [A, B], [A|B].

%%% ===========================================================================
%%% 2.11 String
%%% ===========================================================================

multiline_string() ->
    "",
    "plain",
    "plain string",
    "multi
     line",
    "
".

%%% ===========================================================================
%%% 2.13 Boolean
%%% ===========================================================================

bools() ->
    true, false.

true() ->
    true.

false() ->
    false.

%%% ===========================================================================
%%% 2.14 Escape sequences
%%% ===========================================================================

escape_sequences() ->
    "\b",
    "\d",
    "\e",
    "\f",
    "\n",
    "\r",
    "\s",
    "\t",
    "\v",
    "\a", % no such modifier
    "\c", % no such modifier
    "\1",
    "\11",
    "\111",
    "\1111", % The last digit is not part of the modifier
    "\88", % "8" is not an octal digit
    "\x1", % Incorrect
    "\x11",
    "\xaF",
    "\x111", % The last digit is not part of the modifier
    "\x{1}",
    "\x{abcDEF}",
    "\^a \^z ",
    "\^A \^Z",
    "\'",
    "\"",
    "\\".

escape_sequences() ->
    '\b',
    '\d',
    '\e',
    '\f',
    '\n',
    '\r',
    '\s',
    '\t',
    '\v',
    '\1',
    '\11',
    '\111',
    '\x11',
    '\x{1}',
    '\x{abcDEF}',
    '\^a \^z ',
    '\^A \^Z',
    '\'',
    '\"',
    '\\'.

%%% ===========================================================================
%%% 4 Modules
%%% ===========================================================================

%%% ===========================================================================
%%% 4.1 Module Syntax
%%% ===========================================================================

-module(Module).
-export(Functions).
-import(Module,Functions).
-compile(Options).
-vsn(Vsn).
-on_load(Function).
-behaviour(Behaviour).
-behavior(Behaviour).
-file(File, Line).
-other(File, Line).

macros() ->
    ?FILE, ?LINE.

-export_type([my_struct_type/0, orddict/2]).

-export(Functions). -export(Functions).

-
export(Functions).

  - % comment
export(Functions).

%%% ===========================================================================
%%% 4.3 Comments
%%% ===========================================================================

% Comment
%% Comment
%%% Comment
%%%% Comment

%%% ===========================================================================
%%% Functions
%%% ===========================================================================

%%% ===========================================================================
%%% 5.1  Function Declaration Syntax
%%% ===========================================================================

f({A}, [H|T]) when H > 0, T == 0;
                   H < 0 ->
    ok;
f(_X, _) ->
    ok.

%%% ===========================================================================
%%% 6 Types and Function Specifications
%%% ===========================================================================

% TODO

%%% ===========================================================================
%%% 6.3  Type declarations of user-defined types
%%% ===========================================================================

-spec my_type() = term()
                | binary()
                | bitstring()
                | boolean()
                | byte()
                | char()
                | number()
                | list()
                | maybe_improper_list()
                | maybe_improper_list(T)
                | string()
                | nonempty_string()
                | iolist()
                | module()
                | mfa()
                | node()
                | timeout()
                | no_return()
                | non_neg_integer()
                | pos_integer()
                | neg_integer().

%%% ===========================================================================
%%% 7 Expressions
%%% ===========================================================================

%%% ===========================================================================
%%% 7.3 Variables
%%% ===========================================================================

variables() ->
    X,
    Name1,
    PhoneNumber,
    Phone_number,
    _,
    _Height,
    Var@case. % just a variable

%%% ===========================================================================
%%% 7.6 Function calls
%%% ===========================================================================

func_calls() ->

    func(),
    func (),

    func
    (),

    func % comment
    (),

    a:b, % bad

    mod:func(),
    my_mod:my_func(),
    mod_:func_(),

    1mod:func(), % bad
    @mod:func(), % bad
    m@d:f@nc(), % good
    mod : func(), % good

    mod
    :
    func(),

    mod % comment
    : % comment
    func().

function_call_examples() ->
    ''(),
    hello(),
    phone_number(),
    hello@you(),
    'Monday'(),
    'phone number'(),
    case@case().

func_calls() ->

    Func(),
    Func (),

    Func
    (),

    Func % comment
    (),

    a:b, % bad

    mod:Func(),
    my_mod:my_func(),
    mod_:func_(),

    1mod:Func(), % bad
    @mod:Func(), % bad
    m@d:f@nc(), % good
    mod : Func(), % good

    mod
    :
    Func(),

    mod % comment
    : % comment
    Func().

%%% ===========================================================================
%%% 7.7 If
%%% ===========================================================================

if_example() ->
    if
        A -> A;
        true -> ok
    end.

%%% ===========================================================================
%%% 7.8 Case
%%% ===========================================================================

case_example() ->
    case X of
        A when A > 0 ->
            ok;
        A when A < 0 ->
            ok
    end.

%%% ===========================================================================
%%% 7.9 Send
%%% ===========================================================================

send_example() ->
    a ! b.

%%% ===========================================================================
%%% 7.10 Receive
%%% ===========================================================================

receive_example() ->
    receive
        A -> A;
        B -> B
    after
        T -> T
    end.

%%% ===========================================================================
%%% 7.11 Term Comparisons
%%% ===========================================================================

term_comparisons() ->
    A == A,
    A /= A,
    A =< A,
    A < A,
    A >= A,
    A > A,
    A =:= A,
    A =/= A,
    ok.

%%% ===========================================================================
%%% 7.12 Arithmetic Expressions
%%% ===========================================================================

unary_operators() ->
    + A,
    - A,
    bnot A.

binary_operators() ->
    A + A,
    A - A,
    A * A,
    A / A,
    A div A,
    A rem A,
    A band A,
    A bor A,
    A bxor A,
    A bsl A,
    A bsr A.

%%% ===========================================================================
%%% 7.13 Boolean Expressions
%%% ===========================================================================

unary_boolean() ->
    not A,
    ok.

binary_boolean() ->
    A and A,
    A or A,
    A xor A,
    ok.

%%% ===========================================================================
%%% 7.14 Short-Circuit Expressions
%%% ===========================================================================

short_circuit() ->
    A andalso A,
    A orelse A.

%%% ===========================================================================
%%% 7.15 List Operations
%%% ===========================================================================

list_operations() ->
    A ++ A,
    A -- A.

%%% ===========================================================================
%%% 7.16 Bit Syntax Expressions
%%% ===========================================================================

bit_syntax() ->
    <<>>,

    <<
    >>,

    <<A>>,
    <<A:1>>,
    << A : 1 >>,

    <<A/bits>>,
    <<A:1/bits>>,
    << A : 1 / bits >>,

    <<A/integer>>,
    <<A:1/integer>>,
    << A : 1 / integer >>.

bit_syntax_types() ->
     <<A/integer,
       A/float,
       A/binary,
       A/bytes,
       A/bitstring,
       A/bits,
       A/utf8,
       A/utf16,
       A/utf32>>,

     <<A/integer-signed>>,
     <<A/integer-unsigned>>,
     <<A/signed-integer>>,
     <<A/unsigned-integer>>,

     <<A/integer-big>>,
     <<A/integer-little>>,
     <<A/integer-native>>,

     <<A:1/integer-native-unit:1>>,

     <<A
       :
       1
       /
       integer
       -
       native
       -
       unit
       :
       1
     >>,

     <<A % comment
       : % comment
       1 % comment
       / % comment
       integer % comment
       - % comment
       native % comment
       - % comment
       unit % comment
       : % comment
       1 % comment
     >>, % comment

     <<$a/utf8,$b/utf8,$c/utf8>>.

just_atoms() ->
    integer,
    float,
    binary,
    bytes,
    bitstring,
    bits,
    utf8,
    utf16,
    utf32,
    signed,
    unsigned,
    big,
    little,
    native,
    unit,
    utf8.

%%% ===========================================================================
%%% 7.17 Fun Expressions
%%% ===========================================================================

f() ->
    fun func/0,
    fun mod:func/0,
    fun (A) when A > 0 -> A;
        (B) when B > 0 -> B
    end.

f() ->
    fun Func/0,
    fun mod:Func/0,
    fun (A) when A > 0 -> A;
        (B) when B > 0 -> B
    end.

%%% ===========================================================================
%%% 7.18  Catch and Throw
%%% ===========================================================================

catch_example() ->
    catch 1 + 2.

throw_example() ->
    throw(hello).

%%% ===========================================================================
%%% 7.19 Try
%%% ===========================================================================

try_example() ->
    try
        f()
    of
        A -> B
    catch
        throw:E -> E;
        exit:E -> E;
        error:E -> E
    after
        Timeout -> timeout
    end.

try_example() ->
    try
        f()
    catch
        _:_ -> error;
    end.

%%% ===========================================================================
%%% 7.24 Guard Sequences
%%% ===========================================================================

test_type_bifs() when is_atom(A);
                      is_binary(A);
                      is_bitstring(A);
                      is_boolean(A);
                      is_float(A);
                      is_function(A);
                      is_function(A, B);
                      is_integer(A);
                      is_list(A);
                      is_number(A);
                      is_pid(A);
                      is_port(A);
                      is_record(A, B);
                      is_record(A, B, C);
                      is_reference(A);
                      is_tuple(A) ->
    ok.

old_guards() when abs(Number);
                  bit_size(Bitstring);
                  byte_size(Bitstring);
                  element(N, Tuple);
                  float(Term);
                  hd(List);
                  length(List);
                  node();
                  node(Pid|Ref|Port);
                  round(Number);
                  self();
                  size(Tuple|Bitstring);
                  tl(List);
                  trunc(Number);
                  tuple_size(Tuple) ->
    ok.

not_guards() when myfunc(Number) ->
    ok.

%%% ===========================================================================
%%% 8 The Preprocessor
%%% ===========================================================================

%%% ===========================================================================
%%% 8.1 File Inclusion
%%% ===========================================================================

-include("my_records.hrl").
-include("incdir/my_records.hrl").
-include("/home/user/proj/my_records.hrl").
-include("$PROJ_ROOT/my_records.hrl").
-include_lib("kernel/include/file.hrl").

%%% ===========================================================================
%%% 8.2 Defining and Using Macros
%%% ===========================================================================

-define(TIMEOUT, 200).
-define(timeout, 200).
-define(_Timeout, 200).
-define(_timeout, 200).
-define(_, 200).

call(Request) ->
    server:call(refserver, Request, ?TIMEOUT).

-define(MACRO1(X, Y), {a, X, b, Y}).

bar(X) ->
    ?MACRO1,
    ??MACRO1,
    ?MACRO1(a, b),
    ?MACRO1(X, 123).
    ??MACRO1(X, 123).

%%% ===========================================================================
%%% 8.3 Predefined Macros
%%% ===========================================================================

predefined_macros() ->
    ?MODULE,
    ?MODULE_STRING,
    ?FILE,
    ?LINE,
    ?MACHINE.

%%% ===========================================================================
%%% 8.5 Flow Control in Macros
%%% ===========================================================================

-undef(Macro).
-ifdef(Macro).
-ifndef(Macro).
-else.
-endif.

%%% ===========================================================================
%%% 8.6 Stringifying Macro Arguments
%%% ===========================================================================

-define(TESTCALL(Call), io:format("Call ~s: ~w~n", [??Call, Call])).

f() ->
    ?TESTCALL(myfunction(1,2)),
    ?TESTCALL(you:function(2,1)).

%%% ===========================================================================
%%% 9 Records
%%% ===========================================================================

%%% ===========================================================================
%%% 9.1 Defining Records
%%% ===========================================================================

-record(person, {name,
                 phone=0,
                 address}).

access_fields(Name, Tab) ->
    ets:match_object(Tab, #person{name=Name, _='_'}),
    lists:keysearch(Name, #person.name, List).

update_fields() ->
    Person#person{field1=Expr1, field1=ExprK}.

%%% ===========================================================================
%%% 11 Processes
%%% ===========================================================================

bifs() ->
    register(Name, Pid),
    registered(),
    whereis(Name),
    spawn(),
    spawn_link(),
    spawn_opt(),
    link(),
    unlink(),
    process_flag(trap_exit, true),
    {'DOWN', Ref, process, Pid2, Reason}.

process_dictionary_bifs() ->
    put(Key, Value),
    get(Key),
    get(),
    get_keys(Value),
    erase(Key),
    erase().

%%% ===========================================================================
%%% 12 Distributed Erlang
%%% ===========================================================================

%%% ===========================================================================
%%% 12.8 Distribution BIFs
%%% ===========================================================================

distribution_bifs() ->
    disconnect_node(Node),
    erlang:get_cookie(),
    erlang:set_cookie(Node, Cookie),
    is_alive(),
    monitor_node(Node, true),
    node(),
    node(Arg),
    nodes(),
    nodes(Arg).

just_atoms() ->
    disconnect_node,
    erlang:get_cookie,
    erlang:set_cookie,
    is_alive,
    monitor_node,
    node,
    nodes.

%%% ===========================================================================
%%% Escript
%%% ===========================================================================

#!/usr/bin/env escript
#!/usr/bin/escript

%%% ===========================================================================
%%% io:format
%%% ===========================================================================

%%% http://erlang.org/doc/man/io.html#format-1
io_format_control_sequences() ->
    '~a', % no highlight
    "~cxx~10.5cxx~-10.5cxx~.5cxx~10.cxx~tcxx~1tc", % highlight
    "~-.5cxx~t1c", % no highlight
    "~fxx~22fxx~-22.11fxx~.11fxx~.*fxx~.*.1f", % highlight
    "~2n ~1~ ~1i", % no highlight
    "|~10s|~n", 

    io:fwrite("|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]),
    io:fwrite("~tc~n",[1024]),
    io:fwrite("~c~n",[1024]),
    io:fwrite("|~10w|~n", [{hey, hey, hey}]),
    io:fwrite("|~10s|~n", [io_lib:write({hey, hey, hey})]),
    io:fwrite("|~-10.8s|~n", [io_lib:write({hey, hey, hey})]),
    io:fwrite("~ts~n",[[1024]]),
    io:fwrite("~s~n",[[1024]]),
    io:fwrite("~w~n", [T]),
    io:fwrite("~62p~n", [T]),
    io:fwrite("Here T = ~62p~n", [T]),
    io:fwrite("~15p~n", [S]),
    io:fwrite("~15lp~n", [S],
    io:fwrite("~p~n",[[1024]]),
    io:fwrite("~tp~n",[[1024]]),
    io:fwrite("~tp~n", [<<128,128>>]),
    io:fwrite("~tp~n", [<<208,128>>]),
    io:fwrite("~W~n", [T,9]),
    io:fwrite("~62P~n", [T,9]),
    io:fwrite("~.16B~n", [31]),
    io:fwrite("~.2B~n", [-19]),
    io:fwrite("~.36B~n", [5*36+35],
    io:fwrite("~X~n", [31,"10#"]),
    io:fwrite("~.16X~n", [-31,"0x"]),
    io:fwrite("~.10#~n", [31]),
    io:fwrite("~.16#~n", [-31]).

%%% ===========================================================================
%%% Maps
%%% ===========================================================================

%%% http://www.erlang.org/eeps/eep-0043.html
maps() ->
    M0 = #{},                   % empty map
    M1 = #{ a => <<"hello">> }, % single association with literals
    M2 = #{ 1 => 2, b => b },   % multiple associations with literals
    M3 = #{ A => B },           % single association with variables
    M4 = #{ {A, B} => f() },    % compound key associated to an evaluated expression

    M0 = #{},
    M1 = M0#{ a => 0 },
    M2 = M1#{ a => 1, b => 2 },
    M3 = M2#{ "function" => fun() -> f() end },
    M4 = M3#{ a := 2, b := 3 },  % 'a' and 'b' was added in `M1` and `M2`.

    M1 = #{ a => 1, c => 3 },
    3 = M1#{ c },

    M = #{ a => {1,2}},
    #{ a := {1,B}} = M,

    M1 = #{ E0 => E1 || K := V <- M0  }.  % map comprehension

-spec func(Opt, M) -> #{ 'status' => S, 'c' => integer() } when
      Opt :: 'inc' | 'dec',
        M :: #{ 'status' => S, 'c' => integer() },
        S :: 'update' | 'keep'.

func(inc, #{ status := update, c := C} = M) -> M#{ c := C + 1};
func(dec, #{ status := update, c := C} = M) -> M#{ c := C - 1};
func(_,   #{ status := keep } = M)          -> M.

-type map1() :: #{ binary() => integer() }.
-type map2() :: #{ <<"list1">> | <<"list2">> => [numbers()] }.
