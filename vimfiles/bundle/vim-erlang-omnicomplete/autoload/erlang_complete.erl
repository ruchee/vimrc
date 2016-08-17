#!/usr/bin/env escript

-include_lib("xmerl/include/xmerl.hrl").

%%------------------------------------------------------------------------------
%% @doc Print completions.
%% @end
%%------------------------------------------------------------------------------
-spec main([string()]) -> no_return().
main([]) ->
    io:format("Usage: see --help.~n"),
    halt(2);
main(Args) ->
    PositionalParams = parse_args(Args),
    case PositionalParams of
        ["list-modules"] ->
            run(list_modules);
        ["list-functions", ModuleString] ->
            run({list_functions, list_to_atom(ModuleString)});
        _ ->
            log_error("Erroneous parameters: ~p", [PositionalParams]),
            halt(2)
    end.

%%------------------------------------------------------------------------------
%% @doc Parse the argument list.
%%
%% Put the options into the process dictionary and return the list of files.
%% @end
%%------------------------------------------------------------------------------
-spec parse_args(Args :: string()) -> [PositionalParam :: string()].
parse_args(Args) ->
    lists:reverse(parse_args(Args, [])).

-spec parse_args(Args :: string(), Acc :: [PositionalParam :: string()]) ->
          [PositionalParam :: string()].
parse_args([], Acc) ->
    Acc;
parse_args([Help|_], _Acc) when Help == "-h";
                                Help == "--help" ->
    print_help(),
    halt(0);
parse_args([Verbose|OtherArgs], Acc) when Verbose == "-v";
                                          Verbose == "--verbose" ->
    put(verbose, true),
    log("Verbose mode on.~n"),
    parse_args(OtherArgs, Acc);
parse_args(["--basedir", BaseDir|OtherArgs], Acc) ->
    put(basedir, BaseDir),
    parse_args(OtherArgs, Acc);
parse_args(["--basedir"], _Acc) ->
    log_error("Argument needed after '--basedir'.~n", []),
    halt(2);
parse_args(["--"|PosPars], Acc) ->
    PosPars ++ Acc;
parse_args([[$-|_] = Arg|_], _Acc) ->
    log_error("Unknown option: ~s~n", [Arg]),
    halt(2);
parse_args([PosPar|OtherArgs], Acc) ->
    parse_args(OtherArgs, [PosPar|Acc]).

%%------------------------------------------------------------------------------
%% @doc Print the script's help text and exit.
%% @end
%%------------------------------------------------------------------------------
-spec print_help() -> ok.
print_help() ->
    Text =
"Usage: erlang_complete.erl [options] [--] list-modules
        erlang_complete.erl [options] [--] list-functions MODULE

Description:
  erlang_complete lists all modules or all functions of a module.

Options:
  --            Process all remaining parameters as filenames.
  -h, --help    Print help.
  -v, --verbose Verbose output.
  --basedir DIR
                When searching for rebar.config files, the search will start
                from this directory.
",
    io:format(Text).

%%------------------------------------------------------------------------------
%% @doc Log the given entry if we are in verbose mode.
%% @end
%%------------------------------------------------------------------------------
-spec log(io:format()) -> ok.
log(Format) ->
    log(Format, []).

-spec log(io:format(), [term()]) -> ok.
log(Format, Data) ->
    case get(verbose) of
        true ->
            io:format(Format, Data);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Log the given error.
%% @end
%%------------------------------------------------------------------------------
-spec log_error(io:format(), [term()]) -> ok.
log_error(Format, Data) ->
    io:format(standard_error, Format, Data).


%%------------------------------------------------------------------------------
%% @doc Complete the given query.
%%
%% This function sets the code paths too.
%% @end
%%------------------------------------------------------------------------------
-spec run(list_modules) -> ok;
         ({list_functions, Module :: atom()}) -> ok.
run(Target) ->

    AbsDir =
        case get(basedir) of
            undefined ->
                {ok, Cwd} = file:get_cwd(),
                Cwd;
            BaseDir ->
                filename:absname(BaseDir)
        end,

    process_rebar_configs(AbsDir),
    code:add_patha(absname(AbsDir, "ebin")),
    code:add_patha(absname(filename:dirname(AbsDir), "ebin")),
    run2(Target).


%%------------------------------------------------------------------------------
%% @doc Complete the given query.
%%
%% This function assumes that the code paths are all set.
%% @end
%%------------------------------------------------------------------------------
-spec run2(list_modules) -> ok;
          ({list_functions, Module :: atom()}) -> ok.
run2(list_modules) ->
    Modules = [filename:basename(File, ".beam")
               || Dir <- code:get_path(),
                  File <- filelib:wildcard(filename:join(Dir, "*.beam"))],
    [io:format("~s\n", [Mod]) || Mod <- lists:sort(Modules)],
    ok;

run2({list_functions, Mod}) ->

    Edoc =
        try
            module_edoc(Mod)
        catch
            throw:not_found ->
                [];
            error:{badmatch, _} ->
                [];
            exit:error ->
                []
        end,

    Info =
        try
            module_info2(Mod)
        catch
            error:undef ->
                []
        end,

    FunSpecs = merge_functions(Edoc, Info),
    [print_function(Fun) || Fun <- FunSpecs ],
    ok.

%%------------------------------------------------------------------------------
%% @doc Find, read and apply the rebar config files appropriate for the given
%% path.
%%
%% This function traverses the directory tree upward until it finds
%% the root directory. It finds all rebar.config files along the way and applies
%% all of them (e.g. the dependency directory in all of them is added to the
%% code path). It returns the options in the first rebar.config file (e.g. the
%% one that is the closest to the file to be compiled).
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar_configs(string()) -> ok | error.
process_rebar_configs(AbsDir) ->
    ConfigFileName = filename:join(AbsDir, "rebar.config"),

    Res =
        case filelib:is_file(ConfigFileName) of
            true ->
                case file:consult(ConfigFileName) of
                    {ok, ConfigTerms} ->
                        log("rebar.config read: ~s~n", [ConfigFileName]),
                        process_rebar_config(AbsDir, ConfigTerms);
                    {error, Reason} ->
                        log_error("rebar.config consult unsuccessful: ~p: ~p~n",
                                  [ConfigFileName, Reason]),
                        error
                end;
            false ->
                ok
        end,

    case {Res, AbsDir} of
        {error, _} ->
            error;
        {_, "/"} ->
            ok;
        {_, [_|":/"]} ->
            %% E.g. "C:/". This happens on Windows.
            ok;
        {_, _} ->
            process_rebar_configs(filename:dirname(AbsDir))
    end.

%%------------------------------------------------------------------------------
%% @doc Apply a rebar.config file.
%%
%% This function adds the directories in the rebar.config file to the code path
%% and returns and compilation options to be used when compiling the file.
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar_config(Dir :: string(), ConfigTerms :: [term()]) ->
          [Option :: term()].
process_rebar_config(Dir, Terms) ->
    % The reasons for why these directories are added is documented in the same
    % function of https://github.com/vim-erlang/vim-erlang-compiler/blob/master/compiler/erlang_check.erl.

    % ebin -> code_path
    code:add_pathsa([absname(Dir, "ebin")]),

    % deps -> code_path
    RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    code:add_pathsa(filelib:wildcard(absname(Dir, RebarDepsDir) ++ "/*/ebin")),

    % sub_dirs -> code_path
    [ code:add_pathsa(filelib:wildcard(absname(Dir, SubDir) ++ "/ebin"))
      || SubDir <- proplists:get_value(sub_dirs, Terms, []) ],

    ok.

module_edoc(Mod) ->
    File = case filename:find_src(Mod) of
        {error, _} ->
            BeamFile = atom_to_list(Mod) ++ ".beam",
            case code:where_is_file(BeamFile) of
                non_existing ->
                    throw(not_found);
                BeamPath ->
                    SrcPath = beam_to_src_path(BeamPath),
                    case filelib:is_regular(SrcPath) of
                        true ->
                            SrcPath;
                        false ->
                            throw(not_found)
                    end
            end;
        {File0, _} ->
            File0 ++ ".erl"
    end,
    {_, Doc} = edoc:get_doc(File),
    Funs = xmerl_xpath:string("/module/functions/function", Doc),
    FunSpecs = map_functions(fun(Fun) -> analyze_function(Fun) end, Funs),
    lists:keysort(1, FunSpecs).

beam_to_src_path(BeamPath) ->
    PathParts = filename:split(BeamPath),
    {Dirs, [BeamFile]} = lists:split(length(PathParts) - 1, PathParts),
    {Dirs2, [DirsLast]} = lists:split(length(Dirs) - 1, Dirs),
    case filename:pathtype(BeamPath) of
        absolute ->
            Dirs3 = case DirsLast of
                "ebin" ->
                    Dirs2 ++ ["src"];
                _ ->
                    Dirs
            end;
        relative ->
            Dirs3 = Dirs
    end,
    filename:join(Dirs3 ++ [beam_to_src_file(BeamFile)]).

beam_to_src_file(BeamFile) ->
    [ModName, "beam"] = string:tokens(BeamFile, "."),
    ModName ++ ".erl".

map_functions(_, []) ->
    [];
map_functions(F, [H | T]) ->
    try
        [F(H) | map_functions(F, T)]
    catch
        throw:no_spec ->
            map_functions(F, T)
    end.

analyze_function(Fun) ->
    Name = list_to_atom(get_attribute(Fun, "name")),
    Args0 = xmerl_xpath:string("typespec/type/fun/argtypes/type", Fun),
    Args = lists:map(fun(Arg) -> get_attribute(Arg, "name") end, Args0),
    Return = analyze_return(Fun),
    {Name, Args, Return}.

analyze_return(Fun) ->
    case xmerl_xpath:string("typespec/type/fun/type/*", Fun) of
        [Return] ->
            simplify_return(xmerl_lib:simplify_element(Return));
        [] ->
            throw(no_spec)
    end.

simplify_return({typevar, [{name, Name}], _}) ->
    Name;
simplify_return({type, _, [Type]}) ->
    simplify_return(Type);
simplify_return({abstype, _, [Type|_]}) ->
    {erlangName, Attrs, _} = Type,
    Name = proplists:get_value(name, Attrs),
    Name ++ "()";
simplify_return({record, _, [Type]}) ->
    simplify_return(Type) ++ "()";
simplify_return({nonempty_list, _, [Type]}) ->
    "[" ++ simplify_return(Type) ++ "]";
simplify_return({tuple, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "{" ++ string:join(Elems, ", ") ++ "}";
simplify_return({list, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "[" ++ string:join(Elems, ", ") ++ "]";
simplify_return({paren, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "(" ++ string:join(Elems, ", ") ++ ")";
simplify_return({union, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    string:join(Elems, " | ");
simplify_return({integer, [{value, Val}], _}) ->
    Val;
simplify_return({atom, [{value, Val}], _}) ->
    Val;
simplify_return({nil, _, _}) ->
    "[]";
simplify_return({map_field, _, [Key, Value]}) ->
    simplify_return(Key) ++ " => " ++ simplify_return(Value);
simplify_return({map, _, PairList}) ->
    Pairs = string:join([ simplify_return(Pair) || Pair <- PairList ], ", "),
    "#{" ++ Pairs ++ "}".

get_attribute(Elem, AttrName) ->
    [Attr] = xmerl_xpath:string("@" ++ AttrName, Elem),
    Attr#xmlAttribute.value.

module_info2(Mod) ->
    lists:keysort(1, Mod:module_info(exports)).

merge_functions(Edoc, Info) ->
    merge_functions(Edoc, Info, []).

merge_functions([], [], Funs) ->
    lists:reverse(Funs);
merge_functions([], Info, Funs) ->
    lists:reverse(Funs, Info);
merge_functions(Edoc, [], Funs) ->
    lists:reverse(Funs, Edoc);
merge_functions(Edoc, Info, Funs) ->
    [H1 = {K1, _, _} | T1] = Edoc,
    [H2 = {K2, _} | T2] = Info,
    if
        K1 == K2 ->
            merge_functions(T1, T2, [H1 | Funs]);
        K1 < K2 ->
            merge_functions(T1, Info, [H1 | Funs]);
        K1 > K2 ->
            merge_functions(Edoc, T2, [H2 | Funs])
    end.

print_function({Name, Arity}) ->
    io:format("~s/~B~n", [Name, Arity]);
print_function({Name, Args, Return}) ->
    io:format("~s(~s) -> ~s~n", [Name, string:join(Args, ", "), Return]).

%%------------------------------------------------------------------------------
%% @doc Return the absolute name of the file which is in the given directory.
%%
%% Example:
%%
%% - cwd = "/home/my"
%% - Dir = "projects/erlang"
%% - Filename = "rebar.config"
%% - Result: "/home/my/projects/erlang/rebar.config"
%% @end
%%------------------------------------------------------------------------------
-spec absname(Dir :: string(), Filename :: string()) -> string().
absname(Dir, Filename) ->
    filename:absname(filename:join(Dir, Filename)).
