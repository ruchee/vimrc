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
%% @doc Print the given error reason in a Vim-friendly and human-friendly way.
%% @end
%%------------------------------------------------------------------------------
-spec file_error(string(), term()) -> error.
file_error(File, Reason) ->
    Reason2 = file:format_error(Reason),
    io:format(user, "~s: ~s~n", [File, Reason2]),
    error.

%%------------------------------------------------------------------------------
%% @doc Log the given error.
%% @end
%%------------------------------------------------------------------------------
-spec log_error(io:format()) -> ok.
log_error(Format) ->
    io:format(standard_error, Format, []).

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

    {BuildSystem, Files} = guess_build_system(AbsDir),
    %% TODO Where does {result, _} and error come from?
    {opts, _} = load_build_files(BuildSystem, AbsDir, Files),
    % code:add_patha(absname(AbsDir, "ebin")),
    % code:add_patha(absname(filename:dirname(AbsDir), "ebin")),
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
%% @doc Check for some known files and try to guess what build system is being
%% used.
%% @end
%%------------------------------------------------------------------------------
-spec guess_build_system(string()) -> {atom(), string()}.
guess_build_system(Path) ->
    % The order is important, at least Makefile needs to come last since a lot
    % of projects include a Makefile along any other build system.
    BuildSystems = [
                    {rebar3, [
                              "rebar.lock"
                             ]
                    },
                    {rebar, [
                             "rebar.config",
                             "rebar.config.script"
                            ]
                    },
                    {makefile, [
                            "Makefile"
                           ]
                    }
                   ],
    guess_build_system(Path, BuildSystems).

guess_build_system(_Path, []) ->
    log("Unknown build system"),
    {unknown_build_system, []};
guess_build_system(Path, [{BuildSystem, Files}|Rest]) ->
    log("Try build system: ~p~n", [BuildSystem]),
    case find_files(Path, Files) of
        [] -> guess_build_system(Path, Rest);
        FoundFiles when is_list(FoundFiles) -> {BuildSystem, FoundFiles}
    end.

%%------------------------------------------------------------------------------
%% @doc Recursively search upward through the path tree and returns the absolute
%% path to all files matching the given filenames.
%% @end
%%------------------------------------------------------------------------------
-spec find_files(string(), [string()]) -> [string()].
find_files("/", Files) ->
    find_file("/", Files);
find_files([_|":/"] = Path, Files) ->
    %% E.g. "C:/". This happens on Windows.
    find_file(Path, Files);
find_files(Path, Files) ->
    %find_files(Path, Files, Files).
    ParentPath = filename:dirname(Path),
    find_file(Path, Files) ++
    find_files(ParentPath, Files).

%%------------------------------------------------------------------------------
%% @doc Find the first file matching one of the filenames in the given path.
%% @end
%%------------------------------------------------------------------------------
-spec find_file(string(), [string()]) -> [string()].
find_file(_Path, []) ->
    [];
find_file(Path, [File|Rest]) ->
    AbsFile = absname(Path, File),
    case filelib:is_regular(AbsFile) of
        true ->
            log("Found build file: [~p] ~p~n", [Path, AbsFile]),
            % Return file and continue searching in parent directory.
            [AbsFile];
        false ->
            find_file(Path, Rest)
    end.

%%------------------------------------------------------------------------------
%% @doc Load the settings from a given set of build system files.
%% @end
%%------------------------------------------------------------------------------
-spec load_build_files(atom(), string(), [string()]) ->
    {opts, [{atom(), term()}]} |
    {result, term()} |
    error.
load_build_files(rebar, _ProjectRoot, ConfigFiles) ->
    load_rebar_files(ConfigFiles, no_config);
load_build_files(rebar3, ProjectRoot, _ConfigFiles) ->
    % _ConfigFiles is a list containing only rebar.lock.
    ConfigNames = ["rebar.config", "rebar.config.script"],
    case find_files(ProjectRoot, ConfigNames) of
        [] ->
            log_error("rebar.config not found in ~p~n", [ProjectRoot]),
            error;
        [RebarConfigFile|_] ->
            load_rebar3_files(RebarConfigFile)
    end;
load_build_files(makefile, _ProjectRoot, ConfigFiles) ->
    load_makefiles(ConfigFiles);
load_build_files(unknown_build_system, ProjectRoot, _) ->
    {opts, [
            {i, absname(ProjectRoot, "include")},
            {i, absname(ProjectRoot, "../include")},
            {i, ProjectRoot}
           ]}.

%%------------------------------------------------------------------------------
%% @doc Load the content of each rebar file.
%%
%% Note worthy: The config returned by this function only represents the first
%% rebar file (the one closest to the file to compile). The subsequent rebar
%% files will be processed for code path only.
%% @end
%%------------------------------------------------------------------------------
-spec load_rebar_files([string()], no_config | [{atom(), term()}]) ->
    {opts, [{atom(), term()}]} | error.
load_rebar_files([], no_config) ->
    error;
load_rebar_files([], Config) ->
    {opts, Config};
load_rebar_files([ConfigFile|Rest], Config) ->
    ConfigPath = filename:dirname(ConfigFile),
    ConfigResult = case filename:extension(ConfigFile) of
                       ".script" -> file:script(ConfigFile);
                       ".config" -> file:consult(ConfigFile)
                   end,
    case ConfigResult of
        {ok, ConfigTerms} ->
            log("rebar.config read: ~s~n", [ConfigFile]),
            NewConfig = process_rebar_config(ConfigPath, ConfigTerms, Config),
            case load_rebar_files(Rest, NewConfig) of
                {opts, SubConfig} -> {opts, SubConfig};
                error -> {opts, NewConfig}
            end;
        {error, Reason} ->
            log_error("rebar.config consult failed:~n"),
            file_error(ConfigFile, Reason),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Load the content of each rebar3 file.
%%
%% Note worthy: The config returned by this function only represent the first
%% rebar file (the one closest to the file to compile).
%% @end
%%------------------------------------------------------------------------------
-spec load_rebar3_files(string()) ->
    {opts, [{atom(), term()}]} | error.
load_rebar3_files(ConfigFile) ->
    ConfigPath = filename:dirname(ConfigFile),
    ConfigResult = case filename:extension(ConfigFile) of
                       ".script" -> file:script(ConfigFile);
                       ".config" -> file:consult(ConfigFile)
                   end,
    case ConfigResult of
        {ok, ConfigTerms} ->
            log("rebar.config read: ~s~n", [ConfigFile]),
            case process_rebar3_config(ConfigPath, ConfigTerms) of
                error ->
                    error;
                Config ->
                    {opts, Config}
            end;
        {error, Reason} ->
            log_error("rebar.config consult failed:~n"),
            file_error(ConfigFile, Reason),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Apply a rebar.config file.
%%
%% This function adds the directories in the rebar.config file to the code path
%% and returns and compilation options to be used when compiling the file.
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar_config(string(), [{atom(), term()}],
                           [{atom(), term()}] | no_config) ->
    [{atom(), term()}].
process_rebar_config(Path, Terms, Config) ->

    % App layout:
    %
    % * rebar.config
    % * src/
    % * ebin/ => ebin -> code_path
    % * include/ => ".." -> include. This is needed because files in src may
    %                use `-include_lib("appname/include/f.hrl")`

    % Project layout:
    %
    % * rebar.config
    % * src/
    % * $(deps_dir)/
    %   * $(app_name)/
    %     * ebin/ => deps -> code_path
    % * apps/
    %   * $(sub_dir)/
    %     * ebin/ => sub_dirs -> code_path
    %     * include/ => apps -> include

    DepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    LibDirs = proplists:get_value(lib_dirs, Terms, []),
    SubDirs = proplists:get_value(sub_dirs, Terms, []),
    ErlOpts = proplists:get_value(erl_opts, Terms, []),

    % ebin -> code_path (when the rebar.config file is in the app directory
    code:add_pathsa([absname(Path, "ebin")]),

    % deps -> code_path
    code:add_pathsa(filelib:wildcard(absname(Path, DepsDir) ++ "/*/ebin")),

    % libs -> code_path
    code:add_pathsa(filelib:wildcard(absname(Path, LibDirs) ++ "/*/ebin")),

    % sub_dirs -> code_path
    [ code:add_pathsa(filelib:wildcard(absname(Path, SubDir) ++ "/ebin"))
      || SubDir <- SubDirs ],

    case Config of
        no_config ->
            Includes =
            [ {i, absname(Path, Dir)}
              || Dir <- ["apps", "include"] ] ++
            [ {i, absname(Path, filename:append(SubDir, "include"))}
              || SubDir <- SubDirs ],

            Opts = ErlOpts ++ Includes,
            remove_warnings_as_errors(Opts);
        _ ->
            Config
    end.

%%------------------------------------------------------------------------------
%% @doc Apply a rebar.config file.
%%
%% This function adds the directories returned by rebar3 to the code path and
%% returns and compilation options to be used when compiling the file.
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar3_config(string(), [{atom(), term()}]) ->
    [{atom(), term()}] | error.
process_rebar3_config(ConfigPath, Terms) ->
    case find_rebar3(ConfigPath) of
        not_found ->
            % Compilation would likely fail without settings the paths, so let's
            % give an explicit error instead of proceeding anyway.
            log_error("rebar3 executable not found.~n"),
            error;
        {ok, Rebar3} ->
            % load the profile used by rebar3 to print the dependency path list
            Profile = rebar3_get_profile(Terms),
            % "rebar3 path" prints all paths that belong to the project; we add
            % these to the Erlang paths.
            %
            % QUIET=1 ensures that it won't print other messages, see
            % https://github.com/erlang/rebar3/issues/1143.
            {ok, Cwd} = file:get_cwd(),
            file:set_cwd(ConfigPath),
            Paths = os:cmd(
                      io_lib:format("QUIET=1 ~p as ~p path", [Rebar3, Profile])
                     ),
            file:set_cwd(Cwd),
            CleanedPaths = [absname(ConfigPath, SubDir)
                            || SubDir <- string:tokens(Paths, " ")],
            code:add_pathsa(CleanedPaths),

            ErlOpts = proplists:get_value(erl_opts, Terms, []),
            remove_warnings_as_errors(ErlOpts)
    end.

%%------------------------------------------------------------------------------
%% @doc Read the profile name defined in rebar.config for Rebar3
%%
%% Look inside rebar.config to find a special configuration called
%% `vim_erlang_compiler`.
%%
%% E.g. to use the "test" profile:
%% {vim_erlang_compiler, [
%%   {profile, "test"}
%% ]}.
%%------------------------------------------------------------------------------
rebar3_get_profile(Terms) ->
  case proplists:get_value(vim_erlang_compiler, Terms) of
    undefined -> "default";
    Options -> proplists:get_value(profile, Options, "default")
  end.

%%------------------------------------------------------------------------------
%% @doc Find the rebar3 executable.
%%
%% First we try to find rebar3 in the project directory. Second we try to find
%% it in the PATH.
%% @end
%%------------------------------------------------------------------------------
-spec find_rebar3([string()]) -> {ok, string()} |
                                 not_found.
find_rebar3(ConfigPath) ->
    case find_files(ConfigPath, ["rebar3"]) of
        [Rebar3|_] ->
            {ok, Rebar3};
        [] ->
            case os:find_executable("rebar3") of
                false ->
                    not_found;
                Rebar3 ->
                    {ok, Rebar3}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Remove the "warnings_as_errors" option from the given Erlang options.
%%
%% If "warnings_as_errors" is left in, rebar sometimes prints the following
%% line:
%%
%%     compile: warnings being treated as errors
%%
%% The problem is that Vim interprets this as a line about an actual warning
%% about a file called "compile", so it will jump to the "compile" file.
%%
%% And anyway, it is fine to show warnings as warnings as not errors: the
%% developer know whether their project handles warnings as errors and interpret
%% them accordingly.
%% @end
%%------------------------------------------------------------------------------
-spec remove_warnings_as_errors([{atom(), string()}]) -> [{atom(), string()}].
remove_warnings_as_errors(ErlOpts) ->
    proplists:delete(warnings_as_errors, ErlOpts).

%%------------------------------------------------------------------------------
%% @doc Set code paths and options for a simple Makefile
%% @end
%%------------------------------------------------------------------------------
-spec load_makefiles([string()]) -> {ok, [{atom(), term()}]} | error.
load_makefiles([Makefile|_Rest]) ->
    Path = filename:dirname(Makefile),
    code:add_pathsa([absname(Path, "ebin")]),
    code:add_pathsa(filelib:wildcard(absname(Path, "deps") ++ "/*/ebin")),
    {opts, [{i, absname(Path, "include")},
            {i, absname(Path, "deps")}]}.

-compile({nowarn_deprecated_function, [{filename, find_src, 1}]}).

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
    try
        Return = analyze_return(Fun),
        {Name, Args, Return}
    catch
        throw:no_spec ->
            Arity = get_attribute(Fun, "arity"),
            {Name, list_to_integer(Arity)}
    end.

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
simplify_return({record, _, [Type|_]}) ->
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
    [H1 | T1] = Edoc,
    [H2 = {K2, _} | T2] = Info,
    K1 = case H1 of
        {Name, _, _} -> Name;
        {Name, _} -> Name
    end,
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
