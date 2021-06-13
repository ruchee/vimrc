#!/usr/bin/env escript

%%% This script prints all modules or prints all functions of a module.
%%%
%%% See more information in the {@link print_help/0} function.

% 'compile' mode gives better error messages if the script throws an error.
-mode(compile).

% The filename:find_src/1 function is deprecated. Its replacement function is
% filelib:find_source, which has a different interface and was introduced only
% in Erlang 20. As long as filename:find_src/1 is present in the newest Erlang
% version, we can keep using filename:find_src/1.
-compile({nowarn_deprecated_function, [{filename, find_src, 1}]}).

-include_lib("xmerl/include/xmerl.hrl").

%%%=============================================================================
%%% Types
%%%=============================================================================

-type build_system() :: rebar | rebar3 | makefile.

-type function_spec() ::
        {FunName :: atom(),
         ArgNames :: [string()],
         ReturnType :: string()} |
        {FunName :: atom(),
         Arity :: non_neg_integer()}.
%% The specification of a function.
%%
%% If we are lucky, we know the names of arguments and the return type. If we
%% are less lucky, then we know only the arity.

-type simplified_xml_element() ::
        {XmlTag :: atom(),
         Attributes :: [{Name :: atom(), Value :: string()}],
         Content :: [(ChildXmlElement :: simplified_xml_element()) |
                     (Text :: string())]}.
%% A `simplified_xml_element()' term represents an XML element.
%%
%% `xmerl_lib:simplify_element/1' can be used to convert an #xmlElement{} record
%% into a `simplified_xml_element()' term.
%%
%% An example `simplified_xml_element()' term:
%%
%% ```
%% [{function,
%%   [{name,"my_fun"},{arity,"1"},{exported,"yes"},{label,"my_fun-1"}],
%%   [{args,[],
%%     [{arg,[]
%%       [{argName,[],
%%         ["X1"]}]}]},
%%    {typespec,[],
%%     [{erlangName,[{name,"my_fun"}],[]},
%%      {type,[],
%%       [{'fun',[],
%%         [{argtypes,[],
%%           [{type, [{name,"X1"}],
%%             [{abstype,[],
%%               [{erlangName,[{name,"integer"}],[]}]}]}]},
%%          {type,[],
%%           [{abstype,[],
%%             [{erlangName,[{name,"float"}],[]}]}]}]}]}]},
%% '''
%%
%% This term represents the following XML structure (with pseudo-XML notation):
%%
%% ```
%% <function name="my_fun" arity="1" exported="yes" label="my_fun-1">
%%   <args>
%%     <arg>
%%       <argName>
%%         X1
%%   <typespec>
%%     <erlangName name="my_fun">
%%     <type>
%%       <fun>
%%         <argtypes>
%%           <type name="X1">
%%             <abstype>
%%               <erlangName name="integer">
%%         <type>
%%           <abstype>
%%             <erlangName name="float">
%% '''

%%%=============================================================================
%%% Main function
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc This function is the entry point of the script.
%%
%% Print all modules or print all functions of a module.
%% @end
%%------------------------------------------------------------------------------
-spec main(Args) -> no_return() when
      Args :: [string()].
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
            log_error("Erroneous parameters: ~p~n", [PositionalParams]),
            halt(2)
    end.

%%%=============================================================================
%%% Parse command line arguments
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Parse the argument list.
%%
%% Put the options into the process dictionary and return the positional
%% parameters.
%% @end
%%------------------------------------------------------------------------------
-spec parse_args(Args) -> PositionalParams when
      Args :: [string()],
      PositionalParams :: [string()].
parse_args(Args) ->
    lists:reverse(parse_args(Args, [])).

%%------------------------------------------------------------------------------
%% @doc Parse the argument list.
%%
%% Put the options into the process dictionary and return the positional
%% parameters in reverse order.
%% @end
%%------------------------------------------------------------------------------
-spec parse_args(Args, Acc) -> PositionalParams when
      Args :: [string()],
      Acc :: [PositionalParam :: string()],
      PositionalParams :: [string()].
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
%% @doc Print the script's help text.
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

Examples:

  $ erlang_complete.erl list-modules | head -n 5
  ELDAPv3
  OTP-PUB-KEY
  PKCS-FRAME
  alarm_handler
  application

  $ erlang_complete.erl list-functions my_module
  module_info/0
  module_info/1
  my_fun_1(Float) -> integer()
  my_fun_2(Integer) -> float()
",
    io:format(Text).

%%%=============================================================================
%%% Preparation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Complete the given query.
%%
%% This function sets the code paths too.
%% @end
%%------------------------------------------------------------------------------
-spec run(Query) -> ok when
      Query :: list_modules |
               {list_functions, module()}.
run(Target) ->

    AbsDir =
        case get(basedir) of
            undefined ->
                {ok, Cwd} = file:get_cwd(),
                Cwd;
            BaseDir ->
                filename:absname(BaseDir)
        end,
    put(compiled_file_path, AbsDir),

    {_AppRoot, _ProjectRoot, BuildSystemOpts} = load_build_info(AbsDir),

    case BuildSystemOpts of
        {opts, _Opts} ->
            try
                run2(Target)
            catch
                throw:error ->
                    % The error messages were already printed.
                    ok
            end;
        error ->
            error
    end.

%%%=============================================================================
%%% Load build information.
%%%
%%% This code block is also present in erlang_check.erl in the
%%% vim-erlang-compiler project. If you modify this code block, please also
%%% modify erlang_check.erl.
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Load information about the build system.
%%
%% The `Path' parameter is the absolute path to the parent directory of the
%% relevant Erlang source file.
%%
%% The code paths are set by the function. The include paths are returned in
%% `BuildSystemOpts'.
%% @end
%%------------------------------------------------------------------------------
-spec load_build_info(Path) -> Result when
      Path :: string(),
      Result :: {AppRoot, ProjectRoot, BuildSystemOpts},
      AppRoot :: string(),
      ProjectRoot :: string(),
      BuildSystemOpts ::  {opts, [{atom(), term()}]} |
                          error.
load_build_info(Path) ->

    % AppRoot: the directory of the Erlang app.
    AppRoot =
        case find_app_root(Path) of
            no_root ->
                log("Could not find project root.~n"),
                Path;
            Root ->
                log("Found project root: ~p~n", [Root]),
                Root
        end,

    {BuildSystem, BuildFiles} = guess_build_system(AppRoot),

    % ProjectRoot: the directory of the Erlang release (if it exists; otherwise
    % same as AppRoot).
    ProjectRoot = get_project_root(BuildSystem, BuildFiles, AppRoot),
    BuildSystemOpts = load_build_files(BuildSystem, ProjectRoot, BuildFiles),

    {AppRoot, ProjectRoot, BuildSystemOpts}.

%%------------------------------------------------------------------------------
%% @doc Traverse the directory structure upwards until is_app_root matches.
%% @end
%%------------------------------------------------------------------------------
-spec find_app_root(Path) -> Root when
      Path :: string(),
      Root :: string() | 'no_root'.
find_app_root("/") ->
    case is_app_root("/") of
        true -> "/";
        false -> no_root
    end;
find_app_root(Path) ->
    case is_app_root(Path) of
        true -> Path;
        false -> find_app_root(filename:dirname(Path))
    end.

%%------------------------------------------------------------------------------
%% @doc Check directory if it is the root of an OTP application.
%% @end
%%------------------------------------------------------------------------------
-spec is_app_root(Path) -> boolean() when
      Path :: string().
is_app_root(Path) ->
    filelib:wildcard("ebin/*.app", Path) /= [] orelse
    filelib:wildcard("src/*.app.src", Path) /= [].

%%------------------------------------------------------------------------------
%% @doc Check for some known files and try to guess what build system is being
%% used.
%% @end
%%------------------------------------------------------------------------------
-spec guess_build_system(Path) -> Result when
      Path :: string(),
      Result :: {build_system(),
                 BuildFiles :: [string()]}.
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

%%------------------------------------------------------------------------------
%% @doc Check which build system's files are contained by the project.
%% @end
%%------------------------------------------------------------------------------
-spec guess_build_system(Path, BuildSystems) -> Result when
      BuildSystems :: [{build_system(),
                        BaseNames :: [string()]}],
      Path :: string(),
      Result :: {build_system(),
                 BuildFiles :: [string()]}.
guess_build_system(_Path, []) ->
    log("Unknown build system.~n"),
    {unknown_build_system, []};
guess_build_system(Path, [{BuildSystem, BaseNames}|Rest]) ->
    log("Try build system: ~p~n", [BuildSystem]),
    case find_files(Path, BaseNames) of
        [] ->
            guess_build_system(Path, Rest);
        BuildFiles ->
            {BuildSystem, BuildFiles}
    end.

%%------------------------------------------------------------------------------
%% @doc Get the root directory of the project.
%% @end
%%------------------------------------------------------------------------------
-spec get_project_root(BuildSystem, BuildFiles, AppRoot) -> ProjectRoot when
      BuildSystem :: build_system(),
      BuildFiles :: [string()],
      AppRoot :: string(),
      ProjectRoot :: string().
get_project_root(rebar3, BuildFiles, _) ->
    RebarLocks = [F || F <- BuildFiles,
                       filename:basename(F) == "rebar.lock"],
    RebarLocksWithPriority = [{F, rebar3_lock_priority(F)} || F <- RebarLocks],
    {RebarLock, _Priority} = hd(lists:keysort(2, RebarLocksWithPriority)),
    filename:dirname(RebarLock);
get_project_root(_BuildSystem, _Files, AppRoot) ->
    AppRoot.

%%------------------------------------------------------------------------------
%% @doc Get the priority of a rebar3 lock file.
%%
%% Standalone rebar3 lock files found along the parent paths could end up making
%% their directories be prioritised in our attempt to search for the true root
%% of the project.
%%
%% This will in turn result in 'rebar.config not found in [...]' kind of errors
%% being printed out when checking for syntax errors.
%%
%% This function attempts to minimise the risk of that happening by prioritising
%% the found locks according to simple heuristics for how likely are those lock
%% files to be the genuine article.
%% @end
%%------------------------------------------------------------------------------
-spec rebar3_lock_priority(Filename) -> Result when
      Filename :: string(),
      Result :: [non_neg_integer()].
rebar3_lock_priority(Filename) ->
    Dir = filename:dirname(Filename),
    AbsDir = filename:absname(Dir),
    {ok, Siblings} = file:list_dir(AbsDir),
    {SiblingDirs, SiblingFiles} =
        lists:partition(fun filelib:is_dir/1, Siblings),
    AbsDirComponents = filename:split(AbsDir),

    MightBeRebarProject = lists:member("rebar.config", SiblingFiles),
    MightBeSingleApp = lists:member("src", SiblingDirs),
    MightBeUmbrellaApp = lists:member("apps", SiblingDirs),
    Depth = length(AbsDirComponents),

    if MightBeRebarProject ->
           % Lock files standing beside a rebar.config file
           % get a higher priority than to those that don't.
           % Between them, those higher in file system hierarchy will
           % themselves get prioritised.
           [1, Depth];
       MightBeSingleApp xor MightBeUmbrellaApp ->
           % Lock files standing beside either a src or apps directory
           % get a higher priority than those that don't.
           % Between them, those higher in file system hierarchy will
           % themselves get prioritised.
           [2, Depth];
       true ->
           % No good criteria remain. Prioritise by placement in
           % file system hierarchy.
           [3, Depth]
    end.

%%------------------------------------------------------------------------------
%% @doc Load the settings from a given set of build system files.
%% @end
%%------------------------------------------------------------------------------
-spec load_build_files(BuildSystem, ProjectRoot, ConfigFiles) -> Result when
      BuildSystem :: build_system(),
      ProjectRoot :: string(),
      ConfigFiles :: [string()],
      Result :: {opts, [{atom(), term()}]} |
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
-spec load_rebar_files(ConfigFiles, Config) -> Result when
      ConfigFiles :: [string()],
      Config :: no_config | [{atom(), term()}],
      Result :: {opts, [{atom(), term()}]} |
                error.
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
            file_error(ConfigFile, {file_error, Reason}),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Apply a rebar.config file.
%%
%% This function adds the directories in the rebar.config file to the code path
%% and returns and compilation options to be used when compiling the file.
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar_config(Path, Terms, Config) -> Result when
      Path :: string(),
      Terms :: [{atom(), term()}],
      Config :: no_config | [{atom(), term()}],
      Result :: [{atom(), term()}].
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
%% @doc Load the content of each rebar3 file.
%%
%% Note worthy: The config returned by this function only represent the first
%% rebar file (the one closest to the file to compile).
%% @end
%%------------------------------------------------------------------------------
-spec load_rebar3_files(ConfigFile) -> Result when
      ConfigFile :: string(),
      Result :: {opts, [{atom(), term()}]} |
                error.
load_rebar3_files(ConfigFile) ->
    ConfigPath = filename:dirname(ConfigFile),
    ConfigResult = case filename:extension(ConfigFile) of
                       ".script" -> file:script(ConfigFile);
                       ".config" -> file:consult(ConfigFile)
                   end,
    case ConfigResult of
        {ok, ConfigTerms} ->
            log("rebar.config read: ~s~n", [ConfigFile]),
            try process_rebar3_config(ConfigPath, ConfigTerms) of
                error ->
                    error;
                Config ->
                    {opts, Config}
            catch
                throw:error ->
                    error
            end;
        {error, Reason} ->
            log_error("rebar.config consult failed:~n"),
            file_error(ConfigFile, {file_error, Reason}),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc Apply a rebar.config file.
%%
%% This function adds the directories returned by rebar3 to the code path and
%% returns and compilation options to be used when compiling the file.
%% @end
%%------------------------------------------------------------------------------
-spec process_rebar3_config(ConfigPath, Terms) -> Result when
      ConfigPath :: string(),
      Terms :: [{atom(), term()}],
      Result :: [{atom(), term()}] | error.
process_rebar3_config(ConfigPath, Terms) ->
    case find_rebar3(ConfigPath) of
        not_found ->
            % Compilation would likely fail without settings the paths, so let's
            % give an explicit error instead of proceeding anyway.
            log_error("rebar3 executable not found.~n"),
            error;
        {ok, Rebar3Rel} ->
            log("rebar3 executable found: ~s~n", [Rebar3Rel]),
            Rebar3 = filename:absname(Rebar3Rel),
            log("Absolute path to rebar3 executable: ~s~n", [Rebar3]),
            % load the profile used by rebar3 to print the dependency path list
            Profile = rebar3_get_profile(Terms),
            % "rebar3 path" prints all paths that belong to the project; we add
            % these to the Erlang paths.
            %
            % QUIET=1 ensures that it won't print other messages, see
            % https://github.com/erlang/rebar3/issues/1143.
            {ok, Cwd} = file:get_cwd(),
            file:set_cwd(ConfigPath),
            os:putenv("QUIET", "1"),
            MainCmd = io_lib:format("~p as ~p path", [Rebar3, Profile]),
            log("Call: ~s~n", [MainCmd]),
            {ExitCode, Output} = command(MainCmd, []),
            log("Result: ~p~n", [{ExitCode, Output}]),
            file:set_cwd(Cwd),

            Paths =
                case ExitCode of
                    0 ->
                        Output;
                    _ ->
                        file_error(
                          get(compiled_file_path),
                          {format,
                           "'~s' failed with exit code ~p: ~s~n",
                           [MainCmd, ExitCode, Output]}),
                        throw(error)
                end,

            CleanedPaths = [absname(ConfigPath, SubDir)
                            || SubDir <- string:tokens(Paths, " ")],
            code:add_pathsa(CleanedPaths),

            % Add _checkouts dependencies to code_path.
            %
            % These dependencies are compiled into the following directories:
            %
            % - `_checkouts/<app>/ebin' until rebar 3.13
            % - `_build/<profile>/checkouts/<app>/ebin/' from rebar 3.14
            %
            % Documentation for _checkouts dependencies:
            % https://www.rebar3.org/docs/dependencies#section-checkout-dependencies
            code:add_pathsa(
              filelib:wildcard(absname(ConfigPath, "_checkouts") ++
                               "/*/ebin")),
            code:add_pathsa(
              filelib:wildcard(absname(ConfigPath, "_build") ++
                               "/default/checkouts/*/ebin")),

            lists:foreach(
              fun({ProfileName, Deps}) ->
                      Apps = string:join([atom_to_list(D) || D <- Deps], ","),
                      file:set_cwd(ConfigPath),
                      Cmd2 = io_lib:format("QUIET=1 ~p as ~p path --app=~s",
                                          [Rebar3, ProfileName, Apps]),
                      log("Call: ~s~n", [Cmd2]),
                      ProfilePaths = os:cmd(Cmd2),
                      log("Result: ~s~n", [Paths]),
                      file:set_cwd(Cwd),
                      Cleaned = [absname(ConfigPath, SubDir)
                                 || SubDir <- string:tokens(ProfilePaths, " ")],
                      code:add_pathsa(Cleaned);
                 (_) -> ok
              end, rebar3_get_extra_profiles(Terms)),

            ErlOpts = proplists:get_value(erl_opts, Terms, []),
            remove_warnings_as_errors(ErlOpts)
    end.

%%------------------------------------------------------------------------------
%% @doc Find the rebar3 executable.
%%
%% First we try to find rebar3 in the project directory. Second we try to find
%% it in the PATH.
%% @end
%%------------------------------------------------------------------------------
-spec find_rebar3(ConfigPath) -> Result when
      ConfigPath :: [string()],
      Result :: {ok, string()} |
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
%% @doc Read the profile name defined in rebar.config for Rebar3
%%
%% Look inside rebar.config to find a special configuration called
%% `vim_erlang_compiler`.
%%
%% E.g. to use the "test" profile:
%%
%% ```
%% {vim_erlang_compiler, [
%%   {profile, "test"}
%% ]}.
%% '''
%%------------------------------------------------------------------------------
-spec rebar3_get_profile(Terms) -> Result when
      Terms :: [{atom(), term()}],
      Result :: string().
rebar3_get_profile(Terms) ->
  case proplists:get_value(vim_erlang_compiler, Terms) of
    undefined -> "default";
    Options -> proplists:get_value(profile, Options, "default")
  end.

%%------------------------------------------------------------------------------
%% @doc Read all extra profile names declared within the rebar.config
%%
%%------------------------------------------------------------------------------
-spec rebar3_get_extra_profiles(Terms) -> Result when
      Terms :: [{atom(), term()}],
      Result :: [{ProfileName :: string(),
                  [Dependency :: term()]}].
rebar3_get_extra_profiles(Terms) ->
    case proplists:get_value(profiles, Terms, []) of
        [] ->
            [];
        Profiles ->
            lists:flatmap(
              fun({ProfileName, Profile}) ->
                      case proplists:get_value(deps, Profile, []) of
                          [] ->
                              [];
                          Deps ->
                              [{ProfileName, [Dep || {Dep, _} <- Deps]}]
                      end;
                 (_) ->
                      []
              end, Profiles)
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
%% And anyway, it is fine to show warnings as warnings not not errors: the
%% developer knows whether their project handles warnings as errors and can
%% interpret them accordingly.
%% @end
%%------------------------------------------------------------------------------
-spec remove_warnings_as_errors(ErlOpts) -> ErlOpts when
      ErlOpts :: [{atom(), string()}].
remove_warnings_as_errors(ErlOpts) ->
    proplists:delete(warnings_as_errors, ErlOpts).

%%------------------------------------------------------------------------------
%% @doc Set code paths and options for a simple Makefile
%% @end
%%------------------------------------------------------------------------------
-spec load_makefiles(BuildFiles) -> Result when
      BuildFiles :: [string()],
      Result :: {opts, [{atom(), term()}]} |
                error.
load_makefiles([Makefile|_Rest]) ->
    Path = filename:dirname(Makefile),
    code:add_pathsa([absname(Path, "ebin")]),
    code:add_pathsa(filelib:wildcard(absname(Path, "deps") ++ "/*/ebin")),
    code:add_pathsa(filelib:wildcard(absname(Path, "lib") ++ "/*/ebin")),
    {opts, [{i, absname(Path, "include")},
            {i, absname(Path, "deps")},
            {i, absname(Path, "lib")}]}.

%%%=============================================================================
%%% Execution
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Print the completions for a target.
%% 
%% If there was no error, then the function first prints the line
%% "execution_successful", and it prints the completion results afterwards. If
%% there was an error, then the errors are printed.
%%
%% The reason for this design is that edoc:get_doc/1 prints its errors to the
%% standard output before returning. This means that we have no easy way of
%% printing anything before Edoc to indicate to the caller of
%% erlang_complete.erl that an error message will follow. (This caller is
%% usually erlang_complete.vim.) This design is also useful in case of other
%% errors, e.g. if the erlang_complete.erl script itself cannot be executed, and
%% the error is printed by the Erlang environment to the standard
%% output or standard error.
%%
%% This function assumes that the code paths are all set.
%% @end
%%------------------------------------------------------------------------------
-spec run2(Target) -> ok when
      Target :: list_modules |
                {list_functions, module()}.

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
                []
        end,

    Info =
        try
            module_info2(Mod)
        catch
            error:undef ->
                []
        end,

    case {Edoc, Info} of
        {[], []} ->
            io:format("Module not found.\n");
        _ ->
            FunSpecs = merge_functions(Edoc, Info),
            io:format("execution_successful\n"),
            [print_function(Fun) || Fun <- FunSpecs ],
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Return the specification of all exported functions.
%%
%% The returned list is sorted by function name.
%% @end
%%------------------------------------------------------------------------------
-spec module_edoc(Mod) -> Result when
      Mod :: module(),
      Result :: [function_spec()].
module_edoc(Mod) ->
    File =
        case filename:find_src(Mod) of
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

    {_, Doc} =
        try
            % We ask Edoc to:
            %
            % 1. Parse the module's source code.
            % 2. Extract information about functions, types and docstrings.
            % 3. Put this information into an Erlang term that represents an XML
            %    (and can be queried with functions in the xmerl application).
            FileRel = relatizive_path_maybe(File),
            edoc:get_doc(FileRel)
        catch
            _:Error ->
                % If edoc:get_doc/1 was unsuccessful, it already printed its
                % errors and throw an exception. Sometimes it might be still
                % useful to print the exception, so let's print it.
                io:format("Edoc error: ~p~n", [Error]),
                throw(error)
        end,

    Funs = xmerl_xpath:string("/module/functions/function", Doc),
    FunSpecs = lists:map(fun analyze_function/1, Funs),
    lists:keysort(1, FunSpecs).

%%------------------------------------------------------------------------------
%% @doc Convert the path of a BEAM file to the path of the corresponding Erlang
%% source file.
%%
%% This is only a heuristic that we try if `filename:find_src/1' fails.
%% @end
%%------------------------------------------------------------------------------
-spec beam_to_src_path(BeamPath) -> Result when
      BeamPath :: file:filename(),
      Result :: file:filename_all().
beam_to_src_path(BeamPath) ->
    % Example:
    % - PathParts = ["myproject", "apps", "ebin", "mymodule.beam"]
    % - Dirs = ["myproject", "apps", "ebin"]
    % - BeamFile = "mymodule.beam"
    % - Dirs2 = ["myproject", "apps"]
    % - DirLast = "ebin"
    PathParts = filename:split(BeamPath),
    {Dirs, [BeamFile]} = lists:split(length(PathParts) - 1, PathParts),
    {Dirs2, [DirsLast]} = lists:split(length(Dirs) - 1, Dirs),
    Dirs3 =
        case filename:pathtype(BeamPath) of
            absolute ->
                case DirsLast of
                    "ebin" ->
                        Dirs2 ++ ["src"];
                    _ ->
                        Dirs
                end;
            relative ->
                Dirs
        end,
    filename:join(Dirs3 ++ [beam_to_src_file(BeamFile)]).

%%------------------------------------------------------------------------------
%% @doc Convert "<name>.beam" into "<name>.erl".
%% @end
%%------------------------------------------------------------------------------
-spec beam_to_src_file(BeamFile) -> Result when
      BeamFile :: file:filename(),
      Result :: file:filename().
beam_to_src_file(BeamFile) ->
    [ModName, "beam"] = string:tokens(BeamFile, "."),
    ModName ++ ".erl".

%%------------------------------------------------------------------------------
%% @doc Return the specification of a function.
%%
%% Example (with pseudo-XML notation):
%% 
%% ```
%% % Original Erlang function
%% -spec my_fun(integer()) -> float();
%%             (string()) -> binary().
%% my_fun(_) ->
%%     ok.
%%
%% FunXmlElement =
%%
%%     <function name="my_fun" arity="1" exported="yes" label="my_fun-1">
%%
%%       <!-- Type spec clause 1 -->
%%       <args>
%%         <arg>
%%           <argName>
%%             X1
%%       <typespec>
%%         <erlangName name="my_fun">
%%         <type>
%%           <fun>
%%             <argtypes>
%%               <type name="X1">
%%                 <abstype>
%%                   <erlangName name="integer">
%%             <type>
%%               <abstype>
%%                 <erlangName name="float">
%%
%%       <!-- Type spec clause 2. These XML elements exist only when Erlang
%%            OTP version is at least 23. The analyze_function function
%%            currently ignores these XML elements. -->
%%       <args>
%%         <arg>
%%           <argName>
%%             X1
%%       <typespec>
%%         <erlangName name="my_fun">
%%         <type>
%%           <fun>
%%             <argtypes>
%%               <type name="X1">
%%                 <abstype>
%%                   <erlangName name="string">
%%             <type>
%%               <abstype>
%%                 <erlangName name="binary">
%%
%% Result = {my_fun, ["X1"], "float()"}
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec analyze_function(FunXmlElement) -> Result when
      FunXmlElement :: #xmlElement{},
      Result :: function_spec().
analyze_function(Fun) ->
    Name = list_to_atom(get_attribute(Fun, "name")),
    TypeSpecClauses = xmerl_xpath:string("typespec", Fun),
    case TypeSpecClauses of
        [] ->
            % This function has no type spec.
            Arity = get_attribute(Fun, "arity"),
            {Name, list_to_integer(Arity)};
        [TypeSpecClause|_OtherTypeSpecClauses] ->
            % A function might have more than one type spec clauses (but only if
            % Erlang OTP version is at least 23). We currently show only the
            % first one and ignore the rest.
            Args0 = xmerl_xpath:string("type/fun/argtypes/type",
                                       TypeSpecClause),
            ArgNames = lists:map(fun(Arg) -> get_attribute(Arg, "name") end,
                                 Args0),
            Return = analyze_return(TypeSpecClause),
            {Name, ArgNames, Return}
    end.

%%------------------------------------------------------------------------------
%% @doc Return the value of an attribute in an XML element.
%%
%% Example (with pseudo-XML notation):
%% 
%% ```
%% Elem = <function name="complete_a1"
%%                  arity="0"
%%                  exported="yes"
%%                  label="complete_a1-0">
%%          [...]
%% AttrName = "arity"
%% Result = "0"
%% ```
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_attribute(Elem, AttrName) -> Result when
      Elem :: #xmlElement{},
      AttrName :: string(),
      Result :: iolist() | atom() | integer().
get_attribute(Elem, AttrName) ->
    [Attr] = xmerl_xpath:string("@" ++ AttrName, Elem),
    Attr#xmlAttribute.value.

%%------------------------------------------------------------------------------
%% @doc Return the return type of a function as a string.
%%
%% Example (in pseudo-XML notation):
%% 
%% ```
%% TypeSpecClause =
%%     <typespec>
%%       <erlangName name="my_fun">
%%       <type>
%%         <fun>
%%           <argtypes>
%%             <type name="X1">
%%               <abstype>
%%                 <erlangName name="integer">
%%           <type>
%%             <abstype>
%%               <erlangName name="float">
%% Result = "float()"
%%
%% If the type spec clause does not contain the return value, "?" is returned.
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec analyze_return(TypeSpecClause) -> Result when
      TypeSpecClause :: #xmlElement{},
      Result :: string().
analyze_return(TypeSpecClause) ->
    case xmerl_xpath:string("type/fun/type/*", TypeSpecClause) of
        [ReturnType] ->
            simplify_return(xmerl_lib:simplify_element(ReturnType));
        _ ->
            "?"
    end.

%%------------------------------------------------------------------------------
%% @doc Return a function's return type as a string.
%%
%% Example:
%% 
%% ```
%% SimplifiedTypeSpecClause = {abstype,[],[{erlangName,[{name,"float"}],[]}]}
%% Result = "float()"
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec simplify_return(SimplifiedTypeSpecClause) -> Result when
      SimplifiedTypeSpecClause :: simplified_xml_element(),
      Result :: string().
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

%%------------------------------------------------------------------------------
%% @doc Get the specifications of all exported functions.
%%
%% The result will contain only arities. For example:
%%
%% ```
%% > lists:keysort(1, lists:module_info(exports)).
%% [{all,2},
%%  {any,2},
%%  {append,2},
%%  {append,1},
%%  {concat,1},
%% '''
%%
%% The returned list is sorted by function name.
%% @end
%%------------------------------------------------------------------------------
-spec module_info2(Mod) -> Result when
      Mod :: module(),
      Result :: [function_spec()].
module_info2(Mod) ->
    lists:keysort(1, Mod:module_info(exports)).

%%------------------------------------------------------------------------------
%% @doc Merge functions specifications read with edoc and with `module_info'.
%%
%% Notes:
%%
%% 1.  This function assumes that the input lists are sorted.
%%
%% 2.  If a function is present in both lists, edoc wins. This is because the
%%     `module_info' function specification contains only the arity, so the edoc
%%     function specification is either the same or better (if it contains the
%%     argument list and return type).
%% @end
%%------------------------------------------------------------------------------
-spec merge_functions(Edoc, Info) -> Result when
      Edoc :: [function_spec()],
      Info :: [function_spec()],
      Result :: [function_spec()].
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
    K1 =
        case H1 of
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

%%------------------------------------------------------------------------------
%% @doc Print a function speficiation in a human-friendly way.
%%
%% When this script is called by the plugin, the printed lines are sent back to
%% Vim, who shows them to the user in the completion menu.
%% @end
%%------------------------------------------------------------------------------
-spec print_function(Fun) -> ok when
      Fun :: function_spec().
print_function({FunName, Arity}) ->
    io:format("~s/~B~n", [FunName, Arity]);
print_function({FunName, Args, ReturnType}) ->
    io:format("~s(~s) -> ~s~n", [FunName, string:join(Args, ", "), ReturnType]).

%%%=============================================================================
%%% Utility functions (in alphabetical order)
%%%=============================================================================

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
-spec absname(Dir, Filename) -> Result when
      Dir :: string(),
      Filename :: string(),
      Result :: string().
absname(Dir, Filename) ->
    filename:absname(filename:join(Dir, Filename)).

%%------------------------------------------------------------------------------
%% @doc Execute the given OS command.
%%
%% The command's output is printed, and its exit code is returned.
%%
%% Original code from
%% http://erlang.org/pipermail/erlang-questions/2007-February/025210.html
%% @end
%%------------------------------------------------------------------------------
-spec command(Cmd, Options) -> Result when
      Cmd :: string(),
      Options :: [{print_output, boolean()}],
      ExitCode :: integer(),
      Output :: string(),
      Result :: {ExitCode, Output}.
command(Cmd, Options) ->
     PortOptions = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
     Port = open_port({spawn, Cmd}, PortOptions),
     command_loop(Port, Options, []).

%%------------------------------------------------------------------------------
%% @doc Read the output of an OS command started via a port.
%% @end
%%------------------------------------------------------------------------------
-spec command_loop(Port, Options, OutputAcc) -> Result when
      Port :: port(),
      Options :: [{print_output, boolean()}],
      OutputAcc :: [string()],
      ExitCode :: integer(),
      Output :: string(),
      Result :: {ExitCode, Output}.
command_loop(Port, Options, OutputAcc) ->
     receive
         {Port, {data, Data}} ->
             case proplists:get_value(print_output, Options, false) of
                 true ->
                     io:format(user, "~s", [Data]);
                 false ->
                     ok
             end,
             command_loop(Port, Options, [Data|OutputAcc]);
         {Port, eof} ->
             port_close(Port),
             receive
                 {Port, {exit_status, ExitCode}} ->
                     Output = lists:append(lists:reverse(OutputAcc)),
                     {ExitCode, Output}
             end
     end.

%%------------------------------------------------------------------------------
%% @doc Print the given error reason in a Vim-friendly and human-friendly way.
%% @end
%%------------------------------------------------------------------------------
-spec file_error(File, Error) -> ok when
      File :: string(),
      Error :: {format, Format, Data} |
               {file_error, FileError},
      Format :: io:format(),
      Data :: [term()],
      FileError :: file:posix() | badarg | terminated | system_limit |
                   {Line :: integer(), Mod :: module(), Term :: term()}.
file_error(File, Error) ->

    LineNumber =
        case Error of
            {file_error, {LineNumber0, _, _}} ->
                LineNumber0;
            _ ->
                % The error doesn't belong to a specific line, but Vim shows it
                % only if it has a line number, so let's assign line 1 to it.
                1
        end,

    FileRel = relatizive_path_maybe(File),
    io:format(standard_io, "~s:~p: ", [FileRel, LineNumber]),

    case Error of
        {file_error, FileError} ->
            io:format(standard_io, "~s~n", [file:format_error(FileError)]);
        {format, Format, Data} ->
            io:format(standard_io, Format, Data)
    end.


%%------------------------------------------------------------------------------
%% @doc Find the first file matching one of the filenames in the given path.
%% @end
%%------------------------------------------------------------------------------
-spec find_file(Path, Files) -> Result when
      Path :: string(),
      Files :: [string()],
      Result :: [string()].
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
%% @doc Recursively search upward through the path tree and returns the absolute
%% path to all files matching the given filenames.
%% @end
%%------------------------------------------------------------------------------
-spec find_files(Path, Files) -> Result when
      Path :: string(),
      Files :: [string()],
      Result :: [string()].
find_files("/", Files) ->
    find_file("/", Files);
find_files([_|":/"] = Path, Files) ->
    % E.g. "C:/". This happens on Windows.
    find_file(Path, Files);
find_files(Path, Files) ->
    ParentPath = filename:dirname(Path),
    find_file(Path, Files) ++
    find_files(ParentPath, Files).

%%------------------------------------------------------------------------------
%% @doc Log the given entry if we are in verbose mode.
%% @end
%%------------------------------------------------------------------------------
-spec log(Format) -> ok when
      Format :: io:format().
log(Format) ->
    log(Format, []).

%%------------------------------------------------------------------------------
%% @doc Log the given entry if we are in verbose mode.
%% @end
%%------------------------------------------------------------------------------
-spec log(Format, Data) -> ok when
      Format :: io:format(),
      Data :: [term()].
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
-spec log_error(Format) -> ok when
      Format :: io:format().
log_error(Format) ->
    io:format(standard_error, Format, []).

%%------------------------------------------------------------------------------
%% @doc Log the given error.
%% @end
%%------------------------------------------------------------------------------
-spec log_error(Format, Data) -> ok when
      Format :: io:format(),
      Data :: [term()].
log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

%%------------------------------------------------------------------------------
%% @doc Try to convert a path to a relative path.
%% @end
%%------------------------------------------------------------------------------
-spec relatizive_path_maybe(Path) -> Path when
      Path :: file:filename().
relatizive_path_maybe(Path) ->
    {ok, Cwd} = file:get_cwd(),

    case lists:prefix(Cwd, Path) of
        true ->
            % Example:
            % Cwd = "/home/my"
            % Path = "/home/my/dir/my_file.erl"
            %                ^ length(Cwd)
            %                  ^ Start
            %                  <-------------> Len
            % FileRel = "dir/my_file.erl"
            Start = length(Cwd) + 2,
            Len = length(Path) - length(Cwd) - 1,
            lists:sublist(Path, Start, Len);
        false ->
            % The path is not below the current directory, so let's keep it
            % absolute.
            Path
    end.
