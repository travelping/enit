-module(enit_boot).
-export([start/1, load_specs/1]).
-export([env_keeper/1, env_keeper_loop/1, get_boot_app_environments/0]).

-define(ENV_SERVER, enit_env_keeper).

-include("enit.hrl").

start([RelDir, ConfDir, ReleaseName]) ->
    try
        case application:load(enit) of
            ok ->
                ok;
            {error, {already_loaded, enit}} ->
                ok;
            {error, LoadError} ->
                throw({load_app, enit, LoadError})
        end,

        enit_log:init([{syslog, getenv(syslog, false)}]),
        StartRetries = getenv(start_retries, unlimited),
        StartDelay = getenv(start_delay, 15),

        {ok, Info} = enit:get_release_info(RelDir, ConfDir, ReleaseName),
        enit_log:info("booting release ~s ~s~n", [Info#release.name, Info#release.version]),

        AppEnvironments = load_specs(Info#release.applications),
        proc_lib:spawn(?MODULE, env_keeper, [AppEnvironments]),

        enit_log:info("applying bootstrap configuration~n", []),
        apply_config(Info#release.config),

        IncludedBy = build_included_by(AppEnvironments),
        try_app_boot(Info, gb_sets:empty(), IncludedBy, StartRetries, StartDelay),

        enit_log:info("started release ~s ~s~n", [Info#release.name, Info#release.version])
    catch
        Error ->
            enit_log:error("Error: ~s~n", [format_error(Error)]),
            halt(1)
    end.

getenv(Key, Default) ->
    case application:get_env(enit, Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

try_app_boot(Info, Started, IncludedBy, Retries, Delay) ->
    try
        start_apps(Info#release.applications, Started, IncludedBy)
    catch
        Error = {start_failed, App, StartError, NewStarted} ->
            if
                (Retries == unlimited) orelse (Retries >= 1) ->
                    error_logger:error_report([{enit_boot, {start_error, App}}, {retries_left, Retries}, {delay, Delay}, {error, StartError}]),
                    case Retries of
                        unlimited ->
                            enit_log:error("Error (will retry forever in ~ps): ~s~n", [Delay, format_error(Error)]),
                            NewRetries = Retries;
                        _N ->
                            enit_log:error("Error (will retry another ~p times in ~ps): ~s~n", [Retries, Delay, format_error(Error)]),
                            NewRetries = Retries - 1
                    end,
                    timer:sleep(timer:seconds(Delay)),
                    try_app_boot(Info, NewStarted, IncludedBy, NewRetries, Delay);
                true ->
                    error_logger:error_report([{enit_boot, start_error}, {giving_up, sorry}]),
                    enit_log:error("Giving up on ~s. Sorry.~n", [Info#release.name]),
                    erlang:halt(1)
            end
    end.

format_error({start_failed, App, Error, _Started}) ->
    io_lib:format("could not start application ~p: ~p", [App, Error]);
format_error({load_failed, App, Module, Error}) ->
    io_lib:format("could load module ~p (in ~p): ~p", [Module, App, Error]);
format_error({load_app, App, Error}) ->
    io_lib:format("failed to load appfile for ~p: ~p", [App, Error]);
format_error(Error) ->
    io_lib:format("unknown error: ~p", [Error]).

apply_config(Config) ->
    lists:foreach(fun ({App, AppConfig}) ->
                          lists:foreach(fun ({Key, Value}) ->
                                                application:set_env(App, Key, Value)
                                        end, AppConfig)
                  end, Config).

-spec load_specs([atom()]) -> [{atom(), [{atom(), term()}]}].
load_specs(Applications) ->
    load_specs(Applications, []).

load_specs([App | Rest], Acc) ->
    case application:load(App) of
        ok ->
            {ok, Env} = application:get_key(App, env),
            load_specs(Rest, [{App, Env}] ++ load_dep_specs(App) ++ Acc);
        {error, {already_loaded, App}} ->
            {ok, Env} = application:get_key(App, env),
            load_specs(Rest, [{App, Env}] ++ load_dep_specs(App) ++ Acc);
        {error, Error} ->
            throw({load_app, App, Error})
    end;
load_specs([], Acc) ->
    lists:ukeysort(1, Acc).

load_dep_specs(App) ->
    {ok, Deps} = application:get_key(App, applications),
    {ok, Incl} = application:get_key(App, included_applications),
    load_specs(Deps, []) ++ load_specs(Incl, []).

build_included_by(AppEnvironments) ->
    lists:foldl(fun ({App, _}, Map) ->
                      case application:get_key(App, included_applications) of
                          undefined ->
                              Map;
                          {ok, IncludedApps} ->
                              lists:foldl(fun (IncludedApp, M) ->
                                                  case gb_trees:lookup(IncludedApp, Map) of
                                                      none ->
                                                          gb_trees:insert(IncludedApp, App, M);
                                                      {value, OtherApp} ->
                                                          throw({duplicate_included, IncludedApp, OtherApp, App})
                                                  end
                                          end, Map, IncludedApps)
                      end
                end, gb_trees:empty(), AppEnvironments).

start_apps(AppsToStart, AppsStarted, IncludedBy) ->
    {Actions, _} = lists:foldl(fun (App, {CurActions, CurStarted}) ->
                                       {ok, Keys} = application:get_all_key(App),
                                       Spec = {application, App, Keys},
                                       start_app(Spec, IncludedBy, gb_sets:empty(), CurActions, CurStarted)
                               end, {[], AppsStarted}, AppsToStart),
    lists:foreach(fun ({start, App}) ->
                          case application:start(App) of
                              ok -> enit_log:info("started ~s~n", [App]);
                              {error, {already_started, App}} -> ok;
                              {error, StartReason} -> throw({start_failed, App, StartReason, AppsStarted})
                          end;
                      ({load_module, App, Mod}) ->
                          case code:is_loaded(Mod) of
                              {file, _} ->
                                  ok;
                              false ->
                                  case code:ensure_loaded(Mod) of
                                      {module, _} -> ok;
                                      {error, LoadError} -> throw({load_failed, App, Mod, LoadError})
                                  end
                          end
                  end, lists:reverse(Actions)).

start_app({application, App, Keys}, IncludedBy, Starting, Actions, Started) ->
    case gb_sets:is_member(App, Starting) of
        true ->
            throw({circular_dependency, App, gb_sets:to_list(Starting)});
        false ->
            case gb_sets:is_member(App, Started) of
                true ->
                    {Actions, Started};
                false ->
                    NewStarting = gb_sets:add(App, Starting),

                    % Ensure that all dependencies have been started,
                    % including the dependencies of any included applications.
                    Apps = case lists:keysearch(included_applications, 1, Keys) of
                               {value, {included_applications, As}} -> [App | As];
                               false -> [App]
                           end,
                    Deps = lists:foldl(fun (A, Acc) ->
                                               {ok, K} = application:get_all_key(A),
                                               case lists:keysearch(applications, 1, K) of
                                                   {value, {applications, D}} -> D ++ Acc;
                                                   false -> Acc
                                               end
                                       end, [], Apps),
                    {DepActions, DepStarted} =
                        lists:foldl(
                            fun (Dep, {CurActions, CurStarted}) ->
                                    % Can't depend on an application included by
                                    % another, because the application controller
                                    % won't consider it started.
                                    case gb_trees:lookup(Dep, IncludedBy) of
                                        none ->
                                            ok;
                                        {value, OtherApp} ->
                                            throw ({dependency_included, App, Dep, OtherApp})
                                    end,
                                    {ok, DepKeys} = application:get_all_key(Dep),
                                    DepSpec = {application, Dep, DepKeys},
                                    start_app(DepSpec, IncludedBy, NewStarting, CurActions, CurStarted)
                            end, {Actions, Started}, Deps),

                    % Ensure the application is started: if the application is
                    % included by another application, start the containing
                    % application; otherwise, just start it.
                    case gb_trees:lookup(App, IncludedBy) of
                        {value, ContainingApp} ->
                            {ok, ContainingKeys} = application:get_all_key(ContainingApp),
                            ContainingSpec = {application, ContainingApp, ContainingKeys},
                            {ContainingActions, ContainingStarted} = start_app(ContainingSpec, IncludedBy, NewStarting, DepActions, DepStarted),
                            {ContainingActions, gb_sets:add(App, ContainingStarted)};
                        none ->
                            {ok, Modules} = application:get_key(App, modules),
                            LoadActions = [{load_module, App, Mod} || Mod <- Modules],
                            {[{start, App} | LoadActions] ++ DepActions, gb_sets:add(App, DepStarted)}
                    end
            end
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- Environment Keeper Process

%% this process is necessary because the application_controller does not keep a
%% cached copy of the original application environment, but enit needs it for dynamic configuration.
env_keeper(Env) ->
    register(?ENV_SERVER, self()),
    proc_lib:hibernate(?MODULE, env_keeper_loop, [Env]).

env_keeper_loop(Env) ->
    receive
        {get_original_environments, Pid} when is_pid(Pid) ->
            Pid ! {?ENV_SERVER, Env},
            proc_lib:hibernate(?MODULE, env_keeper_loop, [Env])
    end.

get_boot_app_environments() ->
    ?ENV_SERVER ! {get_original_environments, self()},
    receive
        {?ENV_SERVER, Env} ->
            Env
    end.
