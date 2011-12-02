-module(enit_boot).
-export([start/1, load_specs/1]).
-export([env_keeper/1, env_keeper_loop/1, get_boot_app_environments/0]).

-define(ENV_SERVER, enit_env_keeper).

-include("enit.hrl").

start([RelDir, ConfDir, ReleaseName, OptionString]) ->
    try
        Options = parse_term(OptionString),
        enit_log:init(Options),

        {ok, Info} = enit:get_release_info(RelDir, ConfDir, ReleaseName),
        enit_log:info("booting release ~s ~s~n", [Info#release.name, Info#release.version]),

        AppEnvironments = load_specs(Info#release.applications),
        proc_lib:spawn(?MODULE, env_keeper, [AppEnvironments]),

        enit_log:info("applying bootstrap configuration~n", []),
        apply_config(Info#release.config),

        IncludedBy = build_included_by(AppEnvironments),
        RunningApplications = gb_sets:from_list([App || {App, _, _} <- application:which_applications()]),
        start_apps(Info#release.applications, RunningApplications, IncludedBy),

        enit_log:info("release ~s ~s started~n", [Info#release.name, Info#release.version])
    catch
        Error ->
            enit_log:error("Error: ~s~n", [format_error(Error)]),
            halt(1)
    end.

parse_term(String) ->
    {ok, Toks, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Toks),
    Term.

format_error(Error) ->
    enit:format_error(Error).

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
            throw({error, {load_app, App, Error}})
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
                          enit_log:info("starting ~s~n", [App]),
                          case application:start(App) of
                              ok -> ok;
                              {error, {already_started, App}} -> ok;
                              {error, StartReason} -> throw({start_failed, App, StartReason})
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
                            {[{start, App} | DepActions], gb_sets:add(App, DepStarted)}
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