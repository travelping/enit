%% Copyright (c) 2011-2012 by Travelping GmbH <info@travelping.com>

%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @doc Functions that fetch information from a remote Erlang node.
-module(enit_remote).
-export([ensure_loaded/1, remote_get_status/1, remote_config_change/1,
         unique_nodename/1, gen_nodename/1]).
%% RPC functions called in the target VM.
-export([get_status_proplist/0, config_change/1]).

-include("enit.hrl").

-spec remote_get_status(#release{}) -> #status{}.
remote_get_status(Info) ->
    with_status(Info, fun (X) -> {ok, X} end).

-spec remote_config_change(#release{}) -> ok | {error, term()}.
remote_config_change(Info = #release{nodename = Nodename, config = Config}) ->
    with_status(Info,
                fun (#status{alive = true, running_config = RunningConfig, app_config_defaults = Defaults}) ->
                        ConfigWithAppDefaults = enit_config:merge(Defaults, lists:keydelete(node, 1, Config)),
                        case enit_config:diff(RunningConfig, ConfigWithAppDefaults) of
                            [] ->
                                enit_log:info("reconfigure: config unchanged\n", []);
                            ConfigDiff ->
                                case rpc:call(Nodename, ?MODULE, config_change, [ConfigDiff]) of
                                    ok ->
                                        enit_log:info("reconfigure: config changes applied\n", []);
                                    {error, Error} ->
                                        {error, {config_change, Error}};
                                    {badrpc, Error} ->
                                        {error, {badrpc, Nodename, Error}}
                                end
                        end;
                    (#status{alive = false}) ->
                        {error, {not_running, Nodename}}
                end).

with_status(#release{nodename = Node, cookie = Cookie}, Fun) ->
    maybe_start_network(),
    erlang:set_cookie(Node, Cookie),
    case is_node_online(Node, 3) of
        true ->
            case ensure_loaded(Node) of
                ok ->
                    case rpc:call(Node, ?MODULE, get_status_proplist, []) of
                        {error, Error} ->
                            {error, Error};
                        {badrpc, Error} ->
                            {error, {badrpc, Node, Error}};
                        Props ->
                            Fun(#status{alive               = true,
                                        otp_version         = proplists:get_value(otp_version, Props),
                                        uptime_seconds      = proplists:get_value(uptime_seconds, Props),
                                        memory_info         = proplists:get_value(memory_info, Props),
                                        os_pid              = proplists:get_value(os_pid, Props),
                                        app_config_defaults = proplists:get_value(app_config_defaults, Props, []),
                                        running_apps        = proplists:get_value(running_apps, Props, []),
                                        running_config      = proplists:get_value(running_config, Props, []),
                                        connected_nodes     = proplists:get_value(connected_nodes, Props)})
                    end;
                Error ->
                    Error
            end;
        false ->
            Fun(#status{alive = false, running_apps = [], running_config = [], connected_nodes = []})
    end.

%% @doc Ensure the ``enit_remote'' module is loaded on a remote node.
%%    This function assumes that the current node is alive and the
%%    remote node's cookie is set correctly.
ensure_loaded(Node) ->
    case rpc:call(Node, code, is_loaded, [?MODULE]) of
        {badrpc, Error} ->
            {error, {badrpc, Node, Error}};
        {file, _} ->
            %% module is already loaded
            ok;
        false ->
            {Mod, Code, File} = code:get_object_code(?MODULE),
            case rpc:call(Node, code, load_binary, [Mod, File, Code]) of
                {badrpc, Error} ->
                    {error, {badrpc, Node, Error}};
                {error, Error} ->
                    {error, {remote_load, Node, Mod, Error}};
                {module, Mod} ->
                    ok
            end
    end.

maybe_start_network() ->
    case is_alive() of
        true ->
            ok;
        false ->
            OwnNodename = unique_nodename("enit"),
            net_kernel:monitor_nodes(true),
            net_kernel:start([OwnNodename, shortnames]),
            receive
                {nodeup, OwnNodename} ->
                    net_kernel:monitor_nodes(false)
            after
                2000 ->
                    net_kernel:monitor_nodes(false),
                    throw({error, net_timeout})
            end
    end.

is_node_online(_, 0) ->
    false;
is_node_online(Node, N) ->
    case net_adm:ping(Node) of
        pang ->
            timer:sleep(100),
            is_node_online(Node, N - 1);
        pong ->
            true
    end.

unique_nodename(Prefix) ->
    unique_nodename(Prefix, 5).

unique_nodename(Prefix, 0) ->
    gen_nodename(Prefix ++ integer_to_list(erlang:phash2(make_ref(), 20)));
unique_nodename(Prefix, Retries) ->
    Name = Prefix ++ integer_to_list(erlang:phash2(make_ref(), 20)),
    case erl_epmd:names() of
        {ok, EpmdNames} ->
            case lists:keymember(Name, 1, EpmdNames) of
                true ->
                    unique_nodename(Prefix, Retries -1);
                false ->
                    gen_nodename(Name)
            end;
        _ ->
            gen_nodename(Name)
    end.

gen_nodename(Relname) ->
    {ok, Hostname} = inet:gethostname(),
    list_to_atom(Relname ++ "@" ++ Hostname).

%% ----------------------------------------------------------------------------------------------------
%% -- Code that runs on the target
%% @private
get_status_proplist() ->
    Applications = lists:keysort(1, [{App, Vsn} || {App, _Desc, Vsn} <- application:which_applications()]),
    {UptimeMillis, _} = erlang:statistics(wall_clock),
    [{uptime_seconds, UptimeMillis div 1000},
     {running_apps, Applications},
     {running_config, running_config()},
     {app_config_defaults, app_config_defaults()},
     {os_pid, os:getpid()},
     {memory_info, erlang:memory()},
     {otp_version, erlang:system_info(otp_release)},
     {connected_nodes, nodes()}].

app_config_defaults() ->
    case catch enit_boot:get_boot_app_environments() of
        {'EXIT', _} -> BootEnvs = [];
        BootEnvs when is_list(BootEnvs) -> ok
    end,
    ConfigDefaults = lists:flatmap(fun ({App, _Desc, _Vsn}) ->
                                           case proplists:get_value(App, BootEnvs) of
                                               undefined ->
                                                   [];
                                               Defaults ->
                                                   case lists:keydelete(included_applications, 1, Defaults) of
                                                       [] ->
                                                           [];
                                                       WithoutIncl ->
                                                           [{App, lists:keysort(1, WithoutIncl)}]
                                                   end
                                           end
                                   end, application:loaded_applications()),
    lists:keysort(1, ConfigDefaults).

running_config() ->
    Apps = lists:sort(fun erlang:'>'/2, [App || {App, _Desc, _Vsn} <- application:loaded_applications()]),
    running_config(Apps, []).

running_config([App | Rest], Acc) ->
    Env = [{K, V} || {K, V} <- application:get_all_env(App), is_atom(K), K /= included_applications],
    case Env of
        [] ->
            running_config(Rest, Acc);
        _ ->
            running_config(Rest, [{App, lists:keysort(1, Env)} | Acc])
    end;
running_config([], Acc) ->
    Acc.

%% @private
config_change(ConfigChanges) ->
    LoadedApps = [App || {App, _Desc, _Vsn} <- application:loaded_applications()],
    PreviousEnv = application_controller:prep_config_change(),
    lists:foreach(fun (App) ->
                          set_app_variables(App, proplists:get_value(App, ConfigChanges, {[], [], []}))
                  end, LoadedApps),
    application_controller:config_change(PreviousEnv).

set_app_variables(App, {Added, Removed, Changed}) ->
    lists:foreach(fun ({Key, Value}) ->
                          application:set_env(App, Key, Value)
                  end, Added),
    lists:foreach(fun ({Key, Value}) ->
                          application:set_env(App, Key, Value)
                  end, Changed),
    lists:foreach(fun ({Key, _Value}) ->
                          application:unset_env(App, Key)
                  end, Removed).
