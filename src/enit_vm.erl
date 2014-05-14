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

-module(enit_vm).
-export([startfg/2, startfg/4, start_remsh/1, stop/1]).

-include("enit.hrl").
-include("enit_posix.hrl").

-define(DEFAULT_STOP_TIMEOUT, 10000).

erl_binary() ->
    filename:join([code:root_dir(), "bin", "erl"]).

startfg(Rel = #release{}, Options) ->
    startfg(Rel#release.name, Rel#release.nodename, Rel#release.config, Options).

startfg(RelName, NodeName, Config, OptionsIn) ->
    case enit_posix:getuid() of
        {ok, 0} -> startfg2(RelName, NodeName, Config, OptionsIn);
        {ok, _} -> {error, notroot}
    end.

startfg2(RelName, NodeName, Config, OptionsIn) ->
    {ok, RelPath} = application:get_env(enit, release_dir),
    {ok, ConfPath} = application:get_env(enit, config_dir),

    DefaultArgs = ["-sname", atom_to_list(NodeName),
                   "-noshell",
                   "-run", "enit_boot", "start", RelPath, ConfPath, RelName],
    ConfigArgs = build_erl_args(Config),
    Options = set_retry_options(OptionsIn, set_retry_options(proplists:get_value(node, Config, []), OptionsIn)),
    EnitParams = lists:flatmap(fun ({Key, Value}) ->
                                       ["-enit", atom_to_list(Key), lists:flatten(io_lib:format("~p", [Value]))]
                               end, Options),

    case set_config_user_and_group(Config) of
        ok ->
            exec_erlang(DefaultArgs ++ ConfigArgs ++ EnitParams);
        {error, Error} ->
            {error, Error}
    end.

set_retry_options([{start_retries, "unlimited"} | Rest], Options) ->
    set_retry_options(Rest, lists:keystore(start_retries, 1, Options, {start_retries, unlimited}));
set_retry_options([{start_retries, unlimited} | Rest], Options) ->
    set_retry_options(Rest, lists:keystore(start_retries, 1, Options, {start_retries, unlimited}));
set_retry_options([{start_retries, Retries} | Rest], Options) ->
    set_retry_options(Rest, lists:keystore(start_retries, 1, Options, {start_retries, to_integer(Retries)}));
set_retry_options([{start_delay, Delay} | Rest], Options) ->
    set_retry_options(Rest, lists:keystore(start_delay, 1, Options, {start_delay, to_integer(Delay)}));
set_retry_options([_ | Rest], Options) ->
    set_retry_options(Rest, Options);
set_retry_options([], Options) ->
    Options.

to_integer(Int) when is_integer(Int) -> Int;
to_integer(String) when is_list(String) -> list_to_integer(String).

start_remsh(Release) ->
    Args = ["-remsh", atom_to_list(Release#release.nodename),
            "-setcookie", atom_to_list(Release#release.cookie),
            "-sname", atom_to_list(enit_remote:unique_nodename("enit-remsh")),
            "-hidden", "-smp", "disable"],
    KernelArgs = build_kernel_args(proplists:get_value(kernel, Release#release.config, [])),
    exec_erlang(Args ++ KernelArgs).

exec_erlang(CmdlineArgs) ->
    %% disconnect distribution, because epmd gets confused if we don't
    net_kernel:stop(),
    enit_posix:exec([erl_binary() | CmdlineArgs]).

set_config_user_and_group(Config) ->
    case enit_config:get(node, run_as_group, Config) of
        undefined ->
            set_config_user(Config);
        Group ->
            case enit_posix:getgrnam(Group) of
                {error, Error} ->
                    {error, {getgrnam, Group, Error}};
                {ok, #posix_group{gr_gid = Gid}} ->
                    case enit_posix:setgid(Gid) of
                        ok ->
                            set_config_user(Config);
                        {error, Error} ->
                            {error, {setgid, Group, Error}}
                    end
            end
    end.

set_config_user(Config) ->
    case enit_config:get(node, run_as_user, Config) of
        undefined ->
            ok;
        User ->
            case enit_posix:getpwnam(User) of
                {error, Error} ->
                    {error, {getpwnam, User, Error}};
                {ok, #posix_passwd{pw_uid = Uid, pw_dir = Home}} ->
                    case enit_posix:setuid(Uid) of
                        {error, Error} ->
                            {error, {setuid, User, Error}};
                        ok ->
                            os:putenv("HOME", Home),
                            ok
                    end
            end
    end.

build_erl_args([{kernel, Params} | R]) ->
    build_kernel_args(Params) ++ build_erl_args(R);
build_erl_args([{node, Params} | R]) ->
    SMP = case proplists:get_value(smp, Params) of
              undefined ->
                  [];
              Atom ->
                  ["-smp", enit:to_str(Atom)]
          end,
    Con = case proplists:get_value(connect_all, Params) of
              false ->
                  ["-connect_all", "false"];
              _ ->
                  []
          end,
    Hid = case proplists:get_value(hidden, Params) of
              true ->
                  ["-hidden"];
              _ ->
                  []
          end,
    SMP ++ Con ++ Hid ++ build_erl_args(R);
build_erl_args([_ | R]) ->
    build_erl_args(R);
build_erl_args([]) ->
    [].

build_kernel_args(Params) ->
    lists:flatmap(fun ({K, V}) ->
                          ["-kernel", atom_to_list(K), lists:flatten(io_lib:format("~p", [V]))]
                  end, Params).

stop(#release{nodename = Nodename, config = Config}) ->
    erlang:monitor_node(Nodename, true),
    case rpc:call(Nodename, init, stop, []) of
        ok ->
            ok;
        {badrpc, Error} ->
            throw({error, ?MODULE, {cantstop, Nodename, Error}})
    end,
    enit_log:info("waiting for node '~s' to go down...~n", [Nodename]),
    StopTimeout = enit_config:get(node, stop_timeout, Config, ?DEFAULT_STOP_TIMEOUT),
    receive
        {nodedown, Nodename} ->
            ok
    after
        StopTimeout ->
            throw({error, ?MODULE, {stop_timeout, Nodename, StopTimeout}})
    end.
