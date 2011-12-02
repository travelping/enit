-module(enit_vm).
-export([startfg/2, startfg/5, start_remsh/1, stop/1]).

-include("enit.hrl").
-include("enit_posix.hrl").

-define(DEFAULT_STOP_TIMEOUT, 10000).

erl_binary() ->
    filename:join([code:root_dir(), "bin", "erl"]).

startfg(Rel = #release{}, Options) ->
    startfg(Rel#release.name, Rel#release.nodename, Rel#release.cookie, Rel#release.config, Options).

startfg(RelName, NodeName, Cookie, Config, Options) ->
    {ok, RelPath} = application:get_env(enit, release_dir),
    {ok, ConfPath} = application:get_env(enit, config_dir),
    OptionString = lists:flatten(io_lib:format("~p", [Options])),

    DefaultArgs = ["-noshell",
                   "-sname", atom_to_list(NodeName),
                   "-setcookie", atom_to_list(Cookie),
                   "-run", "enit_boot", "start", RelPath, ConfPath, RelName, OptionString],
    ConfigArgs = build_erl_args(Config),

    case set_config_user_and_group(Config) of
        ok ->
            exec_erlang(ConfigArgs ++ DefaultArgs);
        {error, Error} ->
            {error, Error}
    end.

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
