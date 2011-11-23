-module(enit_vm).
-export([start/1, start/4, stop/1]).
-export([load_nif/0]).
-export([exec/1, getpwnam/1, getgrnam/1, setuid/1, setgid/1, syslog/3]).

-include("enit.hrl").
-define(DEFAULT_STOP_TIMEOUT, 10000).

load_nif() ->
    PrivDir = case code:priv_dir(enit) of
        {error, _} -> "priv";
        X -> X
    end,
    Lib = filename:join(PrivDir, "enit_posix"),
    erlang:load_nif(Lib, 0).

erl_binary() ->
    filename:join([code:root_dir(), "bin", "erl"]).

start(Rel = #release{}) ->
    start(Rel#release.name, Rel#release.nodename, Rel#release.cookie, Rel#release.config).

start(RelName, NodeName, Cookie, Config) ->
    {ok, RelPath} = application:get_env(enit, release_dir),
    {ok, ConfPath} = application:get_env(enit, config_dir),
    DefaultArgs = ["-noshell",
                   "-sname", atom_to_list(NodeName),
                   "-setcookie", atom_to_list(Cookie),
                   "-run", "enit_boot", "start", RelPath, ConfPath, RelName],
    ConfigArgs = build_erl_args(Config),

    %% disconnect distribution, because epmd gets confused if we don't
    net_kernel:stop(),

    case set_config_user_and_group(Config) of
        ok ->
            exec([erl_binary()] ++ DefaultArgs ++ ConfigArgs);
        {error, Error} ->
            {error, Error}
    end.

set_config_user_and_group(Config) ->
    case enit_config:get(node, run_as_group, Config) of
        undefined ->
            set_config_user(Config);
        Group ->
            case getgrnam(Group) of
                {error, Error} ->
                    {getgrnam, Group, Error};
                {ok, Grp} ->
                    case setgid(proplists:get_value(gid, Grp)) of
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
            case getpwnam(User) of
                {error, Error} ->
                    {getpwnam, User, Error};
                {ok, Pwd} ->
                    case setuid(proplists:get_value(uid, Pwd)) of
                        ok ->
                            ok;
                        {error, Error} ->
                            {error, {setuid, User, Error}}
                    end
            end
    end.

build_erl_args([{kernel, Params} | R]) ->
    lists:flatmap(fun ({K, V}) ->
                          ["-kernel", atom_to_list(K), lists:flatten(io_lib:format("~p", [V]))]
                  end, Params) ++ build_erl_args(R);
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

stop(#release{nodename = Nodename, config = Config}) ->
    erlang:monitor_node(Nodename, true),
    case rpc:call(Nodename, init, stop, []) of
        ok ->
            ok;
        {badrpc, Error} ->
            throw({error, ?MODULE, {cantstop, Nodename, Error}})
    end,
    io:format("waiting for node '~s' to go down...~n", [Nodename]),
    StopTimeout = enit_config:get(node, stop_timeout, Config, ?DEFAULT_STOP_TIMEOUT),
    receive
        {nodedown, Nodename} ->
            ok
    after
        StopTimeout ->
            throw({error, ?MODULE, {stop_timeout, Nodename, StopTimeout}})
    end.

-spec exec([string(), ...]) -> {error, file:posix()} | no_return().
exec(_Argv) ->
    error(nif_not_loaded).

-spec getpwnam(nonempty_string()) -> {ok, [{atom(), term()}]} | {error, file:posix()}.
getpwnam(_Name) ->
    error(nif_not_loaded).

-spec getgrnam(nonempty_string()) -> {ok, [{atom(), term()}]} | {error, file:posix()}.
getgrnam(_Name) ->
    error(nif_not_loaded).

-spec setuid(non_neg_integer()) -> ok | {error, file:posix()}.
setuid(_User) ->
    error(nif_not_loaded).

-spec setgid(non_neg_integer()) -> ok | {error, file:posix()}.
setgid(_Group) ->
    error(nif_not_loaded).

-spec syslog(sasl_syslog:severity(), io:format(), [term()]) -> ok.
syslog(Level, Format, Data) ->
    Msg = lists:flatten(io_lib:format(Format, Data)),
    syslog(severity_int(Level), Msg).

-spec syslog(non_neg_integer(), string()) -> ok.
syslog(_Level, _Msg) ->
    error(nif_not_loaded).

severity_int(I) when is_integer(I), I >= 0, I =< 7 -> I;
severity_int(emergency)                            -> 0;
severity_int(emerg)                                -> 0;
severity_int(alert)                                -> 1;
severity_int(critical)                             -> 2;
severity_int(crit)                                 -> 2;
severity_int(error)                                -> 3;
severity_int(err)                                  -> 3;
severity_int(warning)                              -> 4;
severity_int(notice)                               -> 5;
severity_int(informational)                        -> 6;
severity_int(info)                                 -> 6;
severity_int(debug)                                -> 7.

