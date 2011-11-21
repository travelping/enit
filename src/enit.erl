-module(enit).
-export([cli_list/0, cli_status/1, cli_startfg/1, cli_stop/1]).
-export([get_release_info/1, get_release_info/3, get_status/1, format_error/1]).
-export([to_str/1]).

-include("enit.hrl").

%% ----------------------------------------------------------------------------------------------------
%% -- CLI commands
cli_list() ->
    {ok, Dir} = application:get_env(enit, release_dir),
    case file:list_dir(Dir) of
        {ok, []} ->
            io:format("no releases~n");
        {ok, Releases} ->
            lists:foreach(fun (Release) ->
                                  case get_release_info(Release) of
                                      {ok, Info} ->
                                          case get_status(Info) of
                                              {ok, Status} ->
                                                  show_brief_info(Info, Status);
                                              {error, Error} ->
                                                  throw({error, {get_status, Error}})
                                          end;
                                      {error, Error} ->
                                          throw({error, {get_release_info, Error}})
                                  end
                          end, Releases);
        {error, Error} ->
            {error, {list_dir, Dir, Error}}
    end.

show_brief_info(#release{name = Name, version = Version, nodename = Nodename}, Status) ->
    case Status#status.alive of
        true ->
            RunDesc = "running";
        false ->
            RunDesc = "not running"
    end,
    io:format("* ~s-~s [~s] (~s)~n", [Name, Version, Nodename, RunDesc]).

cli_status(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            case get_status(Info) of
                {ok, Status} ->
                    show_long_info(Info, Status);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

show_long_info(Info, Status) ->
    show_table([{"Release:", Info#release.name},
                {"Version:", Info#release.version},
                {"Node:",    Info#release.nodename},
                {"Cookie:",  Info#release.cookie}]),
    io:nl(),
    show_status(Status).

show_status(#status{alive = false}) ->
    io:format("This node is currently offline.~n");
show_status(Status = #status{alive = true}) ->
    io:format("This node is currently online.~n~n"),
    show_table([{"OTP:", Status#status.otp_version},
                {"Pid:", Status#status.os_pid}]),
    show_table([{"Uptime:", format_duration(Status#status.uptime_seconds)}]),
    case Status#status.connected_nodes of
        [N] when N == node() ->
            ok;
        ConnNodes ->
            io:format("Connected Nodes:~n", []),
            show_table([{"", Node} || Node <- ConnNodes, Node /= node()])
    end,
    io:format("Running Applications:~n", []),
    show_table([{"", io_lib:format("~s-~s", [App,Vsn])} || {App, Vsn} <- Status#status.running_apps]),
    io:format("Memory Usage (erlang:memory/0):~n", []),
    show_table([{"", mem_item(Item), format_mem(Bytes)} || {Item, Bytes} <- Status#status.memory_info]).

mem_item(ets) -> "ETS:";
mem_item(atom_used) -> "Atom (used):";
mem_item(processes_used) -> "Processes (used):";
mem_item(Item) ->
    Str = atom_to_list(Item),
    string:to_upper([hd(Str)]) ++ [tl(Str)] ++ ":".

cli_startfg(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            case get_status(Info) of
                {ok, #status{alive = true}} ->
                    {error, {already_running, Release}};
                {ok, #status{alive = false}} ->
                    enit_vm:start(Info);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

cli_stop(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            case get_status(Info) of
                {ok, #status{alive = true}} ->
                    enit_vm:stop(Info);
                {ok, #status{alive = false}} ->
                    {error, {not_running, Release}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% ----------------------------------------------------------------------------------------------------
%% -- Release Info
-spec get_release_info(string()) -> {ok, #release{}} | {error, term()}.
get_release_info(ReleaseName) ->
    {ok, RelDir} = application:get_env(enit, release_dir),
    {ok, ConfigDir} = application:get_env(enit, config_dir),
    get_release_info(RelDir, ConfigDir, ReleaseName).

-spec get_release_info(file:name(), file:name(), string()) -> {ok, #release{}} | {error, term()}.
get_release_info(RelDir, ConfigDir, ReleaseName) ->
    Path = filename:join(RelDir, ReleaseName),
    RelFile = filename:join(Path, "release.enit"),
    case file:consult(RelFile) of
        {ok, [{release, _Name, Properties}]} ->
            Info1 = #release{name = ReleaseName, path = Path},
            case apply_properties(Properties, Info1) of
                {ok, Info2} ->
                    case get_config(RelDir, ConfigDir, ReleaseName) of
                        {ok, Config} ->
                            apply_config(Config, Info2);
                        Error ->
                            Error
                    end;
                {error, {badprop, Prop}} ->
                    {error, {bad_rel_prop, RelFile, Prop}}
            end;
        {ok, _Terms} ->
            {error, {badrel, RelFile}};
        {error, Error} when is_atom(Error) ->
            {error, {faccess, RelFile, Error}};
        {error, Einfo} ->
            {error, {consult, RelFile, Einfo}}
    end.

apply_properties([{vsn, Version} | Props], Info) ->
    apply_properties(Props, Info#release{version = Version});
apply_properties([{applications, Applications} | Props], Info) ->
    apply_properties(Props, Info#release{applications = Applications});
apply_properties([{Prop, _Value} | Props], Info) when is_atom(Prop) ->
    apply_properties(Props, Info);
apply_properties([_Term | _Props], _Info) ->
    {error, badprop};
apply_properties([], Info) ->
    {ok, Info}.

apply_config(Config, Info) ->
    NodeProps = proplists:get_value(node, Config, []),
    case proplists:is_defined(cookie, NodeProps) of
        true ->
            Nodename = proplists:get_value(nodename, NodeProps, gen_nodename(Info#release.name)),
            {ok, Info#release{cookie = to_atom(proplists:get_value(cookie, NodeProps)),
                              nodename = to_atom(Nodename),
                              config = Config}};
        false ->
            {error, {config_nocookie, Info#release.name, Info#release.version}}
    end.

to_atom(Str) when is_list(Str) -> list_to_atom(Str);
to_atom(Bin) when is_binary(Bin) -> binary_to_atom(Bin, utf8);
to_atom(Atm) when is_atom(Atm) -> Atm.

get_config(RelDir, ConfigDir, Release) ->
    BaseConfig = filename:join([RelDir, Release, "defaults.config"]),
    UserConfig = filename:join([ConfigDir, Release, "user.config"]),
    enit_config:read_files([BaseConfig, UserConfig]).

gen_nodename(Relname) ->
    {ok, Hostname} = inet:gethostname(),
    list_to_atom(Relname ++ "@" ++ Hostname).

%% ----------------------------------------------------------------------------------------------------
%% -- Release Status
-spec get_status(#release{}) -> #status{}.
get_status(#release{nodename = Node, cookie = Cookie}) ->
    maybe_start_network(),
    erlang:set_cookie(Node, Cookie),
    case is_node_online(Node, 3) of
        true ->
            WhichApplications = rpc(Node, application, which_applications, []),
            Apps = [{Name, Version} || {Name, _Desc, Version} <- WhichApplications],
            {UpTimeMillis, _} = rpc(Node, erlang, statistics, [wall_clock]),
            RemConfig = get_remote_config(Node, Apps, []),
            {ok, #status{alive = true,
                         otp_version = rpc(Node, erlang, system_info, [otp_release]),
                         uptime_seconds = UpTimeMillis div 1000,
                         memory_info = rpc(Node, erlang, memory, []),
                         os_pid = rpc(Node, os, getpid, []),
                         running_apps = Apps,
                         running_config = RemConfig,
                         connected_nodes = rpc(Node, erlang, nodes, [])}};
        false ->
            {ok, #status{alive = false, running_apps = [], running_config = [], connected_nodes = []}}
    end.

rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Error} ->
            throw({error, {badrpc, Node, Error}});
        Res ->
            Res
    end.

maybe_start_network() ->
    case is_alive() of
        true ->
            ok;
        false ->
            OwnNodename = gen_nodename("enit"),
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

get_remote_config(Node, [{App, _} | Rest], Acc) ->
    case rpc:call(Node, application, get_all_env, [App]) of
        {badrpc, Error} ->
            {error, {badrpc, Node, Error}};
        Env ->
            get_remote_config(Node, Rest, [{App, Env} | Acc])
    end;
get_remote_config(_Node, [], Acc) ->
    Acc.

%% ----------------------------------------------------------------------------------------------------
%% -- Misc
format_error(Error) ->
    lists:flatten(io_lib:format("~p", [Error])).

show_table(Table) ->
    show_table(Table, " ").
show_table(Table, ColSeparator) ->
    Widths = column_widths(Table),
    lists:foreach(fun (Row) ->
                          LastCol = tuple_size(Row),
                          lists:foldl(fun (Col, N) when N == LastCol ->
                                              io:put_chars([to_str(Col), $\n]); %% don't pad the last column
                                          (Col, N) ->
                                              io:put_chars([pad_str(to_str(Col), proplists:get_value(N, Widths)), ColSeparator]),
                                              N + 1
                                      end, 1, tuple_to_list(Row))
                  end, Table).

column_widths(Table) ->
    lists:foldl(fun ({_OneElementRow}, OuterAcc) ->
                        OuterAcc; %% ignore one-element rows in width calculation
                    (Row, OuterAcc) ->
                        {_, NewAcc} = lists:foldl(fun (Col, {N, InnerAcc}) ->
                                                          LastWidth = proplists:get_value(N, InnerAcc, 0),
                                                          ColWidth = iolist_size(to_str(Col)),
                                                          {N + 1, lists:keystore(N, 1, InnerAcc, {N, max(LastWidth, ColWidth)})}
                                                  end, {1, OuterAcc}, tuple_to_list(Row)),
                        NewAcc
                end, [], Table).

to_str(List) when is_list(List)  -> List;
to_str(Bin) when is_binary(Bin)  -> Bin;
to_str(Atm) when is_atom(Atm)    -> atom_to_list(Atm);
to_str(Int) when is_integer(Int) -> integer_to_list(Int);
to_str(Term) -> io_lib:format("~p", Term).

pad_str(Str, N) ->
    case iolist_size(Str) of
        Len when Len < N -> [Str, lists:duplicate(N - Len, $\s)];
        _                -> Str
    end.

format_duration(0) ->
    "0 sec";
format_duration(Seconds) ->
    format_duration(Seconds, [{86400, "d"}, {3600, "h"}, {60, "min"}, {1, "sec"}]).

format_duration(0, _) ->
    [];
format_duration(Seconds, [{N, Unit} | DispSpec]) ->
    if
        Seconds >= N ->
            Value = Seconds div N,
            Rest  = Seconds rem N,
            [integer_to_list(Value), Unit, " " | format_duration(Rest, DispSpec)];
        true ->
            format_duration(Seconds, DispSpec)
    end.

format_mem(Bytes) ->
    format_mem(Bytes, [{1099511627776, "TiB"}, {1073741824, "GiB"}, {1048576, "MiB"}, {1024, "KiB"}, {0, "B"}]).

format_mem(Bytes, [{S, _U} | R]) when Bytes =< S ->
    format_mem(Bytes, R);
format_mem(Bytes, [{S, U} | _R]) ->
    [integer_to_list(Bytes div S), " ", U].