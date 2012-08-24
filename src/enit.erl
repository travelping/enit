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

-module(enit).
-export([cli_list/1, cli_status/2, cli_startfg/2, cli_stop/2, cli_remsh/2,
         cli_traceip/2, cli_tracefile/2, cli_reconfigure/2]).
-export([get_release_info/1, get_release_info/3, format_error/1]).
-export([parse_cmdline/2, to_str/1]).

%% API for extern using
-export([configurate/2, configurate/3]).

-include("enit.hrl").

%% ----------------------------------------------------------------------------------------------------
%% -- CLI commands
cli_list(_Options) ->
    {ok, Dir} = application:get_env(enit, release_dir),
    case file:list_dir(Dir) of
        {ok, []} ->
            io:format("no releases~n");
        {ok, Releases} ->
            lists:foreach(fun (Release) ->
                                  case get_release_info(Release) of
                                      {ok, Info} ->
                                          case enit_remote:remote_get_status(Info) of
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

show_brief_info(#release{name = Name, version = Version, nodename = Nodename} = Release, Status) ->
    RunDesc = case Status#status.alive of
                  true ->
                      case check_all_running(Release, Status) of
                          true ->
                              "running";
                          {false, _} ->
                              "starting"
                      end;
                  false ->
                      "not running"
              end,
    io:format("* ~s ~s [~s] (~s)~n", [Name, Version, Nodename, RunDesc]).

cli_status(Release, Options) ->
    check_match(Release, Options, fun status_fun/1).

status_fun(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            case enit_remote:remote_get_status(Info) of
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
    show_status(Info, Status).

show_status(_Info, #status{alive = false}) ->
    io:format("This node is currently offline.~n");
show_status(Info, Status = #status{alive = true}) ->
    io:format("This node is currently online.~n"),
    Check = check_all_running(Info, Status),
    case Check of
        true ->
            ok;
        {false, Apps} ->
            io:format(user, "Applications are starting: ~p~n", [Apps])
    end,
    io:nl(),
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
    show_table([{"", App, Vsn} || {App, Vsn} <- lists:keysort(1, Status#status.running_apps)]),
    io:format("Memory Usage (erlang:memory/0):~n", []),
    show_table([{"", mem_item(Item), format_mem(Bytes)} || {Item, Bytes} <- Status#status.memory_info]).

mem_item(ets) -> "ETS:";
mem_item(atom_used) -> "Atom (used):";
mem_item(processes_used) -> "Processes (used):";
mem_item(Item) ->
    Str = atom_to_list(Item),
    string:to_upper([hd(Str)]) ++ [tl(Str)] ++ ":".

cli_startfg(Release, Options) ->
    enit_log:init(Options),
    check_match(Release, Options, fun(ReleaseName) -> startfg_fun(ReleaseName, Options) end).

startfg_fun(Release, Options) ->
    case get_release_info(Release) of
        {ok, Info} ->
            case enit_remote:remote_get_status(Info) of
                {ok, #status{alive = true}} ->
                    {error, {already_running, Release}};
                {ok, #status{alive = false}} ->
                    enit_vm:startfg(Info, Options);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%cli_update(Release, Options) ->
%    enit_log:init(Options),
%    check_match(Release, Options, fun(ReleaseName) -> update_fun(ReleaseName, Options) end).

%update_fun(Release, Options) ->
%    case get_release_info(Release) of
%        {ok, Info} ->
%            case enit_remote:remote_get_status(Info) of
%                {ok, #status{alive = true}} ->
%                    enit_remote:update(Info, Options);
%                {ok, #status{alive = false}} ->
%                    {error, {not_started, Release}};
%                Error ->
%                    Error
%            end;
%        Error ->
%            Error
%    end.

cli_stop(Release, Options) ->
    enit_log:init(Options),
    check_match(Release, Options, fun stop_fun/1).

stop_fun(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            case enit_remote:remote_get_status(Info) of
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

cli_reconfigure(Release, Options) ->
    enit_log:init(Options),
    check_match(Release, Options, fun reconfigure_fun/1).

reconfigure_fun(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            enit_remote:remote_config_change(Info);
        Error ->
            Error
    end.

cli_remsh(Release, Options) ->
    check_match(Release, Options, fun remsh_fun/1).

remsh_fun(Release) ->
    case get_release_info(Release) of
        {ok, Info} ->
            enit_vm:start_remsh(Info);
        Error ->
            Error
    end.

cli_traceip(PortString, Options) ->
    case catch (list_to_integer(PortString)) of
        {'EXIT', _} ->
            {error, {badport, PortString}};
        Port ->
            Host = proplists:get_value(host, Options, "127.0.0.1"),
            case try_resolve(Host) of
                {ok, AddrString} ->
                    enit_log:info("connecting TCP trace client to ~s:~b...~n", [AddrString, Port]),
                    join_pid(dbg:trace_client(ip, {AddrString, Port}));
                Error ->
                    Error
            end
    end.

try_resolve(Host) ->
    case inet:getaddr(Host, inet) of
        {ok, Addr} ->
            {ok, inet_parse:ntoa(Addr)};
        {error, Posix} ->
            io:format(standard_error, "unable to resolve ~s (~s)~n", [Host, Posix]),
            {error, {resolve, Posix}}
    end.

cli_tracefile(File, Options) ->
    case filelib:is_file(File) of
        true ->
            case proplists:get_value(follow, [Options], false) of
                false ->
                    join_pid(dbg:trace_client(file, File));
                true ->
                    join_pid(dbg:trace_client(follow_file, File))
            end;
        false ->
            {error, {faccess, File}}
    end.

join_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end.

% --------------------------------------------------------------------------------------------------
% -- API for extern using

configurate(Release, Options) ->
    configurate(Release, <<"all">>, Options).

configurate(Release, App, Options) ->
    application:load(enit),
    case check_match(Release, Options, fun get_release_info/1) of
        {ok, Info} ->
            case App of
                <<"all">> ->
                    enit_boot:apply_config(Info#release.config);
                _ ->
                    enit_boot:apply_config(Info#release.config, App)
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
            Nodename = proplists:get_value(nodename, NodeProps, enit_remote:gen_nodename(Info#release.name)),
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

%% ----------------------------------------------------------------------------------------------------
%% -- Misc

check_match(Release, Options, Fun) ->
    case Fun(Release) of
        {error, {faccess, _, enoent}} = Error ->
            case lists:member({match, true}, Options) of
                true ->
                    case match_release_name(Release) of
                        {match, FoundRelease} ->
                            Fun(FoundRelease);
                        nomatch ->
                            Error
                    end;
                false ->
                    Error
            end;
        OtherResult ->
            OtherResult
    end.

match_release_name(String) ->
    {ok, Dir} = application:get_env(enit, release_dir),
    case file:list_dir(Dir) of
        {ok, []} ->
            nomatch;
        {ok, Releases} ->
            match(String, Releases);
        _ ->
            nomatch
    end.

match(_String, []) -> nomatch;
match(String, [Name | Names]) ->
    case string:str(Name, String) of
        0 ->
            match(String, Names);
        _ ->
            {match, Name}
    end.

check_all_running(#release{applications = Applications}, #status{running_apps = RunningApplications}) ->
    NotRunning = lists:foldl(fun({App, _}, ConfiguratedApps) ->
                                     lists:delete(App, ConfiguratedApps)
                             end, Applications, RunningApplications),
    case NotRunning of
        [] ->
            true;
        _ ->
            {false, NotRunning}
    end.

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

%% ----------------------------------------------------------------------------------------------------
%% -- getopt
-type option() :: {option, Name::atom(), ArgSize::pos_integer(), [string()]}.
-type flag()   :: {flag, Name::atom(), [string()]}.
-spec parse_cmdline([string()], [option() | flag()]) -> {[tuple(), ...], [string(), ...]}.

parse_cmdline(Args, OptionDesc) ->
    try parse_options(Args, OptionDesc, false, [], []) of
        {Options, RemainingArgs} ->
            {ok, Options, RemainingArgs}
    catch
        throw:{error, Error} ->
            {error, {parse_cmdline, Error}}
    end.

parse_options([], _OptDesc, _NextIsArg, Result, Args) ->
    {Result, lists:reverse(Args)};
parse_options([Arg | Rest], OptDesc, NextIsArg, Result, Args) ->
    case Arg of
        "--" ->
            parse_options(Rest, OptDesc, true, Result, Args);
        "-"  ++ _Name when not NextIsArg ->
            {ThisOption, TheRest} = parse_option(Arg, Rest, OptDesc),
            NewResult = [ThisOption | Result],
            parse_options(TheRest, OptDesc, NextIsArg, NewResult, Args);
        _ ->
            parse_options(Rest, OptDesc, NextIsArg, Result, [Arg | Args])
    end.

parse_option(Option, Rest, OptDefs) ->
    case find_option(Option, OptDefs) of
        {option, Name, ArgSize, _Flags} ->
            case catch lists:split(ArgSize, Rest) of
                {'EXIT', _} ->
                    throw({error, {option_args, Option, ArgSize}});
                {OptionArgs, Remaining} ->
                    {list_to_tuple([Name | OptionArgs]), Remaining}
            end;
        {flag, Name, _Flags} ->
            {{Name, true}, Rest};
        undefined ->
            throw({error, {unknown_option, Option}})
    end.

find_option(_Option, []) -> undefined;
find_option(Option, [OptDef | Rest]) ->
    case lists:member(Option, element(tuple_size(OptDef), OptDef)) of
        false -> find_option(Option, Rest);
        true  -> OptDef
    end.
