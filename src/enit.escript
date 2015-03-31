#!/usr/bin/env escript
%%! -hidden -connect_all false -smp disable -kernel inet_dist_use_interface {127,0,0,1}
-mode(compile).

main(["-v"]) ->
    io:setopts([{encoding, unicode}]),
    application:load(enit),
    SN = escript:script_name(),
    {ok, Version} = application:get_key(enit, vsn),
    io:format("~s version: ~s~n", [SN, Version]);

main([Command | Args]) ->
    io:setopts([{encoding, unicode}]),
    application:load(enit),

    %% don't start native dns resolver subprocess
    enit_posix:load_nif_or_die(),

    %% ensure epmd is running
    os:cmd("epmd -daemon"),
    run(Command, Args);
main(_) ->
    io:setopts([{encoding, unicode}]),
    SN = escript:script_name(),
    DebugInfo = debug_available(assert_loaded_redbug()),
    io:format("Usage: ~s <command> <args...>~n"
              "~n"
              "Commands:~n"
              "    startfg <release>      → start a release VM without detaching~n"
              "      --start-retries <n> (try starting applications <n> times, <n> can also be 'infinity')~n"
              "      --start-delay <m> (wait <m> seconds between retries)~n"
              "    list                   → list installed releases~n"
              "    stop <release>         → stop a release VM~n"
              "    reconfigure <release>  → dynamically apply config changes~n"
              %"    update <release>       → dynamicly update release~n"
              "    status <release>       → get status information on a release~n~n"
              "Commands for debugging:~n"
              "    initrel <release>      → initialize release~n"            
              "    gen_default_conf <release> → generate default configuraion for release~n"            
              "    remsh <release>        → open a remote shell into a release~n"
              "    traceip <port>         → start a TCP trace client (see dbg:trace_client/3)~n"
              "      -h, --host <host> (connect to host instead of localhost)~n"
              "    tracefile <file>       → read a trace file~n"
              "      -f, --follow (keep reading from file)~n"
              "For all commands with <release> there is flag:~n"
              "      -m, --match          → if no release with this name, find if exists~n"
              "                             first release, that matched~n"
              ++ DebugInfo,
              [SN]).

run("initrel", Argv) ->
  cli_command("initrel", Argv, 1, match_options());
run("gen_default_conf", Argv) ->
  cli_command("gen_default_conf", Argv, 1, match_options());
run("startfg", Argv) ->
    cli_command("startfg", Argv, 1, match_options() ++ syslog_options() ++ start_options());
run("stop", Argv) ->
    cli_command("stop", Argv, 1, match_options() ++ syslog_options());
run("reconfigure", Argv) ->
    cli_command("reconfigure", Argv, 1, match_options() ++ syslog_options());
run("remsh", Argv) ->
    cli_command("remsh", Argv, 1, match_options());
run("traceip", Argv) ->
    cli_command("traceip", Argv, 1, [{option, host, 1, ["-h", "--host"]}]);
run("tracefile", Argv) ->
    cli_command("tracefile", Argv, 1, [{option, follow, 1, ["-f", "--follow"]}]);
run("status", Argv) ->
    cli_command("status", Argv, 1, match_options());
run("list", Argv) ->
    cli_command("list", Argv, 0, []);
run(_, _) ->
    io:format("Error: unknown command (run with no arguments for help)~n", []),
    erlang:halt(255).

syslog_options() ->
    [{flag, syslog, ["-s", "--syslog"]}].

start_options() ->
    [{option, start_retries, 1, ["--start-retries"]},
     {option, start_delay, 1, ["--start-delay"]}].

match_options() ->
    [{flag, match, ["-m", "--match"]}].

cli_command(Command, Argv, ArgCount, OptionSpec) ->
    DebugOptions = [{option, debug_spec, all, ["--debug_spec"]} || assert_loaded_redbug()],
    case enit:parse_cmdline(Argv, DebugOptions ++ OptionSpec) of
        {error, Error} ->
            io:format("Error: invalid options: ~s~n", [enit:format_error(Error)]),
            erlang:halt(255);
        {ok, Options, Args} when length(Args) == ArgCount ->
            DebugOption = lists:keyfind(debug_spec, 1, Options),
            dbg_start(DebugOption),
            cli_command1(Command, Args, Options);
        {ok, _Options, _Args} ->
            io:format("Error: invalid number of arguments (command ~s expects ~b)~n", [Command, ArgCount]),
            erlang:halt(255)
    end.

cli_command1(Command, Args, Options) ->
    try apply(enit, list_to_atom("cli_" ++ Command), Args ++ [Options]) of
        {error, Error} ->
            enit_log:error("Error: ~s~n", [enit:format_error(Error)]),
            halt(1);
        _ ->
            halt(0)
    catch
        {error, Error} ->
            enit_log:error("Error: ~s~n", [enit:format_error(Error)]),
            halt(1);
        Class:Other ->
            enit_log:error("enit crashed: ~s:~p~n~p", [Class, Other, erlang:get_stacktrace()]),
            halt(10)
    end.

debug_available(false) -> "";
debug_available(true) -> "For debugging erlang functions:~n"
                         "     --debug_spec          → used as last option with a list of functions~n".

assert_loaded_redbug() ->
    case application:load(redbug) of
        ok ->
            true;
        {error, {already_loaded, _App}} ->
            true;
        _ ->
            false
    end.

dbg_start(false) -> ok;
dbg_start(DebugOption) ->
    [debug_spec | DebugSpecs] = tuple_to_list(DebugOption),
    redbug:start(DebugSpecs, [{time, infinity}, {msgs, 100000}]).
