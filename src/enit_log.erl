-module(enit_log).
-export([init/1, error/2, info/2]).

init(Options) ->
    put(enit_log_output_function, make_output_function(Options)).

info(Fmt, Args) ->
    apply(get_output_function(), [info, Fmt, Args]).
error(Fmt, Args) ->
    apply(get_output_function(), [error, Fmt, Args]).

get_output_function() ->
    case get(enit_log_output_function) of
        Fun when is_function(Fun) ->
            Fun;
        _ ->
            fun default_output_function/3
    end.

default_output_function(_Sev, Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

make_output_function(Options) ->
    case proplists:get_value(syslog, Options) of
        true ->
            enit_posix:load_nif_or_die(),
            fun (Sev, Fmt, Args) ->
                io:format(standard_error, "ENIT: " ++ Fmt, Args),
                syslog_lines(Sev, Fmt, Args)
            end;
        _ ->
            fun default_output_function/3
    end.

syslog_lines(Sev, Fmt, Args) ->
    lists:foreach(fun ([]) ->
                          ok;
                      (Line) ->
                          enit_posix:syslog(Sev, "~s", [Line])
                  end, re:split(io_lib:format(Fmt, Args), "\\n", [{return, list}])).
