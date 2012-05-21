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
