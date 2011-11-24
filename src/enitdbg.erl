-module(enitdbg).
-export([ip/1, file/1, mod/1, fn/2]).

-include_lib("stdlib/include/ms_transform.hrl").

ip(Port) ->
    dbg:tracer(port, dbg:trace_port(ip, Port)).

file(File) ->
    dbg:tracer(port, dbg:trace_port(file, File)).

mod(Module) ->
    dbg:tpl(Module, dbg:fun2ms(fun (_) -> return_trace() end)).

fn(Module, Function) ->
    dbg:tpl(Module, Function, dbg:fun2ms(fun (_) -> return_trace() end)).