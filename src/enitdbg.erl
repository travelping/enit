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
