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

-module(enit_init).

-export([create_defaults/1, apply_conf/1, get_mappings/1]).

-include("enit.hrl").

%%
%% API functions
%%

create_defaults(Release) ->
    release_mkdirs(Release),
    User = "user",
    Group = "group",
    Default_config = [{node, [
                        {run_as_user, User},
                        {run_as_group, Group},
                        {smp, enabled}
                     ]},
                     {kernel, [
                        {start_timer, true},
                        {start_disk_log, true}
                     ]},
                     {sasl, [
                        {sasl_error_logger, false},
                        {utc_log, true}
                     ]}],
    write_terms(filename:join(["/var/lib/enit", Release, "default.config"]), Default_config),

    Release_enit = [{release, list_to_atom(Release), [
                        {vsn, "1.0"},
                        {applications, [kernel, stdlib, crypto]}
                   ]}],
    write_terms(filename:join(["/var/lib/enit", Release, "release.enit"]), Release_enit),

    User_config = [{node, [{cookie, "monster"}]}, {kernel, []}],
    write_terms(filename:join(["/etc/enit", Release, "user.config"]), User_config),
    io:format("The default configuration has been successfully generated~n").

apply_conf(Info) ->
    {ok, ConfDir} = application:get_env(enit, config_dir),
    Conf = cuttlefish_conf:files(filelib:wildcard(filename:join([ConfDir, Info#release.name, "*.conf"]))),
    Schemas = get_schemas(Info),
    case cuttlefish_generator:map(Schemas, Conf) of
        {error, Type, Error} -> 
            {error, Type, Error};
        Cfg -> 
            Config = Info#release.config,
            Info#release{config = enit_config:unsorted_merge(Config, Cfg)}
    end.

get_mappings(Apps) ->
    get_mappings(Apps, []).

get_mappings([], Mappings) ->
    Mappings;
get_mappings([App|Tail], Mappings) ->
    Schema = filename:join(code:priv_dir(App), "*.schema"),
    {_, Maps, _} = cuttlefish_schema:files(filelib:wildcard(Schema)),
    get_mappings(Tail, lists:append(Mappings, Maps)).

%%
%% Internal function
%%

get_schemas(Info) ->
    PrivSchemas = lists:flatmap(fun(App) -> 
                                    case code:priv_dir(App) of
                                        {error, bad_name} -> [];
                                        PrivDir -> filelib:wildcard(PrivDir ++ "/*.schema")
                                    end
                                end, Info#release.applications),
    cuttlefish_schema:files(PrivSchemas).

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~40tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).

release_mkdirs(Release) ->
    Dirs = [filename:join(["/var/lib/enit", Release]), filename:join(["/etc/enit", Release])],
    [begin
        filelib:ensure_dir(Dir),
        case file:make_dir(Dir) of
            ok -> ok;
            {error, Reason} -> {error, Reason}
        end
    end || Dir <- Dirs].
