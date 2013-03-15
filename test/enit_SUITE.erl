-module(enit_SUITE).
% CT Callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).
% Tests cases
-export([simple_config_cascading/1]).

-include("../include/enit.hrl").
% --------------------------------------------------------------------------------------------------
% -- help macros

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
         (Expected@@@, Actual@@@) ->
             ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                    [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
             false
     end)(Expected, Actual) orelse error(badmatch)).

-define(match(Guard, Expr),
        ((fun () ->
                  case (Expr) of
                      Guard -> ok;
                      V -> ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                                   [?FILE, ?LINE, ??Expr, ??Guard, V]),
                error(badmatch)
                  end
          end)())).

% -----------------------------------------------------------------
% -- common_test callbacks

all() ->
    [simple_config_cascading].

init_per_suite(Config) ->
    meck:new(file, [passthrough, no_link, unstick]),
    meck:new(filelib, [passthrough, no_link, unstick]),
    meck:expect(file, consult, fun consult/1),
    meck:expect(filelib, wildcard, fun wildcard/1),
    Config.

end_per_suite(_Config) ->
    meck:unload(file),
    meck:unload(filelib).

% --------------------------------------------------------------------------------------------------
% -- test cases

simple_config_cascading(_Config) ->
    DefaultApps = [kernel, sasl, stdlib],
    testcase("r", "c1", [], DefaultApps),
    testcase("r", "c2", ['extension-name-1'], [runtime_tools | DefaultApps]),
    testcase("r", "c3", ['extension-name-1', 'extension-name-2'], [runtime_tools, crypto | DefaultApps]),
    ok.

% --------------------------------------------------------------------------------------------------
% -- helpers

testcase(RelDir, ConfDir, Extensions, Applications) ->
    {ok, Release} = enit:get_release_info(RelDir, ConfDir, ""),
    ?equal(Extensions, Release#release.extensions),
    diff(Applications, Release#release.applications).

diff(Need, Exists) ->
    ?equal(Exists -- Need, []),
    ?equal(Need -- Exists, []).

consult("r/release.enit") ->
    {ok, [{release, name, [
        {vsn, "1.0"},
        {applications, [
            kernel,
            stdlib,
            sasl
        ]}
    ]}]};

consult("r1.config") -> default();
consult("c1.config") -> default();
consult("c2.config") ->
    {ok, [{extension, 'extension-name-1',
        [{applications, [runtime_tools]}]},
     {sasl_syslog, [
         {multiline, true}
            ]}
    ]};
consult("c3.config") ->
    {ok, [{extension, 'extension-name-2',
        [{applications, [crypto]}]},
     {sasl_syslog, [
         {enabled, false}
            ]},
     {crypto, [
         {unknown_parameter, true}
            ]}
    ]};
consult(_) ->
    throw('consult-cannt-be-used').

wildcard("r/*.config") -> ["r1.config"];
wildcard("c1/*.config") -> ["c1.config"];
wildcard("c2/*.config") -> ["c1.config", "c2.config"];
wildcard("c3/*.config") -> ["c1.config", "c2.config", "c3.config"];
wildcard(File) -> filelib:wildcard(File, "").

default() ->
    {ok, [
     {node, [
        {cookie, "monster"}
            ]},
     {sasl, [
         {sasl_error_logger, false},
         {utc_log, true}
            ]},
     {sasl_syslog, [
         {enabled, true},
         {multiline, false}
            ]}
    ]}.
