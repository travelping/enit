-module(enit_config).
-export([read_files/1, merge/2, diff/2, get/3, get/4]).
-export_type([config/0]).

-include("enit.hrl").

%% config is a key-sorted proplist, the values themselves being key-sorted proplists.
-type config() :: [{atom(), [{atom(), term()}, ...]}, ...].

get(App, Key, Config) ->
    get(App, Key, Config, undefined).
get(App, Key, Config, Default) ->
    proplists:get_value(Key, proplists:get_value(App, Config, []), Default).

%% ----------------------------------------------------------------------------------------------------
%% -- Reading Config
-spec read_files([file:name(), ...]) -> {ok, config()} | {error, {consult_config, file:name(), term()}}.
read_files(Files) ->
    read_files(Files, []).

read_files([File | R], Acc) ->
    case file:consult(File) of
        {ok, Terms} ->
            read_files(R, merge(Acc, dedup_keys(Terms)));
        {error, enoent} ->
            read_files(R, Acc);
        {error, Error} ->
            {error, {consult_config, File, Error}}
    end;
read_files([], Acc) ->
    {ok, Acc}.

%% ----------------------------------------------------------------------------------------------------
%% -- Merge/Diff
-spec merge(config(), config()) -> config().
merge([{K1, V1} | R1], [{K2, V2} | R2]) when K1 == K2 ->
    [{K1, plmerge(V1, V2)} | merge(R1, R2)];
merge([{K1, V1} | R1], [{K2, V2} | R2]) when K1 < K2 ->
    [{K1, V1} | merge(R1, [{K2, V2} | R2])];
merge([{K1, V1} | R1], [{K2, V2} | R2]) when K1 > K2 ->
    [{K2, V2} | merge([{K1, V1} | R1], R2)];
merge([], []) ->
    [];
merge([], R2) ->
    R2;
merge(R1, []) ->
    R1.

-spec diff(config(), config()) -> [{atom(), {Added::config(), Removed::config(), Changed::config()}}, ...].
diff([{App1, Env1} | R1], [{App2, Env2} | R2]) when App1 == App2 ->
    case pldiff(Env1, Env2) of
        {[], [], []} ->
            diff(R1, R2);
        Changes ->
            [{App1, Changes} | diff(R1, R2)]
    end;
diff([{App1, Env1} | R1], [{App2, Env2} | R2]) when App1 < App2 ->
    %% everything in Env1 was 'added' because App1 is not in Config2
    [{App1, {Env1, [], []}} | diff(R1, [{App2, Env2} | R2])];
diff([{App1, Env1} | R1], [{App2, Env2} | R2]) when App1 > App2 ->
    %% everything in Env2 was 'added' because App2 is not in Config1
    [{App2, {Env2, [], []}} | diff([{App1, Env1} | R1], R2)];
diff([], []) ->
    [];
diff([], R2) ->
    lists:map(fun ({App2, Env2}) -> {App2, {Env2, [], []}} end, R2);
diff(R1, []) ->
    lists:map(fun ({App1, Env1}) -> {App1, {Env1, [], []}} end, R1).

dedup_keys(Proplist) ->
    dedup_keys1(lists:keysort(1, Proplist)).

dedup_keys1(Proplist) ->
    lists:foldr(fun ({K, V1}, [{K, V2} | R]) ->
                        [{K, plmerge(V1, V2)} | R];
                    ({K, V}, R) ->
                        [{K, V} | R]
                end, [], Proplist).

%% merge proplists
plmerge(List1, List2) ->
    M1 = [{K, V} || {K, V} <- List1, not proplists:is_defined(K, List2)],
    lists:keysort(1, M1 ++ List2).

%% diff _sorted_ proplists
pldiff(List1, List2) ->
    pldiff(lists:keysort(1, List1), lists:keysort(1, List2), [], [], []).

pldiff([{K1, V1} | R1], [{K2, V2} | R2], Added, Removed, Changed) ->
    if
        K1 =:= K2, V1 /= V2 ->
            pldiff(R1, R2, Added, Removed, [{K1, V2} | Changed]);
        K1 =:= K2 ->
            pldiff(R1, R2, Added, Removed, Changed);
        K1 < K2 ->
            pldiff(R1, [{K2, V2} | R2], Added, [{K1, V1} | Removed], Changed);
        K1 > K2 ->
            pldiff([{K1, V1} | R1], R2, [{K2, V2} | Added], Removed, Changed)
    end;
pldiff([], [], Added, Removed, Changed) ->
    {lists:reverse(Added), lists:reverse(Removed), lists:reverse(Changed)};
pldiff(R1, [], Added, Removed, Changed) ->
    {lists:reverse(Added), lists:reverse(R1 ++ Removed), lists:reverse(Changed)};
pldiff([], R2, Added, Removed, Changed) ->
    {lists:reverse(R2 ++ Added), lists:reverse(Removed), lists:reverse(Changed)}.
