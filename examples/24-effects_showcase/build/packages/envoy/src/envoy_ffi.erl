-module(envoy_ffi).

-export([get/1, set/2, unset/1, all/0]).

get(Key) ->
    case os:getenv(unicode:characters_to_list(Key)) of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

set(Key, Value) ->
    os:putenv(unicode:characters_to_list(Key), unicode:characters_to_list(Value)),
    nil.

unset(Key) ->
    os:unsetenv(unicode:characters_to_list(Key)),
    nil.

all() ->
    BinVars = lists:map(fun(VarString) ->
        [VarName, VarVal] = string:split(VarString, "="),
        {unicode:characters_to_binary(VarName), unicode:characters_to_binary(VarVal)}
    end, os:getenv()),
    maps:from_list(BinVars).
