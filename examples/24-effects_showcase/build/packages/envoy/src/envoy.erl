-module(envoy).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/1, set/2, unset/1, all/0]).

-file("/Users/louis/src/gleam/envoy/src/envoy.gleam", 17).
-spec get(binary()) -> {ok, binary()} | {error, nil}.
get(Name) ->
    envoy_ffi:get(Name).

-file("/Users/louis/src/gleam/envoy/src/envoy.gleam", 33).
-spec set(binary(), binary()) -> nil.
set(Name, Value) ->
    envoy_ffi:set(Name, Value).

-file("/Users/louis/src/gleam/envoy/src/envoy.gleam", 51).
-spec unset(binary()) -> nil.
unset(Name) ->
    envoy_ffi:unset(Name).

-file("/Users/louis/src/gleam/envoy/src/envoy.gleam", 57).
-spec all() -> gleam@dict:dict(binary(), binary()).
all() ->
    envoy_ffi:all().
