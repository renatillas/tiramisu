-module(lustre@internals@mutable_map).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/internals/mutable_map.gleam").
-export([new/0, get/2, has_key/2, insert/3, delete/2, size/1, is_empty/1]).
-export_type([mutable_map/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type mutable_map(NXM, NXN) :: any() | {gleam_phantom, NXM, NXN}.

-file("src/lustre/internals/mutable_map.gleam", 21).
?DOC(false).
-spec new() -> mutable_map(any(), any()).
new() ->
    gleam@dict:new().

-file("src/lustre/internals/mutable_map.gleam", 27).
?DOC(false).
-spec get(mutable_map(NXS, NXT), NXS) -> {ok, NXT} | {error, nil}.
get(Map, Key) ->
    gleam@dict:get(Map, Key).

-file("src/lustre/internals/mutable_map.gleam", 33).
?DOC(false).
-spec has_key(mutable_map(NXY, any()), NXY) -> boolean().
has_key(Map, Key) ->
    gleam@dict:has_key(Map, Key).

-file("src/lustre/internals/mutable_map.gleam", 39).
?DOC(false).
-spec insert(mutable_map(NYC, NYD), NYC, NYD) -> mutable_map(NYC, NYD).
insert(Map, Key, Value) ->
    gleam@dict:insert(Map, Key, Value).

-file("src/lustre/internals/mutable_map.gleam", 49).
?DOC(false).
-spec delete(mutable_map(NYI, NYJ), NYI) -> mutable_map(NYI, NYJ).
delete(Map, Key) ->
    gleam@dict:delete(Map, Key).

-file("src/lustre/internals/mutable_map.gleam", 55).
?DOC(false).
-spec size(mutable_map(any(), any())) -> integer().
size(Map) ->
    gleam@dict:size(Map).

-file("src/lustre/internals/mutable_map.gleam", 59).
?DOC(false).
-spec is_empty(mutable_map(any(), any())) -> boolean().
is_empty(Map) ->
    gleam@dict:size(Map) =:= 0.
