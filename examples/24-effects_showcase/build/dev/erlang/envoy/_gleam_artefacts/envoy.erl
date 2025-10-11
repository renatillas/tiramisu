-module(envoy).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/envoy.gleam").
-export([get/1, set/2, unset/1, all/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/envoy.gleam", 17).
?DOC(
    " Get an environment variable by name.\n"
    "\n"
    " ```gleam\n"
    " get(\"HOME\")\n"
    " // -> Ok(\"/home/lucy\")\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " get(\"WORLD_PEACE\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec get(binary()) -> {ok, binary()} | {error, nil}.
get(Name) ->
    envoy_ffi:get(Name).

-file("src/envoy.gleam", 33).
?DOC(
    " Set an environment variable.\n"
    "\n"
    " ```gleam\n"
    " get(\"FAVOURITE_COLOUR\")\n"
    " // -> Error(Nil)\n"
    " set(\"FAVOURITE_COLOUR\", \"Pink\")\n"
    "\n"
    " get(\"FAVOURITE_COLOUR\")\n"
    " // -> Ok(\"Pink\")\n"
    " ```\n"
).
-spec set(binary(), binary()) -> nil.
set(Name, Value) ->
    envoy_ffi:set(Name, Value).

-file("src/envoy.gleam", 51).
?DOC(
    " Unset an environment variable.\n"
    "\n"
    " ```gleam\n"
    " set(\"FAVOURITE_COLOUR\", \"Pink\")\n"
    "\n"
    " get(\"FAVOURITE_COLOUR\")\n"
    " // -> Ok(\"Pink\")\n"
    "\n"
    " unset(\"FAVOURITE_COLOUR\")\n"
    "\n"
    " get(\"FAVOURITE_COLOUR\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec unset(binary()) -> nil.
unset(Name) ->
    envoy_ffi:unset(Name).

-file("src/envoy.gleam", 57).
?DOC(" Get all the environment variables.\n").
-spec all() -> gleam@dict:dict(binary(), binary()).
all() ->
    envoy_ffi:all().
