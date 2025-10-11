-module(argv).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([load/0]).
-export_type([argv/0]).

-type argv() :: {argv, binary(), binary(), list(binary())}.

-spec load() -> argv().
load() ->
    {Runtime, Program, Arguments} = argv_ffi:load(),
    {argv, Runtime, Program, Arguments}.
