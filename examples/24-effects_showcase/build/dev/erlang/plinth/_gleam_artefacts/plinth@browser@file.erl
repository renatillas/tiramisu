-module(plinth@browser@file).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/file.gleam").
-export_type([file/0]).

-type file() :: any().


