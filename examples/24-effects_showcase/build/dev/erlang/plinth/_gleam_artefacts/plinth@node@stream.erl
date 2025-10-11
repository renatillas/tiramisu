-module(plinth@node@stream).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/node/stream.gleam").
-export_type([writable/0]).

-type writable() :: any().


