-module(plinth@browser@worker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/worker.gleam").
-export_type([worker/0]).

-type worker() :: any().


