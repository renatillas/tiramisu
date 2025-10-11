-module(plinth@browser@storage).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/storage.gleam").
-export_type([storage_manager/0, estimate/0]).

-type storage_manager() :: any().

-type estimate() :: {estimate, integer(), integer()}.


