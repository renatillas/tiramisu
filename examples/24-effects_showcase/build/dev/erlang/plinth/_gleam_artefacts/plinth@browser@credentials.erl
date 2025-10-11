-module(plinth@browser@credentials).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/credentials.gleam").
-export_type([credentials_container/0]).

-type credentials_container() :: any().


