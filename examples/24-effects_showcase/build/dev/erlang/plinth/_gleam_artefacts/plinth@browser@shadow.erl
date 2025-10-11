-module(plinth@browser@shadow).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/shadow.gleam").
-export_type([shadow_root/0, mode/0]).

-type shadow_root() :: any().

-type mode() :: open | closed.


