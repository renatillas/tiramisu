-module(plinth@browser@window).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/window.gleam").
-export_type([window/0, wake_lock_sentinal/0, request_i_d/0]).

-type window() :: any().

-type wake_lock_sentinal() :: any().

-type request_i_d() :: any().


