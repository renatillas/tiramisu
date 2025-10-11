-module(plinth@browser@message).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/message.gleam").
-export_type([message/0, client/0]).

-type message() :: any().

-type client() :: any().


