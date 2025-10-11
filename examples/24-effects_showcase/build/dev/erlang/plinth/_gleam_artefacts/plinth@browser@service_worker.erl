-module(plinth@browser@service_worker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/service_worker.gleam").
-export_type([service_worker/0, global_scope/0, fetch_event/0, activate_event/0, request/0, response/0, registration/0]).

-type service_worker() :: any().

-type global_scope() :: any().

-type fetch_event() :: any().

-type activate_event() :: any().

-type request() :: any().

-type response() :: any().

-type registration() :: any().


