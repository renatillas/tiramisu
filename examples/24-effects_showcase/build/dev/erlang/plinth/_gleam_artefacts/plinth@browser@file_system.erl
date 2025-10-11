-module(plinth@browser@file_system).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/plinth/browser/file_system.gleam").
-export_type([handle/1, d/0, f/0, writable_file_stream/0]).

-type handle(ALQK) :: any() | {gleam_phantom, ALQK}.

-type d() :: any().

-type f() :: any().

-type writable_file_stream() :: any().


