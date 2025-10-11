-module(gleam@javascript@promise).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/javascript/promise.gleam").
-export_type([promise/1]).

-type promise(JWX) :: any() | {gleam_phantom, JWX}.


