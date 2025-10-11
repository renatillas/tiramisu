-module(gleam@javascript@array).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/javascript/array.gleam").
-export_type([array/1]).

-type array(JVN) :: any() | {gleam_phantom, JVN}.


