-module(mist@internal@next).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/next.gleam").
-export_type([next/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type next(MRV, MRW) :: {continue,
        MRV,
        gleam@option:option(gleam@erlang@process:selector(MRW))} |
    normal_stop |
    {abnormal_stop, binary()}.


