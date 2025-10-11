-module(booklet).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/booklet.gleam").
-export([new/1, get/1, update/2, set/2]).
-export_type([booklet/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type booklet(I) :: any() | {gleam_phantom, I}.

-file("src/booklet.gleam", 16).
?DOC(" Create a new Booklet, storing an initial value in it.\n").
-spec new(J) -> booklet(J).
new(Initial_value) ->
    booklet_ffi:make(Initial_value).

-file("src/booklet.gleam", 21).
?DOC(" Get the current value stored in the Booklet.\n").
-spec get(booklet(L)) -> L.
get(Booklet) ->
    booklet_ffi:get(Booklet).

-file("src/booklet.gleam", 32).
?DOC(
    " Atomically update the value stored in the booklet, and return the final value.\n"
    "\n"
    " Updates are guaranteed to be atomic and serialisable. If multiple processes\n"
    " try to update the value at the same time, all but one process  have to retry\n"
    " their update operation (by calling the updater again) until the update succeeds.\n"
    "\n"
    " Note that Javascript is single-threaded, so no extra guards are in place.\n"
).
-spec update(booklet(N), fun((N) -> N)) -> N.
update(Booklet, Updater) ->
    booklet_ffi:update(Booklet, Updater).

-file("src/booklet.gleam", 38).
?DOC(" Atomically replace the value stored in the booklet.\n").
-spec set(booklet(P), P) -> nil.
set(Booklet, New_value) ->
    booklet_ffi:update(Booklet, fun(_) -> New_value end),
    nil.
