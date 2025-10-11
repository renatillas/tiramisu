-module(gleam@io).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/io.gleam").
-export([print/1, print_error/1, println/1, println_error/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/gleam/io.gleam", 15).
?DOC(
    " Writes a string to standard output (stdout).\n"
    "\n"
    " If you want your output to be printed on its own line see `println`.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " io.print(\"Hi mum\")\n"
    " // -> Nil\n"
    " // Hi mum\n"
    " ```\n"
).
-spec print(binary()) -> nil.
print(String) ->
    gleam_stdlib:print(String).

-file("src/gleam/io.gleam", 31).
?DOC(
    " Writes a string to standard error (stderr).\n"
    "\n"
    " If you want your output to be printed on its own line see `println_error`.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```\n"
    " io.print_error(\"Hi pop\")\n"
    " // -> Nil\n"
    " // Hi pop\n"
    " ```\n"
).
-spec print_error(binary()) -> nil.
print_error(String) ->
    gleam_stdlib:print_error(String).

-file("src/gleam/io.gleam", 45).
?DOC(
    " Writes a string to standard output (stdout), appending a newline to the end.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " io.println(\"Hi mum\")\n"
    " // -> Nil\n"
    " // Hi mum\n"
    " ```\n"
).
-spec println(binary()) -> nil.
println(String) ->
    gleam_stdlib:println(String).

-file("src/gleam/io.gleam", 59).
?DOC(
    " Writes a string to standard error (stderr), appending a newline to the end.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " io.println_error(\"Hi pop\")\n"
    " // -> Nil\n"
    " // Hi pop\n"
    " ```\n"
).
-spec println_error(binary()) -> nil.
println_error(String) ->
    gleam_stdlib:println_error(String).
