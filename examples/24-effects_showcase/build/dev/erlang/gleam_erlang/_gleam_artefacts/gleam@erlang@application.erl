-module(gleam@erlang@application).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/erlang/application.gleam").
-export([priv_directory/1]).
-export_type([start_type/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " An Erlang application is a collection of code that can be loaded into the\n"
    " Erlang virtual machine and even started and stopped if they define a\n"
    " start module and supervision tree. Each Gleam package is an Erlang\n"
    " application.\n"
).

-type start_type() :: normal |
    {takeover, gleam@erlang@node:node_()} |
    {failover, gleam@erlang@node:node_()}.

-file("src/gleam/erlang/application.gleam", 37).
?DOC(
    " Returns the path of an application's `priv` directory, where extra non-Gleam\n"
    " or Erlang files are typically kept. Each Gleam package is an Erlang\n"
    " application.\n"
    "\n"
    " Returns an error if no application was found with the given name.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " application.priv_directory(\"my_app\")\n"
    " // -> Ok(\"/some/location/my_app/priv\")\n"
    " ```\n"
).
-spec priv_directory(binary()) -> {ok, binary()} | {error, nil}.
priv_directory(Name) ->
    gleam_erlang_ffi:priv_directory(Name).
