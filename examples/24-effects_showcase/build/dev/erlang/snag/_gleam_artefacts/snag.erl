-module(snag).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/snag.gleam").
-export([new/1, error/1, layer/2, context/2, map_error/2, pretty_print/1, line_print/1]).
-export_type([snag/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type snag() :: {snag, binary(), list(binary())}.

-file("src/snag.gleam", 39).
?DOC(
    " Create a new `Snag` with the given issue text.\n"
    "\n"
    " See also the `error` function for creating a `Snag` wrapped in a `Result`.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> line_print\n"
    " \"error: Not enough credit\"\n"
    " ```\n"
).
-spec new(binary()) -> snag().
new(Issue) ->
    {snag, Issue, []}.

-file("src/snag.gleam", 51).
?DOC(
    " Create a new `Snag` wrapped in a `Result` with the given issue text.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > error(\"Not enough credit\")\n"
    " Error(new(\"Not enough credit\"))\n"
    " ```\n"
).
-spec error(binary()) -> {ok, any()} | {error, snag()}.
error(Issue) ->
    {error, new(Issue)}.

-file("src/snag.gleam", 68).
?DOC(
    " Add additional contextual information to a `Snag`.\n"
    "\n"
    " See also the `context` function for adding contextual information to a `Snag`\n"
    " wrapped in a `Result`.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> layer(\"Unable to make purchase\")\n"
    " > |> line_print\n"
    " \"error: Unable to make purchase <- Not enough credit\"\n"
    " ```\n"
).
-spec layer(snag(), binary()) -> snag().
layer(Snag, Issue) ->
    {snag, Issue, [erlang:element(2, Snag) | erlang:element(3, Snag)]}.

-file("src/snag.gleam", 82).
?DOC(
    " Add additional contextual information to a `Snag` wrapped in a `Result`.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > error(\"Not enough credit\")\n"
    " > |> context(\"Unable to make purchase\")\n"
    " > |> result.map_error(line_print)\n"
    " Error(\"error: Unable to make purchase <- Not enough credit\")\n"
    " ```\n"
).
-spec context({ok, MFU} | {error, snag()}, binary()) -> {ok, MFU} |
    {error, snag()}.
context(Result, Issue) ->
    case Result of
        {ok, _} ->
            Result;

        {error, Snag} ->
            {error, layer(Snag, Issue)}
    end.

-file("src/snag.gleam", 102).
?DOC(
    " Maps the error type in a `Result` to a `Snag` given a describing function.\n"
    " The describing function should produce a human friendly string\n"
    " reprensentation of the error.\n"
    " \n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > my_app.read_file(\"api_key.txt\")\n"
    " > |> snag.map_error(my_app.describe_error)\n"
    " > |> snag.context(\"Could not load API key\")\n"
    " > |> snag.line_print\n"
    " \"error: Could not load API key <- File is locked\"\n"
    " ```\n"
).
-spec map_error({ok, MFX} | {error, MFY}, fun((MFY) -> binary())) -> {ok, MFX} |
    {error, snag()}.
map_error(Result, Describer) ->
    case Result of
        {ok, A} ->
            {ok, A};

        {error, B} ->
            _pipe = Describer(B),
            error(_pipe)
    end.

-file("src/snag.gleam", 137).
-spec pretty_print_cause(list(binary())) -> binary().
pretty_print_cause(Cause) ->
    _pipe = Cause,
    _pipe@1 = gleam@list:index_map(
        _pipe,
        fun(Line, Index) ->
            erlang:list_to_binary(
                [<<"  "/utf8>>,
                    erlang:integer_to_binary(Index),
                    <<": "/utf8>>,
                    Line,
                    <<"\n"/utf8>>]
            )
        end
    ),
    erlang:list_to_binary(_pipe@1).

-file("src/snag.gleam", 128).
?DOC(
    " Turn a snag into a multi-line string, optimised for readability.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> layer(\"Unable to make purchase\")\n"
    " > |> layer(\"Character creation failed\")\n"
    " > |> pretty_print\n"
    " \"error: Character creation failed\n"
    "\n"
    " cause:\n"
    "   0: Unable to make purchase\n"
    "   1: Not enough credit\n"
    " \"\n"
    " ```\n"
).
-spec pretty_print(snag()) -> binary().
pretty_print(Snag) ->
    Output = <<<<"error: "/utf8, (erlang:element(2, Snag))/binary>>/binary,
        "\n"/utf8>>,
    case erlang:element(3, Snag) of
        [] ->
            Output;

        Cause ->
            <<<<Output/binary, "\ncause:\n"/utf8>>/binary,
                (pretty_print_cause(Cause))/binary>>
    end.

-file("src/snag.gleam", 157).
?DOC(
    " Turn a snag into a single-line string, optimised for compactness. This may be\n"
    " useful for logging snags.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " > new(\"Not enough credit\")\n"
    " > |> layer(\"Unable to make purchase\")\n"
    " > |> layer(\"Character creation failed\")\n"
    " > |> line_print\n"
    " \"error: Character creation failed <- Unable to make purchase <- Not enough credit\"\n"
    " ```\n"
).
-spec line_print(snag()) -> binary().
line_print(Snag) ->
    _pipe = [gleam@string:append(<<"error: "/utf8>>, erlang:element(2, Snag)) |
        erlang:element(3, Snag)],
    gleam@string:join(_pipe, <<" <- "/utf8>>).
