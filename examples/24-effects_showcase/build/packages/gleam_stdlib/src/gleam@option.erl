-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/option.gleam").
-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type option(FG) :: {some, FG} | none.

-file("src/gleam/option.gleam", 59).
-spec reverse_and_prepend(list(FV), list(FV)) -> list(FV).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("src/gleam/option.gleam", 44).
-spec all_loop(list(option(FM)), list(FM)) -> option(list(FM)).
all_loop(List, Acc) ->
    case List of
        [] ->
            {some, lists:reverse(Acc)};

        [none | _] ->
            none;

        [{some, First} | Rest] ->
            all_loop(Rest, [First | Acc])
    end.

-file("src/gleam/option.gleam", 40).
?DOC(
    " Combines a list of `Option`s into a single `Option`.\n"
    " If all elements in the list are `Some` then returns a `Some` holding the list of values.\n"
    " If any element is `None` then returns`None`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " all([Some(1), Some(2)])\n"
    " // -> Some([1, 2])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " all([Some(1), None])\n"
    " // -> None\n"
    " ```\n"
).
-spec all(list(option(FH))) -> option(list(FH)).
all(List) ->
    all_loop(List, []).

-file("src/gleam/option.gleam", 80).
?DOC(
    " Checks whether the `Option` is a `Some` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_some(Some(1))\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_some(None)\n"
    " // -> False\n"
    " ```\n"
).
-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-file("src/gleam/option.gleam", 98).
?DOC(
    " Checks whether the `Option` is a `None` value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_none(Some(1))\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_none(None)\n"
    " // -> True\n"
    " ```\n"
).
-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-file("src/gleam/option.gleam", 116).
?DOC(
    " Converts an `Option` type to a `Result` type.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " to_result(Some(1), \"some_error\")\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " to_result(None, \"some_error\")\n"
    " // -> Error(\"some_error\")\n"
    " ```\n"
).
-spec to_result(option(GD), GG) -> {ok, GD} | {error, GG}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        none ->
            {error, E}
    end.

-file("src/gleam/option.gleam", 137).
?DOC(
    " Converts a `Result` type to an `Option` type.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " from_result(Ok(1))\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_result(Error(\"some_error\"))\n"
    " // -> None\n"
    " ```\n"
).
-spec from_result({ok, GJ} | {error, any()}) -> option(GJ).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        {error, _} ->
            none
    end.

-file("src/gleam/option.gleam", 158).
?DOC(
    " Extracts the value from an `Option`, returning a default value if there is none.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unwrap(Some(1), 0)\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " unwrap(None, 0)\n"
    " // -> 0\n"
    " ```\n"
).
-spec unwrap(option(GO), GO) -> GO.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-file("src/gleam/option.gleam", 179).
?DOC(
    " Extracts the value from an `Option`, evaluating the default function if the option is `None`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " lazy_unwrap(Some(1), fn() { 0 })\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_unwrap(None, fn() { 0 })\n"
    " // -> 0\n"
    " ```\n"
).
-spec lazy_unwrap(option(GQ), fun(() -> GQ)) -> GQ.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-file("src/gleam/option.gleam", 204).
?DOC(
    " Updates a value held within the `Some` of an `Option` by calling a given function\n"
    " on it.\n"
    "\n"
    " If the `Option` is a `None` rather than `Some`, the function is not called and the\n"
    " `Option` stays the same.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map(over: Some(1), with: fn(x) { x + 1 })\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " map(over: None, with: fn(x) { x + 1 })\n"
    " // -> None\n"
    " ```\n"
).
-spec map(option(GS), fun((GS) -> GU)) -> option(GU).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-file("src/gleam/option.gleam", 230).
?DOC(
    " Merges a nested `Option` into a single layer.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " flatten(Some(Some(1)))\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " flatten(Some(None))\n"
    " // -> None\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " flatten(None)\n"
    " // -> None\n"
    " ```\n"
).
-spec flatten(option(option(GW))) -> option(GW).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-file("src/gleam/option.gleam", 269).
?DOC(
    " Updates a value held within the `Some` of an `Option` by calling a given function\n"
    " on it, where the given function also returns an `Option`. The two options are\n"
    " then merged together into one `Option`.\n"
    "\n"
    " If the `Option` is a `None` rather than `Some` the function is not called and the\n"
    " option stays the same.\n"
    "\n"
    " This function is the equivalent of calling `map` followed by `flatten`, and\n"
    " it is useful for chaining together multiple functions that return `Option`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " then(Some(1), fn(x) { Some(x + 1) })\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " then(Some(1), fn(x) { Some(#(\"a\", x)) })\n"
    " // -> Some(#(\"a\", 1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " then(Some(1), fn(_) { None })\n"
    " // -> None\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " then(None, fn(x) { Some(x + 1) })\n"
    " // -> None\n"
    " ```\n"
).
-spec then(option(HA), fun((HA) -> option(HC))) -> option(HC).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-file("src/gleam/option.gleam", 300).
?DOC(
    " Returns the first value if it is `Some`, otherwise returns the second value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " or(Some(1), Some(2))\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " or(Some(1), None)\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " or(None, Some(2))\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " or(None, None)\n"
    " // -> None\n"
    " ```\n"
).
-spec 'or'(option(HF), option(HF)) -> option(HF).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-file("src/gleam/option.gleam", 331).
?DOC(
    " Returns the first value if it is `Some`, otherwise evaluates the given function for a fallback value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " lazy_or(Some(1), fn() { Some(2) })\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_or(Some(1), fn() { None })\n"
    " // -> Some(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_or(None, fn() { Some(2) })\n"
    " // -> Some(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " lazy_or(None, fn() { None })\n"
    " // -> None\n"
    " ```\n"
).
-spec lazy_or(option(HJ), fun(() -> option(HJ))) -> option(HJ).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-file("src/gleam/option.gleam", 352).
-spec values_loop(list(option(HR)), list(HR)) -> list(HR).
values_loop(List, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [none | Rest] ->
            values_loop(Rest, Acc);

        [{some, First} | Rest@1] ->
            values_loop(Rest@1, [First | Acc])
    end.

-file("src/gleam/option.gleam", 348).
?DOC(
    " Given a list of `Option`s,\n"
    " returns only the values inside `Some`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " values([Some(1), None, Some(3)])\n"
    " // -> [1, 3]\n"
    " ```\n"
).
-spec values(list(option(HN))) -> list(HN).
values(Options) ->
    values_loop(Options, []).
