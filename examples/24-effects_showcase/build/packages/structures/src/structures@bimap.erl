-module(structures@bimap).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/structures/bimap.gleam").
-export([new/0, get/2, get_val/2, delete/2, delete_val/2, insert/3]).
-export_type([bi_map/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A bidirectional map (BiMap)\n"
    " A data structure similar to a Dictionary but stores\n"
    " the association in both directions\n"
    "\n"
    " For example, keys have association to values:\n"
    " ```\n"
    " animal → cat\n"
    " color → red\n"
    " ```\n"
    " And values have reverse association to those keys\n"
    " ```\n"
    " cat → animal\n"
    " red → color\n"
    " ```\n"
    "\n"
).

-opaque bi_map(DUE, DUF) :: {bi_map,
        gleam@dict:dict(DUE, DUF),
        gleam@dict:dict(DUF, DUE)}.

-file("src/structures/bimap.gleam", 23).
-spec new() -> bi_map(any(), any()).
new() ->
    {bi_map, maps:new(), maps:new()}.

-file("src/structures/bimap.gleam", 42).
?DOC(" Get the associated value for a key\n").
-spec get(bi_map(DUM, DUN), DUM) -> {ok, DUN} | {error, nil}.
get(From, Key) ->
    gleam_stdlib:map_get(erlang:element(2, From), Key).

-file("src/structures/bimap.gleam", 47).
?DOC(" Get the associated key for a value\n").
-spec get_val(bi_map(DUS, DUT), DUT) -> {ok, DUS} | {error, nil}.
get_val(From, Value) ->
    gleam_stdlib:map_get(erlang:element(3, From), Value).

-file("src/structures/bimap.gleam", 52).
?DOC(" Delete a key and associated value from the bimap\n").
-spec delete(bi_map(DUY, DUZ), DUY) -> bi_map(DUY, DUZ).
delete(From, Key) ->
    Value = get(From, Key),
    Direct = gleam@dict:delete(erlang:element(2, From), Key),
    Reverse = case Value of
        {ok, Previous} ->
            gleam@dict:delete(erlang:element(3, From), Previous);

        {error, nil} ->
            erlang:element(3, From)
    end,
    {bi_map, Direct, Reverse}.

-file("src/structures/bimap.gleam", 66).
?DOC(" Delete a value and associated key from the bimap\n").
-spec delete_val(bi_map(DVD, DVE), DVE) -> bi_map(DVD, DVE).
delete_val(From, Value) ->
    Key = get_val(From, Value),
    Reverse = gleam@dict:delete(erlang:element(3, From), Value),
    Direct = case Key of
        {ok, Previous} ->
            gleam@dict:delete(erlang:element(2, From), Previous);

        {error, nil} ->
            erlang:element(2, From)
    end,
    {bi_map, Direct, Reverse}.

-file("src/structures/bimap.gleam", 28).
?DOC(" Insert a key an associated value\n").
-spec insert(bi_map(DUH, DUI), DUH, DUI) -> bi_map(DUH, DUI).
insert(Into, Key, Value) ->
    Without = begin
        _pipe = Into,
        _pipe@1 = delete(_pipe, Key),
        delete_val(_pipe@1, Value)
    end,
    Direct = begin
        _pipe@2 = erlang:element(2, Without),
        gleam@dict:insert(_pipe@2, Key, Value)
    end,
    Reverse = begin
        _pipe@3 = erlang:element(3, Without),
        gleam@dict:insert(_pipe@3, Value, Key)
    end,
    {bi_map, Direct, Reverse}.
