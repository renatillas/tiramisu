-module(structures@bimultimap).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/structures/bimultimap.gleam").
-export([new/0, insert/3, get/2, get_val/2, delete/2, delete_val/2, delete_key_val/3]).
-export_type([bi_multi_map/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A bidirectional multi map (BiMultiMap)\n"
    " A data structure that stores associations between keys and values\n"
    " in both directions\n"
    " A key can point to multiple values\n"
    " A value can point to multiple keys\n"
    "\n"
    " For example, keys have association to values:\n"
    " ```\n"
    " animal → [cat, lion]\n"
    " pet → [cat]\n"
    " ```\n"
    " And values have reverse association to those keys\n"
    " ```\n"
    " cat → [animal, pet]\n"
    " lion → [animal]\n"
    " ```\n"
).

-opaque bi_multi_map(DYB, DYC) :: {bi_multi_map,
        gleam@dict:dict(DYB, gleam@set:set(DYC)),
        gleam@dict:dict(DYC, gleam@set:set(DYB))}.

-file("src/structures/bimultimap.gleam", 27).
-spec new() -> bi_multi_map(any(), any()).
new() ->
    {bi_multi_map, maps:new(), maps:new()}.

-file("src/structures/bimultimap.gleam", 32).
?DOC(" Insert a key an associated value\n").
-spec insert(bi_multi_map(DYE, DYF), DYE, DYF) -> bi_multi_map(DYE, DYF).
insert(Into, Key, Value) ->
    Direct = gleam@dict:upsert(
        erlang:element(2, Into),
        Key,
        fun(Option) -> case Option of
                none ->
                    gleam@set:from_list([Value]);

                {some, Existing} ->
                    gleam@set:insert(Existing, Value)
            end end
    ),
    Reverse = gleam@dict:upsert(
        erlang:element(3, Into),
        Value,
        fun(Option@1) -> case Option@1 of
                none ->
                    gleam@set:from_list([Key]);

                {some, Existing@1} ->
                    gleam@set:insert(Existing@1, Key)
            end end
    ),
    {bi_multi_map, Direct, Reverse}.

-file("src/structures/bimultimap.gleam", 51).
?DOC(" Get the associated values for a key\n").
-spec get(bi_multi_map(DYJ, DYK), DYJ) -> gleam@set:set(DYK).
get(From, Key) ->
    _pipe = gleam_stdlib:map_get(erlang:element(2, From), Key),
    gleam@result:unwrap(_pipe, gleam@set:new()).

-file("src/structures/bimultimap.gleam", 56).
?DOC(" Get the associated keys for a value\n").
-spec get_val(bi_multi_map(DYO, DYP), DYP) -> gleam@set:set(DYO).
get_val(From, Value) ->
    _pipe = gleam_stdlib:map_get(erlang:element(3, From), Value),
    gleam@result:unwrap(_pipe, gleam@set:new()).

-file("src/structures/bimultimap.gleam", 61).
-spec delete_in_dict(gleam@dict:dict(DYU, gleam@set:set(DYV)), DYU, DYV) -> gleam@dict:dict(DYU, gleam@set:set(DYV)).
delete_in_dict(Dict, Dict_key, Value) ->
    gleam@dict:upsert(Dict, Dict_key, fun(Option) -> case Option of
                none ->
                    gleam@set:new();

                {some, Existing} ->
                    gleam@set:delete(Existing, Value)
            end end).

-file("src/structures/bimultimap.gleam", 73).
?DOC(
    " Delete a key from the multimap\n"
    " This removes the association from this key to any values\n"
    " and from values to this key\n"
).
-spec delete(bi_multi_map(DYX, DYY), DYX) -> bi_multi_map(DYX, DYY).
delete(From, Key) ->
    Values = get(From, Key),
    Direct = gleam@dict:delete(erlang:element(2, From), Key),
    Reverse = gleam@set:fold(
        Values,
        erlang:element(3, From),
        fun(Acc, Value) -> delete_in_dict(Acc, Value, Key) end
    ),
    {bi_multi_map, Direct, Reverse}.

-file("src/structures/bimultimap.gleam", 88).
?DOC(
    " Delete a value from the multimap\n"
    " This removes association from keys to this value and\n"
    " from this value to keys\n"
).
-spec delete_val(bi_multi_map(DZC, DZD), DZD) -> bi_multi_map(DZC, DZD).
delete_val(From, Value) ->
    Keys = get_val(From, Value),
    Reverse = gleam@dict:delete(erlang:element(3, From), Value),
    Direct = gleam@set:fold(
        Keys,
        erlang:element(2, From),
        fun(Acc, Key) -> delete_in_dict(Acc, Key, Value) end
    ),
    {bi_multi_map, Direct, Reverse}.

-file("src/structures/bimultimap.gleam", 99).
?DOC(" Delete a specific combination of key and value\n").
-spec delete_key_val(bi_multi_map(DZH, DZI), DZH, DZI) -> bi_multi_map(DZH, DZI).
delete_key_val(From, Key, Value) ->
    Direct = delete_in_dict(erlang:element(2, From), Key, Value),
    Reverse = delete_in_dict(erlang:element(3, From), Value, Key),
    {bi_multi_map, Direct, Reverse}.
