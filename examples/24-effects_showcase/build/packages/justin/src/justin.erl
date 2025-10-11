-module(justin).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([snake_case/1, camel_case/1, pascal_case/1, kebab_case/1, sentence_case/1]).

-spec add(list(binary()), binary()) -> list(binary()).
add(Words, Word) ->
    case Word of
        <<""/utf8>> ->
            Words;

        _ ->
            [Word | Words]
    end.

-spec is_upper(binary()) -> boolean().
is_upper(G) ->
    gleam@string:lowercase(G) /= G.

-spec split(list(binary()), boolean(), binary(), list(binary())) -> list(binary()).
split(In, Up, Word, Words) ->
    case In of
        [] when Word =:= <<""/utf8>> ->
            gleam@list:reverse(Words);

        [] ->
            gleam@list:reverse(add(Words, Word));

        [<<"\n"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"\t"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"!"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"?"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"#"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"."/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"-"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<"_"/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [<<" "/utf8>> | In@1] ->
            split(In@1, false, <<""/utf8>>, add(Words, Word));

        [G | In@2] ->
            case is_upper(G) of
                false ->
                    split(In@2, false, <<Word/binary, G/binary>>, Words);

                true when Up ->
                    split(In@2, Up, <<Word/binary, G/binary>>, Words);

                true ->
                    split(In@2, true, G, add(Words, Word))
            end
    end.

-spec split_words(binary()) -> list(binary()).
split_words(Text) ->
    _pipe = Text,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    split(_pipe@1, false, <<""/utf8>>, []).

-spec snake_case(binary()) -> binary().
snake_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<"_"/utf8>>),
    gleam@string:lowercase(_pipe@2).

-spec camel_case(binary()) -> binary().
camel_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@list:index_map(_pipe@1, fun(Word, I) -> case I of
                0 ->
                    gleam@string:lowercase(Word);

                _ ->
                    gleam@string:capitalise(Word)
            end end),
    gleam@string:concat(_pipe@2).

-spec pascal_case(binary()) -> binary().
pascal_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam@string:capitalise/1),
    gleam@string:concat(_pipe@2).

-spec kebab_case(binary()) -> binary().
kebab_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<"-"/utf8>>),
    gleam@string:lowercase(_pipe@2).

-spec sentence_case(binary()) -> binary().
sentence_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<" "/utf8>>),
    gleam@string:capitalise(_pipe@2).
