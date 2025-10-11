-module(justin).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/justin.gleam").
-export([snake_case/1, camel_case/1, pascal_case/1, kebab_case/1, sentence_case/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/justin.gleam", 130).
-spec add(list(binary()), binary()) -> list(binary()).
add(Words, Word) ->
    case Word of
        <<""/utf8>> ->
            Words;

        _ ->
            [Word | Words]
    end.

-file("src/justin.gleam", 137).
-spec is_upper(binary()) -> boolean().
is_upper(G) ->
    string:lowercase(G) /= G.

-file("src/justin.gleam", 95).
-spec split(list(binary()), boolean(), binary(), list(binary())) -> list(binary()).
split(In, Up, Word, Words) ->
    case In of
        [] when Word =:= <<""/utf8>> ->
            lists:reverse(Words);

        [] ->
            lists:reverse(add(Words, Word));

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

-file("src/justin.gleam", 89).
-spec split_words(binary()) -> list(binary()).
split_words(Text) ->
    _pipe = Text,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    split(_pipe@1, false, <<""/utf8>>, []).

-file("src/justin.gleam", 13).
?DOC(
    " Convert a string to a `snake_case`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " snake_case(\"Hello World\")\n"
    " // -> \"hello_world\"\n"
    " ```\n"
).
-spec snake_case(binary()) -> binary().
snake_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<"_"/utf8>>),
    string:lowercase(_pipe@2).

-file("src/justin.gleam", 29).
?DOC(
    " Convert a string to a `camelCase`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " camel_case(\"Hello World\")\n"
    " // -> \"helloWorld\"\n"
    " ```\n"
).
-spec camel_case(binary()) -> binary().
camel_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@list:index_map(_pipe@1, fun(Word, I) -> case I of
                0 ->
                    string:lowercase(Word);

                _ ->
                    gleam@string:capitalise(Word)
            end end),
    erlang:list_to_binary(_pipe@2).

-file("src/justin.gleam", 50).
?DOC(
    " Convert a string to a `PascalCase`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " pascal_case(\"Hello World\")\n"
    " // -> \"HelloWorld\"\n"
    " ```\n"
).
-spec pascal_case(binary()) -> binary().
pascal_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam@string:capitalise/1),
    erlang:list_to_binary(_pipe@2).

-file("src/justin.gleam", 66).
?DOC(
    " Convert a string to a `kebab-case`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " kabab_case(\"Hello World\")\n"
    " // -> \"hello-world\n"
    " ```\n"
).
-spec kebab_case(binary()) -> binary().
kebab_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<"-"/utf8>>),
    string:lowercase(_pipe@2).

-file("src/justin.gleam", 82).
?DOC(
    " Convert a string to a `Sentence case`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " sentence_case(\"hello-world\")\n"
    " // -> \"Hello world\n"
    " ```\n"
).
-spec sentence_case(binary()) -> binary().
sentence_case(Text) ->
    _pipe = Text,
    _pipe@1 = split_words(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<" "/utf8>>),
    gleam@string:capitalise(_pipe@2).
