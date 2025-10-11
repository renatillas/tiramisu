-module(gleam@string).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/string.gleam").
-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, slice/3, crop/2, drop_end/2, contains/2, starts_with/2, ends_with/2, split_once/2, append/2, concat/1, repeat/2, join/2, pad_start/3, pad_end/3, trim_start/1, trim_end/1, trim/1, pop_grapheme/1, to_graphemes/1, split/2, to_utf_codepoints/1, from_utf_codepoints/1, utf_codepoint/1, utf_codepoint_to_int/1, to_option/1, first/1, last/1, capitalise/1, inspect/1, byte_size/1, drop_start/2]).
-export_type([direction/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Strings in Gleam are UTF-8 binaries. They can be written in your code as\n"
    " text surrounded by `\"double quotes\"`.\n"
).

-type direction() :: leading | trailing.

-file("src/gleam/string.gleam", 23).
?DOC(
    " Determines if a `String` is empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_empty(\"\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_empty(\"the world\")\n"
    " // -> False\n"
    " ```\n"
).
-spec is_empty(binary()) -> boolean().
is_empty(Str) ->
    Str =:= <<""/utf8>>.

-file("src/gleam/string.gleam", 51).
?DOC(
    " Gets the number of grapheme clusters in a given `String`.\n"
    "\n"
    " This function has to iterate across the whole string to count the number of\n"
    " graphemes, so it runs in linear time. Avoid using this in a loop.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " length(\"Gleam\")\n"
    " // -> 5\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length(\"ß↑e̊\")\n"
    " // -> 3\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length(\"\")\n"
    " // -> 0\n"
    " ```\n"
).
-spec length(binary()) -> integer().
length(String) ->
    string:length(String).

-file("src/gleam/string.gleam", 65).
?DOC(
    " Reverses a `String`.\n"
    "\n"
    " This function has to iterate across the whole `String` so it runs in linear\n"
    " time. Avoid using this in a loop.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " reverse(\"stressed\")\n"
    " // -> \"desserts\"\n"
    " ```\n"
).
-spec reverse(binary()) -> binary().
reverse(String) ->
    _pipe = String,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = string:reverse(_pipe@1),
    unicode:characters_to_binary(_pipe@2).

-file("src/gleam/string.gleam", 86).
?DOC(
    " Creates a new `String` by replacing all occurrences of a given substring.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " replace(\"www.example.com\", each: \".\", with: \"-\")\n"
    " // -> \"www-example-com\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " replace(\"a,b,c,d,e\", each: \",\", with: \"/\")\n"
    " // -> \"a/b/c/d/e\"\n"
    " ```\n"
).
-spec replace(binary(), binary(), binary()) -> binary().
replace(String, Pattern, Substitute) ->
    _pipe = String,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = gleam_stdlib:string_replace(_pipe@1, Pattern, Substitute),
    unicode:characters_to_binary(_pipe@2).

-file("src/gleam/string.gleam", 111).
?DOC(
    " Creates a new `String` with all the graphemes in the input `String` converted to\n"
    " lowercase.\n"
    "\n"
    " Useful for case-insensitive comparisons.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " lowercase(\"X-FILES\")\n"
    " // -> \"x-files\"\n"
    " ```\n"
).
-spec lowercase(binary()) -> binary().
lowercase(String) ->
    string:lowercase(String).

-file("src/gleam/string.gleam", 127).
?DOC(
    " Creates a new `String` with all the graphemes in the input `String` converted to\n"
    " uppercase.\n"
    "\n"
    " Useful for case-insensitive comparisons and VIRTUAL YELLING.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " uppercase(\"skinner\")\n"
    " // -> \"SKINNER\"\n"
    " ```\n"
).
-spec uppercase(binary()) -> binary().
uppercase(String) ->
    string:uppercase(String).

-file("src/gleam/string.gleam", 145).
?DOC(
    " Compares two `String`s to see which is \"larger\" by comparing their graphemes.\n"
    "\n"
    " This does not compare the size or length of the given `String`s.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " compare(\"Anthony\", \"Anthony\")\n"
    " // -> order.Eq\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " compare(\"A\", \"B\")\n"
    " // -> order.Lt\n"
    " ```\n"
).
-spec compare(binary(), binary()) -> gleam@order:order().
compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        _ ->
            case gleam_stdlib:less_than(A, B) of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

-file("src/gleam/string.gleam", 194).
?DOC(
    " Takes a substring given a start grapheme index and a length. Negative indexes\n"
    " are taken starting from the *end* of the list.\n"
    "\n"
    " This function runs in linear time with the size of the index and the\n"
    " length. Negative indexes are linear with the size of the input string in\n"
    " addition to the other costs.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " slice(from: \"gleam\", at_index: 1, length: 2)\n"
    " // -> \"le\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " slice(from: \"gleam\", at_index: 1, length: 10)\n"
    " // -> \"leam\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " slice(from: \"gleam\", at_index: 10, length: 3)\n"
    " // -> \"\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " slice(from: \"gleam\", at_index: -2, length: 2)\n"
    " // -> \"am\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " slice(from: \"gleam\", at_index: -12, length: 2)\n"
    " // -> \"\"\n"
    " ```\n"
).
-spec slice(binary(), integer(), integer()) -> binary().
slice(String, Idx, Len) ->
    case Len =< 0 of
        true ->
            <<""/utf8>>;

        false ->
            case Idx < 0 of
                true ->
                    Translated_idx = string:length(String) + Idx,
                    case Translated_idx < 0 of
                        true ->
                            <<""/utf8>>;

                        false ->
                            gleam_stdlib:slice(String, Translated_idx, Len)
                    end;

                false ->
                    gleam_stdlib:slice(String, Idx, Len)
            end
    end.

-file("src/gleam/string.gleam", 232).
?DOC(
    " Drops contents of the first `String` that occur before the second `String`.\n"
    " If the `from` string does not contain the `before` string, `from` is\n"
    " returned unchanged.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " crop(from: \"The Lone Gunmen\", before: \"Lone\")\n"
    " // -> \"Lone Gunmen\"\n"
    " ```\n"
).
-spec crop(binary(), binary()) -> binary().
crop(String, Substring) ->
    gleam_stdlib:crop_string(String, Substring).

-file("src/gleam/string.gleam", 268).
?DOC(
    " Drops *n* graphemes from the end of a `String`.\n"
    "\n"
    " This function traverses the full string, so it runs in linear time with the\n"
    " size of the string. Avoid using this in a loop.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " drop_end(from: \"Cigarette Smoking Man\", up_to: 2)\n"
    " // -> \"Cigarette Smoking M\"\n"
    " ```\n"
).
-spec drop_end(binary(), integer()) -> binary().
drop_end(String, Num_graphemes) ->
    case Num_graphemes =< 0 of
        true ->
            String;

        false ->
            slice(String, 0, string:length(String) - Num_graphemes)
    end.

-file("src/gleam/string.gleam", 296).
?DOC(
    " Checks if the first `String` contains the second.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " contains(does: \"theory\", contain: \"ory\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " contains(does: \"theory\", contain: \"the\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " contains(does: \"theory\", contain: \"THE\")\n"
    " // -> False\n"
    " ```\n"
).
-spec contains(binary(), binary()) -> boolean().
contains(Haystack, Needle) ->
    gleam_stdlib:contains_string(Haystack, Needle).

-file("src/gleam/string.gleam", 309).
?DOC(
    " Checks whether the first `String` starts with the second one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " starts_with(\"theory\", \"ory\")\n"
    " // -> False\n"
    " ```\n"
).
-spec starts_with(binary(), binary()) -> boolean().
starts_with(String, Prefix) ->
    gleam_stdlib:string_starts_with(String, Prefix).

-file("src/gleam/string.gleam", 322).
?DOC(
    " Checks whether the first `String` ends with the second one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " ends_with(\"theory\", \"ory\")\n"
    " // -> True\n"
    " ```\n"
).
-spec ends_with(binary(), binary()) -> boolean().
ends_with(String, Suffix) ->
    gleam_stdlib:string_ends_with(String, Suffix).

-file("src/gleam/string.gleam", 361).
?DOC(
    " Splits a `String` a single time on the given substring.\n"
    "\n"
    " Returns an `Error` if substring not present.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split_once(\"home/gleam/desktop/\", on: \"/\")\n"
    " // -> Ok(#(\"home\", \"gleam/desktop/\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split_once(\"home/gleam/desktop/\", on: \"?\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec split_once(binary(), binary()) -> {ok, {binary(), binary()}} |
    {error, nil}.
split_once(String, Substring) ->
    case string:split(String, Substring) of
        [First, Rest] ->
            {ok, {First, Rest}};

        _ ->
            {error, nil}
    end.

-file("src/gleam/string.gleam", 392).
?DOC(
    " Creates a new `String` by joining two `String`s together.\n"
    "\n"
    " This function typically copies both `String`s and runs in linear time, but\n"
    " the exact behaviour will depend on how the runtime you are using optimises\n"
    " your code. Benchmark and profile your code if you need to understand its\n"
    " performance better.\n"
    "\n"
    " If you are joining together large string and want to avoid copying any data\n"
    " you may want to investigate using the [`string_tree`](../gleam/string_tree.html)\n"
    " module.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " append(to: \"butter\", suffix: \"fly\")\n"
    " // -> \"butterfly\"\n"
    " ```\n"
).
-spec append(binary(), binary()) -> binary().
append(First, Second) ->
    <<First/binary, Second/binary>>.

-file("src/gleam/string.gleam", 412).
-spec concat_loop(list(binary()), binary()) -> binary().
concat_loop(Strings, Accumulator) ->
    case Strings of
        [String | Strings@1] ->
            concat_loop(Strings@1, <<Accumulator/binary, String/binary>>);

        [] ->
            Accumulator
    end.

-file("src/gleam/string.gleam", 408).
?DOC(
    " Creates a new `String` by joining many `String`s together.\n"
    "\n"
    " This function copies all the `String`s and runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " concat([\"never\", \"the\", \"less\"])\n"
    " // -> \"nevertheless\"\n"
    " ```\n"
).
-spec concat(list(binary())) -> binary().
concat(Strings) ->
    erlang:list_to_binary(Strings).

-file("src/gleam/string.gleam", 437).
-spec repeat_loop(integer(), binary(), binary()) -> binary().
repeat_loop(Times, Doubling_acc, Acc) ->
    Acc@1 = case Times rem 2 of
        0 ->
            Acc;

        _ ->
            <<Acc/binary, Doubling_acc/binary>>
    end,
    Times@1 = Times div 2,
    case Times@1 =< 0 of
        true ->
            Acc@1;

        false ->
            repeat_loop(
                Times@1,
                <<Doubling_acc/binary, Doubling_acc/binary>>,
                Acc@1
            )
    end.

-file("src/gleam/string.gleam", 430).
?DOC(
    " Creates a new `String` by repeating a `String` a given number of times.\n"
    "\n"
    " This function runs in loglinear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " repeat(\"ha\", times: 3)\n"
    " // -> \"hahaha\"\n"
    " ```\n"
).
-spec repeat(binary(), integer()) -> binary().
repeat(String, Times) ->
    case Times =< 0 of
        true ->
            <<""/utf8>>;

        false ->
            repeat_loop(Times, String, <<""/utf8>>)
    end.

-file("src/gleam/string.gleam", 467).
-spec join_loop(list(binary()), binary(), binary()) -> binary().
join_loop(Strings, Separator, Accumulator) ->
    case Strings of
        [] ->
            Accumulator;

        [String | Strings@1] ->
            join_loop(
                Strings@1,
                Separator,
                <<<<Accumulator/binary, Separator/binary>>/binary,
                    String/binary>>
            )
    end.

-file("src/gleam/string.gleam", 460).
?DOC(
    " Joins many `String`s together with a given separator.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " join([\"home\",\"evan\",\"Desktop\"], with: \"/\")\n"
    " // -> \"home/evan/Desktop\"\n"
    " ```\n"
).
-spec join(list(binary()), binary()) -> binary().
join(Strings, Separator) ->
    case Strings of
        [] ->
            <<""/utf8>>;

        [First | Rest] ->
            join_loop(Rest, Separator, First)
    end.

-file("src/gleam/string.gleam", 545).
-spec padding(integer(), binary()) -> binary().
padding(Size, Pad_string) ->
    Pad_string_length = string:length(Pad_string),
    Num_pads = case Pad_string_length of
        0 -> 0;
        Gleam@denominator -> Size div Gleam@denominator
    end,
    Extra = case Pad_string_length of
        0 -> 0;
        Gleam@denominator@1 -> Size rem Gleam@denominator@1
    end,
    <<(repeat(Pad_string, Num_pads))/binary,
        (slice(Pad_string, 0, Extra))/binary>>.

-file("src/gleam/string.gleam", 498).
?DOC(
    " Pads the start of a `String` until it has a given length.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " pad_start(\"121\", to: 5, with: \".\")\n"
    " // -> \"..121\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pad_start(\"121\", to: 3, with: \".\")\n"
    " // -> \"121\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pad_start(\"121\", to: 2, with: \".\")\n"
    " // -> \"121\"\n"
    " ```\n"
).
-spec pad_start(binary(), integer(), binary()) -> binary().
pad_start(String, Desired_length, Pad_string) ->
    Current_length = string:length(String),
    To_pad_length = Desired_length - Current_length,
    case To_pad_length =< 0 of
        true ->
            String;

        false ->
            <<(padding(To_pad_length, Pad_string))/binary, String/binary>>
    end.

-file("src/gleam/string.gleam", 531).
?DOC(
    " Pads the end of a `String` until it has a given length.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " pad_end(\"123\", to: 5, with: \".\")\n"
    " // -> \"123..\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pad_end(\"123\", to: 3, with: \".\")\n"
    " // -> \"123\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pad_end(\"123\", to: 2, with: \".\")\n"
    " // -> \"123\"\n"
    " ```\n"
).
-spec pad_end(binary(), integer(), binary()) -> binary().
pad_end(String, Desired_length, Pad_string) ->
    Current_length = string:length(String),
    To_pad_length = Desired_length - Current_length,
    case To_pad_length =< 0 of
        true ->
            String;

        false ->
            <<String/binary, (padding(To_pad_length, Pad_string))/binary>>
    end.

-file("src/gleam/string.gleam", 589).
?DOC(
    " Removes whitespace at the start of a `String`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " trim_start(\"  hats  \\n\")\n"
    " // -> \"hats  \\n\"\n"
    " ```\n"
).
-spec trim_start(binary()) -> binary().
trim_start(String) ->
    string:trim(String, leading).

-file("src/gleam/string.gleam", 603).
?DOC(
    " Removes whitespace at the end of a `String`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " trim_end(\"  hats  \\n\")\n"
    " // -> \"  hats\"\n"
    " ```\n"
).
-spec trim_end(binary()) -> binary().
trim_end(String) ->
    string:trim(String, trailing).

-file("src/gleam/string.gleam", 567).
?DOC(
    " Removes whitespace on both sides of a `String`.\n"
    "\n"
    " Whitespace in this function is the set of nonbreakable whitespace\n"
    " codepoints, defined as Pattern_White_Space in [Unicode Standard Annex #31][1].\n"
    "\n"
    " [1]: https://unicode.org/reports/tr31/\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " trim(\"  hats  \\n\")\n"
    " // -> \"hats\"\n"
    " ```\n"
).
-spec trim(binary()) -> binary().
trim(String) ->
    _pipe = String,
    _pipe@1 = trim_start(_pipe),
    trim_end(_pipe@1).

-file("src/gleam/string.gleam", 630).
?DOC(
    " Splits a non-empty `String` into its first element (head) and rest (tail).\n"
    " This lets you pattern match on `String`s exactly as you would with lists.\n"
    "\n"
    " ## Performance\n"
    "\n"
    " There is a notable overhead to using this function, so you may not want to\n"
    " use it in a tight loop. If you wish to efficiently parse a string you may\n"
    " want to use alternatives such as the [splitter package](https://hex.pm/packages/splitter).\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " pop_grapheme(\"gleam\")\n"
    " // -> Ok(#(\"g\", \"leam\"))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pop_grapheme(\"\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec pop_grapheme(binary()) -> {ok, {binary(), binary()}} | {error, nil}.
pop_grapheme(String) ->
    gleam_stdlib:string_pop_grapheme(String).

-file("src/gleam/string.gleam", 647).
-spec to_graphemes_loop(binary(), list(binary())) -> list(binary()).
to_graphemes_loop(String, Acc) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {Grapheme, Rest}} ->
            to_graphemes_loop(Rest, [Grapheme | Acc]);

        {error, _} ->
            Acc
    end.

-file("src/gleam/string.gleam", 641).
?DOC(
    " Converts a `String` to a list of\n"
    " [graphemes](https://en.wikipedia.org/wiki/Grapheme).\n"
    "\n"
    " ```gleam\n"
    " to_graphemes(\"abc\")\n"
    " // -> [\"a\", \"b\", \"c\"]\n"
    " ```\n"
).
-spec to_graphemes(binary()) -> list(binary()).
to_graphemes(String) ->
    _pipe = String,
    _pipe@1 = to_graphemes_loop(_pipe, []),
    lists:reverse(_pipe@1).

-file("src/gleam/string.gleam", 333).
?DOC(
    " Creates a list of `String`s by splitting a given string on a given substring.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split(\"home/gleam/desktop/\", on: \"/\")\n"
    " // -> [\"home\", \"gleam\", \"desktop\", \"\"]\n"
    " ```\n"
).
-spec split(binary(), binary()) -> list(binary()).
split(X, Substring) ->
    case Substring of
        <<""/utf8>> ->
            to_graphemes(X);

        _ ->
            _pipe = X,
            _pipe@1 = gleam_stdlib:identity(_pipe),
            _pipe@2 = gleam@string_tree:split(_pipe@1, Substring),
            gleam@list:map(_pipe@2, fun unicode:characters_to_binary/1)
    end.

-file("src/gleam/string.gleam", 694).
-spec to_utf_codepoints_loop(bitstring(), list(integer())) -> list(integer()).
to_utf_codepoints_loop(Bit_array, Acc) ->
    case Bit_array of
        <<First/utf8, Rest/binary>> ->
            to_utf_codepoints_loop(Rest, [First | Acc]);

        _ ->
            lists:reverse(Acc)
    end.

-file("src/gleam/string.gleam", 689).
-spec do_to_utf_codepoints(binary()) -> list(integer()).
do_to_utf_codepoints(String) ->
    to_utf_codepoints_loop(<<String/binary>>, []).

-file("src/gleam/string.gleam", 684).
?DOC(
    " Converts a `String` to a `List` of `UtfCodepoint`.\n"
    "\n"
    " See <https://en.wikipedia.org/wiki/Code_point> and\n"
    " <https://en.wikipedia.org/wiki/Unicode#Codespace_and_Code_Points> for an\n"
    " explanation on code points.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " \"a\" |> to_utf_codepoints\n"
    " // -> [UtfCodepoint(97)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " // Semantically the same as:\n"
    " // [\"🏳\", \"️\", \"‍\", \"🌈\"] or:\n"
    " // [waving_white_flag, variant_selector_16, zero_width_joiner, rainbow]\n"
    " \"🏳️‍🌈\" |> to_utf_codepoints\n"
    " // -> [\n"
    " //   UtfCodepoint(127987),\n"
    " //   UtfCodepoint(65039),\n"
    " //   UtfCodepoint(8205),\n"
    " //   UtfCodepoint(127752),\n"
    " // ]\n"
    " ```\n"
).
-spec to_utf_codepoints(binary()) -> list(integer()).
to_utf_codepoints(String) ->
    do_to_utf_codepoints(String).

-file("src/gleam/string.gleam", 734).
?DOC(
    " Converts a `List` of `UtfCodepoint`s to a `String`.\n"
    "\n"
    " See <https://en.wikipedia.org/wiki/Code_point> and\n"
    " <https://en.wikipedia.org/wiki/Unicode#Codespace_and_Code_Points> for an\n"
    " explanation on code points.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(a) = utf_codepoint(97)\n"
    " let assert Ok(b) = utf_codepoint(98)\n"
    " let assert Ok(c) = utf_codepoint(99)\n"
    " from_utf_codepoints([a, b, c])\n"
    " // -> \"abc\"\n"
    " ```\n"
).
-spec from_utf_codepoints(list(integer())) -> binary().
from_utf_codepoints(Utf_codepoints) ->
    gleam_stdlib:utf_codepoint_list_to_string(Utf_codepoints).

-file("src/gleam/string.gleam", 740).
?DOC(
    " Converts an integer to a `UtfCodepoint`.\n"
    "\n"
    " Returns an `Error` if the integer does not represent a valid UTF codepoint.\n"
).
-spec utf_codepoint(integer()) -> {ok, integer()} | {error, nil}.
utf_codepoint(Value) ->
    case Value of
        I when I > 1114111 ->
            {error, nil};

        I@1 when (I@1 >= 55296) andalso (I@1 =< 57343) ->
            {error, nil};

        I@2 when I@2 < 0 ->
            {error, nil};

        I@3 ->
            {ok, gleam_stdlib:identity(I@3)}
    end.

-file("src/gleam/string.gleam", 761).
?DOC(
    " Converts an UtfCodepoint to its ordinal code point value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert [utf_codepoint, ..] = to_utf_codepoints(\"💜\")\n"
    " utf_codepoint_to_int(utf_codepoint)\n"
    " // -> 128156\n"
    " ```\n"
).
-spec utf_codepoint_to_int(integer()) -> integer().
utf_codepoint_to_int(Cp) ->
    gleam_stdlib:identity(Cp).

-file("src/gleam/string.gleam", 778).
?DOC(
    " Converts a `String` into `Option(String)` where an empty `String` becomes\n"
    " `None`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " to_option(\"\")\n"
    " // -> None\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " to_option(\"hats\")\n"
    " // -> Some(\"hats\")\n"
    " ```\n"
).
-spec to_option(binary()) -> gleam@option:option(binary()).
to_option(String) ->
    case String of
        <<""/utf8>> ->
            none;

        _ ->
            {some, String}
    end.

-file("src/gleam/string.gleam", 801).
?DOC(
    " Returns the first grapheme cluster in a given `String` and wraps it in a\n"
    " `Result(String, Nil)`. If the `String` is empty, it returns `Error(Nil)`.\n"
    " Otherwise, it returns `Ok(String)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " first(\"\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " first(\"icecream\")\n"
    " // -> Ok(\"i\")\n"
    " ```\n"
).
-spec first(binary()) -> {ok, binary()} | {error, nil}.
first(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {First, _}} ->
            {ok, First};

        {error, E} ->
            {error, E}
    end.

-file("src/gleam/string.gleam", 827).
?DOC(
    " Returns the last grapheme cluster in a given `String` and wraps it in a\n"
    " `Result(String, Nil)`. If the `String` is empty, it returns `Error(Nil)`.\n"
    " Otherwise, it returns `Ok(String)`.\n"
    "\n"
    " This function traverses the full string, so it runs in linear time with the\n"
    " length of the string. Avoid using this in a loop.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " last(\"\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " last(\"icecream\")\n"
    " // -> Ok(\"m\")\n"
    " ```\n"
).
-spec last(binary()) -> {ok, binary()} | {error, nil}.
last(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {First, <<""/utf8>>}} ->
            {ok, First};

        {ok, {_, Rest}} ->
            {ok, slice(Rest, -1, 1)};

        {error, E} ->
            {error, E}
    end.

-file("src/gleam/string.gleam", 845).
?DOC(
    " Creates a new `String` with the first grapheme in the input `String`\n"
    " converted to uppercase and the remaining graphemes to lowercase.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " capitalise(\"mamouna\")\n"
    " // -> \"Mamouna\"\n"
    " ```\n"
).
-spec capitalise(binary()) -> binary().
capitalise(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {First, Rest}} ->
            append(string:uppercase(First), string:lowercase(Rest));

        {error, _} ->
            <<""/utf8>>
    end.

-file("src/gleam/string.gleam", 876).
?DOC(
    " Returns a `String` representation of a term in Gleam syntax.\n"
    "\n"
    " This may be occasionally useful for quick-and-dirty printing of values in\n"
    " scripts. For error reporting and other uses prefer constructing strings by\n"
    " pattern matching on the values.\n"
    "\n"
    " ## Limitations\n"
    "\n"
    " The output format of this function is not stable and could change at any\n"
    " time. The output is not suitable for parsing.\n"
    "\n"
    " This function works using runtime reflection, so the output may not be\n"
    " perfectly accurate for data structures where the runtime structure doesn't\n"
    " hold enough information to determine the original syntax. For example,\n"
    " tuples with an Erlang atom in the first position will be mistaken for Gleam\n"
    " records.\n"
    "\n"
    " ## Security and safety\n"
    "\n"
    " There is no limit to how large the strings that this function can produce.\n"
    " Be careful not to call this function with large data structures or you\n"
    " could use very large amounts of memory, potentially causing runtime\n"
    " problems.\n"
).
-spec inspect(any()) -> binary().
inspect(Term) ->
    _pipe = Term,
    _pipe@1 = gleam_stdlib:inspect(_pipe),
    unicode:characters_to_binary(_pipe@1).

-file("src/gleam/string.gleam", 900).
?DOC(
    " Returns the number of bytes in a `String`.\n"
    "\n"
    " This function runs in constant time on Erlang and in linear time on\n"
    " JavaScript.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " byte_size(\"🏳️‍⚧️🏳️‍🌈👩🏾‍❤️‍👨🏻\")\n"
    " // -> 58\n"
    " ```\n"
).
-spec byte_size(binary()) -> integer().
byte_size(String) ->
    erlang:byte_size(String).

-file("src/gleam/string.gleam", 245).
?DOC(
    " Drops *n* graphemes from the start of a `String`.\n"
    "\n"
    " This function runs in linear time with the number of graphemes to drop.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " drop_start(from: \"The Lone Gunmen\", up_to: 2)\n"
    " // -> \"e Lone Gunmen\"\n"
    " ```\n"
).
-spec drop_start(binary(), integer()) -> binary().
drop_start(String, Num_graphemes) ->
    case Num_graphemes =< 0 of
        true ->
            String;

        false ->
            Prefix = gleam_stdlib:slice(String, 0, Num_graphemes),
            Prefix_size = erlang:byte_size(Prefix),
            binary:part(
                String,
                Prefix_size,
                erlang:byte_size(String) - Prefix_size
            )
    end.
