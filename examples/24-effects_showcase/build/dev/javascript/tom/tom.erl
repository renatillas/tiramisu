-module(tom).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/tom.gleam").
-export([get/2, get_int/2, get_float/2, get_bool/2, get_string/2, get_date/2, get_time_of_day/2, get_calendar_time/2, get_timestamp/2, get_array/2, get_table/2, get_number/2, as_int/1, as_float/1, as_bool/1, as_string/1, as_date/1, as_time_of_day/1, as_timestamp/1, as_calendar_time/1, as_array/1, as_table/1, as_number/1, parse/1]).
-export_type([toml/0, offset/0, sign/0, parse_error/0, number_/0, get_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A pure Gleam TOML parser!\n"
    "\n"
    " ```gleam\n"
    " import tom\n"
    " \n"
    " const config = \"\n"
    "   [person]\n"
    "   name = \\\"Lucy\\\"\n"
    "   is_cool = true\n"
    " \"\n"
    " \n"
    " pub fn main() {\n"
    "   // Parse a string of TOML\n"
    "   let assert Ok(parsed) = tom.parse(config)\n"
    " \n"
    "   // Now you can work with the data directly, or you can use the `get_*`\n"
    "   // functions to retrieve values.\n"
    " \n"
    "   tom.get_string(parsed, [\"person\", \"name\"])\n"
    "   // -> Ok(\"Lucy\")\n"
    " \n"
    "   let is_cool = tom.get_bool(parsed, [\"person\", \"is_cool\"])\n"
    "   // -> Ok(True)\n"
    " }\n"
    " ```\n"
).

-type toml() :: {int, integer()} |
    {float, float()} |
    {infinity, sign()} |
    {nan, sign()} |
    {bool, boolean()} |
    {string, binary()} |
    {date, gleam@time@calendar:date()} |
    {time, gleam@time@calendar:time_of_day()} |
    {date_time,
        gleam@time@calendar:date(),
        gleam@time@calendar:time_of_day(),
        offset()} |
    {array, list(toml())} |
    {array_of_tables, list(gleam@dict:dict(binary(), toml()))} |
    {table, gleam@dict:dict(binary(), toml())} |
    {inline_table, gleam@dict:dict(binary(), toml())}.

-type offset() :: local | {offset, gleam@time@duration:duration()}.

-type sign() :: positive | negative.

-type parse_error() :: {unexpected, binary(), binary()} |
    {key_already_in_use, list(binary())}.

-type number_() :: {number_int, integer()} |
    {number_float, float()} |
    {number_infinity, sign()} |
    {number_nan, sign()}.

-type get_error() :: {not_found, list(binary())} |
    {wrong_type, list(binary()), binary(), binary()}.

-file("src/tom.gleam", 377).
-spec classify(toml()) -> binary().
classify(Toml) ->
    case Toml of
        {int, _} ->
            <<"Int"/utf8>>;

        {float, _} ->
            <<"Float"/utf8>>;

        {nan, positive} ->
            <<"NaN"/utf8>>;

        {nan, negative} ->
            <<"Negative NaN"/utf8>>;

        {infinity, positive} ->
            <<"Infinity"/utf8>>;

        {infinity, negative} ->
            <<"Negative Infinity"/utf8>>;

        {bool, _} ->
            <<"Bool"/utf8>>;

        {string, _} ->
            <<"String"/utf8>>;

        {date, _} ->
            <<"Date"/utf8>>;

        {time, _} ->
            <<"Time"/utf8>>;

        {date_time, _, _, _} ->
            <<"DateTime"/utf8>>;

        {array, _} ->
            <<"Array"/utf8>>;

        {array_of_tables, _} ->
            <<"Array"/utf8>>;

        {table, _} ->
            <<"Table"/utf8>>;

        {inline_table, _} ->
            <<"Table"/utf8>>
    end.

-file("src/tom.gleam", 397).
-spec push_key({ok, EFT} | {error, get_error()}, binary()) -> {ok, EFT} |
    {error, get_error()}.
push_key(Result, Key) ->
    case Result of
        {ok, T} ->
            {ok, T};

        {error, {not_found, Path}} ->
            {error, {not_found, [Key | Path]}};

        {error, {wrong_type, Path@1, Expected, Got}} ->
            {error, {wrong_type, [Key | Path@1], Expected, Got}}
    end.

-file("src/tom.gleam", 110).
?DOC(
    " Get a value of any type from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 1\")\n"
    " get(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(Int(1))\n"
    " ```\n"
).
-spec get(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok, toml()} |
    {error, get_error()}.
get(Toml, Key) ->
    case Key of
        [] ->
            {error, {not_found, []}};

        [K] ->
            gleam@result:replace_error(
                gleam_stdlib:map_get(Toml, K),
                {not_found, [K]}
            );

        [K@1 | Key@1] ->
            case gleam_stdlib:map_get(Toml, K@1) of
                {ok, {table, T}} ->
                    push_key(get(T, Key@1), K@1);

                {ok, {inline_table, T@1}} ->
                    push_key(get(T@1, Key@1), K@1);

                {ok, Other} ->
                    {error,
                        {wrong_type, [K@1], <<"Table"/utf8>>, classify(Other)}};

                {error, _} ->
                    {error, {not_found, [K@1]}}
            end
    end.

-file("src/tom.gleam", 139).
?DOC(
    " Get an int from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 1\")\n"
    " get_int(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(1)\n"
    " ```\n"
).
-spec get_int(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        integer()} |
    {error, get_error()}.
get_int(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {int, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Int"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 161).
?DOC(
    " Get a float from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 1.1\")\n"
    " get_float(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(1.1)\n"
    " ```\n"
).
-spec get_float(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        float()} |
    {error, get_error()}.
get_float(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {float, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Float"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 183).
?DOC(
    " Get a bool from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = true\")\n"
    " get_bool(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(True)\n"
    " ```\n"
).
-spec get_bool(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        boolean()} |
    {error, get_error()}.
get_bool(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {bool, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Bool"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 205).
?DOC(
    " Get a string from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = \\\"ok\\\"\")\n"
    " get_string(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(\"ok\")\n"
    " ```\n"
).
-spec get_string(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        binary()} |
    {error, get_error()}.
get_string(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {string, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"String"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 226).
?DOC(
    " Get a date from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 1979-05-27\")\n"
    " get_date(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(Date(1979, May, 27))\n"
    " ```\n"
).
-spec get_date(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        gleam@time@calendar:date()} |
    {error, get_error()}.
get_date(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {date, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Date"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 247).
?DOC(
    " Get a time from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 07:32:00\")\n"
    " get_time_of_day(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(TimeOfDay(7, 32, 0, 0))\n"
    " ```\n"
).
-spec get_time_of_day(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        gleam@time@calendar:time_of_day()} |
    {error, get_error()}.
get_time_of_day(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {time, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Time"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 268).
?DOC(
    " Get a date-time from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 1979-05-27T07:32:00\")\n"
    " get_calendar_time(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(#(Date(1979, May, 27), TimeOfDay(7, 32, 0, 0), Local))\n"
    " ```\n"
).
-spec get_calendar_time(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        {gleam@time@calendar:date(),
            gleam@time@calendar:time_of_day(),
            offset()}} |
    {error, get_error()}.
get_calendar_time(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {date_time, D, T, O}} ->
            {ok, {D, T, O}};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"DateTime"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 293).
?DOC(
    " Get an unambiguous time from a TOML document dictionary.\n"
    "\n"
    " If a TOML date time has no offset it is ambiguous and cannot be converted\n"
    " into a timestamp. There's no way to know what actual point in time it would\n"
    " be as it would be different in different time zones.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = 1970-00-00T00:00:00Z\")\n"
    " get_timestamp(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(timestamp.from_unix_seconds(0))\n"
    " ```\n"
).
-spec get_timestamp(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        gleam@time@timestamp:timestamp()} |
    {error, get_error()}.
get_timestamp(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {date_time, Date, Time, {offset, Offset}}} ->
            {ok, gleam@time@timestamp:from_calendar(Date, Time, Offset)};

        {ok, Other} ->
            {error,
                {wrong_type,
                    Key,
                    <<"DateTime with offset"/utf8>>,
                    classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 316).
?DOC(
    " Get an array from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = [1, 2]\")\n"
    " get_array(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok([Int(1), Int(2)])\n"
    " ```\n"
).
-spec get_array(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        list(toml())} |
    {error, get_error()}.
get_array(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {array, I}} ->
            {ok, I};

        {ok, {array_of_tables, I@1}} ->
            {ok, gleam@list:map(I@1, fun(Field@0) -> {table, Field@0} end)};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Array"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 339).
?DOC(
    " Get a table from a TOML document dictionary.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = { d = 1 }\")\n"
    " get_table(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(dict.from_list([#(\"d\", Int(1))]))\n"
    " ```\n"
).
-spec get_table(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, get_error()}.
get_table(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {table, I}} ->
            {ok, I};

        {ok, {inline_table, I@1}} ->
            {ok, I@1};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Table"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 363).
?DOC(
    " Get a number of any kind from a TOML document dictionary.\n"
    " This could be an int, a float, a NaN, or an infinity.\n"
    "\n"
    " ## Examples\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(parsed) = parse(\"a.b.c = { d = inf }\")\n"
    " get_number(parsed, [\"a\", \"b\", \"c\"])\n"
    " // -> Ok(NumberInfinity(Positive)))\n"
    " ```\n"
).
-spec get_number(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        number_()} |
    {error, get_error()}.
get_number(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {int, X}} ->
            {ok, {number_int, X}};

        {ok, {float, X@1}} ->
            {ok, {number_float, X@1}};

        {ok, {nan, X@2}} ->
            {ok, {number_nan, X@2}};

        {ok, {infinity, X@3}} ->
            {ok, {number_infinity, X@3}};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Number"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 563).
-spec merge(gleam@dict:dict(binary(), toml()), binary(), toml(), toml()) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, list(binary())}.
merge(Table, Key, Old, New) ->
    case {Old, New} of
        {{array_of_tables, Tables}, {array_of_tables, New@1}} ->
            {ok,
                gleam@dict:insert(
                    Table,
                    Key,
                    {array_of_tables, lists:append(New@1, Tables)}
                )};

        {_, _} ->
            {error, [Key]}
    end.

-file("src/tom.gleam", 523).
-spec insert_loop(gleam@dict:dict(binary(), toml()), list(binary()), toml()) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, list(binary())}.
insert_loop(Table, Key, Value) ->
    case Key of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"tom"/utf8>>,
                    function => <<"insert_loop"/utf8>>,
                    line => 529});

        [K] ->
            case gleam_stdlib:map_get(Table, K) of
                {error, nil} ->
                    {ok, gleam@dict:insert(Table, K, Value)};

                {ok, Old} ->
                    merge(Table, K, Old, Value)
            end;

        [K@1 | Key@1] ->
            case gleam_stdlib:map_get(Table, K@1) of
                {error, nil} ->
                    case insert_loop(maps:new(), Key@1, Value) of
                        {ok, Inner} ->
                            {ok, gleam@dict:insert(Table, K@1, {table, Inner})};

                        {error, Path} ->
                            {error, [K@1 | Path]}
                    end;

                {ok, {array_of_tables, [Inner@1 | Rest]}} ->
                    case insert_loop(Inner@1, Key@1, Value) of
                        {ok, Inner@2} ->
                            {ok,
                                gleam@dict:insert(
                                    Table,
                                    K@1,
                                    {array_of_tables, [Inner@2 | Rest]}
                                )};

                        {error, Path@1} ->
                            {error, [K@1 | Path@1]}
                    end;

                {ok, {table, Inner@3}} ->
                    case insert_loop(Inner@3, Key@1, Value) of
                        {ok, Inner@4} ->
                            {ok,
                                gleam@dict:insert(Table, K@1, {table, Inner@4})};

                        {error, Path@2} ->
                            {error, [K@1 | Path@2]}
                    end;

                {ok, _} ->
                    {error, [K@1]}
            end
    end.

-file("src/tom.gleam", 512).
-spec insert(gleam@dict:dict(binary(), toml()), list(binary()), toml()) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, parse_error()}.
insert(Table, Key, Value) ->
    case insert_loop(Table, Key, Value) of
        {ok, Table@1} ->
            {ok, Table@1};

        {error, Path} ->
            {error, {key_already_in_use, Path}}
    end.

-file("src/tom.gleam", 578).
-spec expect_end_of_line(
    list(binary()),
    fun((list(binary())) -> {ok, {EHY, list(binary())}} | {error, parse_error()})
) -> {ok, {EHY, list(binary())}} | {error, parse_error()}.
expect_end_of_line(Input, Next) ->
    case Input of
        [<<"\n"/utf8>> | Input@1] ->
            Next(Input@1);

        [<<"\r\n"/utf8>> | Input@2] ->
            Next(Input@2);

        [G | _] ->
            {error, {unexpected, G, <<"\n"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\n"/utf8>>}}
    end.

-file("src/tom.gleam", 663).
-spec parse_key_quoted(list(binary()), binary(), binary()) -> {ok,
        {binary(), list(binary())}} |
    {error, parse_error()}.
parse_key_quoted(Input, Close, Name) ->
    case Input of
        [G | Input@1] when G =:= Close ->
            {ok, {Name, Input@1}};

        [G@1 | Input@2] ->
            parse_key_quoted(Input@2, Close, <<Name/binary, G@1/binary>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, Close}}
    end.

-file("src/tom.gleam", 675).
-spec parse_key_bare(list(binary()), binary()) -> {ok,
        {binary(), list(binary())}} |
    {error, parse_error()}.
parse_key_bare(Input, Name) ->
    case Input of
        [<<" "/utf8>> | Input@1] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input@1}};

        [<<"="/utf8>> | _] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input}};

        [<<"."/utf8>> | _] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input}};

        [<<"]"/utf8>> | _] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input}};

        [<<","/utf8>> | _] when Name =/= <<""/utf8>> ->
            {error, {unexpected, <<","/utf8>>, <<"="/utf8>>}};

        [<<"\n"/utf8>> | _] when Name =/= <<""/utf8>> ->
            {error, {unexpected, <<"\n"/utf8>>, <<"="/utf8>>}};

        [<<"\r\n"/utf8>> | _] when Name =/= <<""/utf8>> ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"="/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"key"/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"key"/utf8>>}};

        [<<"]"/utf8>> | _] ->
            {error, {unexpected, <<"]"/utf8>>, <<"key"/utf8>>}};

        [<<","/utf8>> | _] ->
            {error, {unexpected, <<","/utf8>>, <<"key"/utf8>>}};

        [G | Input@2] ->
            parse_key_bare(Input@2, <<Name/binary, G/binary>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"key"/utf8>>}}
    end.

-file("src/tom.gleam", 693).
-spec skip_line_whitespace(list(binary())) -> list(binary()).
skip_line_whitespace(Input) ->
    gleam@list:drop_while(
        Input,
        fun(G) -> (G =:= <<" "/utf8>>) orelse (G =:= <<"\t"/utf8>>) end
    ).

-file("src/tom.gleam", 650).
-spec parse_key_segment(list(binary())) -> {ok, {binary(), list(binary())}} |
    {error, parse_error()}.
parse_key_segment(Input) ->
    Input@1 = skip_line_whitespace(Input),
    case Input@1 of
        [<<"="/utf8>> | _] ->
            {error, {unexpected, <<"="/utf8>>, <<"Key"/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"Key"/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"Key"/utf8>>}};

        [<<"["/utf8>> | _] ->
            {error, {unexpected, <<"["/utf8>>, <<"Key"/utf8>>}};

        [<<"\""/utf8>> | Input@2] ->
            parse_key_quoted(Input@2, <<"\""/utf8>>, <<""/utf8>>);

        [<<"'"/utf8>> | Input@3] ->
            parse_key_quoted(Input@3, <<"'"/utf8>>, <<""/utf8>>);

        _ ->
            parse_key_bare(Input@1, <<""/utf8>>)
    end.

-file("src/tom.gleam", 697).
-spec skip_whitespace(list(binary())) -> list(binary()).
skip_whitespace(Input) ->
    case Input of
        [<<" "/utf8>> | Input@1] ->
            skip_whitespace(Input@1);

        [<<"\t"/utf8>> | Input@2] ->
            skip_whitespace(Input@2);

        [<<"\n"/utf8>> | Input@3] ->
            skip_whitespace(Input@3);

        [<<"\r\n"/utf8>> | Input@4] ->
            skip_whitespace(Input@4);

        Input@5 ->
            Input@5
    end.

-file("src/tom.gleam", 707).
-spec drop_comments(list(binary()), list(binary()), boolean()) -> list(binary()).
drop_comments(Input, Acc, In_string) ->
    case Input of
        [<<"\\"/utf8>>, <<"\""/utf8>> | Input@1] when In_string ->
            drop_comments(
                Input@1,
                [<<"\""/utf8>>, <<"\\"/utf8>> | Acc],
                In_string
            );

        [<<"\""/utf8>> | Input@2] ->
            drop_comments(Input@2, [<<"\""/utf8>> | Acc], not In_string);

        [<<"#"/utf8>> | Input@3] when In_string ->
            drop_comments(Input@3, [<<"#"/utf8>> | Acc], In_string);

        [<<"#"/utf8>> | Input@4] when not In_string ->
            _pipe = Input@4,
            _pipe@1 = gleam@list:drop_while(
                _pipe,
                fun(G) -> G /= <<"\n"/utf8>> end
            ),
            drop_comments(_pipe@1, Acc, In_string);

        [G@1 | Input@5] ->
            drop_comments(Input@5, [G@1 | Acc], In_string);

        [] ->
            lists:reverse(Acc)
    end.

-file("src/tom.gleam", 722).
-spec do(
    {ok, {EIJ, list(binary())}} | {error, parse_error()},
    fun((EIJ, list(binary())) -> {ok, EIM} | {error, parse_error()})
) -> {ok, EIM} | {error, parse_error()}.
do(Result, Next) ->
    case Result of
        {ok, {A, Input}} ->
            Next(A, Input);

        {error, E} ->
            {error, E}
    end.

-file("src/tom.gleam", 639).
-spec parse_key(list(binary()), list(binary())) -> {ok,
        {list(binary()), list(binary())}} |
    {error, parse_error()}.
parse_key(Input, Segments) ->
    do(
        parse_key_segment(Input),
        fun(Segment, Input@1) ->
            Segments@1 = [Segment | Segments],
            Input@2 = skip_line_whitespace(Input@1),
            case Input@2 of
                [<<"."/utf8>> | Input@3] ->
                    parse_key(Input@3, Segments@1);

                _ ->
                    {ok, {lists:reverse(Segments@1), Input@2}}
            end
        end
    ).

-file("src/tom.gleam", 732).
-spec expect(
    list(binary()),
    binary(),
    fun((list(binary())) -> {ok, {EIR, list(binary())}} | {error, parse_error()})
) -> {ok, {EIR, list(binary())}} | {error, parse_error()}.
expect(Input, Expected, Next) ->
    case Input of
        [G | Input@1] when G =:= Expected ->
            Next(Input@1);

        [G@1 | _] ->
            {error, {unexpected, G@1, Expected}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, Expected}}
    end.

-file("src/tom.gleam", 460).
-spec parse_table_header(list(binary())) -> {ok,
        {list(binary()), list(binary())}} |
    {error, parse_error()}.
parse_table_header(Input) ->
    Input@1 = skip_line_whitespace(Input),
    do(
        parse_key(Input@1, []),
        fun(Key, Input@2) ->
            expect(
                Input@2,
                <<"]"/utf8>>,
                fun(Input@3) ->
                    Input@4 = skip_line_whitespace(Input@3),
                    expect_end_of_line(
                        Input@4,
                        fun(Input@5) -> {ok, {Key, Input@5}} end
                    )
                end
            )
        end
    ).

-file("src/tom.gleam", 807).
-spec parse_hex(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_hex(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_hex(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_hex(Input@2, (Number * 16) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_hex(Input@3, (Number * 16) + 1, Sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_hex(Input@4, (Number * 16) + 2, Sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_hex(Input@5, (Number * 16) + 3, Sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_hex(Input@6, (Number * 16) + 4, Sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_hex(Input@7, (Number * 16) + 5, Sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_hex(Input@8, (Number * 16) + 6, Sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_hex(Input@9, (Number * 16) + 7, Sign);

        [<<"8"/utf8>> | Input@10] ->
            parse_hex(Input@10, (Number * 16) + 8, Sign);

        [<<"9"/utf8>> | Input@11] ->
            parse_hex(Input@11, (Number * 16) + 9, Sign);

        [<<"a"/utf8>> | Input@12] ->
            parse_hex(Input@12, (Number * 16) + 10, Sign);

        [<<"b"/utf8>> | Input@13] ->
            parse_hex(Input@13, (Number * 16) + 11, Sign);

        [<<"c"/utf8>> | Input@14] ->
            parse_hex(Input@14, (Number * 16) + 12, Sign);

        [<<"d"/utf8>> | Input@15] ->
            parse_hex(Input@15, (Number * 16) + 13, Sign);

        [<<"e"/utf8>> | Input@16] ->
            parse_hex(Input@16, (Number * 16) + 14, Sign);

        [<<"f"/utf8>> | Input@17] ->
            parse_hex(Input@17, (Number * 16) + 15, Sign);

        [<<"A"/utf8>> | Input@18] ->
            parse_hex(Input@18, (Number * 16) + 10, Sign);

        [<<"B"/utf8>> | Input@19] ->
            parse_hex(Input@19, (Number * 16) + 11, Sign);

        [<<"C"/utf8>> | Input@20] ->
            parse_hex(Input@20, (Number * 16) + 12, Sign);

        [<<"D"/utf8>> | Input@21] ->
            parse_hex(Input@21, (Number * 16) + 13, Sign);

        [<<"E"/utf8>> | Input@22] ->
            parse_hex(Input@22, (Number * 16) + 14, Sign);

        [<<"F"/utf8>> | Input@23] ->
            parse_hex(Input@23, (Number * 16) + 15, Sign);

        Input@24 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@24}}
    end.

-file("src/tom.gleam", 844).
-spec parse_octal(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_octal(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_octal(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_octal(Input@2, (Number * 8) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_octal(Input@3, (Number * 8) + 1, Sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_octal(Input@4, (Number * 8) + 2, Sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_octal(Input@5, (Number * 8) + 3, Sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_octal(Input@6, (Number * 8) + 4, Sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_octal(Input@7, (Number * 8) + 5, Sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_octal(Input@8, (Number * 8) + 6, Sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_octal(Input@9, (Number * 8) + 7, Sign);

        Input@10 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@10}}
    end.

-file("src/tom.gleam", 867).
-spec parse_binary(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_binary(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_binary(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_binary(Input@2, (Number * 2) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_binary(Input@3, (Number * 2) + 1, Sign);

        Input@4 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@4}}
    end.

-file("src/tom.gleam", 927).
-spec parse_exponent(list(binary()), float(), sign(), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_exponent(Input, N, N_sign, Ex, Ex_sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_exponent(Input@1, N, N_sign, Ex, Ex_sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_exponent(Input@2, N, N_sign, Ex * 10, Ex_sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_exponent(Input@3, N, N_sign, (Ex * 10) + 1, Ex_sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_exponent(Input@4, N, N_sign, (Ex * 10) + 2, Ex_sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_exponent(Input@5, N, N_sign, (Ex * 10) + 3, Ex_sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_exponent(Input@6, N, N_sign, (Ex * 10) + 4, Ex_sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_exponent(Input@7, N, N_sign, (Ex * 10) + 5, Ex_sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_exponent(Input@8, N, N_sign, (Ex * 10) + 6, Ex_sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_exponent(Input@9, N, N_sign, (Ex * 10) + 7, Ex_sign);

        [<<"8"/utf8>> | Input@10] ->
            parse_exponent(Input@10, N, N_sign, (Ex * 10) + 8, Ex_sign);

        [<<"9"/utf8>> | Input@11] ->
            parse_exponent(Input@11, N, N_sign, (Ex * 10) + 9, Ex_sign);

        Input@12 ->
            Number = case N_sign of
                positive ->
                    N;

                negative ->
                    N * -1.0
            end,
            Exponent = erlang:float(case Ex_sign of
                    positive ->
                        Ex;

                    negative ->
                        - Ex
                end),
            Multiplier@1 = case gleam@float:power(10.0, Exponent) of
                {ok, Multiplier} ->
                    Multiplier;

                {error, _} ->
                    1.0
            end,
            {ok, {{float, Number * Multiplier@1}, Input@12}}
    end.

-file("src/tom.gleam", 967).
-spec parse_float(list(binary()), float(), sign(), float()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_float(Input, Number, Sign, Unit) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_float(Input@1, Number, Sign, Unit);

        [<<"0"/utf8>> | Input@2] ->
            parse_float(Input@2, Number, Sign, Unit * 0.1);

        [<<"1"/utf8>> | Input@3] ->
            parse_float(Input@3, Number + (1.0 * Unit), Sign, Unit * 0.1);

        [<<"2"/utf8>> | Input@4] ->
            parse_float(Input@4, Number + (2.0 * Unit), Sign, Unit * 0.1);

        [<<"3"/utf8>> | Input@5] ->
            parse_float(Input@5, Number + (3.0 * Unit), Sign, Unit * 0.1);

        [<<"4"/utf8>> | Input@6] ->
            parse_float(Input@6, Number + (4.0 * Unit), Sign, Unit * 0.1);

        [<<"5"/utf8>> | Input@7] ->
            parse_float(Input@7, Number + (5.0 * Unit), Sign, Unit * 0.1);

        [<<"6"/utf8>> | Input@8] ->
            parse_float(Input@8, Number + (6.0 * Unit), Sign, Unit * 0.1);

        [<<"7"/utf8>> | Input@9] ->
            parse_float(Input@9, Number + (7.0 * Unit), Sign, Unit * 0.1);

        [<<"8"/utf8>> | Input@10] ->
            parse_float(Input@10, Number + (8.0 * Unit), Sign, Unit * 0.1);

        [<<"9"/utf8>> | Input@11] ->
            parse_float(Input@11, Number + (9.0 * Unit), Sign, Unit * 0.1);

        [<<"e"/utf8>>, <<"+"/utf8>> | Input@12] ->
            parse_exponent(Input@12, Number, Sign, 0, positive);

        [<<"e"/utf8>>, <<"-"/utf8>> | Input@13] ->
            parse_exponent(Input@13, Number, Sign, 0, negative);

        [<<"e"/utf8>> | Input@14] ->
            parse_exponent(Input@14, Number, Sign, 0, positive);

        [<<"E"/utf8>>, <<"+"/utf8>> | Input@15] ->
            parse_exponent(Input@15, Number, Sign, 0, positive);

        [<<"E"/utf8>>, <<"-"/utf8>> | Input@16] ->
            parse_exponent(Input@16, Number, Sign, 0, negative);

        [<<"E"/utf8>> | Input@17] ->
            parse_exponent(Input@17, Number, Sign, 0, positive);

        Input@18 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    Number * -1.0
            end,
            {ok, {{float, Number@1}, Input@18}}
    end.

-file("src/tom.gleam", 1013).
-spec parse_string(list(binary()), binary()) -> {ok, {toml(), list(binary())}} |
    {error, parse_error()}.
parse_string(Input, String) ->
    case Input of
        [<<"\""/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [<<"\\"/utf8>>, <<"t"/utf8>> | Input@2] ->
            parse_string(Input@2, <<String/binary, "\t"/utf8>>);

        [<<"\\"/utf8>>, <<"e"/utf8>> | Input@3] ->
            parse_string(Input@3, <<String/binary, "\x{001b}"/utf8>>);

        [<<"\\"/utf8>>, <<"b"/utf8>> | Input@4] ->
            parse_string(Input@4, <<String/binary, "\x{0008}"/utf8>>);

        [<<"\\"/utf8>>, <<"n"/utf8>> | Input@5] ->
            parse_string(Input@5, <<String/binary, "\n"/utf8>>);

        [<<"\\"/utf8>>, <<"r"/utf8>> | Input@6] ->
            parse_string(Input@6, <<String/binary, "\r"/utf8>>);

        [<<"\\"/utf8>>, <<"f"/utf8>> | Input@7] ->
            parse_string(Input@7, <<String/binary, "\f"/utf8>>);

        [<<"\\"/utf8>>, <<"\""/utf8>> | Input@8] ->
            parse_string(Input@8, <<String/binary, "\""/utf8>>);

        [<<"\\"/utf8>>, <<"\\"/utf8>> | Input@9] ->
            parse_string(Input@9, <<String/binary, "\\"/utf8>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"\""/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"\""/utf8>>}};

        [G | Input@10] ->
            parse_string(Input@10, <<String/binary, G/binary>>)
    end.

-file("src/tom.gleam", 1031).
-spec parse_multi_line_string(list(binary()), binary()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_multi_line_string(Input, String) ->
    case Input of
        [<<"\""/utf8>>, <<"\""/utf8>>, <<"\""/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [<<"\\"/utf8>>, <<"\n"/utf8>> | Input@2] ->
            parse_multi_line_string(skip_whitespace(Input@2), String);

        [<<"\\"/utf8>>, <<"\r\n"/utf8>> | Input@3] ->
            parse_multi_line_string(skip_whitespace(Input@3), String);

        [<<"\r\n"/utf8>> | Input@4] when String =:= <<""/utf8>> ->
            parse_multi_line_string(Input@4, String);

        [<<"\n"/utf8>> | Input@5] when String =:= <<""/utf8>> ->
            parse_multi_line_string(Input@5, String);

        [<<"\r\n"/utf8>> | Input@6] when String =:= <<""/utf8>> ->
            parse_multi_line_string(Input@6, String);

        [<<"\\"/utf8>>, <<"t"/utf8>> | Input@7] ->
            parse_multi_line_string(Input@7, <<String/binary, "\t"/utf8>>);

        [<<"\\"/utf8>>, <<"n"/utf8>> | Input@8] ->
            parse_multi_line_string(Input@8, <<String/binary, "\n"/utf8>>);

        [<<"\\"/utf8>>, <<"r"/utf8>> | Input@9] ->
            parse_multi_line_string(Input@9, <<String/binary, "\r"/utf8>>);

        [<<"\\"/utf8>>, <<"\""/utf8>> | Input@10] ->
            parse_multi_line_string(Input@10, <<String/binary, "\""/utf8>>);

        [<<"\\"/utf8>>, <<"\\"/utf8>> | Input@11] ->
            parse_multi_line_string(Input@11, <<String/binary, "\\"/utf8>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [G | Input@12] ->
            parse_multi_line_string(Input@12, <<String/binary, G/binary>>)
    end.

-file("src/tom.gleam", 1051).
-spec parse_multi_line_literal_string(list(binary()), binary()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_multi_line_literal_string(Input, String) ->
    case Input of
        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [<<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>> | _] ->
            {error, {unexpected, <<"''''"/utf8>>, <<"'''"/utf8>>}};

        [<<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [<<"\n"/utf8>> | Input@2] when String =:= <<""/utf8>> ->
            parse_multi_line_literal_string(Input@2, String);

        [<<"\r\n"/utf8>> | Input@3] when String =:= <<""/utf8>> ->
            parse_multi_line_literal_string(Input@3, String);

        [G | Input@4] ->
            parse_multi_line_literal_string(
                Input@4,
                <<String/binary, G/binary>>
            )
    end.

-file("src/tom.gleam", 1067).
-spec parse_literal_string(list(binary()), binary()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_literal_string(Input, String) ->
    case Input of
        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"'"/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"'"/utf8>>}};

        [<<"'"/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [G | Input@2] ->
            parse_literal_string(Input@2, <<String/binary, G/binary>>)
    end.

-file("src/tom.gleam", 1169).
-spec parse_time_ns(list(binary()), integer(), integer(), integer()) -> {ok,
        {{integer(), integer()}, list(binary())}} |
    {error, parse_error()}.
parse_time_ns(Input, Seconds, Ns, Digits_count) ->
    case Input of
        [<<"0"/utf8>> | Input@1] when Digits_count < 9 ->
            parse_time_ns(Input@1, Seconds, (Ns * 10) + 0, Digits_count + 1);

        [<<"1"/utf8>> | Input@2] when Digits_count < 9 ->
            parse_time_ns(Input@2, Seconds, (Ns * 10) + 1, Digits_count + 1);

        [<<"2"/utf8>> | Input@3] when Digits_count < 9 ->
            parse_time_ns(Input@3, Seconds, (Ns * 10) + 2, Digits_count + 1);

        [<<"3"/utf8>> | Input@4] when Digits_count < 9 ->
            parse_time_ns(Input@4, Seconds, (Ns * 10) + 3, Digits_count + 1);

        [<<"4"/utf8>> | Input@5] when Digits_count < 9 ->
            parse_time_ns(Input@5, Seconds, (Ns * 10) + 4, Digits_count + 1);

        [<<"5"/utf8>> | Input@6] when Digits_count < 9 ->
            parse_time_ns(Input@6, Seconds, (Ns * 10) + 5, Digits_count + 1);

        [<<"6"/utf8>> | Input@7] when Digits_count < 9 ->
            parse_time_ns(Input@7, Seconds, (Ns * 10) + 6, Digits_count + 1);

        [<<"7"/utf8>> | Input@8] when Digits_count < 9 ->
            parse_time_ns(Input@8, Seconds, (Ns * 10) + 7, Digits_count + 1);

        [<<"8"/utf8>> | Input@9] when Digits_count < 9 ->
            parse_time_ns(Input@9, Seconds, (Ns * 10) + 8, Digits_count + 1);

        [<<"9"/utf8>> | Input@10] when Digits_count < 9 ->
            parse_time_ns(Input@10, Seconds, (Ns * 10) + 9, Digits_count + 1);

        _ ->
            Exponent = erlang:float(9 - Digits_count),
            Multiplier@1 = case gleam@float:power(10.0, Exponent) of
                {ok, Multiplier} -> Multiplier;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"tom"/utf8>>,
                                function => <<"parse_time_ns"/utf8>>,
                                line => 1200,
                                value => _assert_fail,
                                start => 37108,
                                'end' => 37163,
                                pattern_start => 37119,
                                pattern_end => 37133})
            end,
            {ok, {{Seconds, Ns * erlang:trunc(Multiplier@1)}, Input}}
    end.

-file("src/tom.gleam", 1206).
-spec parse_number_under_60(list(binary()), binary()) -> {ok,
        {integer(), list(binary())}} |
    {error, parse_error()}.
parse_number_under_60(Input, Expected) ->
    case Input of
        [<<"0"/utf8>>, <<"0"/utf8>> | Input@1] ->
            {ok, {0, Input@1}};

        [<<"0"/utf8>>, <<"1"/utf8>> | Input@2] ->
            {ok, {1, Input@2}};

        [<<"0"/utf8>>, <<"2"/utf8>> | Input@3] ->
            {ok, {2, Input@3}};

        [<<"0"/utf8>>, <<"3"/utf8>> | Input@4] ->
            {ok, {3, Input@4}};

        [<<"0"/utf8>>, <<"4"/utf8>> | Input@5] ->
            {ok, {4, Input@5}};

        [<<"0"/utf8>>, <<"5"/utf8>> | Input@6] ->
            {ok, {5, Input@6}};

        [<<"0"/utf8>>, <<"6"/utf8>> | Input@7] ->
            {ok, {6, Input@7}};

        [<<"0"/utf8>>, <<"7"/utf8>> | Input@8] ->
            {ok, {7, Input@8}};

        [<<"0"/utf8>>, <<"8"/utf8>> | Input@9] ->
            {ok, {8, Input@9}};

        [<<"0"/utf8>>, <<"9"/utf8>> | Input@10] ->
            {ok, {9, Input@10}};

        [<<"1"/utf8>>, <<"0"/utf8>> | Input@11] ->
            {ok, {10, Input@11}};

        [<<"1"/utf8>>, <<"1"/utf8>> | Input@12] ->
            {ok, {11, Input@12}};

        [<<"1"/utf8>>, <<"2"/utf8>> | Input@13] ->
            {ok, {12, Input@13}};

        [<<"1"/utf8>>, <<"3"/utf8>> | Input@14] ->
            {ok, {13, Input@14}};

        [<<"1"/utf8>>, <<"4"/utf8>> | Input@15] ->
            {ok, {14, Input@15}};

        [<<"1"/utf8>>, <<"5"/utf8>> | Input@16] ->
            {ok, {15, Input@16}};

        [<<"1"/utf8>>, <<"6"/utf8>> | Input@17] ->
            {ok, {16, Input@17}};

        [<<"1"/utf8>>, <<"7"/utf8>> | Input@18] ->
            {ok, {17, Input@18}};

        [<<"1"/utf8>>, <<"8"/utf8>> | Input@19] ->
            {ok, {18, Input@19}};

        [<<"1"/utf8>>, <<"9"/utf8>> | Input@20] ->
            {ok, {19, Input@20}};

        [<<"2"/utf8>>, <<"0"/utf8>> | Input@21] ->
            {ok, {20, Input@21}};

        [<<"2"/utf8>>, <<"1"/utf8>> | Input@22] ->
            {ok, {21, Input@22}};

        [<<"2"/utf8>>, <<"2"/utf8>> | Input@23] ->
            {ok, {22, Input@23}};

        [<<"2"/utf8>>, <<"3"/utf8>> | Input@24] ->
            {ok, {23, Input@24}};

        [<<"2"/utf8>>, <<"4"/utf8>> | Input@25] ->
            {ok, {24, Input@25}};

        [<<"2"/utf8>>, <<"5"/utf8>> | Input@26] ->
            {ok, {25, Input@26}};

        [<<"2"/utf8>>, <<"6"/utf8>> | Input@27] ->
            {ok, {26, Input@27}};

        [<<"2"/utf8>>, <<"7"/utf8>> | Input@28] ->
            {ok, {27, Input@28}};

        [<<"2"/utf8>>, <<"8"/utf8>> | Input@29] ->
            {ok, {28, Input@29}};

        [<<"2"/utf8>>, <<"9"/utf8>> | Input@30] ->
            {ok, {29, Input@30}};

        [<<"3"/utf8>>, <<"0"/utf8>> | Input@31] ->
            {ok, {30, Input@31}};

        [<<"3"/utf8>>, <<"1"/utf8>> | Input@32] ->
            {ok, {31, Input@32}};

        [<<"3"/utf8>>, <<"2"/utf8>> | Input@33] ->
            {ok, {32, Input@33}};

        [<<"3"/utf8>>, <<"3"/utf8>> | Input@34] ->
            {ok, {33, Input@34}};

        [<<"3"/utf8>>, <<"4"/utf8>> | Input@35] ->
            {ok, {34, Input@35}};

        [<<"3"/utf8>>, <<"5"/utf8>> | Input@36] ->
            {ok, {35, Input@36}};

        [<<"3"/utf8>>, <<"6"/utf8>> | Input@37] ->
            {ok, {36, Input@37}};

        [<<"3"/utf8>>, <<"7"/utf8>> | Input@38] ->
            {ok, {37, Input@38}};

        [<<"3"/utf8>>, <<"8"/utf8>> | Input@39] ->
            {ok, {38, Input@39}};

        [<<"3"/utf8>>, <<"9"/utf8>> | Input@40] ->
            {ok, {39, Input@40}};

        [<<"4"/utf8>>, <<"0"/utf8>> | Input@41] ->
            {ok, {40, Input@41}};

        [<<"4"/utf8>>, <<"1"/utf8>> | Input@42] ->
            {ok, {41, Input@42}};

        [<<"4"/utf8>>, <<"2"/utf8>> | Input@43] ->
            {ok, {42, Input@43}};

        [<<"4"/utf8>>, <<"3"/utf8>> | Input@44] ->
            {ok, {43, Input@44}};

        [<<"4"/utf8>>, <<"4"/utf8>> | Input@45] ->
            {ok, {44, Input@45}};

        [<<"4"/utf8>>, <<"5"/utf8>> | Input@46] ->
            {ok, {45, Input@46}};

        [<<"4"/utf8>>, <<"6"/utf8>> | Input@47] ->
            {ok, {46, Input@47}};

        [<<"4"/utf8>>, <<"7"/utf8>> | Input@48] ->
            {ok, {47, Input@48}};

        [<<"4"/utf8>>, <<"8"/utf8>> | Input@49] ->
            {ok, {48, Input@49}};

        [<<"4"/utf8>>, <<"9"/utf8>> | Input@50] ->
            {ok, {49, Input@50}};

        [<<"5"/utf8>>, <<"0"/utf8>> | Input@51] ->
            {ok, {50, Input@51}};

        [<<"5"/utf8>>, <<"1"/utf8>> | Input@52] ->
            {ok, {51, Input@52}};

        [<<"5"/utf8>>, <<"2"/utf8>> | Input@53] ->
            {ok, {52, Input@53}};

        [<<"5"/utf8>>, <<"3"/utf8>> | Input@54] ->
            {ok, {53, Input@54}};

        [<<"5"/utf8>>, <<"4"/utf8>> | Input@55] ->
            {ok, {54, Input@55}};

        [<<"5"/utf8>>, <<"5"/utf8>> | Input@56] ->
            {ok, {55, Input@56}};

        [<<"5"/utf8>>, <<"6"/utf8>> | Input@57] ->
            {ok, {56, Input@57}};

        [<<"5"/utf8>>, <<"7"/utf8>> | Input@58] ->
            {ok, {57, Input@58}};

        [<<"5"/utf8>>, <<"8"/utf8>> | Input@59] ->
            {ok, {58, Input@59}};

        [<<"5"/utf8>>, <<"9"/utf8>> | Input@60] ->
            {ok, {59, Input@60}};

        [G | _] ->
            {error, {unexpected, G, Expected}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, Expected}}
    end.

-file("src/tom.gleam", 1114).
-spec parse_hour_minute(list(binary())) -> {ok,
        {{integer(), integer()}, list(binary())}} |
    {error, parse_error()}.
parse_hour_minute(Input) ->
    do(case Input of
            [<<"0"/utf8>>, <<"0"/utf8>>, <<":"/utf8>> | Input@1] ->
                {ok, {0, Input@1}};

            [<<"0"/utf8>>, <<"1"/utf8>>, <<":"/utf8>> | Input@2] ->
                {ok, {1, Input@2}};

            [<<"0"/utf8>>, <<"2"/utf8>>, <<":"/utf8>> | Input@3] ->
                {ok, {2, Input@3}};

            [<<"0"/utf8>>, <<"3"/utf8>>, <<":"/utf8>> | Input@4] ->
                {ok, {3, Input@4}};

            [<<"0"/utf8>>, <<"4"/utf8>>, <<":"/utf8>> | Input@5] ->
                {ok, {4, Input@5}};

            [<<"0"/utf8>>, <<"5"/utf8>>, <<":"/utf8>> | Input@6] ->
                {ok, {5, Input@6}};

            [<<"0"/utf8>>, <<"6"/utf8>>, <<":"/utf8>> | Input@7] ->
                {ok, {6, Input@7}};

            [<<"0"/utf8>>, <<"7"/utf8>>, <<":"/utf8>> | Input@8] ->
                {ok, {7, Input@8}};

            [<<"0"/utf8>>, <<"8"/utf8>>, <<":"/utf8>> | Input@9] ->
                {ok, {8, Input@9}};

            [<<"0"/utf8>>, <<"9"/utf8>>, <<":"/utf8>> | Input@10] ->
                {ok, {9, Input@10}};

            [<<"1"/utf8>>, <<"0"/utf8>>, <<":"/utf8>> | Input@11] ->
                {ok, {10, Input@11}};

            [<<"1"/utf8>>, <<"1"/utf8>>, <<":"/utf8>> | Input@12] ->
                {ok, {11, Input@12}};

            [<<"1"/utf8>>, <<"2"/utf8>>, <<":"/utf8>> | Input@13] ->
                {ok, {12, Input@13}};

            [<<"1"/utf8>>, <<"3"/utf8>>, <<":"/utf8>> | Input@14] ->
                {ok, {13, Input@14}};

            [<<"1"/utf8>>, <<"4"/utf8>>, <<":"/utf8>> | Input@15] ->
                {ok, {14, Input@15}};

            [<<"1"/utf8>>, <<"5"/utf8>>, <<":"/utf8>> | Input@16] ->
                {ok, {15, Input@16}};

            [<<"1"/utf8>>, <<"6"/utf8>>, <<":"/utf8>> | Input@17] ->
                {ok, {16, Input@17}};

            [<<"1"/utf8>>, <<"7"/utf8>>, <<":"/utf8>> | Input@18] ->
                {ok, {17, Input@18}};

            [<<"1"/utf8>>, <<"8"/utf8>>, <<":"/utf8>> | Input@19] ->
                {ok, {18, Input@19}};

            [<<"1"/utf8>>, <<"9"/utf8>>, <<":"/utf8>> | Input@20] ->
                {ok, {19, Input@20}};

            [<<"2"/utf8>>, <<"0"/utf8>>, <<":"/utf8>> | Input@21] ->
                {ok, {20, Input@21}};

            [<<"2"/utf8>>, <<"1"/utf8>>, <<":"/utf8>> | Input@22] ->
                {ok, {21, Input@22}};

            [<<"2"/utf8>>, <<"2"/utf8>>, <<":"/utf8>> | Input@23] ->
                {ok, {22, Input@23}};

            [<<"2"/utf8>>, <<"3"/utf8>>, <<":"/utf8>> | Input@24] ->
                {ok, {23, Input@24}};

            [G | _] ->
                {error, {unexpected, G, <<"time"/utf8>>}};

            [] ->
                {error, {unexpected, <<"EOF"/utf8>>, <<"time"/utf8>>}}
        end, fun(Hours, Input@25) ->
            do(
                parse_number_under_60(Input@25, <<"minutes"/utf8>>),
                fun(Minutes, Input@26) -> {ok, {{Hours, Minutes}, Input@26}} end
            )
        end).

-file("src/tom.gleam", 1155).
-spec parse_time_s_ns(list(binary())) -> {ok,
        {{integer(), integer()}, list(binary())}} |
    {error, parse_error()}.
parse_time_s_ns(Input) ->
    case Input of
        [<<":"/utf8>> | Input@1] ->
            do(
                parse_number_under_60(Input@1, <<"seconds"/utf8>>),
                fun(Seconds, Input@2) -> case Input@2 of
                        [<<"."/utf8>> | Input@3] ->
                            parse_time_ns(Input@3, Seconds, 0, 0);

                        _ ->
                            {ok, {{Seconds, 0}, Input@2}}
                    end end
            );

        _ ->
            {ok, {{0, 0}, Input}}
    end.

-file("src/tom.gleam", 1107).
-spec parse_time_minute(list(binary()), integer()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_time_minute(Input, Hours) ->
    do(
        parse_number_under_60(Input, <<"minutes"/utf8>>),
        fun(Minutes, Input@1) ->
            do(
                parse_time_s_ns(Input@1),
                fun(_use0, Input@2) ->
                    {Seconds, Ns} = _use0,
                    Time = {time_of_day, Hours, Minutes, Seconds, Ns},
                    {ok, {{time, Time}, Input@2}}
                end
            )
        end
    ).

-file("src/tom.gleam", 1148).
-spec parse_time_value(list(binary())) -> {ok,
        {gleam@time@calendar:time_of_day(), list(binary())}} |
    {error, parse_error()}.
parse_time_value(Input) ->
    do(
        parse_hour_minute(Input),
        fun(_use0, Input@1) ->
            {Hours, Minutes} = _use0,
            do(
                parse_time_s_ns(Input@1),
                fun(_use0@1, Input@2) ->
                    {Seconds, Ns} = _use0@1,
                    Time = {time_of_day, Hours, Minutes, Seconds, Ns},
                    {ok, {Time, Input@2}}
                end
            )
        end
    ).

-file("src/tom.gleam", 1364).
-spec parse_offset_hours(list(binary()), sign()) -> {ok,
        {offset(), list(binary())}} |
    {error, parse_error()}.
parse_offset_hours(Input, Sign) ->
    do(
        parse_hour_minute(Input),
        fun(_use0, Input@1) ->
            {Hours, Minutes} = _use0,
            Duration = case Sign of
                positive ->
                    gleam@time@duration:add(
                        gleam@time@duration:hours(Hours),
                        gleam@time@duration:minutes(Minutes)
                    );

                negative ->
                    gleam@time@duration:add(
                        gleam@time@duration:hours(- Hours),
                        gleam@time@duration:minutes(- Minutes)
                    )
            end,
            {ok, {{offset, Duration}, Input@1}}
        end
    ).

-file("src/tom.gleam", 1355).
-spec parse_offset(list(binary())) -> {ok, {offset(), list(binary())}} |
    {error, parse_error()}.
parse_offset(Input) ->
    case Input of
        [<<"Z"/utf8>> | Input@1] ->
            {ok, {{offset, {duration, 0, 0}}, Input@1}};

        [<<"+"/utf8>> | Input@2] ->
            parse_offset_hours(Input@2, positive);

        [<<"-"/utf8>> | Input@3] ->
            parse_offset_hours(Input@3, negative);

        _ ->
            {ok, {local, Input}}
    end.

-file("src/tom.gleam", 1337).
-spec parse_date_end(
    list(binary()),
    integer(),
    gleam@time@calendar:month(),
    integer()
) -> {ok, {toml(), list(binary())}} | {error, parse_error()}.
parse_date_end(Input, Year, Month, Day) ->
    Date = {date, Year, Month, Day},
    case Input of
        [<<" "/utf8>> | Input@1] ->
            do(
                parse_time_value(Input@1),
                fun(Time, Input@2) ->
                    do(
                        parse_offset(Input@2),
                        fun(Offset, Input@3) ->
                            {ok, {{date_time, Date, Time, Offset}, Input@3}}
                        end
                    )
                end
            );

        [<<"T"/utf8>> | Input@1] ->
            do(
                parse_time_value(Input@1),
                fun(Time, Input@2) ->
                    do(
                        parse_offset(Input@2),
                        fun(Offset, Input@3) ->
                            {ok, {{date_time, Date, Time, Offset}, Input@3}}
                        end
                    )
                end
            );

        _ ->
            {ok, {{date, Date}, Input}}
    end.

-file("src/tom.gleam", 1294).
-spec parse_date_day(list(binary()), integer(), gleam@time@calendar:month()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_date_day(Input, Year, Month) ->
    case Input of
        [<<"0"/utf8>>, <<"1"/utf8>> | Input@1] ->
            parse_date_end(Input@1, Year, Month, 1);

        [<<"0"/utf8>>, <<"2"/utf8>> | Input@2] ->
            parse_date_end(Input@2, Year, Month, 2);

        [<<"0"/utf8>>, <<"3"/utf8>> | Input@3] ->
            parse_date_end(Input@3, Year, Month, 3);

        [<<"0"/utf8>>, <<"4"/utf8>> | Input@4] ->
            parse_date_end(Input@4, Year, Month, 4);

        [<<"0"/utf8>>, <<"5"/utf8>> | Input@5] ->
            parse_date_end(Input@5, Year, Month, 5);

        [<<"0"/utf8>>, <<"6"/utf8>> | Input@6] ->
            parse_date_end(Input@6, Year, Month, 6);

        [<<"0"/utf8>>, <<"7"/utf8>> | Input@7] ->
            parse_date_end(Input@7, Year, Month, 7);

        [<<"0"/utf8>>, <<"8"/utf8>> | Input@8] ->
            parse_date_end(Input@8, Year, Month, 8);

        [<<"0"/utf8>>, <<"9"/utf8>> | Input@9] ->
            parse_date_end(Input@9, Year, Month, 9);

        [<<"1"/utf8>>, <<"0"/utf8>> | Input@10] ->
            parse_date_end(Input@10, Year, Month, 10);

        [<<"1"/utf8>>, <<"1"/utf8>> | Input@11] ->
            parse_date_end(Input@11, Year, Month, 11);

        [<<"1"/utf8>>, <<"2"/utf8>> | Input@12] ->
            parse_date_end(Input@12, Year, Month, 12);

        [<<"1"/utf8>>, <<"3"/utf8>> | Input@13] ->
            parse_date_end(Input@13, Year, Month, 13);

        [<<"1"/utf8>>, <<"4"/utf8>> | Input@14] ->
            parse_date_end(Input@14, Year, Month, 14);

        [<<"1"/utf8>>, <<"5"/utf8>> | Input@15] ->
            parse_date_end(Input@15, Year, Month, 15);

        [<<"1"/utf8>>, <<"6"/utf8>> | Input@16] ->
            parse_date_end(Input@16, Year, Month, 16);

        [<<"1"/utf8>>, <<"7"/utf8>> | Input@17] ->
            parse_date_end(Input@17, Year, Month, 17);

        [<<"1"/utf8>>, <<"8"/utf8>> | Input@18] ->
            parse_date_end(Input@18, Year, Month, 18);

        [<<"1"/utf8>>, <<"9"/utf8>> | Input@19] ->
            parse_date_end(Input@19, Year, Month, 19);

        [<<"2"/utf8>>, <<"0"/utf8>> | Input@20] ->
            parse_date_end(Input@20, Year, Month, 20);

        [<<"2"/utf8>>, <<"1"/utf8>> | Input@21] ->
            parse_date_end(Input@21, Year, Month, 21);

        [<<"2"/utf8>>, <<"2"/utf8>> | Input@22] ->
            parse_date_end(Input@22, Year, Month, 22);

        [<<"2"/utf8>>, <<"3"/utf8>> | Input@23] ->
            parse_date_end(Input@23, Year, Month, 23);

        [<<"2"/utf8>>, <<"4"/utf8>> | Input@24] ->
            parse_date_end(Input@24, Year, Month, 24);

        [<<"2"/utf8>>, <<"5"/utf8>> | Input@25] ->
            parse_date_end(Input@25, Year, Month, 25);

        [<<"2"/utf8>>, <<"6"/utf8>> | Input@26] ->
            parse_date_end(Input@26, Year, Month, 26);

        [<<"2"/utf8>>, <<"7"/utf8>> | Input@27] ->
            parse_date_end(Input@27, Year, Month, 27);

        [<<"2"/utf8>>, <<"8"/utf8>> | Input@28] ->
            parse_date_end(Input@28, Year, Month, 28);

        [<<"2"/utf8>>, <<"9"/utf8>> | Input@29] ->
            parse_date_end(Input@29, Year, Month, 29);

        [<<"3"/utf8>>, <<"0"/utf8>> | Input@30] ->
            parse_date_end(Input@30, Year, Month, 30);

        [<<"3"/utf8>>, <<"1"/utf8>> | Input@31] ->
            parse_date_end(Input@31, Year, Month, 31);

        [G | _] ->
            {error, {unexpected, G, <<"date day"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"date day"/utf8>>}}
    end.

-file("src/tom.gleam", 1274).
-spec parse_date(list(binary()), integer()) -> {ok, {toml(), list(binary())}} |
    {error, parse_error()}.
parse_date(Input, Year) ->
    case Input of
        [<<"0"/utf8>>, <<"1"/utf8>>, <<"-"/utf8>> | Input@1] ->
            parse_date_day(Input@1, Year, january);

        [<<"0"/utf8>>, <<"2"/utf8>>, <<"-"/utf8>> | Input@2] ->
            parse_date_day(Input@2, Year, february);

        [<<"0"/utf8>>, <<"3"/utf8>>, <<"-"/utf8>> | Input@3] ->
            parse_date_day(Input@3, Year, march);

        [<<"0"/utf8>>, <<"4"/utf8>>, <<"-"/utf8>> | Input@4] ->
            parse_date_day(Input@4, Year, april);

        [<<"0"/utf8>>, <<"5"/utf8>>, <<"-"/utf8>> | Input@5] ->
            parse_date_day(Input@5, Year, may);

        [<<"0"/utf8>>, <<"6"/utf8>>, <<"-"/utf8>> | Input@6] ->
            parse_date_day(Input@6, Year, june);

        [<<"0"/utf8>>, <<"7"/utf8>>, <<"-"/utf8>> | Input@7] ->
            parse_date_day(Input@7, Year, july);

        [<<"0"/utf8>>, <<"8"/utf8>>, <<"-"/utf8>> | Input@8] ->
            parse_date_day(Input@8, Year, august);

        [<<"0"/utf8>>, <<"9"/utf8>>, <<"-"/utf8>> | Input@9] ->
            parse_date_day(Input@9, Year, september);

        [<<"1"/utf8>>, <<"0"/utf8>>, <<"-"/utf8>> | Input@10] ->
            parse_date_day(Input@10, Year, october);

        [<<"1"/utf8>>, <<"1"/utf8>>, <<"-"/utf8>> | Input@11] ->
            parse_date_day(Input@11, Year, november);

        [<<"1"/utf8>>, <<"2"/utf8>>, <<"-"/utf8>> | Input@12] ->
            parse_date_day(Input@12, Year, december);

        [G | _] ->
            {error, {unexpected, G, <<"date month"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"date month"/utf8>>}}
    end.

-file("src/tom.gleam", 884).
-spec parse_number(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_number(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_number(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_number(Input@2, (Number * 10) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_number(Input@3, (Number * 10) + 1, Sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_number(Input@4, (Number * 10) + 2, Sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_number(Input@5, (Number * 10) + 3, Sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_number(Input@6, (Number * 10) + 4, Sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_number(Input@7, (Number * 10) + 5, Sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_number(Input@8, (Number * 10) + 6, Sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_number(Input@9, (Number * 10) + 7, Sign);

        [<<"8"/utf8>> | Input@10] ->
            parse_number(Input@10, (Number * 10) + 8, Sign);

        [<<"9"/utf8>> | Input@11] ->
            parse_number(Input@11, (Number * 10) + 9, Sign);

        [<<"-"/utf8>> | Input@12] ->
            parse_date(Input@12, Number);

        [<<":"/utf8>> | Input@13] when Number < 24 ->
            parse_time_minute(Input@13, Number);

        [<<"."/utf8>> | Input@14] ->
            parse_float(Input@14, erlang:float(Number), Sign, 0.1);

        [<<"e"/utf8>>, <<"+"/utf8>> | Input@15] ->
            parse_exponent(Input@15, erlang:float(Number), Sign, 0, positive);

        [<<"e"/utf8>>, <<"-"/utf8>> | Input@16] ->
            parse_exponent(Input@16, erlang:float(Number), Sign, 0, negative);

        [<<"e"/utf8>> | Input@17] ->
            parse_exponent(Input@17, erlang:float(Number), Sign, 0, positive);

        [<<"E"/utf8>>, <<"+"/utf8>> | Input@18] ->
            parse_exponent(Input@18, erlang:float(Number), Sign, 0, positive);

        [<<"E"/utf8>>, <<"-"/utf8>> | Input@19] ->
            parse_exponent(Input@19, erlang:float(Number), Sign, 0, negative);

        [<<"E"/utf8>> | Input@20] ->
            parse_exponent(Input@20, erlang:float(Number), Sign, 0, positive);

        Input@21 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@21}}
    end.

-file("src/tom.gleam", 1386).
?DOC(
    " Get a int from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_int(Int(1))\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_int(Float(1.4))\n"
    " // -> Error(WrongType([], \"Int\", \"Float\"))\n"
    " ```\n"
).
-spec as_int(toml()) -> {ok, integer()} | {error, get_error()}.
as_int(Toml) ->
    case Toml of
        {int, F} ->
            {ok, F};

        Other ->
            {error, {wrong_type, [], <<"Int"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1406).
?DOC(
    " Get a float from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_float(Float(1.5))\n"
    " // -> Ok(1.5)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_float(Int(1))\n"
    " // -> Error(WrongType([], \"Float\", \"Int\"))\n"
    " ```\n"
).
-spec as_float(toml()) -> {ok, float()} | {error, get_error()}.
as_float(Toml) ->
    case Toml of
        {float, F} ->
            {ok, F};

        Other ->
            {error, {wrong_type, [], <<"Float"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1426).
?DOC(
    " Get a bool from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_bool(Bool(true))\n"
    " // -> Ok(true)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_bool(Int(1))\n"
    " // -> Error(WrongType([], \"Bool\", \"Int\"))\n"
    " ```\n"
).
-spec as_bool(toml()) -> {ok, boolean()} | {error, get_error()}.
as_bool(Toml) ->
    case Toml of
        {bool, B} ->
            {ok, B};

        Other ->
            {error, {wrong_type, [], <<"Bool"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1446).
?DOC(
    " Get a string from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_string(String(\"hello\"))\n"
    " // -> Ok(\"hello\")\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_string(Int(1))\n"
    " // -> Error(WrongType([], \"String\", \"Int\"))\n"
    " ```\n"
).
-spec as_string(toml()) -> {ok, binary()} | {error, get_error()}.
as_string(Toml) ->
    case Toml of
        {string, S} ->
            {ok, S};

        Other ->
            {error, {wrong_type, [], <<"String"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1466).
?DOC(
    " Get a date from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_date(Date(date))\n"
    " // -> Ok(date)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_date(Int(1))\n"
    " // -> Error(WrongType([], \"Date\", \"Int\"))\n"
    " ```\n"
).
-spec as_date(toml()) -> {ok, gleam@time@calendar:date()} | {error, get_error()}.
as_date(Toml) ->
    case Toml of
        {date, D} ->
            {ok, D};

        Other ->
            {error, {wrong_type, [], <<"Date"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1486).
?DOC(
    " Get a time from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_time_of_day(Time(time))\n"
    " // -> Ok(time)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_time_of_day(Int(1))\n"
    " // -> Error(WrongType([], \"Time\", \"Int\"))\n"
    " ```\n"
).
-spec as_time_of_day(toml()) -> {ok, gleam@time@calendar:time_of_day()} |
    {error, get_error()}.
as_time_of_day(Toml) ->
    case Toml of
        {time, T} ->
            {ok, T};

        Other ->
            {error, {wrong_type, [], <<"Time"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1501).
?DOC(
    " Get an unambiguous time from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_timestamp(Int(1))\n"
    " // -> Error(WrongType([], \"DateTime with offset\", \"Int\"))\n"
    " ```\n"
).
-spec as_timestamp(toml()) -> {ok, gleam@time@timestamp:timestamp()} |
    {error, get_error()}.
as_timestamp(Toml) ->
    case Toml of
        {date_time, Date, Time, {offset, Offset}} ->
            {ok, gleam@time@timestamp:from_calendar(Date, Time, Offset)};

        Other ->
            {error,
                {wrong_type,
                    [],
                    <<"DateTime with offset"/utf8>>,
                    classify(Other)}}
    end.

-file("src/tom.gleam", 1523).
?DOC(
    " Get a datetime from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_calendar_time(DateTime(datetime))\n"
    " // -> Ok(datetime)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_calendar_time(Int(1))\n"
    " // -> Error(WrongType([], \"DateTime\", \"Int\"))\n"
    " ```\n"
).
-spec as_calendar_time(toml()) -> {ok,
        {gleam@time@calendar:date(),
            gleam@time@calendar:time_of_day(),
            offset()}} |
    {error, get_error()}.
as_calendar_time(Toml) ->
    case Toml of
        {date_time, D, T, O} ->
            {ok, {D, T, O}};

        Other ->
            {error, {wrong_type, [], <<"DateTime"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1545).
?DOC(
    " Get an array from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_array(Array([]))\n"
    " // -> Ok([])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_array(Int(1))\n"
    " // -> Error(WrongType([], \"Array\", \"Int\"))\n"
    " ```\n"
).
-spec as_array(toml()) -> {ok, list(toml())} | {error, get_error()}.
as_array(Toml) ->
    case Toml of
        {array, Arr} ->
            {ok, Arr};

        Other ->
            {error, {wrong_type, [], <<"Array"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1565).
?DOC(
    " Get a table from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_table(Table(dict.new()))\n"
    " // -> Ok(dict.new())\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_table(Int(1))\n"
    " // -> Error(WrongType([], \"Table\", \"Int\"))\n"
    " ```\n"
).
-spec as_table(toml()) -> {ok, gleam@dict:dict(binary(), toml())} |
    {error, get_error()}.
as_table(Toml) ->
    case Toml of
        {table, Tbl} ->
            {ok, Tbl};

        {inline_table, Tbl@1} ->
            {ok, Tbl@1};

        Other ->
            {error, {wrong_type, [], <<"Table"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1591).
?DOC(
    " Get a number (int or float) from a TOML document.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " as_number(Int(1))\n"
    " // -> Ok(NumberInt(1))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_number(Float(1.5))\n"
    " // -> Ok(NumberFloat(1.5))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " as_number(Bool(true))\n"
    " // -> Error(WrongType([], \"Number\", \"Bool\"))\n"
    " ```\n"
).
-spec as_number(toml()) -> {ok, number_()} | {error, get_error()}.
as_number(Toml) ->
    case Toml of
        {int, X} ->
            {ok, {number_int, X}};

        {float, X@1} ->
            {ok, {number_float, X@1}};

        {nan, X@2} ->
            {ok, {number_nan, X@2}};

        {infinity, X@3} ->
            {ok, {number_infinity, X@3}};

        Other ->
            {error, {wrong_type, [], <<"Number"/utf8>>, classify(Other)}}
    end.

-file("src/tom.gleam", 1077).
-spec reverse_arrays_of_tables(toml()) -> toml().
reverse_arrays_of_tables(Toml) ->
    case Toml of
        {array_of_tables, Tables} ->
            {array_of_tables, reverse_arrays_of_tables_array(Tables, [])};

        {table, Table} ->
            {table, reverse_arrays_of_tables_table(Table)};

        _ ->
            Toml
    end.

-file("src/tom.gleam", 1088).
-spec reverse_arrays_of_tables_table(gleam@dict:dict(binary(), toml())) -> gleam@dict:dict(binary(), toml()).
reverse_arrays_of_tables_table(Table) ->
    gleam@dict:map_values(Table, fun(_, V) -> reverse_arrays_of_tables(V) end).

-file("src/tom.gleam", 1094).
-spec reverse_arrays_of_tables_array(
    list(gleam@dict:dict(binary(), toml())),
    list(gleam@dict:dict(binary(), toml()))
) -> list(gleam@dict:dict(binary(), toml())).
reverse_arrays_of_tables_array(Array, Acc) ->
    case Array of
        [] ->
            Acc;

        [First | Rest] ->
            First@1 = reverse_arrays_of_tables_table(First),
            reverse_arrays_of_tables_array(Rest, [First@1 | Acc])
    end.

-file("src/tom.gleam", 770).
-spec parse_inline_table_property(
    list(binary()),
    gleam@dict:dict(binary(), toml())
) -> {ok, {gleam@dict:dict(binary(), toml()), list(binary())}} |
    {error, parse_error()}.
parse_inline_table_property(Input, Properties) ->
    Input@1 = skip_whitespace(Input),
    do(
        parse_key(Input@1, []),
        fun(Key, Input@2) ->
            Input@3 = skip_line_whitespace(Input@2),
            expect(
                Input@3,
                <<"="/utf8>>,
                fun(Input@4) ->
                    Input@5 = skip_line_whitespace(Input@4),
                    do(
                        parse_value(Input@5),
                        fun(Value, Input@6) ->
                            case insert(Properties, Key, Value) of
                                {ok, Properties@1} ->
                                    {ok, {Properties@1, Input@6}};

                                {error, E} ->
                                    {error, E}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/tom.gleam", 587).
-spec parse_value(list(binary())) -> {ok, {toml(), list(binary())}} |
    {error, parse_error()}.
parse_value(Input) ->
    case Input of
        [<<"t"/utf8>>, <<"r"/utf8>>, <<"u"/utf8>>, <<"e"/utf8>> | Input@1] ->
            {ok, {{bool, true}, Input@1}};

        [<<"f"/utf8>>,
            <<"a"/utf8>>,
            <<"l"/utf8>>,
            <<"s"/utf8>>,
            <<"e"/utf8>> |
            Input@2] ->
            {ok, {{bool, false}, Input@2}};

        [<<"n"/utf8>>, <<"a"/utf8>>, <<"n"/utf8>> | Input@3] ->
            {ok, {{nan, positive}, Input@3}};

        [<<"+"/utf8>>, <<"n"/utf8>>, <<"a"/utf8>>, <<"n"/utf8>> | Input@4] ->
            {ok, {{nan, positive}, Input@4}};

        [<<"-"/utf8>>, <<"n"/utf8>>, <<"a"/utf8>>, <<"n"/utf8>> | Input@5] ->
            {ok, {{nan, negative}, Input@5}};

        [<<"i"/utf8>>, <<"n"/utf8>>, <<"f"/utf8>> | Input@6] ->
            {ok, {{infinity, positive}, Input@6}};

        [<<"+"/utf8>>, <<"i"/utf8>>, <<"n"/utf8>>, <<"f"/utf8>> | Input@7] ->
            {ok, {{infinity, positive}, Input@7}};

        [<<"-"/utf8>>, <<"i"/utf8>>, <<"n"/utf8>>, <<"f"/utf8>> | Input@8] ->
            {ok, {{infinity, negative}, Input@8}};

        [<<"["/utf8>> | Input@9] ->
            parse_array(Input@9, []);

        [<<"{"/utf8>> | Input@10] ->
            parse_inline_table(Input@10, maps:new());

        [<<"0"/utf8>>, <<"x"/utf8>> | Input@11] ->
            parse_hex(Input@11, 0, positive);

        [<<"+"/utf8>>, <<"0"/utf8>>, <<"x"/utf8>> | Input@12] ->
            parse_hex(Input@12, 0, positive);

        [<<"-"/utf8>>, <<"0"/utf8>>, <<"x"/utf8>> | Input@13] ->
            parse_hex(Input@13, 0, negative);

        [<<"0"/utf8>>, <<"o"/utf8>> | Input@14] ->
            parse_octal(Input@14, 0, positive);

        [<<"+"/utf8>>, <<"0"/utf8>>, <<"o"/utf8>> | Input@15] ->
            parse_octal(Input@15, 0, positive);

        [<<"-"/utf8>>, <<"0"/utf8>>, <<"o"/utf8>> | Input@16] ->
            parse_octal(Input@16, 0, negative);

        [<<"0"/utf8>>, <<"b"/utf8>> | Input@17] ->
            parse_binary(Input@17, 0, positive);

        [<<"+"/utf8>>, <<"0"/utf8>>, <<"b"/utf8>> | Input@18] ->
            parse_binary(Input@18, 0, positive);

        [<<"-"/utf8>>, <<"0"/utf8>>, <<"b"/utf8>> | Input@19] ->
            parse_binary(Input@19, 0, negative);

        [<<"+"/utf8>> | Input@20] ->
            parse_number(Input@20, 0, positive);

        [<<"-"/utf8>> | Input@21] ->
            parse_number(Input@21, 0, negative);

        [<<"0"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"1"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"2"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"3"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"4"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"5"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"6"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"7"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"8"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"9"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"\""/utf8>>, <<"\""/utf8>>, <<"\""/utf8>> | Input@22] ->
            parse_multi_line_string(Input@22, <<""/utf8>>);

        [<<"\""/utf8>> | Input@23] ->
            parse_string(Input@23, <<""/utf8>>);

        [<<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>> | Input@24] ->
            parse_multi_line_literal_string(Input@24, <<""/utf8>>);

        [<<"'"/utf8>> | Input@25] ->
            parse_literal_string(Input@25, <<""/utf8>>);

        [G | _] ->
            {error, {unexpected, G, <<"value"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"value"/utf8>>}}
    end.

-file("src/tom.gleam", 744).
-spec parse_inline_table(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_inline_table(Input, Properties) ->
    Input@1 = skip_whitespace(Input),
    case Input@1 of
        [<<"}"/utf8>> | Input@2] ->
            {ok, {{inline_table, Properties}, Input@2}};

        _ ->
            case parse_inline_table_property(Input@1, Properties) of
                {ok, {Properties@1, Input@3}} ->
                    Input@4 = skip_whitespace(Input@3),
                    case Input@4 of
                        [<<"}"/utf8>> | Input@5] ->
                            {ok, {{inline_table, Properties@1}, Input@5}};

                        [<<","/utf8>> | Input@6] ->
                            Input@7 = skip_whitespace(Input@6),
                            parse_inline_table(Input@7, Properties@1);

                        [G | _] ->
                            {error, {unexpected, G, <<"}"/utf8>>}};

                        [] ->
                            {error, {unexpected, <<"EOF"/utf8>>, <<"}"/utf8>>}}
                    end;

                {error, E} ->
                    {error, E}
            end
    end.

-file("src/tom.gleam", 497).
-spec parse_key_value(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        {gleam@dict:dict(binary(), toml()), list(binary())}} |
    {error, parse_error()}.
parse_key_value(Input, Toml) ->
    do(
        parse_key(Input, []),
        fun(Key, Input@1) ->
            Input@2 = skip_line_whitespace(Input@1),
            expect(
                Input@2,
                <<"="/utf8>>,
                fun(Input@3) ->
                    Input@4 = skip_line_whitespace(Input@3),
                    do(
                        parse_value(Input@4),
                        fun(Value, Input@5) -> case insert(Toml, Key, Value) of
                                {ok, Toml@1} ->
                                    {ok, {Toml@1, Input@5}};

                                {error, E} ->
                                    {error, E}
                            end end
                    )
                end
            )
        end
    ).

-file("src/tom.gleam", 477).
-spec parse_table(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        {gleam@dict:dict(binary(), toml()), list(binary())}} |
    {error, parse_error()}.
parse_table(Input, Toml) ->
    Input@1 = skip_whitespace(Input),
    case Input@1 of
        [<<"["/utf8>> | _] ->
            {ok, {Toml, Input@1}};

        [] ->
            {ok, {Toml, Input@1}};

        _ ->
            case parse_key_value(Input@1, Toml) of
                {ok, {Toml@1, Input@2}} ->
                    case skip_line_whitespace(Input@2) of
                        [] ->
                            {ok, {Toml@1, []}};

                        [<<"\n"/utf8>> | In] ->
                            parse_table(In, Toml@1);

                        [<<"\r\n"/utf8>> | In] ->
                            parse_table(In, Toml@1);

                        [G | _] ->
                            {error, {unexpected, G, <<"\n"/utf8>>}}
                    end;

                E ->
                    E
            end
    end.

-file("src/tom.gleam", 449).
-spec parse_array_of_tables(list(binary())) -> {ok,
        {{list(binary()), gleam@dict:dict(binary(), toml())}, list(binary())}} |
    {error, parse_error()}.
parse_array_of_tables(Input) ->
    Input@1 = skip_line_whitespace(Input),
    do(
        parse_key(Input@1, []),
        fun(Key, Input@2) ->
            expect(
                Input@2,
                <<"]"/utf8>>,
                fun(Input@3) ->
                    expect(
                        Input@3,
                        <<"]"/utf8>>,
                        fun(Input@4) ->
                            do(
                                parse_table(Input@4, maps:new()),
                                fun(Table, Input@5) ->
                                    {ok, {{Key, Table}, Input@5}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/tom.gleam", 469).
-spec parse_table_and_header(list(binary())) -> {ok,
        {{list(binary()), gleam@dict:dict(binary(), toml())}, list(binary())}} |
    {error, parse_error()}.
parse_table_and_header(Input) ->
    do(
        parse_table_header(Input),
        fun(Key, Input@1) ->
            do(
                parse_table(Input@1, maps:new()),
                fun(Table, Input@2) -> {ok, {{Key, Table}, Input@2}} end
            )
        end
    ).

-file("src/tom.gleam", 417).
-spec parse_tables(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, parse_error()}.
parse_tables(Input, Toml) ->
    case Input of
        [<<"["/utf8>>, <<"["/utf8>> | Input@1] ->
            case parse_array_of_tables(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, {{Key, Table}, Input@2}} ->
                    case insert(Toml, Key, {array_of_tables, [Table]}) of
                        {ok, Toml@1} ->
                            parse_tables(Input@2, Toml@1);

                        {error, E@1} ->
                            {error, E@1}
                    end
            end;

        [<<"["/utf8>> | Input@3] ->
            case parse_table_and_header(Input@3) of
                {error, E@2} ->
                    {error, E@2};

                {ok, {{Key@1, Table@1}, Input@4}} ->
                    case insert(Toml, Key@1, {table, Table@1}) of
                        {ok, Toml@2} ->
                            parse_tables(Input@4, Toml@2);

                        {error, E@3} ->
                            {error, E@3}
                    end
            end;

        [G | _] ->
            {error, {unexpected, G, <<"["/utf8>>}};

        [] ->
            {ok, Toml}
    end.

-file("src/tom.gleam", 406).
-spec parse(binary()) -> {ok, gleam@dict:dict(binary(), toml())} |
    {error, parse_error()}.
parse(Input) ->
    Input@1 = gleam@string:to_graphemes(Input),
    Input@2 = drop_comments(Input@1, [], false),
    Input@3 = skip_whitespace(Input@2),
    do(
        parse_table(Input@3, maps:new()),
        fun(Toml, Input@4) -> case parse_tables(Input@4, Toml) of
                {ok, Toml@1} ->
                    {ok, reverse_arrays_of_tables_table(Toml@1)};

                {error, E} ->
                    {error, E}
            end end
    ).

-file("src/tom.gleam", 786).
-spec parse_array(list(binary()), list(toml())) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_array(Input, Elements) ->
    Input@1 = skip_whitespace(Input),
    case Input@1 of
        [<<"]"/utf8>> | Input@2] ->
            {ok, {{array, lists:reverse(Elements)}, Input@2}};

        _ ->
            do(
                parse_value(Input@1),
                fun(Element, Input@3) ->
                    Elements@1 = [Element | Elements],
                    Input@4 = skip_whitespace(Input@3),
                    case Input@4 of
                        [<<"]"/utf8>> | Input@5] ->
                            {ok, {{array, lists:reverse(Elements@1)}, Input@5}};

                        [<<","/utf8>> | Input@6] ->
                            Input@7 = skip_whitespace(Input@6),
                            parse_array(Input@7, Elements@1);

                        [G | _] ->
                            {error, {unexpected, G, <<"]"/utf8>>}};

                        [] ->
                            {error, {unexpected, <<"EOF"/utf8>>, <<"]"/utf8>>}}
                    end
                end
            )
    end.
