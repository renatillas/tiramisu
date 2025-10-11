-module(gleam@time@timestamp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam/time/timestamp.gleam").
-export([compare/2, system_time/0, difference/2, add/2, to_calendar/2, to_rfc3339/2, from_unix_seconds/1, from_unix_seconds_and_nanoseconds/2, to_unix_seconds/1, to_unix_seconds_and_nanoseconds/1, from_calendar/3, parse_rfc3339/1]).
-export_type([timestamp/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Welcome to the timestamp module! This module and its `Timestamp` type are\n"
    " what you will be using most commonly when working with time in Gleam.\n"
    "\n"
    " A timestamp represents a moment in time, represented as an amount of time\n"
    " since the calendar time 00:00:00 UTC on 1 January 1970, also known as the\n"
    " _Unix epoch_.\n"
    "\n"
    " # Wall clock time and monotonicity\n"
    "\n"
    " Time is very complicated, especially on computers! While they generally do\n"
    " a good job of keeping track of what the time is, computers can get\n"
    " out-of-sync and start to report a time that is too late or too early. Most\n"
    " computers use \"network time protocol\" to tell each other what they think\n"
    " the time is, and computers that realise they are running too fast or too\n"
    " slow will adjust their clock to correct it. When this happens it can seem\n"
    " to your program that the current time has changed, and it may have even\n"
    " jumped backwards in time!\n"
    "\n"
    " This measure of time is called _wall clock time_, and it is what people\n"
    " commonly think of when they think of time. It is important to be aware that\n"
    " it can go backwards, and your program must not rely on it only ever going\n"
    " forwards at a steady rate. For example, for tracking what order events happen\n"
    " in. \n"
    "\n"
    " This module uses wall clock time. If your program needs time values to always\n"
    " increase you will need a _monotonic_ time instead. It's uncommon that you\n"
    " would need monotonic time, one example might be if you're making a\n"
    " benchmarking framework.\n"
    "\n"
    " The exact way that time works will depend on what runtime you use. The\n"
    " Erlang documentation on time has a lot of detail about time generally as well\n"
    " as how it works on the BEAM, it is worth reading.\n"
    " <https://www.erlang.org/doc/apps/erts/time_correction>.\n"
    "\n"
    " # Converting to local time\n"
    "\n"
    " Timestamps don't take into account time zones, so a moment in time will\n"
    " have the same timestamp value regardless of where you are in the world. To\n"
    " convert them to local time you will need to know the offset for the time\n"
    " zone you wish to use, likely from a time zone database. See the\n"
    " `gleam/time/calendar` module for more information.\n"
    "\n"
).

-opaque timestamp() :: {timestamp, integer(), integer()}.

-file("src/gleam/time/timestamp.gleam", 116).
?DOC(
    " Ensure the time is represented with `nanoseconds` being positive and less\n"
    " than 1 second.\n"
    "\n"
    " This function does not change the time that the timestamp refers to, it\n"
    " only adjusts the values used to represent the time.\n"
).
-spec normalise(timestamp()) -> timestamp().
normalise(Timestamp) ->
    Multiplier = 1000000000,
    Nanoseconds = case Multiplier of
        0 -> 0;
        Gleam@denominator -> erlang:element(3, Timestamp) rem Gleam@denominator
    end,
    Overflow = erlang:element(3, Timestamp) - Nanoseconds,
    Seconds = erlang:element(2, Timestamp) + (case Multiplier of
        0 -> 0;
        Gleam@denominator@1 -> Overflow div Gleam@denominator@1
    end),
    case Nanoseconds >= 0 of
        true ->
            {timestamp, Seconds, Nanoseconds};

        false ->
            {timestamp, Seconds - 1, Multiplier + Nanoseconds}
    end.

-file("src/gleam/time/timestamp.gleam", 138).
?DOC(
    " Compare one timestamp to another, indicating whether the first is further\n"
    " into the future (greater) or further into the past (lesser) than the\n"
    " second.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " compare(from_unix_seconds(1), from_unix_seconds(2))\n"
    " // -> order.Lt\n"
    " ```\n"
).
-spec compare(timestamp(), timestamp()) -> gleam@order:order().
compare(Left, Right) ->
    gleam@order:break_tie(
        gleam@int:compare(erlang:element(2, Left), erlang:element(2, Right)),
        gleam@int:compare(erlang:element(3, Left), erlang:element(3, Right))
    ).

-file("src/gleam/time/timestamp.gleam", 157).
?DOC(
    " Get the current system time.\n"
    "\n"
    " Note this time is not unique or monotonic, it could change at any time or\n"
    " even go backwards! The exact behaviour will depend on the runtime used. See\n"
    " the module documentation for more information.\n"
    "\n"
    " On Erlang this uses [`erlang:system_time/1`][1]. On JavaScript this uses\n"
    " [`Date.now`][2].\n"
    "\n"
    " [1]: https://www.erlang.org/doc/apps/erts/erlang#system_time/1\n"
    " [2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now\n"
).
-spec system_time() -> timestamp().
system_time() ->
    {Seconds, Nanoseconds} = gleam_time_ffi:system_time(),
    normalise({timestamp, Seconds, Nanoseconds}).

-file("src/gleam/time/timestamp.gleam", 177).
?DOC(
    " Calculate the difference between two timestamps.\n"
    "\n"
    " This is effectively substracting the first timestamp from the second.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " difference(from_unix_seconds(1), from_unix_seconds(5))\n"
    " // -> duration.seconds(4)\n"
    " ```\n"
).
-spec difference(timestamp(), timestamp()) -> gleam@time@duration:duration().
difference(Left, Right) ->
    Seconds = gleam@time@duration:seconds(
        erlang:element(2, Right) - erlang:element(2, Left)
    ),
    Nanoseconds = gleam@time@duration:nanoseconds(
        erlang:element(3, Right) - erlang:element(3, Left)
    ),
    gleam@time@duration:add(Seconds, Nanoseconds).

-file("src/gleam/time/timestamp.gleam", 192).
?DOC(
    " Add a duration to a timestamp.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " add(from_unix_seconds(1000), duration.seconds(5))\n"
    " // -> from_unix_seconds(1005)\n"
    " ```\n"
).
-spec add(timestamp(), gleam@time@duration:duration()) -> timestamp().
add(Timestamp, Duration) ->
    {Seconds, Nanoseconds} = gleam@time@duration:to_seconds_and_nanoseconds(
        Duration
    ),
    _pipe = {timestamp,
        erlang:element(2, Timestamp) + Seconds,
        erlang:element(3, Timestamp) + Nanoseconds},
    normalise(_pipe).

-file("src/gleam/time/timestamp.gleam", 259).
-spec pad_digit(integer(), integer()) -> binary().
pad_digit(Digit, Desired_length) ->
    _pipe = erlang:integer_to_binary(Digit),
    gleam@string:pad_start(_pipe, Desired_length, <<"0"/utf8>>).

-file("src/gleam/time/timestamp.gleam", 305).
-spec duration_to_minutes(gleam@time@duration:duration()) -> integer().
duration_to_minutes(Duration) ->
    erlang:round(gleam@time@duration:to_seconds(Duration) / 60.0).

-file("src/gleam/time/timestamp.gleam", 367).
-spec modulo(integer(), integer()) -> integer().
modulo(N, M) ->
    case gleam@int:modulo(N, M) of
        {ok, N@1} ->
            N@1;

        {error, _} ->
            0
    end.

-file("src/gleam/time/timestamp.gleam", 374).
-spec floored_div(integer(), float()) -> integer().
floored_div(Numerator, Denominator) ->
    N = case Denominator of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Numerator) / Gleam@denominator
    end,
    erlang:round(math:floor(N)).

-file("src/gleam/time/timestamp.gleam", 380).
-spec to_civil(integer()) -> {integer(), integer(), integer()}.
to_civil(Minutes) ->
    Raw_day = floored_div(Minutes, (60.0 * 24.0)) + 719468,
    Era = case Raw_day >= 0 of
        true ->
            Raw_day div 146097;

        false ->
            (Raw_day - 146096) div 146097
    end,
    Day_of_era = Raw_day - (Era * 146097),
    Year_of_era = (((Day_of_era - (Day_of_era div 1460)) + (Day_of_era div 36524))
    - (Day_of_era div 146096))
    div 365,
    Year = Year_of_era + (Era * 400),
    Day_of_year = Day_of_era - (((365 * Year_of_era) + (Year_of_era div 4)) - (Year_of_era
    div 100)),
    Mp = ((5 * Day_of_year) + 2) div 153,
    Month = case Mp < 10 of
        true ->
            Mp + 3;

        false ->
            Mp - 9
    end,
    Day = (Day_of_year - (((153 * Mp) + 2) div 5)) + 1,
    Year@1 = case Month =< 2 of
        true ->
            Year + 1;

        false ->
            Year
    end,
    {Year@1, Month, Day}.

-file("src/gleam/time/timestamp.gleam", 309).
-spec to_calendar_from_offset(timestamp(), integer()) -> {integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()}.
to_calendar_from_offset(Timestamp, Offset) ->
    Total = erlang:element(2, Timestamp) + (Offset * 60),
    Seconds = modulo(Total, 60),
    Total_minutes = floored_div(Total, 60.0),
    Minutes = modulo(Total, 60 * 60) div 60,
    Hours = case (60 * 60) of
        0 -> 0;
        Gleam@denominator -> modulo(Total, (24 * 60) * 60) div Gleam@denominator
    end,
    {Year, Month, Day} = to_civil(Total_minutes),
    {Year, Month, Day, Hours, Minutes, Seconds}.

-file("src/gleam/time/timestamp.gleam", 278).
?DOC(
    " Convert a `Timestamp` to calendar time, suitable for presenting to a human\n"
    " to read.\n"
    "\n"
    " If you want a machine to use the time value then you should not use this\n"
    " function and should instead keep it as a timestamp. See the documentation\n"
    " for the `gleam/time/calendar` module for more information.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " timestamp.from_unix_seconds(0)\n"
    " |> timestamp.to_calendar(calendar.utc_offset)\n"
    " // -> #(Date(1970, January, 1), TimeOfDay(0, 0, 0, 0))\n"
    " ```\n"
).
-spec to_calendar(timestamp(), gleam@time@duration:duration()) -> {gleam@time@calendar:date(),
    gleam@time@calendar:time_of_day()}.
to_calendar(Timestamp, Offset) ->
    Offset@1 = duration_to_minutes(Offset),
    {Year, Month, Day, Hours, Minutes, Seconds} = to_calendar_from_offset(
        Timestamp,
        Offset@1
    ),
    Month@1 = case Month of
        1 ->
            january;

        2 ->
            february;

        3 ->
            march;

        4 ->
            april;

        5 ->
            may;

        6 ->
            june;

        7 ->
            july;

        8 ->
            august;

        9 ->
            september;

        10 ->
            october;

        11 ->
            november;

        _ ->
            december
    end,
    Nanoseconds = erlang:element(3, Timestamp),
    Date = {date, Year, Month@1, Day},
    Time = {time_of_day, Hours, Minutes, Seconds, Nanoseconds},
    {Date, Time}.

-file("src/gleam/time/timestamp.gleam", 443).
-spec do_remove_trailing_zeros(list(integer())) -> list(integer()).
do_remove_trailing_zeros(Reversed_digits) ->
    case Reversed_digits of
        [] ->
            [];

        [Digit | Digits] when Digit =:= 0 ->
            do_remove_trailing_zeros(Digits);

        Reversed_digits@1 ->
            lists:reverse(Reversed_digits@1)
    end.

-file("src/gleam/time/timestamp.gleam", 437).
?DOC(" Given a list of digits, return new list with any trailing zeros removed.\n").
-spec remove_trailing_zeros(list(integer())) -> list(integer()).
remove_trailing_zeros(Digits) ->
    Reversed_digits = lists:reverse(Digits),
    do_remove_trailing_zeros(Reversed_digits).

-file("src/gleam/time/timestamp.gleam", 458).
-spec do_get_zero_padded_digits(integer(), list(integer()), integer()) -> list(integer()).
do_get_zero_padded_digits(Number, Digits, Count) ->
    case Number of
        Number@1 when (Number@1 =< 0) andalso (Count >= 9) ->
            Digits;

        Number@2 when Number@2 =< 0 ->
            do_get_zero_padded_digits(Number@2, [0 | Digits], Count + 1);

        Number@3 ->
            Digit = Number@3 rem 10,
            Number@4 = floored_div(Number@3, 10.0),
            do_get_zero_padded_digits(Number@4, [Digit | Digits], Count + 1)
    end.

-file("src/gleam/time/timestamp.gleam", 454).
?DOC(
    " Returns the list of digits of `number`.  If the number of digits is less \n"
    " than 9, the result is zero-padded at the front.\n"
).
-spec get_zero_padded_digits(integer()) -> list(integer()).
get_zero_padded_digits(Number) ->
    do_get_zero_padded_digits(Number, [], 0).

-file("src/gleam/time/timestamp.gleam", 417).
?DOC(
    " Converts nanoseconds into a `String` representation of fractional seconds.\n"
    " \n"
    " Assumes that `nanoseconds < 1_000_000_000`, which will be true for any \n"
    " normalised timestamp.\n"
).
-spec show_second_fraction(integer()) -> binary().
show_second_fraction(Nanoseconds) ->
    case gleam@int:compare(Nanoseconds, 0) of
        lt ->
            <<""/utf8>>;

        eq ->
            <<""/utf8>>;

        gt ->
            Second_fraction_part = begin
                _pipe = Nanoseconds,
                _pipe@1 = get_zero_padded_digits(_pipe),
                _pipe@2 = remove_trailing_zeros(_pipe@1),
                _pipe@3 = gleam@list:map(
                    _pipe@2,
                    fun erlang:integer_to_binary/1
                ),
                gleam@string:join(_pipe@3, <<""/utf8>>)
            end,
            <<"."/utf8, Second_fraction_part/binary>>
    end.

-file("src/gleam/time/timestamp.gleam", 237).
?DOC(
    " Convert a timestamp to a RFC 3339 formatted time string, with an offset\n"
    " supplied as an additional argument.\n"
    "\n"
    " The output of this function is also ISO 8601 compatible so long as the\n"
    " offset not negative. Offsets have at-most minute precision, so an offset\n"
    " with higher precision will be rounded to the nearest minute.\n"
    "\n"
    " If you are making an API such as a HTTP JSON API you are encouraged to use\n"
    " Unix timestamps instead of this format or ISO 8601. Unix timestamps are a\n"
    " better choice as they don't contain offset information. Consider:\n"
    "\n"
    " - UTC offsets are not time zones. This does not and cannot tell us the time\n"
    "   zone in which the date was recorded. So what are we supposed to do with\n"
    "   this information?\n"
    " - Users typically want dates formatted according to their local time zone.\n"
    "   What if the provided UTC offset is different from the current user's time\n"
    "   zone? What are we supposed to do with it then?\n"
    " - Despite it being useless (or worse, a source of bugs), the UTC offset\n"
    "   creates a larger payload to transfer.\n"
    "\n"
    " They also uses more memory than a unix timestamp. The way they are better\n"
    " than Unix timestamp is that it is easier for a human to read them, but\n"
    " this is a hinderance that tooling can remedy, and APIs are not primarily\n"
    " for humans.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " timestamp.from_unix_seconds_and_nanoseconds(1000, 123_000_000)\n"
    " |> to_rfc3339(calendar.utc_offset)\n"
    " // -> \"1970-01-01T00:16:40.123Z\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " timestamp.from_unix_seconds(1000)\n"
    " |> to_rfc3339(duration.seconds(3600))\n"
    " // -> \"1970-01-01T01:16:40+01:00\"\n"
    " ```\n"
).
-spec to_rfc3339(timestamp(), gleam@time@duration:duration()) -> binary().
to_rfc3339(Timestamp, Offset) ->
    Offset@1 = duration_to_minutes(Offset),
    {Years, Months, Days, Hours, Minutes, Seconds} = to_calendar_from_offset(
        Timestamp,
        Offset@1
    ),
    Offset_minutes = modulo(Offset@1, 60),
    Offset_hours = gleam@int:absolute_value(floored_div(Offset@1, 60.0)),
    N2 = fun(_capture) -> pad_digit(_capture, 2) end,
    N4 = fun(_capture@1) -> pad_digit(_capture@1, 4) end,
    Out = <<""/utf8>>,
    Out@1 = <<<<<<<<<<Out/binary, (N4(Years))/binary>>/binary, "-"/utf8>>/binary,
                (N2(Months))/binary>>/binary,
            "-"/utf8>>/binary,
        (N2(Days))/binary>>,
    Out@2 = <<Out@1/binary, "T"/utf8>>,
    Out@3 = <<<<<<<<<<Out@2/binary, (N2(Hours))/binary>>/binary, ":"/utf8>>/binary,
                (N2(Minutes))/binary>>/binary,
            ":"/utf8>>/binary,
        (N2(Seconds))/binary>>,
    Out@4 = <<Out@3/binary,
        (show_second_fraction(erlang:element(3, Timestamp)))/binary>>,
    case gleam@int:compare(Offset@1, 0) of
        eq ->
            <<Out@4/binary, "Z"/utf8>>;

        gt ->
            <<<<<<<<Out@4/binary, "+"/utf8>>/binary, (N2(Offset_hours))/binary>>/binary,
                    ":"/utf8>>/binary,
                (N2(Offset_minutes))/binary>>;

        lt ->
            <<<<<<<<Out@4/binary, "-"/utf8>>/binary, (N2(Offset_hours))/binary>>/binary,
                    ":"/utf8>>/binary,
                (N2(Offset_minutes))/binary>>
    end.

-file("src/gleam/time/timestamp.gleam", 587).
-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) ->
    ((Year rem 4) =:= 0) andalso (((Year rem 100) /= 0) orelse ((Year rem 400)
    =:= 0)).

-file("src/gleam/time/timestamp.gleam", 691).
-spec parse_sign(bitstring()) -> {ok, {binary(), bitstring()}} | {error, nil}.
parse_sign(Bytes) ->
    case Bytes of
        <<"+"/utf8, Remaining_bytes/binary>> ->
            {ok, {<<"+"/utf8>>, Remaining_bytes}};

        <<"-"/utf8, Remaining_bytes@1/binary>> ->
            {ok, {<<"-"/utf8>>, Remaining_bytes@1}};

        _ ->
            {error, nil}
    end.

-file("src/gleam/time/timestamp.gleam", 738).
?DOC(" Accept the given value from `bytes` and move past it if found.\n").
-spec accept_byte(bitstring(), integer()) -> {ok, bitstring()} | {error, nil}.
accept_byte(Bytes, Value) ->
    case Bytes of
        <<Byte, Remaining_bytes/binary>> when Byte =:= Value ->
            {ok, Remaining_bytes};

        _ ->
            {error, nil}
    end.

-file("src/gleam/time/timestamp.gleam", 756).
-spec accept_empty(bitstring()) -> {ok, nil} | {error, nil}.
accept_empty(Bytes) ->
    case Bytes of
        <<>> ->
            {ok, nil};

        _ ->
            {error, nil}
    end.

-file("src/gleam/time/timestamp.gleam", 816).
?DOC(
    " Note: It is the callers responsibility to ensure the inputs are valid.\n"
    " \n"
    " See https://www.tondering.dk/claus/cal/julperiod.php#formula\n"
).
-spec julian_day_from_ymd(integer(), integer(), integer()) -> integer().
julian_day_from_ymd(Year, Month, Day) ->
    Adjustment = (14 - Month) div 12,
    Adjusted_year = (Year + 4800) - Adjustment,
    Adjusted_month = (Month + (12 * Adjustment)) - 3,
    (((((Day + (((153 * Adjusted_month) + 2) div 5)) + (365 * Adjusted_year)) + (Adjusted_year
    div 4))
    - (Adjusted_year div 100))
    + (Adjusted_year div 400))
    - 32045.

-file("src/gleam/time/timestamp.gleam", 835).
?DOC(
    " Create a timestamp from a number of seconds since 00:00:00 UTC on 1 January\n"
    " 1970.\n"
).
-spec from_unix_seconds(integer()) -> timestamp().
from_unix_seconds(Seconds) ->
    {timestamp, Seconds, 0}.

-file("src/gleam/time/timestamp.gleam", 850).
?DOC(
    " Create a timestamp from a number of seconds and nanoseconds since 00:00:00\n"
    " UTC on 1 January 1970.\n"
    "\n"
    " # JavaScript int limitations\n"
    "\n"
    " Remember that JavaScript can only perfectly represent ints between positive\n"
    " and negative 9,007,199,254,740,991! If you only use the nanosecond field\n"
    " then you will almost certainly not get the date value you want due to this\n"
    " loss of precision. Always use seconds primarily and then use nanoseconds\n"
    " for the final sub-second adjustment.\n"
).
-spec from_unix_seconds_and_nanoseconds(integer(), integer()) -> timestamp().
from_unix_seconds_and_nanoseconds(Seconds, Nanoseconds) ->
    _pipe = {timestamp, Seconds, Nanoseconds},
    normalise(_pipe).

-file("src/gleam/time/timestamp.gleam", 864).
?DOC(
    " Convert the timestamp to a number of seconds since 00:00:00 UTC on 1\n"
    " January 1970.\n"
    "\n"
    " There may be some small loss of precision due to `Timestamp` being\n"
    " nanosecond accurate and `Float` not being able to represent this.\n"
).
-spec to_unix_seconds(timestamp()) -> float().
to_unix_seconds(Timestamp) ->
    Seconds = erlang:float(erlang:element(2, Timestamp)),
    Nanoseconds = erlang:float(erlang:element(3, Timestamp)),
    Seconds + (Nanoseconds / 1000000000.0).

-file("src/gleam/time/timestamp.gleam", 873).
?DOC(
    " Convert the timestamp to a number of seconds and nanoseconds since 00:00:00\n"
    " UTC on 1 January 1970. There is no loss of precision with this conversion\n"
    " on any target.\n"
).
-spec to_unix_seconds_and_nanoseconds(timestamp()) -> {integer(), integer()}.
to_unix_seconds_and_nanoseconds(Timestamp) ->
    {erlang:element(2, Timestamp), erlang:element(3, Timestamp)}.

-file("src/gleam/time/timestamp.gleam", 699).
-spec offset_to_seconds(binary(), integer(), integer()) -> integer().
offset_to_seconds(Sign, Hours, Minutes) ->
    Abs_seconds = (Hours * 3600) + (Minutes * 60),
    case Sign of
        <<"-"/utf8>> ->
            - Abs_seconds;

        _ ->
            Abs_seconds
    end.

-file("src/gleam/time/timestamp.gleam", 795).
?DOC(
    " `julian_seconds_from_parts(year, month, day, hours, minutes, seconds)` \n"
    " returns the number of Julian \n"
    " seconds represented by the given arguments.\n"
    " \n"
    " Note: It is the callers responsibility to ensure the inputs are valid.\n"
    " \n"
    " See https://www.tondering.dk/claus/cal/julperiod.php#formula\n"
).
-spec julian_seconds_from_parts(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> integer().
julian_seconds_from_parts(Year, Month, Day, Hours, Minutes, Seconds) ->
    Julian_day_seconds = julian_day_from_ymd(Year, Month, Day) * 86400,
    ((Julian_day_seconds + (Hours * 3600)) + (Minutes * 60)) + Seconds.

-file("src/gleam/time/timestamp.gleam", 638).
-spec do_parse_second_fraction_as_nanoseconds(bitstring(), integer(), integer()) -> {ok,
        {integer(), bitstring()}} |
    {error, any()}.
do_parse_second_fraction_as_nanoseconds(Bytes, Acc, Power) ->
    Power@1 = Power div 10,
    case Bytes of
        <<Byte, Remaining_bytes/binary>> when ((16#30 =< Byte) andalso (Byte =< 16#39)) andalso (Power@1 < 1) ->
            do_parse_second_fraction_as_nanoseconds(
                Remaining_bytes,
                Acc,
                Power@1
            );

        <<Byte@1, Remaining_bytes@1/binary>> when (16#30 =< Byte@1) andalso (Byte@1 =< 16#39) ->
            Digit = Byte@1 - 16#30,
            do_parse_second_fraction_as_nanoseconds(
                Remaining_bytes@1,
                Acc + (Digit * Power@1),
                Power@1
            );

        _ ->
            {ok, {Acc, Bytes}}
    end.

-file("src/gleam/time/timestamp.gleam", 618).
-spec parse_second_fraction_as_nanoseconds(bitstring()) -> {ok,
        {integer(), bitstring()}} |
    {error, nil}.
parse_second_fraction_as_nanoseconds(Bytes) ->
    case Bytes of
        <<"."/utf8, Byte, Remaining_bytes/binary>> when (16#30 =< Byte) andalso (Byte =< 16#39) ->
            do_parse_second_fraction_as_nanoseconds(
                <<Byte, Remaining_bytes/bitstring>>,
                0,
                1000000000
            );

        <<"."/utf8, _/binary>> ->
            {error, nil};

        _ ->
            {ok, {0, Bytes}}
    end.

-file("src/gleam/time/timestamp.gleam", 717).
-spec do_parse_digits(bitstring(), integer(), integer(), integer()) -> {ok,
        {integer(), bitstring()}} |
    {error, nil}.
do_parse_digits(Bytes, Count, Acc, K) ->
    case Bytes of
        _ when K >= Count ->
            {ok, {Acc, Bytes}};

        <<Byte, Remaining_bytes/binary>> when (16#30 =< Byte) andalso (Byte =< 16#39) ->
            do_parse_digits(
                Remaining_bytes,
                Count,
                (Acc * 10) + (Byte - 16#30),
                K + 1
            );

        _ ->
            {error, nil}
    end.

-file("src/gleam/time/timestamp.gleam", 710).
?DOC(" Parse and return the given number of digits from the given bytes.\n").
-spec parse_digits(bitstring(), integer()) -> {ok, {integer(), bitstring()}} |
    {error, nil}.
parse_digits(Bytes, Count) ->
    do_parse_digits(Bytes, Count, 0, 0).

-file("src/gleam/time/timestamp.gleam", 549).
-spec parse_year(bitstring()) -> {ok, {integer(), bitstring()}} | {error, nil}.
parse_year(Bytes) ->
    parse_digits(Bytes, 4).

-file("src/gleam/time/timestamp.gleam", 553).
-spec parse_month(bitstring()) -> {ok, {integer(), bitstring()}} | {error, nil}.
parse_month(Bytes) ->
    gleam@result:'try'(
        parse_digits(Bytes, 2),
        fun(_use0) ->
            {Month, Bytes@1} = _use0,
            case (1 =< Month) andalso (Month =< 12) of
                true ->
                    {ok, {Month, Bytes@1}};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/time/timestamp.gleam", 561).
-spec parse_day(bitstring(), integer(), integer()) -> {ok,
        {integer(), bitstring()}} |
    {error, nil}.
parse_day(Bytes, Year, Month) ->
    gleam@result:'try'(
        parse_digits(Bytes, 2),
        fun(_use0) ->
            {Day, Bytes@1} = _use0,
            gleam@result:'try'(case Month of
                    1 ->
                        {ok, 31};

                    3 ->
                        {ok, 31};

                    5 ->
                        {ok, 31};

                    7 ->
                        {ok, 31};

                    8 ->
                        {ok, 31};

                    10 ->
                        {ok, 31};

                    12 ->
                        {ok, 31};

                    4 ->
                        {ok, 30};

                    6 ->
                        {ok, 30};

                    9 ->
                        {ok, 30};

                    11 ->
                        {ok, 30};

                    2 ->
                        case is_leap_year(Year) of
                            true ->
                                {ok, 29};

                            false ->
                                {ok, 28}
                        end;

                    _ ->
                        {error, nil}
                end, fun(Max_day) -> case (1 =< Day) andalso (Day =< Max_day) of
                        true ->
                            {ok, {Day, Bytes@1}};

                        false ->
                            {error, nil}
                    end end)
        end
    ).

-file("src/gleam/time/timestamp.gleam", 591).
-spec parse_hours(bitstring()) -> {ok, {integer(), bitstring()}} | {error, nil}.
parse_hours(Bytes) ->
    gleam@result:'try'(
        parse_digits(Bytes, 2),
        fun(_use0) ->
            {Hours, Bytes@1} = _use0,
            case (0 =< Hours) andalso (Hours =< 23) of
                true ->
                    {ok, {Hours, Bytes@1}};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/time/timestamp.gleam", 599).
-spec parse_minutes(bitstring()) -> {ok, {integer(), bitstring()}} |
    {error, nil}.
parse_minutes(Bytes) ->
    gleam@result:'try'(
        parse_digits(Bytes, 2),
        fun(_use0) ->
            {Minutes, Bytes@1} = _use0,
            case (0 =< Minutes) andalso (Minutes =< 59) of
                true ->
                    {ok, {Minutes, Bytes@1}};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/time/timestamp.gleam", 607).
-spec parse_seconds(bitstring()) -> {ok, {integer(), bitstring()}} |
    {error, nil}.
parse_seconds(Bytes) ->
    gleam@result:'try'(
        parse_digits(Bytes, 2),
        fun(_use0) ->
            {Seconds, Bytes@1} = _use0,
            case (0 =< Seconds) andalso (Seconds =< 60) of
                true ->
                    {ok, {Seconds, Bytes@1}};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/time/timestamp.gleam", 680).
-spec parse_numeric_offset(bitstring()) -> {ok, {integer(), bitstring()}} |
    {error, nil}.
parse_numeric_offset(Bytes) ->
    gleam@result:'try'(
        parse_sign(Bytes),
        fun(_use0) ->
            {Sign, Bytes@1} = _use0,
            gleam@result:'try'(
                parse_hours(Bytes@1),
                fun(_use0@1) ->
                    {Hours, Bytes@2} = _use0@1,
                    gleam@result:'try'(
                        accept_byte(Bytes@2, 16#3A),
                        fun(Bytes@3) ->
                            gleam@result:'try'(
                                parse_minutes(Bytes@3),
                                fun(_use0@2) ->
                                    {Minutes, Bytes@4} = _use0@2,
                                    Offset_seconds = offset_to_seconds(
                                        Sign,
                                        Hours,
                                        Minutes
                                    ),
                                    {ok, {Offset_seconds, Bytes@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/time/timestamp.gleam", 672).
-spec parse_offset(bitstring()) -> {ok, {integer(), bitstring()}} | {error, nil}.
parse_offset(Bytes) ->
    case Bytes of
        <<"Z"/utf8, Remaining_bytes/binary>> ->
            {ok, {0, Remaining_bytes}};

        <<"z"/utf8, Remaining_bytes/binary>> ->
            {ok, {0, Remaining_bytes}};

        _ ->
            parse_numeric_offset(Bytes)
    end.

-file("src/gleam/time/timestamp.gleam", 745).
-spec accept_date_time_separator(bitstring()) -> {ok, bitstring()} |
    {error, nil}.
accept_date_time_separator(Bytes) ->
    case Bytes of
        <<Byte, Remaining_bytes/binary>> when ((Byte =:= 16#54) orelse (Byte =:= 16#74)) orelse (Byte =:= 16#20) ->
            {ok, Remaining_bytes};

        _ ->
            {error, nil}
    end.

-file("src/gleam/time/timestamp.gleam", 765).
?DOC(" Note: The caller of this function must ensure that all inputs are valid.\n").
-spec from_date_time(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> timestamp().
from_date_time(
    Year,
    Month,
    Day,
    Hours,
    Minutes,
    Seconds,
    Second_fraction_as_nanoseconds,
    Offset_seconds
) ->
    Julian_seconds = julian_seconds_from_parts(
        Year,
        Month,
        Day,
        Hours,
        Minutes,
        Seconds
    ),
    Julian_seconds_since_epoch = Julian_seconds - 210866803200,
    _pipe = {timestamp,
        Julian_seconds_since_epoch - Offset_seconds,
        Second_fraction_as_nanoseconds},
    normalise(_pipe).

-file("src/gleam/time/timestamp.gleam", 336).
?DOC(
    " Create a `Timestamp` from a human-readable calendar time.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " timestamp.from_calendar(\n"
    "   date: calendar.Date(2024, calendar.December, 25),\n"
    "   time: calendar.TimeOfDay(12, 30, 50, 0),\n"
    "   offset: calendar.utc_offset,\n"
    " )\n"
    " |> timestamp.to_rfc3339(calendar.utc_offset)\n"
    " // -> \"2024-12-25T12:30:50Z\"\n"
    " ```\n"
).
-spec from_calendar(
    gleam@time@calendar:date(),
    gleam@time@calendar:time_of_day(),
    gleam@time@duration:duration()
) -> timestamp().
from_calendar(Date, Time, Offset) ->
    Month = case erlang:element(3, Date) of
        january ->
            1;

        february ->
            2;

        march ->
            3;

        april ->
            4;

        may ->
            5;

        june ->
            6;

        july ->
            7;

        august ->
            8;

        september ->
            9;

        october ->
            10;

        november ->
            11;

        december ->
            12
    end,
    from_date_time(
        erlang:element(2, Date),
        Month,
        erlang:element(4, Date),
        erlang:element(2, Time),
        erlang:element(3, Time),
        erlang:element(4, Time),
        erlang:element(5, Time),
        erlang:round(gleam@time@duration:to_seconds(Offset))
    ).

-file("src/gleam/time/timestamp.gleam", 509).
?DOC(
    " Parses an [RFC 3339 formatted time string][spec] into a `Timestamp`.\n"
    "\n"
    " [spec]: https://datatracker.ietf.org/doc/html/rfc3339#section-5.6\n"
    " \n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(ts) = timestamp.parse_rfc3339(\"1970-01-01T00:00:01Z\")\n"
    " timestamp.to_unix_seconds_and_nanoseconds(ts)\n"
    " // -> #(1, 0)\n"
    " ```\n"
    " \n"
    " Parsing an invalid timestamp returns an error.\n"
    " \n"
    " ```gleam\n"
    " let assert Error(Nil) = timestamp.parse_rfc3339(\"1995-10-31\")\n"
    " ```\n"
    "\n"
    " # Notes\n"
    " \n"
    " - Follows the grammar specified in section 5.6 Internet Date/Time Format of \n"
    "   RFC 3339 <https://datatracker.ietf.org/doc/html/rfc3339#section-5.6>.\n"
    " - The `T` and `Z` characters may alternatively be lower case `t` or `z`, \n"
    "   respectively.\n"
    " - Full dates and full times must be separated by `T` or `t`. A space is also \n"
    "   permitted.\n"
    " - Leap seconds rules are not considered.  That is, any timestamp may \n"
    "   specify digts `00` - `60` for the seconds.\n"
    " - Any part of a fractional second that cannot be represented in the \n"
    "   nanosecond precision is tructated.  That is, for the time string, \n"
    "   `\"1970-01-01T00:00:00.1234567899Z\"`, the fractional second `.1234567899` \n"
    "   will be represented as `123_456_789` in the `Timestamp`.\n"
).
-spec parse_rfc3339(binary()) -> {ok, timestamp()} | {error, nil}.
parse_rfc3339(Input) ->
    Bytes = gleam_stdlib:identity(Input),
    gleam@result:'try'(
        parse_year(Bytes),
        fun(_use0) ->
            {Year, Bytes@1} = _use0,
            gleam@result:'try'(
                accept_byte(Bytes@1, 16#2D),
                fun(Bytes@2) ->
                    gleam@result:'try'(
                        parse_month(Bytes@2),
                        fun(_use0@1) ->
                            {Month, Bytes@3} = _use0@1,
                            gleam@result:'try'(
                                accept_byte(Bytes@3, 16#2D),
                                fun(Bytes@4) ->
                                    gleam@result:'try'(
                                        parse_day(Bytes@4, Year, Month),
                                        fun(_use0@2) ->
                                            {Day, Bytes@5} = _use0@2,
                                            gleam@result:'try'(
                                                accept_date_time_separator(
                                                    Bytes@5
                                                ),
                                                fun(Bytes@6) ->
                                                    gleam@result:'try'(
                                                        parse_hours(Bytes@6),
                                                        fun(_use0@3) ->
                                                            {Hours, Bytes@7} = _use0@3,
                                                            gleam@result:'try'(
                                                                accept_byte(
                                                                    Bytes@7,
                                                                    16#3A
                                                                ),
                                                                fun(Bytes@8) ->
                                                                    gleam@result:'try'(
                                                                        parse_minutes(
                                                                            Bytes@8
                                                                        ),
                                                                        fun(
                                                                            _use0@4
                                                                        ) ->
                                                                            {Minutes,
                                                                                Bytes@9} = _use0@4,
                                                                            gleam@result:'try'(
                                                                                accept_byte(
                                                                                    Bytes@9,
                                                                                    16#3A
                                                                                ),
                                                                                fun(
                                                                                    Bytes@10
                                                                                ) ->
                                                                                    gleam@result:'try'(
                                                                                        parse_seconds(
                                                                                            Bytes@10
                                                                                        ),
                                                                                        fun(
                                                                                            _use0@5
                                                                                        ) ->
                                                                                            {Seconds,
                                                                                                Bytes@11} = _use0@5,
                                                                                            gleam@result:'try'(
                                                                                                parse_second_fraction_as_nanoseconds(
                                                                                                    Bytes@11
                                                                                                ),
                                                                                                fun(
                                                                                                    _use0@6
                                                                                                ) ->
                                                                                                    {Second_fraction_as_nanoseconds,
                                                                                                        Bytes@12} = _use0@6,
                                                                                                    gleam@result:'try'(
                                                                                                        parse_offset(
                                                                                                            Bytes@12
                                                                                                        ),
                                                                                                        fun(
                                                                                                            _use0@7
                                                                                                        ) ->
                                                                                                            {Offset_seconds,
                                                                                                                Bytes@13} = _use0@7,
                                                                                                            gleam@result:'try'(
                                                                                                                accept_empty(
                                                                                                                    Bytes@13
                                                                                                                ),
                                                                                                                fun(
                                                                                                                    _use0@8
                                                                                                                ) ->
                                                                                                                    nil = _use0@8,
                                                                                                                    {ok,
                                                                                                                        from_date_time(
                                                                                                                            Year,
                                                                                                                            Month,
                                                                                                                            Day,
                                                                                                                            Hours,
                                                                                                                            Minutes,
                                                                                                                            Seconds,
                                                                                                                            Second_fraction_as_nanoseconds,
                                                                                                                            Offset_seconds
                                                                                                                        )}
                                                                                                                end
                                                                                                            )
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            )
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
