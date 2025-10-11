-module(gleam@time@duration).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam/time/duration.gleam").
-export([approximate/1, compare/2, difference/2, add/2, to_iso8601_string/1, seconds/1, minutes/1, hours/1, milliseconds/1, nanoseconds/1, to_seconds/1, to_seconds_and_nanoseconds/1]).
-export_type([duration/0, unit/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque duration() :: {duration, integer(), integer()}.

-type unit() :: nanosecond |
    microsecond |
    millisecond |
    second |
    minute |
    hour |
    day |
    week |
    month |
    year.

-file("src/gleam/time/duration.gleam", 109).
?DOC(
    " Ensure the duration is represented with `nanoseconds` being positive and\n"
    " less than 1 second.\n"
    "\n"
    " This function does not change the amount of time that the duratoin refers\n"
    " to, it only adjusts the values used to represent the time.\n"
).
-spec normalise(duration()) -> duration().
normalise(Duration) ->
    Multiplier = 1000000000,
    Nanoseconds = case Multiplier of
        0 -> 0;
        Gleam@denominator -> erlang:element(3, Duration) rem Gleam@denominator
    end,
    Overflow = erlang:element(3, Duration) - Nanoseconds,
    Seconds = erlang:element(2, Duration) + (case Multiplier of
        0 -> 0;
        Gleam@denominator@1 -> Overflow div Gleam@denominator@1
    end),
    case Nanoseconds >= 0 of
        true ->
            {duration, Seconds, Nanoseconds};

        false ->
            {duration, Seconds - 1, Multiplier + Nanoseconds}
    end.

-file("src/gleam/time/duration.gleam", 75).
?DOC(
    " Convert a duration to a number of the largest number of a unit, serving as\n"
    " a rough description of the duration that a human can understand.\n"
    "\n"
    " The size used for each unit are described in the documentation for the\n"
    " `Unit` type.\n"
    "\n"
    " ```gleam\n"
    " seconds(125)\n"
    " |> approximate\n"
    " // -> #(2, Minute)\n"
    " ```\n"
    "\n"
    " This function rounds _towards zero_. This means that if a duration is just\n"
    " short of 2 days then it will approximate to 1 day.\n"
    "\n"
    " ```gleam\n"
    " hours(47)\n"
    " |> approximate\n"
    " // -> #(1, Day)\n"
    " ```\n"
).
-spec approximate(duration()) -> {integer(), unit()}.
approximate(Duration) ->
    {duration, S, Ns} = Duration,
    Minute = 60,
    Hour = Minute * 60,
    Day = Hour * 24,
    Week = Day * 7,
    Year = (Day * 365) + (Hour * 6),
    Month = Year div 12,
    Microsecond = 1000,
    Millisecond = Microsecond * 1000,
    case nil of
        _ when S < 0 ->
            {Amount, Unit} = begin
                _pipe = {duration, - S, - Ns},
                _pipe@1 = normalise(_pipe),
                approximate(_pipe@1)
            end,
            {- Amount, Unit};

        _ when S >= Year ->
            {case Year of
                    0 -> 0;
                    Gleam@denominator -> S div Gleam@denominator
                end, year};

        _ when S >= Month ->
            {case Month of
                    0 -> 0;
                    Gleam@denominator@1 -> S div Gleam@denominator@1
                end, month};

        _ when S >= Week ->
            {case Week of
                    0 -> 0;
                    Gleam@denominator@2 -> S div Gleam@denominator@2
                end, week};

        _ when S >= Day ->
            {case Day of
                    0 -> 0;
                    Gleam@denominator@3 -> S div Gleam@denominator@3
                end, day};

        _ when S >= Hour ->
            {case Hour of
                    0 -> 0;
                    Gleam@denominator@4 -> S div Gleam@denominator@4
                end, hour};

        _ when S >= Minute ->
            {case Minute of
                    0 -> 0;
                    Gleam@denominator@5 -> S div Gleam@denominator@5
                end, minute};

        _ when S > 0 ->
            {S, second};

        _ when Ns >= Millisecond ->
            {case Millisecond of
                    0 -> 0;
                    Gleam@denominator@6 -> Ns div Gleam@denominator@6
                end, millisecond};

        _ when Ns >= Microsecond ->
            {case Microsecond of
                    0 -> 0;
                    Gleam@denominator@7 -> Ns div Gleam@denominator@7
                end, microsecond};

        _ ->
            {Ns, nanosecond}
    end.

-file("src/gleam/time/duration.gleam", 139).
?DOC(
    " Compare one duration to another, indicating whether the first spans a\n"
    " larger amount of time (and so is greater) or smaller amount of time (and so\n"
    " is lesser) than the second.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " compare(seconds(1), seconds(2))\n"
    " // -> order.Lt\n"
    " ```\n"
    "\n"
    " Whether a duration is negative or positive doesn't matter for comparing\n"
    " them, only the amount of time spanned matters.\n"
    "\n"
    " ```gleam\n"
    " compare(seconds(-2), seconds(1))\n"
    " // -> order.Gt\n"
    " ```\n"
).
-spec compare(duration(), duration()) -> gleam@order:order().
compare(Left, Right) ->
    Parts = fun(X) -> case erlang:element(2, X) >= 0 of
            true ->
                {erlang:element(2, X), erlang:element(3, X)};

            false ->
                {(erlang:element(2, X) * -1) - 1,
                    1000000000 - erlang:element(3, X)}
        end end,
    {Ls, Lns} = Parts(Left),
    {Rs, Rns} = Parts(Right),
    _pipe = gleam@int:compare(Ls, Rs),
    gleam@order:break_tie(_pipe, gleam@int:compare(Lns, Rns)).

-file("src/gleam/time/duration.gleam", 163).
?DOC(
    " Calculate the difference between two durations.\n"
    "\n"
    " This is effectively substracting the first duration from the second.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " difference(seconds(1), seconds(5))\n"
    " // -> seconds(4)\n"
    " ```\n"
).
-spec difference(duration(), duration()) -> duration().
difference(Left, Right) ->
    _pipe = {duration,
        erlang:element(2, Right) - erlang:element(2, Left),
        erlang:element(3, Right) - erlang:element(3, Left)},
    normalise(_pipe).

-file("src/gleam/time/duration.gleam", 177).
?DOC(
    " Add two durations together.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " add(seconds(1), seconds(5))\n"
    " // -> seconds(6)\n"
    " ```\n"
).
-spec add(duration(), duration()) -> duration().
add(Left, Right) ->
    _pipe = {duration,
        erlang:element(2, Left) + erlang:element(2, Right),
        erlang:element(3, Left) + erlang:element(3, Right)},
    normalise(_pipe).

-file("src/gleam/time/duration.gleam", 223).
-spec nanosecond_digits(integer(), integer(), binary()) -> binary().
nanosecond_digits(N, Position, Acc) ->
    case Position of
        9 ->
            Acc;

        _ when (Acc =:= <<""/utf8>>) andalso ((N rem 10) =:= 0) ->
            nanosecond_digits(N div 10, Position + 1, Acc);

        _ ->
            Acc@1 = <<(erlang:integer_to_binary(N rem 10))/binary, Acc/binary>>,
            nanosecond_digits(N div 10, Position + 1, Acc@1)
    end.

-file("src/gleam/time/duration.gleam", 191).
?DOC(
    " Convert the duration to an [ISO8601][1] formatted duration string.\n"
    "\n"
    " The ISO8601 duration format is ambiguous without context due to months and\n"
    " years having different lengths, and because of leap seconds. This function\n"
    " encodes the duration as days, hours, and seconds without any leap seconds.\n"
    " Be sure to take this into account when using the duration strings.\n"
    "\n"
    " [1]: https://en.wikipedia.org/wiki/ISO_8601#Durations\n"
).
-spec to_iso8601_string(duration()) -> binary().
to_iso8601_string(Duration) ->
    Split = fun(Total, Limit) ->
        Amount = case Limit of
            0 -> 0;
            Gleam@denominator -> Total rem Gleam@denominator
        end,
        Remainder = case Limit of
            0 -> 0;
            Gleam@denominator@1 -> (Total - Amount) div Gleam@denominator@1
        end,
        {Amount, Remainder}
    end,
    {Seconds, Rest} = Split(erlang:element(2, Duration), 60),
    {Minutes, Rest@1} = Split(Rest, 60),
    {Hours, Rest@2} = Split(Rest@1, 24),
    Days = Rest@2,
    Add = fun(Out, Value, Unit) -> case Value of
            0 ->
                Out;

            _ ->
                <<<<Out/binary, (erlang:integer_to_binary(Value))/binary>>/binary,
                    Unit/binary>>
        end end,
    Output = begin
        _pipe = <<"P"/utf8>>,
        _pipe@1 = Add(_pipe, Days, <<"D"/utf8>>),
        _pipe@2 = gleam@string:append(_pipe@1, <<"T"/utf8>>),
        _pipe@3 = Add(_pipe@2, Hours, <<"H"/utf8>>),
        Add(_pipe@3, Minutes, <<"M"/utf8>>)
    end,
    case {Seconds, erlang:element(3, Duration)} of
        {0, 0} ->
            Output;

        {_, 0} ->
            <<<<Output/binary, (erlang:integer_to_binary(Seconds))/binary>>/binary,
                "S"/utf8>>;

        {_, _} ->
            F = nanosecond_digits(erlang:element(3, Duration), 0, <<""/utf8>>),
            <<<<<<<<Output/binary, (erlang:integer_to_binary(Seconds))/binary>>/binary,
                        "."/utf8>>/binary,
                    F/binary>>/binary,
                "S"/utf8>>
    end.

-file("src/gleam/time/duration.gleam", 237).
?DOC(" Create a duration of a number of seconds.\n").
-spec seconds(integer()) -> duration().
seconds(Amount) ->
    {duration, Amount, 0}.

-file("src/gleam/time/duration.gleam", 242).
?DOC(" Create a duration of a number of minutes.\n").
-spec minutes(integer()) -> duration().
minutes(Amount) ->
    seconds(Amount * 60).

-file("src/gleam/time/duration.gleam", 247).
?DOC(" Create a duration of a number of hours.\n").
-spec hours(integer()) -> duration().
hours(Amount) ->
    seconds((Amount * 60) * 60).

-file("src/gleam/time/duration.gleam", 252).
?DOC(" Create a duration of a number of milliseconds.\n").
-spec milliseconds(integer()) -> duration().
milliseconds(Amount) ->
    Remainder = Amount rem 1000,
    Overflow = Amount - Remainder,
    Nanoseconds = Remainder * 1000000,
    Seconds = Overflow div 1000,
    _pipe = {duration, Seconds, Nanoseconds},
    normalise(_pipe).

-file("src/gleam/time/duration.gleam", 271).
?DOC(
    " Create a duration of a number of nanoseconds.\n"
    "\n"
    " # JavaScript int limitations\n"
    "\n"
    " Remember that JavaScript can only perfectly represent ints between positive\n"
    " and negative 9,007,199,254,740,991! If you use a single call to this\n"
    " function to create durations larger than that number of nanoseconds then\n"
    " you will likely not get exactly the value you expect. Use `seconds` and\n"
    " `milliseconds` as much as possible for large durations.\n"
).
-spec nanoseconds(integer()) -> duration().
nanoseconds(Amount) ->
    _pipe = {duration, 0, Amount},
    normalise(_pipe).

-file("src/gleam/time/duration.gleam", 281).
?DOC(
    " Convert the duration to a number of seconds.\n"
    "\n"
    " There may be some small loss of precision due to `Duration` being\n"
    " nanosecond accurate and `Float` not being able to represent this.\n"
).
-spec to_seconds(duration()) -> float().
to_seconds(Duration) ->
    Seconds = erlang:float(erlang:element(2, Duration)),
    Nanoseconds = erlang:float(erlang:element(3, Duration)),
    Seconds + (Nanoseconds / 1000000000.0).

-file("src/gleam/time/duration.gleam", 290).
?DOC(
    " Convert the duration to a number of seconds and nanoseconds. There is no\n"
    " loss of precision with this conversion on any target.\n"
).
-spec to_seconds_and_nanoseconds(duration()) -> {integer(), integer()}.
to_seconds_and_nanoseconds(Duration) ->
    {erlang:element(2, Duration), erlang:element(3, Duration)}.
