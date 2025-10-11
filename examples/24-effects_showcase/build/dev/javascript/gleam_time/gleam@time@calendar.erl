-module(gleam@time@calendar).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam/time/calendar.gleam").
-export([local_offset/0, month_to_string/1, month_to_int/1, month_from_int/1, is_leap_year/1, is_valid_date/1, is_valid_time_of_day/1]).
-export_type([date/0, time_of_day/0, month/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This module is for working with the Gregorian calendar, established by\n"
    " Pope Gregory XIII in 1582!\n"
    "\n"
    " ## When should you use this module?\n"
    "\n"
    " The types in this module type are useful when you want to communicate time\n"
    " to a human reader, but they are not ideal for computers to work with.\n"
    " Disadvantages of calendar time types include:\n"
    "\n"
    " - They are ambiguous if you don't know what time-zone they are for.\n"
    " - The type permits invalid states. e.g. `days` could be set to the number\n"
    "   32, but this should not be possible!\n"
    " - There is not a single unique canonical value for each point in time,\n"
    "   thanks to time zones. Two different `Date` + `TimeOfDay` value pairs\n"
    "   could represent the same point in time. This means that you can't check\n"
    "   for time equality with `==` when using calendar types.\n"
    "\n"
    " Prefer to represent your time using the `Timestamp` type, and convert it\n"
    " only to calendar types when you need to display them.\n"
    "\n"
    " ## Time zone offsets\n"
    "\n"
    " This package includes the `utc_offset` value and the `local_offset`\n"
    " function, which are the offset for the UTC time zone and get the time\n"
    " offset the computer running the program is configured to respectively.\n"
    "\n"
    " If you need to use other offsets in your program then you will need to get\n"
    " them from somewhere else, such as from a package which loads the\n"
    " [IANA Time Zone Database](https://www.iana.org/time-zones), or from the\n"
    " website visitor's web browser, which your frontend can send for you.\n"
    "\n"
    " ## Use in APIs\n"
    "\n"
    " If you are making an API such as a HTTP JSON API you are encouraged to use\n"
    " Unix timestamps instead of calendar times.\n"
).

-type date() :: {date, integer(), month(), integer()}.

-type time_of_day() :: {time_of_day, integer(), integer(), integer(), integer()}.

-type month() :: january |
    february |
    march |
    april |
    may |
    june |
    july |
    august |
    september |
    october |
    november |
    december.

-file("src/gleam/time/calendar.gleam", 92).
?DOC(
    " Get the offset for the computer's currently configured time zone.\n"
    "\n"
    " Note this may not be the time zone that is correct to use for your user.\n"
    " For example, if you are making a web application that runs on a server you\n"
    " want _their_ computer's time zone, not yours.\n"
    "\n"
    " This is the _current local_ offset, not the current local time zone. This\n"
    " means that while it will result in the expected outcome for the current\n"
    " time, it may result in unexpected output if used with other timestamps. For\n"
    " example: a timestamp that would locally be during daylight savings time if\n"
    " is it not currently daylight savings time when this function is called.\n"
).
-spec local_offset() -> gleam@time@duration:duration().
local_offset() ->
    gleam@time@duration:seconds(gleam_time_ffi:local_time_offset_seconds()).

-file("src/gleam/time/calendar.gleam", 108).
?DOC(
    " Returns the English name for a month.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " month_to_string(April)\n"
    " // -> \"April\"\n"
    " ```\n"
).
-spec month_to_string(month()) -> binary().
month_to_string(Month) ->
    case Month of
        january ->
            <<"January"/utf8>>;

        february ->
            <<"February"/utf8>>;

        march ->
            <<"March"/utf8>>;

        april ->
            <<"April"/utf8>>;

        may ->
            <<"May"/utf8>>;

        june ->
            <<"June"/utf8>>;

        july ->
            <<"July"/utf8>>;

        august ->
            <<"August"/utf8>>;

        september ->
            <<"September"/utf8>>;

        october ->
            <<"October"/utf8>>;

        november ->
            <<"November"/utf8>>;

        december ->
            <<"December"/utf8>>
    end.

-file("src/gleam/time/calendar.gleam", 133).
?DOC(
    " Returns the number for the month, where January is 1 and December is 12.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " month_to_int(January)\n"
    " // -> 1\n"
    " ```\n"
).
-spec month_to_int(month()) -> integer().
month_to_int(Month) ->
    case Month of
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
    end.

-file("src/gleam/time/calendar.gleam", 158).
?DOC(
    " Returns the month for a given number, where January is 1 and December is 12.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " month_from_int(1)\n"
    " // -> Ok(January)\n"
    " ```\n"
).
-spec month_from_int(integer()) -> {ok, month()} | {error, nil}.
month_from_int(Month) ->
    case Month of
        1 ->
            {ok, january};

        2 ->
            {ok, february};

        3 ->
            {ok, march};

        4 ->
            {ok, april};

        5 ->
            {ok, may};

        6 ->
            {ok, june};

        7 ->
            {ok, july};

        8 ->
            {ok, august};

        9 ->
            {ok, september};

        10 ->
            {ok, october};

        11 ->
            {ok, november};

        12 ->
            {ok, december};

        _ ->
            {error, nil}
    end.

-file("src/gleam/time/calendar.gleam", 235).
?DOC(
    " Determines if a given year is a leap year.\n"
    "\n"
    " A leap year occurs every 4 years, except for years divisible by 100,\n"
    " unless they are also divisible by 400.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " is_leap_year(2024)\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_leap_year(2023)\n"
    " // -> False\n"
    " ```\n"
).
-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) ->
    case (Year rem 400) =:= 0 of
        true ->
            true;

        false ->
            case (Year rem 100) =:= 0 of
                true ->
                    false;

                false ->
                    (Year rem 4) =:= 0
            end
    end.

-file("src/gleam/time/calendar.gleam", 199).
?DOC(
    " Checks if a given date is valid.\n"
    "\n"
    " This function properly accounts for leap years when validating February days.\n"
    " A leap year occurs every 4 years, except for years divisible by 100,\n"
    " unless they are also divisible by 400.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " is_valid_date(Date(2023, April, 15))\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_valid_date(Date(2023, April, 31))\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_valid_date(Date(2024, February, 29))\n"
    " // -> True (2024 is a leap year)\n"
    " ```\n"
).
-spec is_valid_date(date()) -> boolean().
is_valid_date(Date) ->
    {date, Year, Month, Day} = Date,
    case Day < 1 of
        true ->
            false;

        false ->
            case Month of
                january ->
                    Day =< 31;

                march ->
                    Day =< 31;

                may ->
                    Day =< 31;

                july ->
                    Day =< 31;

                august ->
                    Day =< 31;

                october ->
                    Day =< 31;

                december ->
                    Day =< 31;

                april ->
                    Day =< 30;

                june ->
                    Day =< 30;

                september ->
                    Day =< 30;

                november ->
                    Day =< 30;

                february ->
                    Max_february_days = case is_leap_year(Year) of
                        true ->
                            29;

                        false ->
                            28
                    end,
                    Day =< Max_february_days
            end
    end.

-file("src/gleam/time/calendar.gleam", 258).
?DOC(
    " Checks if a time of day is valid.\n"
    "\n"
    " Validates that hours are 0-23, minutes are 0-59, seconds are 0-59,\n"
    " and nanoseconds are 0-999,999,999.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " is_valid_time_of_day(TimeOfDay(12, 30, 45, 123456789))\n"
    " // -> True\n"
    " ```\n"
).
-spec is_valid_time_of_day(time_of_day()) -> boolean().
is_valid_time_of_day(Time) ->
    {time_of_day, Hours, Minutes, Seconds, Nanoseconds} = Time,
    (((((((Hours >= 0) andalso (Hours =< 23)) andalso (Minutes >= 0)) andalso (Minutes
    =< 59))
    andalso (Seconds >= 0))
    andalso (Seconds =< 59))
    andalso (Nanoseconds >= 0))
    andalso (Nanoseconds =< 999999999).
