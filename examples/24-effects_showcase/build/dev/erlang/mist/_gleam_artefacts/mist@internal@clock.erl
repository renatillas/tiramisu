-module(mist@internal@clock).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/clock.gleam").
-export([stop/1, start/2, get_date/0]).
-export_type([clock_message/0, clock_table/0, table_key/0, ets_opts/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type clock_message() :: set_time.

-type clock_table() :: mist_clock.

-type table_key() :: date_header.

-type ets_opts() :: set |
    protected |
    named_table |
    {read_concurrency, boolean()}.

-file("src/mist/internal/clock.gleam", 56).
?DOC(false).
-spec stop(any()) -> gleam@erlang@atom:atom_().
stop(_) ->
    erlang:binary_to_atom(<<"ok"/utf8>>).

-file("src/mist/internal/clock.gleam", 89).
?DOC(false).
-spec weekday_to_short_string(integer()) -> binary().
weekday_to_short_string(Weekday) ->
    case Weekday of
        1 ->
            <<"Mon"/utf8>>;

        2 ->
            <<"Tue"/utf8>>;

        3 ->
            <<"Wed"/utf8>>;

        4 ->
            <<"Thu"/utf8>>;

        5 ->
            <<"Fri"/utf8>>;

        6 ->
            <<"Sat"/utf8>>;

        7 ->
            <<"Sun"/utf8>>;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"erlang weekday outside of 1-7 range"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist/internal/clock"/utf8>>,
                    function => <<"weekday_to_short_string"/utf8>>,
                    line => 98})
    end.

-file("src/mist/internal/clock.gleam", 102).
?DOC(false).
-spec month_to_short_string(integer()) -> binary().
month_to_short_string(Month) ->
    case Month of
        1 ->
            <<"Jan"/utf8>>;

        2 ->
            <<"Feb"/utf8>>;

        3 ->
            <<"Mar"/utf8>>;

        4 ->
            <<"Apr"/utf8>>;

        5 ->
            <<"May"/utf8>>;

        6 ->
            <<"Jun"/utf8>>;

        7 ->
            <<"Jul"/utf8>>;

        8 ->
            <<"Aug"/utf8>>;

        9 ->
            <<"Sep"/utf8>>;

        10 ->
            <<"Oct"/utf8>>;

        11 ->
            <<"Nov"/utf8>>;

        12 ->
            <<"Dec"/utf8>>;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"erlang month outside of 1-12 range"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist/internal/clock"/utf8>>,
                    function => <<"month_to_short_string"/utf8>>,
                    line => 116})
    end.

-file("src/mist/internal/clock.gleam", 73).
?DOC(false).
-spec date() -> binary().
date() ->
    {Weekday, {Year, Month, Day}, {Hour, Minute, Second}} = mist_ffi:now(),
    Weekday@1 = weekday_to_short_string(Weekday),
    Year@1 = begin
        _pipe = erlang:integer_to_binary(Year),
        gleam@string:pad_start(_pipe, 4, <<"0"/utf8>>)
    end,
    Month@1 = month_to_short_string(Month),
    Day@1 = begin
        _pipe@1 = erlang:integer_to_binary(Day),
        gleam@string:pad_start(_pipe@1, 2, <<"0"/utf8>>)
    end,
    Hour@1 = begin
        _pipe@2 = erlang:integer_to_binary(Hour),
        gleam@string:pad_start(_pipe@2, 2, <<"0"/utf8>>)
    end,
    Minute@1 = begin
        _pipe@3 = erlang:integer_to_binary(Minute),
        gleam@string:pad_start(_pipe@3, 2, <<"0"/utf8>>)
    end,
    Second@1 = begin
        _pipe@4 = erlang:integer_to_binary(Second),
        gleam@string:pad_start(_pipe@4, 2, <<"0"/utf8>>)
    end,
    <<<<((<<Weekday@1/binary, ", "/utf8>>))/binary,
            ((<<<<<<<<<<Day@1/binary, " "/utf8>>/binary, Month@1/binary>>/binary,
                        " "/utf8>>/binary,
                    Year@1/binary>>/binary,
                " "/utf8>>))/binary>>/binary,
        ((<<<<<<<<<<Hour@1/binary, ":"/utf8>>/binary, Minute@1/binary>>/binary,
                    ":"/utf8>>/binary,
                Second@1/binary>>/binary,
            " GMT"/utf8>>))/binary>>.

-file("src/mist/internal/clock.gleam", 28).
?DOC(false).
-spec start(any(), any()) -> {ok, gleam@erlang@process:pid_()} |
    {error, gleam@otp@actor:start_error()}.
start(_, _) ->
    _pipe@5 = gleam@otp@actor:new_with_initialiser(
        500,
        fun(Subject) ->
            ets:new(
                mist_clock,
                [set, protected, named_table, {read_concurrency, true}]
            ),
            gleam@erlang@process:send(Subject, set_time),
            _pipe = Subject,
            _pipe@1 = gleam@otp@actor:initialised(_pipe),
            _pipe@3 = gleam@otp@actor:selecting(
                _pipe@1,
                begin
                    _pipe@2 = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:select(_pipe@2, Subject)
                end
            ),
            _pipe@4 = gleam@otp@actor:returning(_pipe@3, Subject),
            {ok, _pipe@4}
        end
    ),
    _pipe@6 = gleam@otp@actor:on_message(_pipe@5, fun(State, Msg) -> case Msg of
                set_time ->
                    ets:insert(mist_clock, {date_header, date()}),
                    gleam@erlang@process:send_after(State, 1000, set_time),
                    gleam@otp@actor:continue(State)
            end end),
    _pipe@7 = gleam@otp@actor:start(_pipe@6),
    gleam@result:map(
        _pipe@7,
        fun(Start) ->
            Pid@1 = case gleam@erlang@process:subject_owner(
                erlang:element(3, Start)
            ) of
                {ok, Pid} -> Pid;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist/internal/clock"/utf8>>,
                                function => <<"start"/utf8>>,
                                line => 51,
                                value => _assert_fail,
                                start => 1096,
                                'end' => 1150,
                                pattern_start => 1107,
                                pattern_end => 1114})
            end,
            Pid@1
        end
    ).

-file("src/mist/internal/clock.gleam", 60).
?DOC(false).
-spec get_date() -> binary().
get_date() ->
    case mist_ffi:ets_lookup_element(mist_clock, date_header, 2) of
        {ok, Value} ->
            Value;

        _ ->
            logging:log(
                warning,
                <<"Failed to lookup date, re-calculating"/utf8>>
            ),
            date()
    end.
