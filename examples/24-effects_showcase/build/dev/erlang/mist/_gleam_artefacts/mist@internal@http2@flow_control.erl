-module(mist@internal@http2@flow_control).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/http2/flow_control.gleam").
-export([compute_receive_window/2, update_send_window/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/mist/internal/http2/flow_control.gleam", 3).
?DOC(false).
-spec compute_receive_window(integer(), integer()) -> {integer(), integer()}.
compute_receive_window(Receive_window_size, Data_size) ->
    New_receive_window_size = Receive_window_size - Data_size,
    Max_window_increment = erlang:'bsl'(1, 31) - 1,
    Max_window_size = Max_window_increment,
    Min_window_size = erlang:'bsl'(1, 30),
    case New_receive_window_size > Min_window_size of
        true ->
            {New_receive_window_size, 0};

        false ->
            Updated_receive_window_size = gleam@int:min(
                New_receive_window_size + Max_window_increment,
                Max_window_size
            ),
            Increment = Updated_receive_window_size - New_receive_window_size,
            {Updated_receive_window_size, Increment}
    end.

-file("src/mist/internal/http2/flow_control.gleam", 27).
?DOC(false).
-spec update_send_window(integer(), integer()) -> {ok, integer()} |
    {error, binary()}.
update_send_window(Current_send_window, Increment) ->
    Max_window_size = erlang:'bsl'(1, 31) - 1,
    Update = Current_send_window + Increment,
    case Update > Max_window_size of
        true ->
            {error, <<"Invalid update increment"/utf8>>};

        false ->
            {ok, Update}
    end.
