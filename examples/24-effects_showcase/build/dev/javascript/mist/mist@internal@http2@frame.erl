-module(mist@internal@http2@frame).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/http2/frame.gleam").
-export([stream_identifier/1, get_stream_identifier/1, decode/1, encode/1, settings_ack/0]).
-export_type([stream_identifier/1, header_priority/0, data/0, push_state/0, setting/0, frame/0, connection_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque stream_identifier(LRM) :: {stream_identifier, integer()} |
    {gleam_phantom, LRM}.

-type header_priority() :: {header_priority,
        boolean(),
        stream_identifier(frame()),
        integer()}.

-type data() :: {complete, bitstring()} | {continued, bitstring()}.

-type push_state() :: enabled | disabled.

-type setting() :: {header_table_size, integer()} |
    {server_push, push_state()} |
    {max_concurrent_streams, integer()} |
    {initial_window_size, integer()} |
    {max_frame_size, integer()} |
    {max_header_list_size, integer()}.

-type frame() :: {data, bitstring(), boolean(), stream_identifier(frame())} |
    {header,
        data(),
        boolean(),
        stream_identifier(frame()),
        gleam@option:option(header_priority())} |
    {priority,
        boolean(),
        stream_identifier(frame()),
        stream_identifier(frame()),
        integer()} |
    {termination, connection_error(), stream_identifier(frame())} |
    {settings, boolean(), list(setting())} |
    {push_promise,
        data(),
        stream_identifier(frame()),
        stream_identifier(frame())} |
    {ping, boolean(), bitstring()} |
    {go_away, bitstring(), connection_error(), stream_identifier(frame())} |
    {window_update, integer(), stream_identifier(frame())} |
    {continuation, data(), stream_identifier(frame())}.

-type connection_error() :: no_error |
    protocol_error |
    internal_error |
    flow_control_error |
    settings_timeout |
    stream_closed |
    frame_size_error |
    refused_stream |
    cancel |
    compression_error |
    connect_error |
    enhance_your_calm |
    inadequate_security |
    http11_required |
    {unsupported, integer()}.

-file("src/mist/internal/http2/frame.gleam", 11).
?DOC(false).
-spec stream_identifier(integer()) -> stream_identifier(frame()).
stream_identifier(Value) ->
    {stream_identifier, Value}.

-file("src/mist/internal/http2/frame.gleam", 15).
?DOC(false).
-spec get_stream_identifier(stream_identifier(any())) -> integer().
get_stream_identifier(Identifier) ->
    {stream_identifier, Value} = Identifier,
    Value.

-file("src/mist/internal/http2/frame.gleam", 148).
?DOC(false).
-spec parse_data(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_data(Identifier, Flags, Length, Payload) ->
    case <<Flags/bitstring, Payload/bitstring>> of
        <<_:4,
            Padding:1,
            _:2,
            End_stream:1,
            Pad_length:Padding/unit:8,
            Data_and_padding/bitstring>> when Identifier =/= 0 ->
            Data_length = case Padding of
                1 ->
                    Length - Pad_length;

                0 ->
                    Length;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Somehow a bit was neither 0 nor 1"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"mist/internal/http2/frame"/utf8>>,
                            function => <<"parse_data"/utf8>>,
                            line => 168})
            end,
            case Data_and_padding of
                <<Data:Data_length/binary, _/bitstring>> ->
                    {ok,
                        {data,
                            Data,
                            End_stream =:= 1,
                            stream_identifier(Identifier)}};

                _ ->
                    {error, protocol_error}
            end;

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 185).
?DOC(false).
-spec parse_header(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_header(Identifier, Flags, Length, Payload) ->
    case <<Flags/bitstring, Payload/bitstring>> of
        <<_:2,
            Priority:1,
            _:1,
            Padded:1,
            End_headers:1,
            _:1,
            End_stream:1,
            Pad_length:Padded/unit:8,
            Exclusive:Priority,
            Stream_dependency:Priority/unit:31,
            Weight:Priority/unit:8,
            Data_and_padding/bitstring>> when (Identifier =/= 0) andalso (Pad_length < Length) ->
            Data_length = case {Padded, Priority} of
                {1, 1} ->
                    (Length - Pad_length) - 6;

                {1, 0} ->
                    (Length - Pad_length) - 1;

                {0, 1} ->
                    Length - 5;

                {0, 0} ->
                    Length;

                {_, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Somehow a bit was set to neither 0 nor 1"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"mist/internal/http2/frame"/utf8>>,
                            function => <<"parse_header"/utf8>>,
                            line => 213})
            end,
            case Data_and_padding of
                <<Data:Data_length/binary, _/bitstring>> ->
                    {ok, {header, case End_headers of
                                1 ->
                                    {complete, Data};

                                0 ->
                                    {continued, Data};

                                _ ->
                                    erlang:error(#{gleam_error => panic,
                                            message => <<"Somehow a bit was set to neither 0 nor 1"/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"mist/internal/http2/frame"/utf8>>,
                                            function => <<"parse_header"/utf8>>,
                                            line => 223})
                            end, (End_stream =:= 1), stream_identifier(
                                Identifier
                            ), case Priority =:= 1 of
                                true ->
                                    {some,
                                        {header_priority,
                                            (Exclusive =:= 1),
                                            stream_identifier(Stream_dependency),
                                            Weight}};

                                false ->
                                    none
                            end}};

                _ ->
                    logging:log(debug, <<"oh noes!"/utf8>>),
                    {error, protocol_error}
            end;

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 251).
?DOC(false).
-spec parse_priority(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_priority(Identifier, Flags, Length, Payload) ->
    case {Length, <<Flags/bitstring, Payload/bitstring>>} of
        {5, <<_:8, Exclusive:1, Dependency:31, Weight:8>>} when Identifier =/= 0 ->
            {ok,
                {priority,
                    Exclusive =:= 1,
                    stream_identifier(Identifier),
                    stream_identifier(Dependency),
                    Weight}};

        {5, _} ->
            {error, protocol_error};

        {_, _} ->
            {error, frame_size_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 316).
?DOC(false).
-spec parse_push_promise(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_push_promise(Identifier, Flags, Length, Payload) ->
    case <<Flags/bitstring, Payload/bitstring>> of
        <<_:4,
            Padded:1,
            End_headers:1,
            _:2,
            Pad_length:Padded/unit:8,
            _:1,
            Promised_identifier:31,
            Data:Length/binary,
            _:Pad_length/binary>> when Identifier =/= 0 ->
            {ok, {push_promise, case End_headers =:= 1 of
                        true ->
                            {complete, Data};

                        false ->
                            {continued, Data}
                    end, stream_identifier(Identifier), stream_identifier(
                        Promised_identifier
                    )}};

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 349).
?DOC(false).
-spec parse_ping(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_ping(Identifier, Flags, Length, Payload) ->
    case {Length, <<Flags/bitstring, Payload/bitstring>>} of
        {8, <<_:7, Ack:1, Data:64/bitstring>>} when Identifier =:= 0 ->
            {ok, {ping, Ack =:= 1, Data}};

        {8, _} ->
            {error, protocol_error};

        {_, _} ->
            {error, frame_size_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 390).
?DOC(false).
-spec parse_window_update(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_window_update(Identifier, Flags, Length, Payload) ->
    case {Length, <<Flags/bitstring, Payload/bitstring>>} of
        {4, <<_:8, _:1, Window_size:31>>} when Window_size =/= 0 ->
            {ok, {window_update, Window_size, stream_identifier(Identifier)}};

        {4, _} ->
            {error, frame_size_error};

        {_, _} ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 410).
?DOC(false).
-spec parse_continuation(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_continuation(Identifier, Flags, Length, Payload) ->
    case <<Flags/bitstring, Payload/bitstring>> of
        <<_:5, End_headers:1, _:2, Data:Length/binary>> when Identifier =/= 0 ->
            {ok, {continuation, case End_headers =:= 1 of
                        true ->
                            {complete, Data};

                        false ->
                            {continued, Data}
                    end, stream_identifier(Identifier)}};

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 594).
?DOC(false).
-spec get_error(integer()) -> connection_error().
get_error(Value) ->
    case Value of
        0 ->
            no_error;

        1 ->
            protocol_error;

        2 ->
            internal_error;

        3 ->
            flow_control_error;

        4 ->
            settings_timeout;

        5 ->
            stream_closed;

        6 ->
            frame_size_error;

        7 ->
            refused_stream;

        8 ->
            cancel;

        9 ->
            compression_error;

        10 ->
            connect_error;

        11 ->
            enhance_your_calm;

        12 ->
            inadequate_security;

        13 ->
            http11_required;

        N ->
            {unsupported, N}
    end.

-file("src/mist/internal/http2/frame.gleam", 279).
?DOC(false).
-spec parse_termination(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_termination(Identifier, Flags, Length, Payload) ->
    case {Length, <<Flags/bitstring, Payload/bitstring>>} of
        {4, <<_:8, Error:32>>} when Identifier =/= 0 ->
            {ok, {termination, get_error(Error), stream_identifier(Identifier)}};

        {4, _} ->
            {error, protocol_error};

        {_, _} ->
            {error, frame_size_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 364).
?DOC(false).
-spec parse_go_away(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_go_away(Identifier, Flags, Length, Payload) ->
    case <<Flags/bitstring, Payload/bitstring>> of
        <<_:8, _:1, Last_stream_id:31, Error:32, Data:Length/binary>> when Identifier =:= 0 ->
            {ok,
                {go_away,
                    Data,
                    get_error(Error),
                    stream_identifier(Last_stream_id)}};

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 630).
?DOC(false).
-spec get_setting(integer(), integer()) -> {ok, setting()} |
    {error, connection_error()}.
get_setting(Identifier, Value) ->
    case Identifier of
        1 ->
            {ok, {header_table_size, Value}};

        2 ->
            {ok, {server_push, case Value of
                        0 ->
                            disabled;

                        1 ->
                            enabled;

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"Somehow a bit was neither 0 nor 1"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"mist/internal/http2/frame"/utf8>>,
                                    function => <<"get_setting"/utf8>>,
                                    line => 638})
                    end}};

        3 ->
            {ok, {max_concurrent_streams, Value}};

        4 ->
            case Value of
                N when N > 2147483647 ->
                    {error, flow_control_error};

                _ ->
                    {ok, {initial_window_size, Value}}
            end;

        5 ->
            case Value of
                N@1 when N@1 > 16777215 ->
                    {error, protocol_error};

                _ ->
                    {ok, {max_frame_size, Value}}
            end;

        6 ->
            {ok, {max_header_list_size, Value}};

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 614).
?DOC(false).
-spec get_settings(bitstring(), list(setting())) -> {ok, list(setting())} |
    {error, connection_error()}.
get_settings(Data, Acc) ->
    case Data of
        <<>> ->
            {ok, Acc};

        <<Identifier:16, Value:32, Rest/bitstring>> ->
            case get_setting(Identifier, Value) of
                {ok, Setting} ->
                    get_settings(Rest, [Setting | Acc]);

                {error, Err} ->
                    {error, Err}
            end;

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 297).
?DOC(false).
-spec parse_settings(integer(), bitstring(), integer(), bitstring()) -> {ok,
        frame()} |
    {error, connection_error()}.
parse_settings(Identifier, Flags, Length, Payload) ->
    case {Length rem 6, <<Flags/bitstring, Payload/bitstring>>} of
        {0, <<_:7, Ack:1, Settings:Length/binary>>} when Identifier =:= 0 ->
            gleam@result:'try'(
                get_settings(Settings, []),
                fun(Settings@1) -> {ok, {settings, Ack =:= 1, Settings@1}} end
            );

        {0, _} ->
            {error, protocol_error};

        {_, _} ->
            {error, frame_size_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 105).
?DOC(false).
-spec decode(bitstring()) -> {ok, {frame(), bitstring()}} |
    {error, connection_error()}.
decode(Frame) ->
    case Frame of
        <<Length:24,
            Frame_type:8,
            Flags:8/bitstring,
            _:1,
            Identifier:31,
            Payload:Length/binary,
            Rest/bitstring>> ->
            _pipe = case Frame_type of
                0 ->
                    parse_data(Identifier, Flags, Length, Payload);

                1 ->
                    parse_header(Identifier, Flags, Length, Payload);

                2 ->
                    parse_priority(Identifier, Flags, Length, Payload);

                3 ->
                    parse_termination(Identifier, Flags, Length, Payload);

                4 ->
                    parse_settings(Identifier, Flags, Length, Payload);

                5 ->
                    parse_push_promise(Identifier, Flags, Length, Payload);

                6 ->
                    parse_ping(Identifier, Flags, Length, Payload);

                7 ->
                    parse_go_away(Identifier, Flags, Length, Payload);

                8 ->
                    parse_window_update(Identifier, Flags, Length, Payload);

                9 ->
                    parse_continuation(Identifier, Flags, Length, Payload);

                _ ->
                    {error, protocol_error}
            end,
            gleam@result:map(_pipe, fun(Frame@1) -> {Frame@1, Rest} end);

        <<Length@1:24, _:8, _:8, _:1, _:31, Rest@1/bitstring>> ->
            case erlang:byte_size(Rest@1) < Length@1 of
                true ->
                    {error, no_error};

                false ->
                    {error, protocol_error}
            end;

        _ ->
            {error, protocol_error}
    end.

-file("src/mist/internal/http2/frame.gleam", 659).
?DOC(false).
-spec from_bool(boolean()) -> integer().
from_bool(Bool) ->
    case Bool of
        true ->
            1;

        false ->
            0
    end.

-file("src/mist/internal/http2/frame.gleam", 666).
?DOC(false).
-spec encode_priority(gleam@option:option(header_priority())) -> bitstring().
encode_priority(Priority) ->
    case Priority of
        {some,
            {header_priority,
                Exclusive,
                {stream_identifier, Dependency},
                Weight}} ->
            Exclusive@1 = from_bool(Exclusive),
            <<Exclusive@1:1, Dependency:31, Weight:8>>;

        none ->
            <<>>
    end.

-file("src/mist/internal/http2/frame.gleam", 676).
?DOC(false).
-spec encode_data(data()) -> {integer(), bitstring()}.
encode_data(Data) ->
    case Data of
        {complete, Data@1} ->
            {1, Data@1};

        {continued, Data@2} ->
            {0, Data@2}
    end.

-file("src/mist/internal/http2/frame.gleam", 683).
?DOC(false).
-spec encode_error(connection_error()) -> integer().
encode_error(Error) ->
    case Error of
        no_error ->
            0;

        protocol_error ->
            1;

        internal_error ->
            2;

        flow_control_error ->
            3;

        settings_timeout ->
            4;

        stream_closed ->
            5;

        frame_size_error ->
            6;

        refused_stream ->
            7;

        cancel ->
            8;

        compression_error ->
            9;

        connect_error ->
            10;

        enhance_your_calm ->
            11;

        inadequate_security ->
            12;

        http11_required ->
            13;

        {unsupported, _} ->
            69
    end.

-file("src/mist/internal/http2/frame.gleam", 704).
?DOC(false).
-spec encode_settings(list(setting())) -> bitstring().
encode_settings(Settings) ->
    gleam@list:fold(Settings, <<>>, fun(Acc, Setting) -> case Setting of
                {header_table_size, Value} ->
                    gleam@bit_array:append(Acc, <<1:16, Value:32>>);

                {server_push, enabled} ->
                    gleam@bit_array:append(Acc, <<2:16, 1:32>>);

                {server_push, disabled} ->
                    gleam@bit_array:append(Acc, <<2:16, 0:32>>);

                {max_concurrent_streams, Value@1} ->
                    gleam@bit_array:append(Acc, <<3:16, Value@1:32>>);

                {initial_window_size, Value@2} ->
                    gleam@bit_array:append(Acc, <<4:16, Value@2:32>>);

                {max_frame_size, Value@3} ->
                    gleam@bit_array:append(Acc, <<5:16, Value@3:32>>);

                {max_header_list_size, Value@4} ->
                    gleam@bit_array:append(Acc, <<6:16, Value@4:32>>)
            end end).

-file("src/mist/internal/http2/frame.gleam", 437).
?DOC(false).
-spec encode(frame()) -> bitstring().
encode(Frame) ->
    case Frame of
        {data, Data, End_stream, {stream_identifier, Identifier}} ->
            Length = erlang:byte_size(Data),
            End = from_bool(End_stream),
            <<Length:24,
                0:8,
                0:4,
                0:1,
                0:2,
                End:1,
                0:1,
                Identifier:31,
                Data/bitstring>>;

        {header,
            Data@1,
            End_stream@1,
            {stream_identifier, Identifier@1},
            Priority} ->
            {End_header, Data@2} = encode_data(Data@1),
            Length@1 = erlang:byte_size(Data@2),
            End@1 = from_bool(End_stream@1),
            Priority_flags = encode_priority(Priority),
            Has_priority = from_bool(gleam@option:is_some(Priority)),
            <<Length@1:24,
                1:8,
                0:2,
                Has_priority:1,
                0:1,
                0:1,
                End_header:1,
                0:1,
                End@1:1,
                0:1,
                Identifier@1:31,
                Priority_flags/bitstring,
                Data@2/bitstring>>;

        {priority,
            Exclusive,
            {stream_identifier, Identifier@2},
            {stream_identifier, Dependency},
            Weight} ->
            Exclusive@1 = from_bool(Exclusive),
            <<5:24,
                2:2,
                0:8,
                0:1,
                Identifier@2:31,
                Exclusive@1:1,
                Dependency:31,
                Weight:8>>;

        {termination, Error, {stream_identifier, Identifier@3}} ->
            Error_code = encode_error(Error),
            <<4:24, 3:8, 0:8, 0:1, Identifier@3:31, Error_code:32>>;

        {settings, Ack, Settings} ->
            Ack@1 = from_bool(Ack),
            Settings@1 = encode_settings(Settings),
            Length@2 = erlang:byte_size(Settings@1),
            <<Length@2:24, 4:8, 0:7, Ack@1:1, 0:1, 0:31, Settings@1/bitstring>>;

        {push_promise,
            Data@3,
            {stream_identifier, Identifier@4},
            {stream_identifier, Promised_identifier}} ->
            {End_headers, Data@4} = encode_data(Data@3),
            <<0:24,
                5:8,
                0:4,
                0:0,
                End_headers:1,
                0:2,
                0:1,
                Identifier@4:31,
                0:1,
                Promised_identifier:31,
                Data@4/bitstring>>;

        {ping, Ack@2, Data@5} ->
            Ack@3 = from_bool(Ack@2),
            <<0:24, 6:8, 0:7, Ack@3:1, 0:1, 0:31, Data@5/bitstring>>;

        {go_away, Data@6, Error@1, {stream_identifier, Last_stream_id}} ->
            Error@2 = encode_error(Error@1),
            Payload_size = erlang:byte_size(Data@6),
            <<Payload_size:24,
                7:8,
                0:8,
                0:1,
                0:31,
                0:1,
                Last_stream_id:31,
                Error@2:32,
                Data@6/bitstring>>;

        {window_update, Amount, {stream_identifier, Identifier@5}} ->
            <<4:24, 8:8, 0:8, 0:1, Identifier@5:31, 0:1, Amount:31>>;

        {continuation, Data@7, {stream_identifier, Identifier@6}} ->
            {End_headers@1, Data@8} = encode_data(Data@7),
            Payload_size@1 = erlang:byte_size(Data@8),
            <<Payload_size@1:24,
                9:8,
                0:5,
                End_headers@1:1,
                0:2,
                0:1,
                Identifier@6:31,
                Data@8/bitstring>>
    end.

-file("src/mist/internal/http2/frame.gleam", 723).
?DOC(false).
-spec settings_ack() -> frame().
settings_ack() ->
    {settings, true, []}.
