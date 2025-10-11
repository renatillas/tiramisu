-module(mist@internal@telemetry).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/telemetry.gleam").
-export([log/4, span/3, attach_many/3, attach/4, configure_logger/0]).
-export_type([event/0, time_unit/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type event() :: start |
    stop |
    mist |
    parse_request |
    parse_request2 |
    decode_packet |
    convert_path |
    parse_method |
    parse_headers |
    parse_rest |
    parse_path |
    parse_transport |
    parse_host |
    parse_port |
    build_request |
    read_data |
    http1_handler |
    http_upgrade |
    http2_handler.

-type time_unit() :: native | microsecond.

-file("src/mist/internal/telemetry.gleam", 67).
?DOC(false).
-spec log(
    list(event()),
    gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    list(any())
) -> nil.
log(Path, Measurements, _, _) ->
    Duration_string = begin
        _pipe = gleam_stdlib:map_get(
            Measurements,
            erlang:binary_to_atom(<<"duration"/utf8>>)
        ),
        _pipe@3 = gleam@result:'try'(_pipe, fun(Val) -> _pipe@1 = Val,
                _pipe@2 = gleam@dynamic@decode:run(
                    _pipe@1,
                    {decoder, fun gleam@dynamic@decode:decode_int/1}
                ),
                gleam@result:replace_error(_pipe@2, nil) end),
        _pipe@4 = gleam@result:map(
            _pipe@3,
            fun(_capture) ->
                erlang:convert_time_unit(_capture, native, microsecond)
            end
        ),
        _pipe@5 = gleam@result:map(
            _pipe@4,
            fun(Time) ->
                <<<<" duration: "/utf8,
                        (erlang:integer_to_binary(Time))/binary>>/binary,
                    "μs, "/utf8>>
            end
        ),
        gleam@result:unwrap(_pipe@5, <<""/utf8>>)
    end,
    logging:log(
        debug,
        <<(gleam@string:inspect(Path))/binary, Duration_string/binary>>
    ).

-file("src/mist/internal/telemetry.gleam", 89).
?DOC(false).
-spec span(
    list(event()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    fun(() -> OEJ)
) -> OEJ.
span(Path, Metadata, Wrapping) ->
    telemetry:span(
        Path,
        Metadata,
        fun() ->
            Res = Wrapping(),
            {Res, maps:new()}
        end
    ).

-file("src/mist/internal/telemetry.gleam", 106).
?DOC(false).
-spec attach_many(
    binary(),
    list(list(event())),
    fun((list(event()), gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()), gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), list(any())) -> nil)
) -> nil.
attach_many(Id, Path, Handler) ->
    telemetry:attach_many(Id, Path, Handler, nil).

-file("src/mist/internal/telemetry.gleam", 135).
?DOC(false).
-spec attach(
    binary(),
    list(event()),
    fun((list(event()), gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()), gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), list(any())) -> nil),
    nil
) -> nil.
attach(Id, Event, Handler, Config) ->
    telemetry:attach(Id, Event, Handler, Config).

-file("src/mist/internal/telemetry.gleam", 148).
?DOC(false).
-spec configure_logger() -> nil.
configure_logger() ->
    attach_many(
        <<"mist-logger"/utf8>>,
        [[mist, parse_request, stop],
            [mist, parse_request2, stop],
            [mist, http1_handler, stop],
            [mist, http_upgrade, stop],
            [mist, http2_handler, stop],
            [mist, decode_packet, stop],
            [mist, convert_path, stop],
            [mist, parse_method, stop],
            [mist, parse_headers, stop],
            [mist, parse_rest, stop],
            [mist, parse_path, stop],
            [mist, parse_transport, stop],
            [mist, parse_host, stop],
            [mist, parse_port, stop],
            [mist, build_request, stop],
            [mist, read_data, stop]],
        fun log/4
    ).
