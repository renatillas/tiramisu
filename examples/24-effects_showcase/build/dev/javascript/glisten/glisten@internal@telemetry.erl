-module(glisten@internal@telemetry).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/glisten/internal/telemetry.gleam").
-export([log/4, span/3, attach_many/3, attach/4, configure_logger/0]).
-export_type([data/0, event/0, time_unit/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type data() :: {data,
        integer(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())}.

-type event() :: start |
    stop |
    glisten |
    handshake |
    handler_loop |
    listener |
    acceptor |
    handler_start |
    handler_init.

-type time_unit() :: native | microsecond.

-file("src/glisten/internal/telemetry.gleam", 43).
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

-file("src/glisten/internal/telemetry.gleam", 65).
?DOC(false).
-spec span(
    list(event()),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    fun(() -> HGF)
) -> HGF.
span(Path, Metadata, Wrapping) ->
    telemetry:span(
        Path,
        Metadata,
        fun() ->
            Res = Wrapping(),
            {Res, maps:new()}
        end
    ).

-file("src/glisten/internal/telemetry.gleam", 82).
?DOC(false).
-spec attach_many(
    binary(),
    list(list(event())),
    fun((list(event()), gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()), gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), list(any())) -> nil)
) -> nil.
attach_many(Id, Path, Handler) ->
    telemetry:attach_many(Id, Path, Handler, nil).

-file("src/glisten/internal/telemetry.gleam", 111).
?DOC(false).
-spec attach(
    binary(),
    list(event()),
    fun((list(event()), gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()), gleam@dict:dict(binary(), gleam@dynamic:dynamic_()), list(any())) -> nil),
    nil
) -> nil.
attach(Id, Event, Handler, Config) ->
    telemetry:attach(Id, Event, Handler, Config).

-file("src/glisten/internal/telemetry.gleam", 130).
?DOC(false).
-spec configure_logger() -> nil.
configure_logger() ->
    attach_many(
        <<"glisten-logger"/utf8>>,
        [[glisten, handshake, stop],
            [glisten, handler_loop, stop],
            [glisten, acceptor, handler_start, stop]],
        fun log/4
    ).
