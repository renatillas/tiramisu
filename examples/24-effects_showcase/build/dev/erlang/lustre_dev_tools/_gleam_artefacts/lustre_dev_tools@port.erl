-module(lustre_dev_tools@port).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/port.gleam").
-export([send/2, start/4]).
-export_type([message/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque message() :: {send, binary()} |
    {data, binary()} |
    {exit, integer()} |
    unknown.

-file("src/lustre_dev_tools/port.gleam", 83).
?DOC(false).
-spec send(gleam@erlang@process:subject(message()), gleam@json:json()) -> nil.
send(Port, Data) ->
    gleam@erlang@process:send(
        Port,
        {send, <<(gleam@json:to_string(Data))/binary, "\n"/utf8>>}
    ).

-file("src/lustre_dev_tools/port.gleam", 92).
?DOC(false).
-spec decode_port_message() -> gleam@dynamic@decode:decoder(message()).
decode_port_message() ->
    gleam@dynamic@decode:at(
        [1],
        begin
            gleam@dynamic@decode:field(
                0,
                gleam@erlang@atom:decoder(),
                fun(Tag) ->
                    Data_tag = erlang:binary_to_atom(<<"data"/utf8>>),
                    Exit_tag = erlang:binary_to_atom(<<"exit"/utf8>>),
                    case Tag of
                        _ when Tag =:= Data_tag ->
                            _pipe = gleam@dynamic@decode:at(
                                [1],
                                {decoder,
                                    fun gleam@dynamic@decode:decode_string/1}
                            ),
                            gleam@dynamic@decode:map(
                                _pipe,
                                fun(Field@0) -> {data, Field@0} end
                            );

                        _ when Tag =:= Exit_tag ->
                            _pipe@1 = gleam@dynamic@decode:at(
                                [1],
                                {decoder, fun gleam@dynamic@decode:decode_int/1}
                            ),
                            gleam@dynamic@decode:map(
                                _pipe@1,
                                fun(Field@0) -> {exit, Field@0} end
                            );

                        _ ->
                            gleam@dynamic@decode:failure(unknown, <<""/utf8>>)
                    end
                end
            )
        end
    ).

-file("src/lustre_dev_tools/port.gleam", 25).
?DOC(false).
-spec start(
    binary(),
    list(binary()),
    fun((gleam@dynamic:dynamic_()) -> nil),
    fun(() -> nil)
) -> {ok, gleam@erlang@process:subject(message())} |
    {error, gleam@otp@actor:start_error()}.
start(Program, Args, Handle_data, Handle_unknown) ->
    _pipe@7 = gleam@otp@actor:new_with_initialiser(
        1000,
        fun(Self) ->
            Port = port_ffi:start(Program, Args),
            Selector = begin
                _pipe = gleam_erlang_ffi:new_selector(),
                _pipe@1 = gleam@erlang@process:select(_pipe, Self),
                gleam@erlang@process:select_record(
                    _pipe@1,
                    Port,
                    1,
                    fun(Message) -> _pipe@2 = Message,
                        _pipe@3 = gleam@dynamic@decode:run(
                            _pipe@2,
                            decode_port_message()
                        ),
                        gleam@result:unwrap(_pipe@3, unknown) end
                )
            end,
            _pipe@4 = gleam@otp@actor:initialised(Port),
            _pipe@5 = gleam@otp@actor:selecting(_pipe@4, Selector),
            _pipe@6 = gleam@otp@actor:returning(_pipe@5, Self),
            {ok, _pipe@6}
        end
    ),
    _pipe@8 = gleam@otp@actor:on_message(
        _pipe@7,
        fun(Port@1, Message@1) -> case Message@1 of
                {send, Data} ->
                    port_ffi:send(Port@1, Data),
                    gleam@otp@actor:continue(Port@1);

                {data, Data@1} ->
                    case gleam@json:parse(
                        Data@1,
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    ) of
                        {ok, Data@2} ->
                            Handle_data(Data@2);

                        {error, _} ->
                            Handle_unknown()
                    end,
                    gleam@otp@actor:continue(Port@1);

                {exit, 0} ->
                    gleam@otp@actor:stop();

                {exit, _} ->
                    gleam@otp@actor:stop_abnormal(
                        <<"Port exited with code non-zero code"/utf8>>
                    );

                unknown ->
                    Handle_unknown(),
                    gleam@otp@actor:continue(Port@1)
            end end
    ),
    _pipe@9 = gleam@otp@actor:start(_pipe@8),
    gleam@result:map(_pipe@9, fun(Actor) -> erlang:element(3, Actor) end).
