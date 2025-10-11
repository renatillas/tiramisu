-module(mist@internal@http2@stream).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/http2/stream.gleam").
-export([make_request/2, new/6, receive_data/2]).
-export_type([message/0, send_message/0, stream_state/0, state/0, internal_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type message() :: ready | {data, bitstring(), boolean()} | done.

-type send_message() :: {send,
        mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
        gleam@http@response:response(mist@internal@http:response_data())}.

-type stream_state() :: open | remote_closed | local_closed | closed.

-type state() :: {state,
        mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
        stream_state(),
        gleam@erlang@process:subject(message()),
        integer(),
        integer(),
        gleam@option:option(integer())}.

-type internal_state() :: {internal_state,
        gleam@erlang@process:selector(message()),
        gleam@erlang@process:subject(message()),
        boolean(),
        gleam@option:option(gleam@http@response:response(mist@internal@http:response_data())),
        bitstring()}.

-file("src/mist/internal/http2/stream.gleam", 140).
?DOC(false).
-spec make_request(
    list({binary(), binary()}),
    gleam@http@request:request(mist@internal@http:connection())
) -> {ok, gleam@http@request:request(mist@internal@http:connection())} |
    {error, nil}.
make_request(Headers, Req) ->
    case Headers of
        [] ->
            {ok, Req};

        [{<<"method"/utf8>>, Method} | Rest] ->
            _pipe = Method,
            _pipe@1 = gleam@http:parse_method(_pipe),
            _pipe@2 = gleam@result:replace_error(_pipe@1, nil),
            _pipe@3 = gleam@result:map(
                _pipe@2,
                fun(_capture) ->
                    gleam@http@request:set_method(Req, _capture)
                end
            ),
            gleam@result:'try'(
                _pipe@3,
                fun(_capture@1) -> make_request(Rest, _capture@1) end
            );

        [{<<"scheme"/utf8>>, Scheme} | Rest@1] ->
            _pipe@4 = Scheme,
            _pipe@5 = gleam@http:scheme_from_string(_pipe@4),
            _pipe@6 = gleam@result:replace_error(_pipe@5, nil),
            _pipe@7 = gleam@result:map(
                _pipe@6,
                fun(_capture@2) ->
                    gleam@http@request:set_scheme(Req, _capture@2)
                end
            ),
            gleam@result:'try'(
                _pipe@7,
                fun(_capture@3) -> make_request(Rest@1, _capture@3) end
            );

        [{<<"authority"/utf8>>, _} | Rest@2] ->
            make_request(Rest@2, Req);

        [{<<"path"/utf8>>, Path} | Rest@3] ->
            _pipe@8 = Path,
            _pipe@9 = gleam@string:split_once(_pipe@8, <<"?"/utf8>>),
            _pipe@13 = gleam@result:map(
                _pipe@9,
                fun(Split) ->
                    gleam@pair:map_second(Split, fun(Query) -> _pipe@10 = Query,
                            _pipe@11 = gleam_stdlib:parse_query(_pipe@10),
                            _pipe@12 = gleam@result:map(
                                _pipe@11,
                                fun(Field@0) -> {some, Field@0} end
                            ),
                            gleam@result:unwrap(_pipe@12, none) end)
                end
            ),
            _pipe@14 = gleam@result:unwrap(_pipe@13, {Path, none}),
            (fun(Tup) -> _pipe@17 = case erlang:element(2, Tup) of
                    {some, Query@1} ->
                        _pipe@15 = Req,
                        _pipe@16 = gleam@http@request:set_path(
                            _pipe@15,
                            erlang:element(1, Tup)
                        ),
                        gleam@http@request:set_query(_pipe@16, Query@1);

                    _ ->
                        gleam@http@request:set_path(Req, erlang:element(1, Tup))
                end,
                make_request(Rest@3, _pipe@17) end)(_pipe@14);

        [{Key, Value} | Rest@4] ->
            _pipe@18 = Req,
            _pipe@19 = gleam@http@request:set_header(_pipe@18, Key, Value),
            make_request(Rest@4, _pipe@19)
    end.

-file("src/mist/internal/http2/stream.gleam", 57).
?DOC(false).
-spec new(
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data())),
    list({binary(), binary()}),
    mist@internal@http:connection(),
    gleam@erlang@process:subject(send_message()),
    boolean()
) -> {ok, gleam@otp@actor:started(gleam@erlang@process:subject(message()))} |
    {error, gleam@otp@actor:start_error()}.
new(Identifier, Handler, Headers, Connection, Sender, End) ->
    _pipe@5 = gleam@otp@actor:new_with_initialiser(
        1000,
        fun(Subject) ->
            Data_selector = begin
                _pipe = gleam_erlang_ffi:new_selector(),
                gleam@erlang@process:select(_pipe, Subject)
            end,
            _pipe@1 = {internal_state, Data_selector, Subject, End, none, <<>>},
            _pipe@2 = gleam@otp@actor:initialised(_pipe@1),
            _pipe@3 = gleam@otp@actor:selecting(_pipe@2, Data_selector),
            _pipe@4 = gleam@otp@actor:returning(_pipe@3, Subject),
            {ok, _pipe@4}
        end
    ),
    _pipe@15 = gleam@otp@actor:on_message(
        _pipe@5,
        fun(State, Msg) -> case {Msg, erlang:element(4, State)} of
                {ready, _} ->
                    Content_length = begin
                        _pipe@6 = Headers,
                        _pipe@7 = gleam@list:key_find(
                            _pipe@6,
                            <<"content-length"/utf8>>
                        ),
                        _pipe@8 = gleam@result:'try'(
                            _pipe@7,
                            fun gleam_stdlib:parse_int/1
                        ),
                        gleam@result:unwrap(_pipe@8, 0)
                    end,
                    Conn = {connection,
                        {stream,
                            gleam_erlang_ffi:map_selector(
                                erlang:element(2, State),
                                fun(Val) ->
                                    Bits@1 = case Val of
                                        {data, Bits, _} -> Bits;
                                        _assert_fail ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        file => <<?FILEPATH/utf8>>,
                                                        module => <<"mist/internal/http2/stream"/utf8>>,
                                                        function => <<"new"/utf8>>,
                                                        line => 88,
                                                        value => _assert_fail,
                                                        start => 2215,
                                                        'end' => 2246,
                                                        pattern_start => 2226,
                                                        pattern_end => 2240}
                                                )
                                    end,
                                    Bits@1
                                end
                            ),
                            <<>>,
                            Content_length,
                            0},
                        erlang:element(3, Connection),
                        erlang:element(4, Connection)},
                    _pipe@9 = gleam@http@request:new(),
                    _pipe@10 = gleam@http@request:set_body(_pipe@9, Conn),
                    _pipe@11 = make_request(Headers, _pipe@10),
                    _pipe@12 = gleam@result:map(_pipe@11, Handler),
                    _pipe@13 = gleam@result:map(
                        _pipe@12,
                        fun(Resp) ->
                            gleam@erlang@process:send(
                                erlang:element(3, State),
                                done
                            ),
                            gleam@otp@actor:continue(
                                {internal_state,
                                    erlang:element(2, State),
                                    erlang:element(3, State),
                                    erlang:element(4, State),
                                    {some, Resp},
                                    erlang:element(6, State)}
                            )
                        end
                    ),
                    _pipe@14 = gleam@result:map_error(
                        _pipe@13,
                        fun(Err) ->
                            gleam@otp@actor:stop_abnormal(
                                <<"Failed to respond to request: "/utf8,
                                    (gleam@string:inspect(Err))/binary>>
                            )
                        end
                    ),
                    gleam@result:unwrap_both(_pipe@14);

                {done, true} ->
                    Resp@2 = case erlang:element(5, State) of
                        {some, Resp@1} -> Resp@1;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"mist/internal/http2/stream"/utf8>>,
                                        function => <<"new"/utf8>>,
                                        line => 113,
                                        value => _assert_fail@1,
                                        start => 2932,
                                        'end' => 2978,
                                        pattern_start => 2943,
                                        pattern_end => 2953})
                    end,
                    gleam@erlang@process:send(
                        Sender,
                        {send, Identifier, Resp@2}
                    ),
                    gleam@otp@actor:continue(State);

                {{data, Bits@2, true}, _} ->
                    gleam@erlang@process:send(erlang:element(3, State), done),
                    gleam@otp@actor:continue(
                        {internal_state,
                            erlang:element(2, State),
                            erlang:element(3, State),
                            true,
                            erlang:element(5, State),
                            <<(erlang:element(6, State))/bitstring,
                                Bits@2/bitstring>>}
                    );

                {{data, Bits@3, _}, _} ->
                    gleam@otp@actor:continue(
                        {internal_state,
                            erlang:element(2, State),
                            erlang:element(3, State),
                            erlang:element(4, State),
                            erlang:element(5, State),
                            <<(erlang:element(6, State))/bitstring,
                                Bits@3/bitstring>>}
                    );

                {_, _} ->
                    gleam@otp@actor:continue(State)
            end end
    ),
    gleam@otp@actor:start(_pipe@15).

-file("src/mist/internal/http2/stream.gleam", 192).
?DOC(false).
-spec receive_data(state(), integer()) -> {state(), integer()}.
receive_data(State, Size) ->
    {New_window_size, Increment} = mist@internal@http2@flow_control:compute_receive_window(
        erlang:element(5, State),
        Size
    ),
    New_state = {state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        New_window_size,
        erlang:element(6, State),
        gleam@option:map(erlang:element(7, State), fun(Val) -> Val - Size end)},
    {New_state, Increment}.
