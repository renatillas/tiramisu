-module(mist@internal@websocket).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/websocket.gleam").
-export([initialize_connection/6]).
-export_type([valid_message/1, websocket_message/1, websocket_connection/0, handler_message/1, websocket_state/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type valid_message(MSA) :: {socket_message, bitstring()} |
    socket_closed_message |
    {user_message, MSA}.

-type websocket_message(MSB) :: {valid, valid_message(MSB)} | invalid.

-type websocket_connection() :: {websocket_connection,
        glisten@socket:socket(),
        glisten@transport:transport(),
        gleam@option:option(gramps@websocket@compression:context())}.

-type handler_message(MSC) :: {internal, gramps@websocket:frame()} | {user, MSC}.

-type websocket_state(MSD) :: {websocket_state,
        bitstring(),
        MSD,
        gleam@option:option(gramps@websocket@compression:compression())}.

-file("src/mist/internal/websocket.gleam", 54).
?DOC(false).
-spec message_selector() -> gleam@erlang@process:selector(websocket_message(any())).
message_selector() ->
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@5 = gleam@erlang@process:select_record(
        _pipe,
        erlang:binary_to_atom(<<"tcp"/utf8>>),
        2,
        fun(Record) ->
            _pipe@1 = begin
                gleam@dynamic@decode:field(
                    2,
                    {decoder, fun gleam@dynamic@decode:decode_bit_array/1},
                    fun(Data) ->
                        gleam@dynamic@decode:success({socket_message, Data})
                    end
                )
            end,
            _pipe@2 = gleam@dynamic@decode:run(Record, _pipe@1),
            _pipe@3 = gleam@result:replace_error(_pipe@2, nil),
            _pipe@4 = gleam@result:map(
                _pipe@3,
                fun(Field@0) -> {valid, Field@0} end
            ),
            gleam@result:unwrap(_pipe@4, invalid)
        end
    ),
    _pipe@10 = gleam@erlang@process:select_record(
        _pipe@5,
        erlang:binary_to_atom(<<"ssl"/utf8>>),
        2,
        fun(Record@1) ->
            _pipe@6 = begin
                gleam@dynamic@decode:field(
                    2,
                    {decoder, fun gleam@dynamic@decode:decode_bit_array/1},
                    fun(Data@1) ->
                        gleam@dynamic@decode:success({socket_message, Data@1})
                    end
                )
            end,
            _pipe@7 = gleam@dynamic@decode:run(Record@1, _pipe@6),
            _pipe@8 = gleam@result:replace_error(_pipe@7, nil),
            _pipe@9 = gleam@result:map(
                _pipe@8,
                fun(Field@0) -> {valid, Field@0} end
            ),
            gleam@result:unwrap(_pipe@9, invalid)
        end
    ),
    _pipe@11 = gleam@erlang@process:select_record(
        _pipe@10,
        erlang:binary_to_atom(<<"ssl_closed"/utf8>>),
        1,
        fun(_) -> {valid, socket_closed_message} end
    ),
    gleam@erlang@process:select_record(
        _pipe@11,
        erlang:binary_to_atom(<<"tcp_closed"/utf8>>),
        1,
        fun(_) -> {valid, socket_closed_message} end
    ).

-file("src/mist/internal/websocket.gleam", 362).
?DOC(false).
-spec set_active(glisten@transport:transport(), glisten@socket:socket()) -> nil.
set_active(Transport, Socket) ->
    case glisten@transport:set_opts(Transport, Socket, [{active_mode, once}]) of
        {ok, _} -> nil;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"mist/internal/websocket"/utf8>>,
                        function => <<"set_active"/utf8>>,
                        line => 363,
                        value => _assert_fail,
                        start => 11155,
                        'end' => 11251,
                        pattern_start => 11166,
                        pattern_end => 11171})
    end,
    nil.

-file("src/mist/internal/websocket.gleam", 369).
?DOC(false).
-spec map_user_selector(gleam@option:option(gleam@erlang@process:selector(MTJ))) -> gleam@option:option(gleam@erlang@process:selector(websocket_message(MTJ))).
map_user_selector(Selector) ->
    gleam@option:map(
        Selector,
        fun(_capture) ->
            gleam_erlang_ffi:map_selector(
                _capture,
                fun(Msg) -> {valid, {user_message, Msg}} end
            )
        end
    ).

-file("src/mist/internal/websocket.gleam", 281).
?DOC(false).
-spec apply_frames(
    list(gramps@websocket:frame()),
    fun((MSZ, handler_message(MTA), websocket_connection()) -> mist@internal@next:next(MSZ, MTA)),
    websocket_connection(),
    mist@internal@next:next(MSZ, websocket_message(MTA)),
    fun((MSZ) -> nil)
) -> mist@internal@next:next(MSZ, websocket_message(MTA)).
apply_frames(Frames, Handler, Connection, Next, On_close) ->
    case {Frames, Next} of
        {_, {abnormal_stop, Reason}} ->
            {abnormal_stop, Reason};

        {_, normal_stop} ->
            normal_stop;

        {[], Next@1} ->
            set_active(
                erlang:element(3, Connection),
                erlang:element(2, Connection)
            ),
            Next@1;

        {[{control, {close_frame, Reason@1}} | _], {continue, State, _}} ->
            _ = glisten@transport:send(
                erlang:element(3, Connection),
                erlang:element(2, Connection),
                gramps@websocket:encode_close_frame(Reason@1, none)
            ),
            On_close(State),
            normal_stop;

        {[{control, {ping_frame, Payload}} | _], {continue, State@1, _}} ->
            _pipe = glisten@transport:send(
                erlang:element(3, Connection),
                erlang:element(2, Connection),
                gramps@websocket:encode_pong_frame(Payload, none)
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_) ->
                    set_active(
                        erlang:element(3, Connection),
                        erlang:element(2, Connection)
                    ),
                    {continue, State@1, none}
                end
            ),
            gleam@result:lazy_unwrap(
                _pipe@1,
                fun() ->
                    On_close(State@1),
                    {abnormal_stop, <<"Failed to send pong frame"/utf8>>}
                end
            );

        {[Frame | Rest], {continue, State@2, Prev_selector}} ->
            case exception_ffi:rescue(
                fun() -> Handler(State@2, {internal, Frame}, Connection) end
            ) of
                {ok, {continue, State@3, Selector}} ->
                    Next_selector = begin
                        _pipe@2 = Selector,
                        _pipe@3 = map_user_selector(_pipe@2),
                        _pipe@4 = gleam@option:'or'(_pipe@3, Prev_selector),
                        gleam@option:map(
                            _pipe@4,
                            fun(With_user) ->
                                gleam_erlang_ffi:merge_selector(
                                    message_selector(),
                                    With_user
                                )
                            end
                        )
                    end,
                    apply_frames(
                        Rest,
                        Handler,
                        Connection,
                        {continue, State@3, Next_selector},
                        On_close
                    );

                {ok, {abnormal_stop, Reason@2}} ->
                    On_close(State@2),
                    {abnormal_stop, Reason@2};

                {ok, normal_stop} ->
                    On_close(State@2),
                    normal_stop;

                {error, Reason@3} ->
                    logging:log(
                        error,
                        <<"Caught error in websocket handler: "/utf8,
                            (gleam@string:inspect(Reason@3))/binary>>
                    ),
                    On_close(State@2),
                    {abnormal_stop, <<"Crash in user websocket handler"/utf8>>}
            end
    end.

-file("src/mist/internal/websocket.gleam", 84).
?DOC(false).
-spec initialize_connection(
    fun((websocket_connection()) -> {MSM,
        gleam@option:option(gleam@erlang@process:selector(MSN))}),
    fun((MSM) -> nil),
    fun((MSM, handler_message(MSN), websocket_connection()) -> mist@internal@next:next(MSM, MSN)),
    glisten@socket:socket(),
    glisten@transport:transport(),
    list(binary())
) -> {ok,
        gleam@otp@actor:started(gleam@erlang@process:subject(websocket_message(MSN)))} |
    {error, nil}.
initialize_connection(On_init, On_close, Handler, Socket, Transport, Extensions) ->
    Takeovers = gramps@websocket:get_context_takeovers(Extensions),
    _pipe@7 = gleam@otp@actor:new_with_initialiser(
        500,
        fun(Subject) ->
            Compression = case gramps@websocket:has_deflate(Extensions) of
                true ->
                    {some, gramps@websocket@compression:init(Takeovers)};

                false ->
                    none
            end,
            Connection = {websocket_connection,
                Socket,
                Transport,
                gleam@option:map(
                    Compression,
                    fun(Compression@1) -> erlang:element(3, Compression@1) end
                )},
            {Initial_state, User_selector} = On_init(Connection),
            Selector = case User_selector of
                {some, User_selector@1} ->
                    _pipe = User_selector@1,
                    _pipe@1 = gleam_erlang_ffi:map_selector(
                        _pipe,
                        fun(Field@0) -> {user_message, Field@0} end
                    ),
                    _pipe@2 = gleam_erlang_ffi:map_selector(
                        _pipe@1,
                        fun(Field@0) -> {valid, Field@0} end
                    ),
                    gleam_erlang_ffi:merge_selector(_pipe@2, message_selector());

                _ ->
                    message_selector()
            end,
            _pipe@3 = {websocket_state, <<>>, Initial_state, Compression},
            _pipe@4 = gleam@otp@actor:initialised(_pipe@3),
            _pipe@5 = gleam@otp@actor:selecting(_pipe@4, Selector),
            _pipe@6 = gleam@otp@actor:returning(_pipe@5, Subject),
            {ok, _pipe@6}
        end
    ),
    _pipe@16 = gleam@otp@actor:on_message(
        _pipe@7,
        fun(State, Msg) ->
            Connection@1 = {websocket_connection,
                Socket,
                Transport,
                gleam@option:map(
                    erlang:element(4, State),
                    fun(Compression@2) -> erlang:element(3, Compression@2) end
                )},
            case Msg of
                {valid, {socket_message, Data}} ->
                    {Frames, Rest} = gramps@websocket:decode_many_frames(
                        <<(erlang:element(2, State))/bitstring, Data/bitstring>>,
                        gleam@option:map(
                            erlang:element(4, State),
                            fun(Compression@3) ->
                                erlang:element(2, Compression@3)
                            end
                        ),
                        []
                    ),
                    _pipe@8 = Frames,
                    _pipe@9 = gramps@websocket:aggregate_frames(
                        _pipe@8,
                        none,
                        []
                    ),
                    _pipe@10 = gleam@result:map(
                        _pipe@9,
                        fun(Frames@1) ->
                            Next = apply_frames(
                                Frames@1,
                                Handler,
                                Connection@1,
                                {continue, erlang:element(3, State), none},
                                On_close
                            ),
                            case Next of
                                {continue, User_state, Selector@1} ->
                                    Next@1 = gleam@otp@actor:continue(
                                        {websocket_state,
                                            Rest,
                                            User_state,
                                            erlang:element(4, State)}
                                    ),
                                    case Selector@1 of
                                        {some, Selector@2} ->
                                            gleam@otp@actor:with_selector(
                                                Next@1,
                                                Selector@2
                                            );

                                        _ ->
                                            Next@1
                                    end;

                                normal_stop ->
                                    _ = gleam@option:map(
                                        erlang:element(4, State),
                                        fun(Contexts) ->
                                            gramps@websocket@compression:close(
                                                erlang:element(3, Contexts)
                                            ),
                                            gramps@websocket@compression:close(
                                                erlang:element(2, Contexts)
                                            )
                                        end
                                    ),
                                    gleam@otp@actor:stop();

                                {abnormal_stop, Reason} ->
                                    _ = gleam@option:map(
                                        erlang:element(4, State),
                                        fun(Contexts@1) ->
                                            gramps@websocket@compression:close(
                                                erlang:element(3, Contexts@1)
                                            ),
                                            gramps@websocket@compression:close(
                                                erlang:element(2, Contexts@1)
                                            )
                                        end
                                    ),
                                    gleam@otp@actor:stop_abnormal(Reason)
                            end
                        end
                    ),
                    gleam@result:lazy_unwrap(
                        _pipe@10,
                        fun() ->
                            logging:log(
                                error,
                                <<"Received a malformed WebSocket frame"/utf8>>
                            ),
                            On_close(erlang:element(3, State)),
                            _ = gleam@option:map(
                                erlang:element(4, State),
                                fun(Contexts@2) ->
                                    gramps@websocket@compression:close(
                                        erlang:element(3, Contexts@2)
                                    ),
                                    gramps@websocket@compression:close(
                                        erlang:element(2, Contexts@2)
                                    )
                                end
                            ),
                            gleam@otp@actor:stop_abnormal(
                                <<"WebSocket received a malformed message"/utf8>>
                            )
                        end
                    );

                {valid, {user_message, Msg@1}} ->
                    _pipe@11 = exception_ffi:rescue(
                        fun() ->
                            Handler(
                                erlang:element(3, State),
                                {user, Msg@1},
                                Connection@1
                            )
                        end
                    ),
                    _pipe@14 = gleam@result:map(
                        _pipe@11,
                        fun(Cont) -> case Cont of
                                {continue, User_state@1, Selector@3} ->
                                    Selector@4 = begin
                                        _pipe@12 = Selector@3,
                                        _pipe@13 = map_user_selector(_pipe@12),
                                        gleam@option:map(
                                            _pipe@13,
                                            fun(With_user) ->
                                                gleam_erlang_ffi:merge_selector(
                                                    message_selector(),
                                                    With_user
                                                )
                                            end
                                        )
                                    end,
                                    Next@2 = gleam@otp@actor:continue(
                                        {websocket_state,
                                            erlang:element(2, State),
                                            User_state@1,
                                            erlang:element(4, State)}
                                    ),
                                    case Selector@4 of
                                        {some, Selector@5} ->
                                            gleam@otp@actor:with_selector(
                                                Next@2,
                                                Selector@5
                                            );

                                        _ ->
                                            Next@2
                                    end;

                                normal_stop ->
                                    _ = gleam@option:map(
                                        erlang:element(4, State),
                                        fun(Contexts@3) ->
                                            gramps@websocket@compression:close(
                                                erlang:element(3, Contexts@3)
                                            ),
                                            gramps@websocket@compression:close(
                                                erlang:element(2, Contexts@3)
                                            )
                                        end
                                    ),
                                    On_close(erlang:element(3, State)),
                                    gleam@otp@actor:stop();

                                {abnormal_stop, Reason@1} ->
                                    _ = gleam@option:map(
                                        erlang:element(4, State),
                                        fun(Contexts@4) ->
                                            gramps@websocket@compression:close(
                                                erlang:element(3, Contexts@4)
                                            ),
                                            gramps@websocket@compression:close(
                                                erlang:element(2, Contexts@4)
                                            )
                                        end
                                    ),
                                    On_close(erlang:element(3, State)),
                                    gleam@otp@actor:stop_abnormal(Reason@1)
                            end end
                    ),
                    _pipe@15 = gleam@result:map_error(
                        _pipe@14,
                        fun(Err) ->
                            logging:log(
                                error,
                                <<"Caught error in websocket handler: "/utf8,
                                    (gleam@string:inspect(Err))/binary>>
                            )
                        end
                    ),
                    gleam@result:lazy_unwrap(
                        _pipe@15,
                        fun() ->
                            _ = gleam@option:map(
                                erlang:element(4, State),
                                fun(Contexts@5) ->
                                    gramps@websocket@compression:close(
                                        erlang:element(3, Contexts@5)
                                    ),
                                    gramps@websocket@compression:close(
                                        erlang:element(2, Contexts@5)
                                    )
                                end
                            ),
                            On_close(erlang:element(3, State)),
                            gleam@otp@actor:stop_abnormal(
                                <<"Crash in user websocket handler"/utf8>>
                            )
                        end
                    );

                {valid, socket_closed_message} ->
                    _ = gleam@option:map(
                        erlang:element(4, State),
                        fun(Contexts@6) ->
                            gramps@websocket@compression:close(
                                erlang:element(3, Contexts@6)
                            ),
                            gramps@websocket@compression:close(
                                erlang:element(2, Contexts@6)
                            )
                        end
                    ),
                    On_close(erlang:element(3, State)),
                    gleam@otp@actor:stop();

                invalid ->
                    logging:log(
                        error,
                        <<"Received a malformed WebSocket frame"/utf8>>
                    ),
                    _ = gleam@option:map(
                        erlang:element(4, State),
                        fun(Contexts@7) ->
                            gramps@websocket@compression:close(
                                erlang:element(3, Contexts@7)
                            ),
                            gramps@websocket@compression:close(
                                erlang:element(2, Contexts@7)
                            )
                        end
                    ),
                    On_close(erlang:element(3, State)),
                    gleam@otp@actor:stop_abnormal(
                        <<"WebSocket received a malformed message"/utf8>>
                    )
            end
        end
    ),
    _pipe@17 = gleam@otp@actor:start(_pipe@16),
    _pipe@18 = gleam@result:replace_error(_pipe@17, nil),
    _pipe@19 = gleam@result:map(
        _pipe@18,
        fun(Subj) ->
            Websocket_pid@1 = case gleam@erlang@process:subject_owner(
                erlang:element(3, Subj)
            ) of
                {ok, Websocket_pid} -> Websocket_pid;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"initialize_connection"/utf8>>,
                                line => 272,
                                value => _assert_fail,
                                start => 8567,
                                'end' => 8630,
                                pattern_start => 8578,
                                pattern_end => 8595})
            end,
            case glisten@transport:controlling_process(
                Transport,
                Socket,
                Websocket_pid@1
            ) of
                {ok, _} -> nil;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"initialize_connection"/utf8>>,
                                line => 273,
                                value => _assert_fail@1,
                                start => 8635,
                                'end' => 8723,
                                pattern_start => 8646,
                                pattern_end => 8651})
            end,
            set_active(Transport, Socket),
            Subj
        end
    ),
    gleam@result:replace_error(_pipe@19, nil).
