-module(mist@internal@http@handler).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/http/handler.gleam").
-export([initial_state/0, call/5]).
-export_type([state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type state() :: {state, gleam@option:option(gleam@erlang@process:timer())}.

-file("src/mist/internal/http/handler.gleam", 27).
?DOC(false).
-spec initial_state() -> state().
initial_state() ->
    {state, none}.

-file("src/mist/internal/http/handler.gleam", 67).
?DOC(false).
-spec log_and_error(
    exception:exception(),
    glisten@socket:socket(),
    glisten@transport:transport(),
    gleam@http@request:request(mist@internal@http:connection()),
    mist@internal@http:http_version()
) -> {ok, nil} | {error, binary()}.
log_and_error(Error, Socket, Transport, Req, Version) ->
    logging:log(error, gleam@string:inspect(Error)),
    Resp = begin
        _pipe = gleam@http@response:new(500),
        _pipe@1 = gleam@http@response:set_body(
            _pipe,
            gleam@bytes_tree:from_bit_array(<<"Internal Server Error"/utf8>>)
        ),
        _pipe@2 = gleam@http@response:prepend_header(
            _pipe@1,
            <<"content-length"/utf8>>,
            <<"21"/utf8>>
        ),
        mist@internal@http:add_default_headers(
            _pipe@2,
            erlang:element(2, Req) =:= head
        )
    end,
    Resp@1 = case Version of
        http1 ->
            mist@internal@http:connection_close(Resp);

        _ ->
            mist@internal@http:maybe_keep_alive(Resp)
    end,
    _ = begin
        _pipe@3 = Resp@1,
        _pipe@4 = mist@internal@encoder:to_bytes_tree(
            _pipe@3,
            mist@internal@http:version_to_string(Version)
        ),
        glisten@transport:send(Transport, Socket, _pipe@4)
    end,
    _ = glisten@transport:close(Transport, Socket),
    {error, gleam@string:inspect(Error)}.

-file("src/mist/internal/http/handler.gleam", 97).
?DOC(false).
-spec close_or_set_timer(
    gleam@http@response:response(gleam@bytes_tree:bytes_tree()),
    mist@internal@http:connection(),
    gleam@erlang@process:subject(glisten@internal@handler:message(any()))
) -> {ok, state()} | {error, {ok, nil} | {error, binary()}}.
close_or_set_timer(Resp, Conn, Sender) ->
    case gleam@http@response:get_header(Resp, <<"connection"/utf8>>) of
        {ok, <<"close"/utf8>>} ->
            _ = glisten@transport:close(
                erlang:element(4, Conn),
                erlang:element(3, Conn)
            ),
            {error, {ok, nil}};

        _ ->
            Timer = gleam@erlang@process:send_after(
                Sender,
                10000,
                {internal, close}
            ),
            {ok, {state, {some, Timer}}}
    end.

-file("src/mist/internal/http/handler.gleam", 155).
?DOC(false).
-spec handle_file_body(
    gleam@http@response:response(mist@internal@http:response_data()),
    mist@internal@http:response_data(),
    mist@internal@http:connection(),
    mist@internal@http:http_version()
) -> {ok, gleam@http@response:response(gleam@bytes_tree:bytes_tree())} |
    {error, glisten@socket:socket_reason()}.
handle_file_body(Resp, Body, Conn, Http_version) ->
    {File_descriptor@1, Offset@1, Length@1} = case Body of
        {file, File_descriptor, Offset, Length} -> {
        File_descriptor,
            Offset,
            Length};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"mist/internal/http/handler"/utf8>>,
                        function => <<"handle_file_body"/utf8>>,
                        line => 161,
                        value => _assert_fail,
                        start => 4605,
                        'end' => 4660,
                        pattern_start => 4616,
                        pattern_end => 4653})
    end,
    Resp@1 = begin
        _pipe = Resp,
        _pipe@1 = gleam@http@response:set_body(_pipe, gleam@bytes_tree:new()),
        _pipe@2 = mist@internal@http:add_date_header(_pipe@1),
        gleam@http@response:prepend_header(
            _pipe@2,
            <<"content-length"/utf8>>,
            erlang:integer_to_binary(Length@1 - Offset@1)
        )
    end,
    Resp@2 = case Http_version of
        http1 ->
            mist@internal@http:connection_close(Resp@1);

        _ ->
            mist@internal@http:maybe_keep_alive(Resp@1)
    end,
    Return = begin
        _pipe@3 = Resp@2,
        _pipe@4 = (fun(R) ->
            mist@internal@encoder:response_builder(
                erlang:element(2, Resp@2),
                erlang:element(3, R),
                mist@internal@http:version_to_string(Http_version)
            )
        end)(_pipe@3),
        _pipe@5 = glisten@transport:send(
            erlang:element(4, Conn),
            erlang:element(3, Conn),
            _pipe@4
        ),
        _pipe@7 = gleam@result:'try'(
            _pipe@5,
            fun(_) ->
                _pipe@6 = mist@internal@file:sendfile(
                    erlang:element(4, Conn),
                    File_descriptor@1,
                    erlang:element(3, Conn),
                    Offset@1,
                    Length@1,
                    []
                ),
                gleam@result:map_error(
                    _pipe@6,
                    fun(Err) ->
                        logging:log(
                            error,
                            <<"Failed to send file: "/utf8,
                                (gleam@string:inspect(Err))/binary>>
                        ),
                        badarg
                    end
                )
            end
        ),
        gleam@result:replace(_pipe@7, Resp@2)
    end,
    case mist_ffi:file_close(File_descriptor@1) of
        {ok, _} ->
            nil;

        {error, Reason} ->
            logging:log(
                error,
                <<"Failed to close file: "/utf8,
                    (gleam@string:inspect(Reason))/binary>>
            )
    end,
    Return.

-file("src/mist/internal/http/handler.gleam", 215).
?DOC(false).
-spec handle_bytes_tree_body(
    gleam@http@response:response(mist@internal@http:response_data()),
    gleam@bytes_tree:bytes_tree(),
    mist@internal@http:connection(),
    gleam@http@request:request(mist@internal@http:connection()),
    mist@internal@http:http_version()
) -> {ok, gleam@http@response:response(gleam@bytes_tree:bytes_tree())} |
    {error, glisten@socket:socket_reason()}.
handle_bytes_tree_body(Resp, Body, Conn, Req, Version) ->
    Resp@1 = begin
        _pipe = Resp,
        _pipe@1 = gleam@http@response:set_body(_pipe, Body),
        mist@internal@http:add_default_headers(
            _pipe@1,
            erlang:element(2, Req) =:= head
        )
    end,
    Resp@2 = case Version of
        http1 ->
            mist@internal@http:connection_close(Resp@1);

        _ ->
            mist@internal@http:maybe_keep_alive(Resp@1)
    end,
    _pipe@2 = Resp@2,
    _pipe@3 = mist@internal@encoder:to_bytes_tree(
        _pipe@2,
        mist@internal@http:version_to_string(Version)
    ),
    _pipe@4 = glisten@transport:send(
        erlang:element(4, Conn),
        erlang:element(3, Conn),
        _pipe@3
    ),
    gleam@result:replace(_pipe@4, Resp@2).

-file("src/mist/internal/http/handler.gleam", 242).
?DOC(false).
-spec int_to_hex(integer()) -> binary().
int_to_hex(Int) ->
    erlang:integer_to_list(Int, 16).

-file("src/mist/internal/http/handler.gleam", 117).
?DOC(false).
-spec handle_chunked_body(
    gleam@http@response:response(mist@internal@http:response_data()),
    gleam@yielder:yielder(gleam@bytes_tree:bytes_tree()),
    mist@internal@http:connection(),
    mist@internal@http:http_version()
) -> {ok, gleam@http@response:response(gleam@bytes_tree:bytes_tree())} |
    {error, glisten@socket:socket_reason()}.
handle_chunked_body(Resp, Body, Conn, Version) ->
    Headers = [{<<"transfer-encoding"/utf8>>, <<"chunked"/utf8>>} |
        erlang:element(3, Resp)],
    Initial_payload = mist@internal@encoder:response_builder(
        erlang:element(2, Resp),
        Headers,
        mist@internal@http:version_to_string(Version)
    ),
    _pipe = glisten@transport:send(
        erlang:element(4, Conn),
        erlang:element(3, Conn),
        Initial_payload
    ),
    _pipe@8 = gleam@result:'try'(_pipe, fun(_) -> _pipe@1 = Body,
            _pipe@2 = gleam@yielder:append(
                _pipe@1,
                gleam@yielder:from_list([gleam@bytes_tree:new()])
            ),
            gleam@yielder:try_fold(
                _pipe@2,
                nil,
                fun(_, Chunk) ->
                    Size = erlang:iolist_size(Chunk),
                    Encoded = begin
                        _pipe@3 = Size,
                        _pipe@4 = int_to_hex(_pipe@3),
                        _pipe@5 = gleam_stdlib:wrap_list(_pipe@4),
                        _pipe@6 = gleam@bytes_tree:append_string(
                            _pipe@5,
                            <<"\r\n"/utf8>>
                        ),
                        _pipe@7 = gleam_stdlib:iodata_append(_pipe@6, Chunk),
                        gleam@bytes_tree:append_string(_pipe@7, <<"\r\n"/utf8>>)
                    end,
                    glisten@transport:send(
                        erlang:element(4, Conn),
                        erlang:element(3, Conn),
                        Encoded
                    )
                end
            ) end),
    gleam@result:replace(
        _pipe@8,
        begin
            _pipe@9 = Resp,
            _pipe@10 = gleam@http@response:set_header(
                _pipe@9,
                <<"tranfer-encoding"/utf8>>,
                <<"chunked"/utf8>>
            ),
            gleam@http@response:set_body(_pipe@10, gleam@bytes_tree:new())
        end
    ).

-file("src/mist/internal/http/handler.gleam", 31).
?DOC(false).
-spec call(
    gleam@http@request:request(mist@internal@http:connection()),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data())),
    mist@internal@http:connection(),
    gleam@erlang@process:subject(glisten@internal@handler:message(any())),
    mist@internal@http:http_version()
) -> {ok, state()} | {error, {ok, nil} | {error, binary()}}.
call(Req, Handler, Conn, Sender, Version) ->
    _pipe = exception_ffi:rescue(fun() -> Handler(Req) end),
    _pipe@1 = gleam@result:map_error(
        _pipe,
        fun(_capture) ->
            log_and_error(
                _capture,
                erlang:element(3, Conn),
                erlang:element(4, Conn),
                Req,
                Version
            )
        end
    ),
    gleam@result:'try'(_pipe@1, fun(Resp) -> case Resp of
                {response, _, _, {websocket, Selector}} ->
                    _ = gleam_erlang_ffi:select(Selector),
                    {error, {ok, nil}};

                {response, _, _, {server_sent_events, Selector}} ->
                    _ = gleam_erlang_ffi:select(Selector),
                    {error, {ok, nil}};

                {response, _, _, Body} = Resp@1 ->
                    _pipe@2 = case Body of
                        {bytes, Body@1} ->
                            handle_bytes_tree_body(
                                Resp@1,
                                Body@1,
                                Conn,
                                Req,
                                Version
                            );

                        {chunked, Body@2} ->
                            handle_chunked_body(Resp@1, Body@2, Conn, Version);

                        {file, _, _, _} ->
                            handle_file_body(Resp@1, Body, Conn, Version);

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"This shouldn't ever happen 🤞"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"mist/internal/http/handler"/utf8>>,
                                    function => <<"call"/utf8>>,
                                    line => 58})
                    end,
                    _pipe@3 = gleam@result:replace_error(_pipe@2, {ok, nil}),
                    gleam@result:'try'(
                        _pipe@3,
                        fun(_capture@1) ->
                            close_or_set_timer(_capture@1, Conn, Sender)
                        end
                    )
            end end).
