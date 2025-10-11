-module(mist@internal@http).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/http.gleam").
-export([from_header/1, read_data/4, version_to_string/1, add_date_header/1, connection_close/1, keep_alive/1, maybe_keep_alive/1, add_content_length/2, add_default_headers/2, handle_continue/1, parse_headers/4, crypto_hash/2, base64_encode/1, parse_chunk/1, parse_request/2, read_body/1, upgrade_socket/2, upgrade/4]).
-export_type([response_data/0, connection/0, packet_type/0, http_uri/0, http_packet/0, decoded_packet/0, decode_error/0, chunk/0, http_version/0, parsed_request/0, body/0, sha_hash/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type response_data() :: {websocket,
        gleam@erlang@process:selector(gleam@erlang@process:down())} |
    {bytes, gleam@bytes_tree:bytes_tree()} |
    {chunked, gleam@yielder:yielder(gleam@bytes_tree:bytes_tree())} |
    {file, mist@internal@file:file_descriptor(), integer(), integer()} |
    {server_sent_events,
        gleam@erlang@process:selector(gleam@erlang@process:down())}.

-type connection() :: {connection,
        body(),
        glisten@socket:socket(),
        glisten@transport:transport()}.

-type packet_type() :: http | httph_bin | http_bin.

-type http_uri() :: {abs_path, bitstring()}.

-type http_packet() :: {http_request,
        gleam@dynamic:dynamic_(),
        http_uri(),
        {integer(), integer()}} |
    {http_header,
        integer(),
        gleam@erlang@atom:atom_(),
        bitstring(),
        bitstring()}.

-type decoded_packet() :: {binary_data, http_packet(), bitstring()} |
    {end_of_headers, bitstring()} |
    {more_data, gleam@option:option(integer())} |
    {http2_upgrade, bitstring()}.

-type decode_error() :: malformed_request |
    invalid_method |
    invalid_path |
    unknown_header |
    unknown_method |
    invalid_body |
    discard_packet |
    no_host_header |
    invalid_http_version.

-type chunk() :: {chunk, bitstring(), mist@internal@buffer:buffer()} | complete.

-type http_version() :: http1 | http11.

-type parsed_request() :: {http1_request,
        gleam@http@request:request(connection()),
        http_version()} |
    {upgrade, bitstring()}.

-type body() :: {initial, bitstring()} |
    {stream,
        gleam@erlang@process:selector(bitstring()),
        bitstring(),
        integer(),
        integer()}.

-type sha_hash() :: sha.

-file("src/mist/internal/http.gleam", 77).
?DOC(false).
-spec from_header(bitstring()) -> binary().
from_header(Value) ->
    Value@2 = case gleam@bit_array:to_string(Value) of
        {ok, Value@1} -> Value@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"mist/internal/http"/utf8>>,
                        function => <<"from_header"/utf8>>,
                        line => 78,
                        value => _assert_fail,
                        start => 1766,
                        'end' => 1815,
                        pattern_start => 1777,
                        pattern_end => 1786})
    end,
    string:lowercase(Value@2).

-file("src/mist/internal/http.gleam", 112).
?DOC(false).
-spec read_data(
    glisten@socket:socket(),
    glisten@transport:transport(),
    mist@internal@buffer:buffer(),
    decode_error()
) -> {ok, bitstring()} | {error, decode_error()}.
read_data(Socket, Transport, Buffer, Error) ->
    To_read = gleam@int:min(erlang:element(2, Buffer), 1000000),
    Timeout = 15000,
    gleam@result:'try'(
        begin
            _pipe = Socket,
            _pipe@1 = glisten@transport:receive_timeout(
                Transport,
                _pipe,
                To_read,
                Timeout
            ),
            gleam@result:replace_error(_pipe@1, Error)
        end,
        fun(Data) ->
            Next_buffer = {buffer,
                gleam@int:max(0, erlang:element(2, Buffer) - To_read),
                <<(erlang:element(3, Buffer))/bitstring, Data/bitstring>>},
            case erlang:element(2, Next_buffer) > 0 of
                true ->
                    read_data(Socket, Transport, Next_buffer, Error);

                false ->
                    {ok, erlang:element(3, Next_buffer)}
            end
        end
    ).

-file("src/mist/internal/http.gleam", 239).
?DOC(false).
-spec version_to_string(http_version()) -> binary().
version_to_string(Version) ->
    case Version of
        http1 ->
            <<"1.0"/utf8>>;

        http11 ->
            <<"1.1"/utf8>>
    end.

-file("src/mist/internal/http.gleam", 254).
?DOC(false).
-spec decode_http_method(gleam@dynamic:dynamic_()) -> {ok, gleam@http:method()} |
    {error, nil}.
decode_http_method(Value) ->
    Options = erlang:binary_to_atom(<<"OPTIONS"/utf8>>),
    Get = erlang:binary_to_atom(<<"GET"/utf8>>),
    Head = erlang:binary_to_atom(<<"HEAD"/utf8>>),
    Post = erlang:binary_to_atom(<<"POST"/utf8>>),
    Put = erlang:binary_to_atom(<<"PUT"/utf8>>),
    Delete = erlang:binary_to_atom(<<"DELETE"/utf8>>),
    Trace = erlang:binary_to_atom(<<"TRACE"/utf8>>),
    case mist_ffi:decode_atom(Value) of
        {ok, Method} when Method =:= Options ->
            {ok, options};

        {ok, Method@1} when Method@1 =:= Get ->
            {ok, get};

        {ok, Method@2} when Method@2 =:= Head ->
            {ok, head};

        {ok, Method@3} when Method@3 =:= Post ->
            {ok, post};

        {ok, Method@4} when Method@4 =:= Put ->
            {ok, put};

        {ok, Method@5} when Method@5 =:= Delete ->
            {ok, delete};

        {ok, Method@6} when Method@6 =:= Trace ->
            {ok, trace};

        _ ->
            case gleam@dynamic@decode:run(
                Value,
                {decoder, fun gleam@dynamic@decode:decode_string/1}
            ) of
                {ok, Str} ->
                    gleam@http:parse_method(Str);

                _ ->
                    {error, nil}
            end
    end.

-file("src/mist/internal/http.gleam", 538).
?DOC(false).
-spec add_date_header(gleam@http@response:response(KUB)) -> gleam@http@response:response(KUB).
add_date_header(Resp) ->
    case gleam@http@response:get_header(Resp, <<"date"/utf8>>) of
        {error, _} ->
            gleam@http@response:set_header(
                Resp,
                <<"date"/utf8>>,
                mist@internal@clock:get_date()
            );

        _ ->
            Resp
    end.

-file("src/mist/internal/http.gleam", 545).
?DOC(false).
-spec connection_close(gleam@http@response:response(KUE)) -> gleam@http@response:response(KUE).
connection_close(Resp) ->
    gleam@http@response:set_header(
        Resp,
        <<"connection"/utf8>>,
        <<"close"/utf8>>
    ).

-file("src/mist/internal/http.gleam", 549).
?DOC(false).
-spec keep_alive(gleam@http@response:response(KUH)) -> gleam@http@response:response(KUH).
keep_alive(Resp) ->
    gleam@http@response:set_header(
        Resp,
        <<"connection"/utf8>>,
        <<"keep-alive"/utf8>>
    ).

-file("src/mist/internal/http.gleam", 553).
?DOC(false).
-spec maybe_keep_alive(gleam@http@response:response(KUK)) -> gleam@http@response:response(KUK).
maybe_keep_alive(Resp) ->
    case gleam@http@response:get_header(Resp, <<"connection"/utf8>>) of
        {ok, _} ->
            Resp;

        _ ->
            gleam@http@response:set_header(
                Resp,
                <<"connection"/utf8>>,
                <<"keep-alive"/utf8>>
            )
    end.

-file("src/mist/internal/http.gleam", 560).
?DOC(false).
-spec maybe_drop_body(
    gleam@http@response:response(gleam@bytes_tree:bytes_tree()),
    boolean()
) -> gleam@http@response:response(gleam@bytes_tree:bytes_tree()).
maybe_drop_body(Resp, Is_head_request) ->
    case Is_head_request of
        true ->
            gleam@http@response:set_body(Resp, gleam@bytes_tree:new());

        false ->
            Resp
    end.

-file("src/mist/internal/http.gleam", 570).
?DOC(false).
-spec add_content_length(boolean(), integer()) -> fun((gleam@http@response:response(KUP)) -> gleam@http@response:response(KUP)).
add_content_length(When, Length) ->
    fun(Resp) -> case When of
            true ->
                {_, Headers} = begin
                    _pipe = erlang:element(3, Resp),
                    _pipe@1 = gleam@list:key_pop(
                        _pipe,
                        <<"content-length"/utf8>>
                    ),
                    gleam@result:lazy_unwrap(
                        _pipe@1,
                        fun() -> {<<""/utf8>>, erlang:element(3, Resp)} end
                    )
                end,
                _pipe@2 = {response,
                    erlang:element(2, Resp),
                    Headers,
                    erlang:element(4, Resp)},
                gleam@http@response:set_header(
                    _pipe@2,
                    <<"content-length"/utf8>>,
                    erlang:integer_to_binary(Length)
                );

            false ->
                Resp
        end end.

-file("src/mist/internal/http.gleam", 590).
?DOC(false).
-spec add_default_headers(
    gleam@http@response:response(gleam@bytes_tree:bytes_tree()),
    boolean()
) -> gleam@http@response:response(gleam@bytes_tree:bytes_tree()).
add_default_headers(Resp, Is_head_response) ->
    Body_size = erlang:iolist_size(erlang:element(4, Resp)),
    {_, Headers} = begin
        _pipe = erlang:element(3, Resp),
        _pipe@1 = gleam@list:key_pop(_pipe, <<"content-length"/utf8>>),
        gleam@result:lazy_unwrap(
            _pipe@1,
            fun() -> {<<""/utf8>>, erlang:element(3, Resp)} end
        )
    end,
    Resp@1 = case {erlang:element(2, Resp), Body_size} of
        {N, _} when (N >= 100) andalso (N =< 199) ->
            {response,
                erlang:element(2, Resp),
                Headers,
                erlang:element(4, Resp)};

        {N@1, _} when N@1 =:= 204 ->
            {response,
                erlang:element(2, Resp),
                Headers,
                erlang:element(4, Resp)};

        {N@2, 0} when N@2 =:= 304 ->
            Resp;

        {_, 0} when Is_head_response =:= true ->
            Resp;

        {_, _} ->
            gleam@http@response:set_header(
                Resp,
                <<"content-length"/utf8>>,
                erlang:integer_to_binary(Body_size)
            )
    end,
    _pipe@2 = Resp@1,
    _pipe@3 = add_date_header(_pipe@2),
    maybe_drop_body(_pipe@3, Is_head_response).

-file("src/mist/internal/http.gleam", 619).
?DOC(false).
-spec is_continue(gleam@http@request:request(connection())) -> boolean().
is_continue(Req) ->
    _pipe = erlang:element(3, Req),
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(Tup) ->
            (gleam@pair:first(Tup) =:= <<"expect"/utf8>>) andalso (gleam@pair:second(
                Tup
            )
            =:= <<"100-continue"/utf8>>)
        end
    ),
    gleam@result:is_ok(_pipe@1).

-file("src/mist/internal/http.gleam", 627).
?DOC(false).
-spec handle_continue(gleam@http@request:request(connection())) -> {ok, nil} |
    {error, decode_error()}.
handle_continue(Req) ->
    case is_continue(Req) of
        true ->
            _pipe = gleam@http@response:new(100),
            _pipe@1 = gleam@http@response:set_body(
                _pipe,
                gleam@bytes_tree:new()
            ),
            _pipe@2 = mist@internal@encoder:to_bytes_tree(
                _pipe@1,
                <<"1.1"/utf8>>
            ),
            _pipe@3 = glisten@transport:send(
                erlang:element(4, erlang:element(4, Req)),
                erlang:element(3, erlang:element(4, Req)),
                _pipe@2
            ),
            gleam@result:replace_error(_pipe@3, malformed_request);

        false ->
            {ok, nil}
    end.

-file("src/mist/internal/http.gleam", 83).
?DOC(false).
-spec parse_headers(
    bitstring(),
    glisten@socket:socket(),
    glisten@transport:transport(),
    gleam@dict:dict(binary(), binary())
) -> {ok, {gleam@dict:dict(binary(), binary()), bitstring()}} |
    {error, decode_error()}.
parse_headers(Bs, Socket, Transport, Headers) ->
    case mist_ffi:decode_packet(httph_bin, Bs, []) of
        {ok, {binary_data, {http_header, _, _, Field, Value}, Rest}} ->
            Field@1 = from_header(Field),
            Value@2 = case gleam@bit_array:to_string(Value) of
                {ok, Value@1} -> Value@1;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"parse_headers"/utf8>>,
                                line => 92,
                                value => _assert_fail,
                                start => 2172,
                                'end' => 2221,
                                pattern_start => 2183,
                                pattern_end => 2192})
            end,
            _pipe = Headers,
            _pipe@1 = gleam@dict:insert(_pipe, Field@1, Value@2),
            parse_headers(Rest, Socket, Transport, _pipe@1);

        {ok, {end_of_headers, Rest@1}} ->
            {ok, {Headers, Rest@1}};

        {ok, {more_data, Size}} ->
            Amount_to_read = gleam@option:unwrap(Size, 0),
            gleam@result:'try'(
                read_data(
                    Socket,
                    Transport,
                    {buffer, Amount_to_read, Bs},
                    unknown_header
                ),
                fun(Next) -> parse_headers(Next, Socket, Transport, Headers) end
            );

        _ ->
            {error, unknown_header}
    end.

-file("src/mist/internal/http.gleam", 648).
?DOC(false).
-spec crypto_hash(sha_hash(), binary()) -> binary().
crypto_hash(Hash, Data) ->
    crypto:hash(Hash, Data).

-file("src/mist/internal/http.gleam", 651).
?DOC(false).
-spec base64_encode(binary()) -> binary().
base64_encode(Data) ->
    base64:encode(Data).

-file("src/mist/internal/http.gleam", 145).
?DOC(false).
-spec parse_chunk(bitstring()) -> chunk().
parse_chunk(String) ->
    case binary:split(String, <<"\r\n"/utf8>>) of
        [<<"0"/utf8>>, _] ->
            complete;

        [Chunk_size, Rest] ->
            Chunk_size@2 = case gleam@bit_array:to_string(Chunk_size) of
                {ok, Chunk_size@1} -> Chunk_size@1;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"parse_chunk"/utf8>>,
                                line => 149,
                                value => _assert_fail,
                                start => 3646,
                                'end' => 3705,
                                pattern_start => 3657,
                                pattern_end => 3671})
            end,
            case gleam@int:base_parse(Chunk_size@2, 16) of
                {ok, Size} ->
                    Size@1 = Size * 8,
                    case Rest of
                        <<Next_chunk:Size@1/bitstring,
                            13/integer,
                            10/integer,
                            Rest@1/bitstring>> ->
                            {chunk,
                                Next_chunk,
                                mist@internal@buffer:new(Rest@1)};

                        _ ->
                            {chunk, <<>>, mist@internal@buffer:new(String)}
                    end;

                {error, _} ->
                    {chunk, <<>>, mist@internal@buffer:new(String)}
            end;

        _ ->
            {chunk, <<>>, mist@internal@buffer:new(String)}
    end.

-file("src/mist/internal/http.gleam", 281).
?DOC(false).
-spec parse_request(bitstring(), connection()) -> {ok, parsed_request()} |
    {error, decode_error()}.
parse_request(Bs, Conn) ->
    case mist_ffi:decode_packet(http_bin, Bs, []) of
        {ok,
            {binary_data,
                {http_request, Http_method, {abs_path, Path}, Version},
                Rest}} ->
            gleam@result:'try'(
                begin
                    _pipe = Http_method,
                    _pipe@1 = decode_http_method(_pipe),
                    gleam@result:replace_error(_pipe@1, unknown_method)
                end,
                fun(Method) ->
                    gleam@result:'try'(
                        parse_headers(
                            Rest,
                            erlang:element(3, Conn),
                            erlang:element(4, Conn),
                            maps:new()
                        ),
                        fun(_use0) ->
                            {Headers, Rest@1} = _use0,
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = Path,
                                    _pipe@3 = gleam@bit_array:to_string(_pipe@2),
                                    gleam@result:replace_error(
                                        _pipe@3,
                                        invalid_path
                                    )
                                end,
                                fun(Path@1) ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@4 = mist_ffi:get_path_and_query(
                                                Path@1
                                            ),
                                            gleam@result:replace_error(
                                                _pipe@4,
                                                invalid_path
                                            )
                                        end,
                                        fun(_use0@1) ->
                                            {Path@2, Query} = _use0@1,
                                            Scheme = case erlang:element(
                                                4,
                                                Conn
                                            ) of
                                                ssl ->
                                                    https;

                                                tcp ->
                                                    http
                                            end,
                                            gleam@result:'try'(
                                                begin
                                                    _pipe@5 = gleam_stdlib:map_get(
                                                        Headers,
                                                        <<"host"/utf8>>
                                                    ),
                                                    gleam@result:replace_error(
                                                        _pipe@5,
                                                        no_host_header
                                                    )
                                                end,
                                                fun(Host_header) ->
                                                    {Hostname, Port} = begin
                                                        _pipe@6 = Host_header,
                                                        _pipe@7 = gleam@string:split_once(
                                                            _pipe@6,
                                                            <<":"/utf8>>
                                                        ),
                                                        gleam@result:unwrap(
                                                            _pipe@7,
                                                            {Host_header,
                                                                <<""/utf8>>}
                                                        )
                                                    end,
                                                    Port@1 = begin
                                                        _pipe@8 = gleam_stdlib:parse_int(
                                                            Port
                                                        ),
                                                        _pipe@9 = gleam@result:map_error(
                                                            _pipe@8,
                                                            fun(_) ->
                                                                case Scheme of
                                                                    https ->
                                                                        443;

                                                                    http ->
                                                                        80
                                                                end
                                                            end
                                                        ),
                                                        gleam@result:unwrap_both(
                                                            _pipe@9
                                                        )
                                                    end,
                                                    Req = {request,
                                                        Method,
                                                        maps:to_list(Headers),
                                                        {connection,
                                                            {initial, Rest@1},
                                                            erlang:element(
                                                                3,
                                                                Conn
                                                            ),
                                                            erlang:element(
                                                                4,
                                                                Conn
                                                            )},
                                                        Scheme,
                                                        Hostname,
                                                        {some, Port@1},
                                                        Path@2,
                                                        gleam@option:from_result(
                                                            Query
                                                        )},
                                                    case Version of
                                                        {1, 0} ->
                                                            {ok,
                                                                {http1_request,
                                                                    Req,
                                                                    http1}};

                                                        {1, 1} ->
                                                            {ok,
                                                                {http1_request,
                                                                    Req,
                                                                    http11}};

                                                        _ ->
                                                            {error,
                                                                invalid_http_version}
                                                    end
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            );

        {ok,
            {http2_upgrade,
                <<13/integer,
                    10/integer,
                    83/integer,
                    77/integer,
                    13/integer,
                    10/integer,
                    13/integer,
                    10/integer,
                    Data/bitstring>>}} ->
            {ok, {upgrade, Data}};

        {ok, {more_data, Size}} ->
            Amount_to_read = gleam@option:unwrap(Size, 0),
            gleam@result:'try'(
                read_data(
                    erlang:element(3, Conn),
                    erlang:element(4, Conn),
                    {buffer, Amount_to_read, Bs},
                    malformed_request
                ),
                fun(Next) -> parse_request(Next, Conn) end
            );

        _ ->
            {error, discard_packet}
    end.

-file("src/mist/internal/http.gleam", 175).
?DOC(false).
-spec read_chunk(
    glisten@socket:socket(),
    glisten@transport:transport(),
    mist@internal@buffer:buffer(),
    gleam@bytes_tree:bytes_tree()
) -> {ok, gleam@bytes_tree:bytes_tree()} | {error, decode_error()}.
read_chunk(Socket, Transport, Buffer, Body) ->
    case {erlang:element(3, Buffer),
        mist_ffi:binary_match(
            erlang:element(3, Buffer),
            <<13/integer, 10/integer>>
        )} of
        {_, {ok, {Offset, _}}} ->
            {Chunk@1, Rest@1} = case erlang:element(3, Buffer) of
                <<Chunk:Offset/binary, _/integer, _/integer, Rest/binary>> -> {
                Chunk,
                    Rest};
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"read_chunk"/utf8>>,
                                line => 183,
                                value => _assert_fail,
                                start => 4529,
                                'end' => 4662,
                                pattern_start => 4540,
                                pattern_end => 4648})
            end,
            gleam@result:'try'(
                begin
                    _pipe = Chunk@1,
                    _pipe@1 = gleam@bit_array:to_string(_pipe),
                    _pipe@2 = gleam@result:map(
                        _pipe@1,
                        fun unicode:characters_to_list/1
                    ),
                    gleam@result:replace_error(_pipe@2, invalid_body)
                end,
                fun(Chunk_size) ->
                    gleam@result:'try'(
                        begin
                            _pipe@3 = mist_ffi:string_to_int(Chunk_size, 16),
                            gleam@result:replace_error(_pipe@3, invalid_body)
                        end,
                        fun(Size) -> case Size of
                                0 ->
                                    {ok, Body};

                                Size@1 ->
                                    case Rest@1 of
                                        <<Next_chunk:Size@1/binary,
                                            13/integer,
                                            10/integer,
                                            Rest@2/binary>> ->
                                            read_chunk(
                                                Socket,
                                                Transport,
                                                {buffer, 0, Rest@2},
                                                gleam@bytes_tree:append(
                                                    Body,
                                                    Next_chunk
                                                )
                                            );

                                        _ ->
                                            gleam@result:'try'(
                                                read_data(
                                                    Socket,
                                                    Transport,
                                                    {buffer,
                                                        0,
                                                        erlang:element(
                                                            3,
                                                            Buffer
                                                        )},
                                                    invalid_body
                                                ),
                                                fun(Next) ->
                                                    read_chunk(
                                                        Socket,
                                                        Transport,
                                                        {buffer, 0, Next},
                                                        Body
                                                    )
                                                end
                                            )
                                    end
                            end end
                    )
                end
            );

        {<<>> = Data, _} ->
            gleam@result:'try'(
                read_data(Socket, Transport, {buffer, 0, Data}, invalid_body),
                fun(Next@1) ->
                    read_chunk(Socket, Transport, {buffer, 0, Next@1}, Body)
                end
            );

        {Data, {error, nil}} ->
            gleam@result:'try'(
                read_data(Socket, Transport, {buffer, 0, Data}, invalid_body),
                fun(Next@1) ->
                    read_chunk(Socket, Transport, {buffer, 0, Next@1}, Body)
                end
            )
    end.

-file("src/mist/internal/http.gleam", 383).
?DOC(false).
-spec read_body(gleam@http@request:request(connection())) -> {ok,
        gleam@http@request:request(bitstring())} |
    {error, decode_error()}.
read_body(Req) ->
    Transport = case erlang:element(5, Req) of
        https ->
            ssl;

        http ->
            tcp
    end,
    case {gleam@http@request:get_header(Req, <<"transfer-encoding"/utf8>>),
        erlang:element(2, erlang:element(4, Req))} of
        {{ok, <<"chunked"/utf8>>}, {initial, Rest}} ->
            gleam@result:'try'(
                handle_continue(Req),
                fun(_) ->
                    gleam@result:'try'(
                        read_chunk(
                            erlang:element(3, erlang:element(4, Req)),
                            Transport,
                            {buffer, 0, Rest},
                            gleam@bytes_tree:new()
                        ),
                        fun(Chunk) ->
                            {ok,
                                gleam@http@request:set_body(
                                    Req,
                                    erlang:list_to_bitstring(Chunk)
                                )}
                        end
                    )
                end
            );

        {_, {initial, Rest@1}} ->
            gleam@result:'try'(
                handle_continue(Req),
                fun(_) ->
                    Body_size = begin
                        _pipe = erlang:element(3, Req),
                        _pipe@1 = gleam@list:find(
                            _pipe,
                            fun(Tup) ->
                                gleam@pair:first(Tup) =:= <<"content-length"/utf8>>
                            end
                        ),
                        _pipe@2 = gleam@result:map(
                            _pipe@1,
                            fun gleam@pair:second/1
                        ),
                        _pipe@3 = gleam@result:'try'(
                            _pipe@2,
                            fun gleam_stdlib:parse_int/1
                        ),
                        gleam@result:unwrap(_pipe@3, 0)
                    end,
                    Remaining = Body_size - erlang:byte_size(Rest@1),
                    _pipe@4 = case {Body_size, Remaining} of
                        {0, 0} ->
                            {ok, <<>>};

                        {0, _} ->
                            {ok, Rest@1};

                        {_, 0} ->
                            {ok, Rest@1};

                        {_, _} ->
                            read_data(
                                erlang:element(3, erlang:element(4, Req)),
                                Transport,
                                {buffer, Remaining, Rest@1},
                                invalid_body
                            )
                    end,
                    _pipe@5 = gleam@result:map(
                        _pipe@4,
                        fun(_capture) ->
                            gleam@http@request:set_body(Req, _capture)
                        end
                    ),
                    gleam@result:replace_error(_pipe@5, invalid_body)
                end
            );

        {_, {stream, Selector, Data, Remaining@1, Attempts}} when Remaining@1 > 0 ->
            Res = begin
                _pipe@6 = Selector,
                _pipe@7 = gleam_erlang_ffi:select(_pipe@6, 1000),
                gleam@result:replace_error(_pipe@7, invalid_body)
            end,
            gleam@result:'try'(
                Res,
                fun(Next) ->
                    Got = erlang:byte_size(Next),
                    Left = gleam@int:max(Remaining@1 - Got, 0),
                    New_data = gleam@bit_array:append(Data, Next),
                    case Left of
                        0 ->
                            {ok, gleam@http@request:set_body(Req, New_data)};

                        _ ->
                            read_body(
                                gleam@http@request:set_body(
                                    Req,
                                    begin
                                        _record = erlang:element(4, Req),
                                        {connection,
                                            {stream,
                                                Selector,
                                                New_data,
                                                Left,
                                                Attempts + 1},
                                            erlang:element(3, _record),
                                            erlang:element(4, _record)}
                                    end
                                )
                            )
                    end
                end
            );

        {_, {stream, _, Data@1, _, _}} ->
            {ok, gleam@http@request:set_body(Req, Data@1)}
    end.

-file("src/mist/internal/http.gleam", 468).
?DOC(false).
-spec parse_websocket_key(binary()) -> binary().
parse_websocket_key(Key) ->
    _pipe = Key,
    _pipe@1 = gleam@string:append(
        _pipe,
        <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"/utf8>>
    ),
    _pipe@2 = crypto:hash(sha, _pipe@1),
    base64:encode(_pipe@2).

-file("src/mist/internal/http.gleam", 475).
?DOC(false).
-spec upgrade_socket(gleam@http@request:request(connection()), list(binary())) -> {ok,
        gleam@http@response:response(gleam@bytes_tree:bytes_tree())} |
    {error, gleam@http@request:request(connection())}.
upgrade_socket(Req, Extensions) ->
    gleam@result:'try'(
        begin
            _pipe = gleam@http@request:get_header(Req, <<"upgrade"/utf8>>),
            gleam@result:replace_error(_pipe, Req)
        end,
        fun(_) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = gleam@http@request:get_header(
                        Req,
                        <<"sec-websocket-key"/utf8>>
                    ),
                    gleam@result:replace_error(_pipe@1, Req)
                end,
                fun(Key) ->
                    gleam@result:'try'(
                        begin
                            _pipe@2 = gleam@http@request:get_header(
                                Req,
                                <<"sec-websocket-version"/utf8>>
                            ),
                            gleam@result:replace_error(_pipe@2, Req)
                        end,
                        fun(_) ->
                            Permessage_deflate = gramps@websocket:has_deflate(
                                Extensions
                            ),
                            Accept_key = parse_websocket_key(Key),
                            Resp = begin
                                _pipe@3 = gleam@http@response:new(101),
                                _pipe@4 = gleam@http@response:set_body(
                                    _pipe@3,
                                    gleam@bytes_tree:new()
                                ),
                                _pipe@5 = gleam@http@response:prepend_header(
                                    _pipe@4,
                                    <<"upgrade"/utf8>>,
                                    <<"websocket"/utf8>>
                                ),
                                _pipe@6 = gleam@http@response:prepend_header(
                                    _pipe@5,
                                    <<"connection"/utf8>>,
                                    <<"Upgrade"/utf8>>
                                ),
                                gleam@http@response:prepend_header(
                                    _pipe@6,
                                    <<"sec-websocket-accept"/utf8>>,
                                    Accept_key
                                )
                            end,
                            case Permessage_deflate of
                                true ->
                                    {ok,
                                        gleam@http@response:prepend_header(
                                            Resp,
                                            <<"sec-websocket-extensions"/utf8>>,
                                            <<"permessage-deflate"/utf8>>
                                        )};

                                false ->
                                    {ok, Resp}
                            end
                        end
                    )
                end
            )
        end
    ).

-file("src/mist/internal/http.gleam", 515).
?DOC(false).
-spec upgrade(
    glisten@socket:socket(),
    glisten@transport:transport(),
    list(binary()),
    gleam@http@request:request(connection())
) -> {ok, nil} | {error, nil}.
upgrade(Socket, Transport, Extensions, Req) ->
    gleam@result:'try'(
        begin
            _pipe = upgrade_socket(Req, Extensions),
            gleam@result:replace_error(_pipe, nil)
        end,
        fun(Resp) ->
            gleam@result:'try'(
                begin
                    _pipe@1 = Resp,
                    _pipe@2 = add_default_headers(
                        _pipe@1,
                        erlang:element(2, Req) =:= head
                    ),
                    _pipe@3 = maybe_keep_alive(_pipe@2),
                    _pipe@4 = mist@internal@encoder:to_bytes_tree(
                        _pipe@3,
                        <<"1.1"/utf8>>
                    ),
                    _pipe@5 = glisten@transport:send(Transport, Socket, _pipe@4),
                    gleam@result:replace_error(_pipe@5, nil)
                end,
                fun(_) -> {ok, nil} end
            )
        end
    ).
