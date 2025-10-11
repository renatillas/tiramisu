-module(glisten@transport).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/glisten/transport.gleam").
-export([controlling_process/3, listen/3, accept_timeout/3, accept/2, handshake/2, receive_timeout/4, 'receive'/3, send/3, close/2, shutdown/2, set_opts/3, negotiated_protocol/2, decode_ip/0, peername/2, socket_info/1, get_socket_opts/3, set_buffer_size/2, sockname/2]).
-export_type([transport/0]).

-type transport() :: tcp | ssl.

-file("src/glisten/transport.gleam", 18).
-spec controlling_process(
    transport(),
    glisten@socket:socket(),
    gleam@erlang@process:pid_()
) -> {ok, nil} | {error, gleam@erlang@atom:atom_()}.
controlling_process(Transport, Socket, Pid) ->
    case Transport of
        tcp ->
            glisten_tcp_ffi:controlling_process(Socket, Pid);

        ssl ->
            glisten_ssl_ffi:controlling_process(Socket, Pid)
    end.

-file("src/glisten/transport.gleam", 29).
-spec listen(transport(), integer(), list(glisten@socket@options:tcp_option())) -> {ok,
        glisten@socket:listen_socket()} |
    {error, glisten@socket:socket_reason()}.
listen(Transport, Port, Opts) ->
    case Transport of
        tcp ->
            glisten@tcp:listen(Port, Opts);

        ssl ->
            glisten@ssl:listen(Port, Opts)
    end.

-file("src/glisten/transport.gleam", 40).
-spec accept_timeout(transport(), glisten@socket:listen_socket(), integer()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept_timeout(Transport, Socket, Timeout) ->
    case Transport of
        tcp ->
            gen_tcp:accept(Socket, Timeout);

        ssl ->
            ssl:transport_accept(Socket, Timeout)
    end.

-file("src/glisten/transport.gleam", 51).
-spec accept(transport(), glisten@socket:listen_socket()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept(Transport, Socket) ->
    case Transport of
        tcp ->
            gen_tcp:accept(Socket);

        ssl ->
            ssl:transport_accept(Socket)
    end.

-file("src/glisten/transport.gleam", 61).
-spec handshake(transport(), glisten@socket:socket()) -> {ok,
        glisten@socket:socket()} |
    {error, nil}.
handshake(Transport, Socket) ->
    case Transport of
        tcp ->
            glisten@tcp:handshake(Socket);

        ssl ->
            ssl:handshake(Socket)
    end.

-file("src/glisten/transport.gleam", 68).
-spec receive_timeout(
    transport(),
    glisten@socket:socket(),
    integer(),
    integer()
) -> {ok, bitstring()} | {error, glisten@socket:socket_reason()}.
receive_timeout(Transport, Socket, Amount, Timeout) ->
    case Transport of
        tcp ->
            gen_tcp:recv(Socket, Amount, Timeout);

        ssl ->
            ssl:recv(Socket, Amount, Timeout)
    end.

-file("src/glisten/transport.gleam", 80).
-spec 'receive'(transport(), glisten@socket:socket(), integer()) -> {ok,
        bitstring()} |
    {error, glisten@socket:socket_reason()}.
'receive'(Transport, Socket, Amount) ->
    case Transport of
        tcp ->
            gen_tcp:recv(Socket, Amount);

        ssl ->
            ssl:recv(Socket, Amount)
    end.

-file("src/glisten/transport.gleam", 91).
-spec send(transport(), glisten@socket:socket(), gleam@bytes_tree:bytes_tree()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
send(Transport, Socket, Data) ->
    case Transport of
        tcp ->
            glisten_tcp_ffi:send(Socket, Data);

        ssl ->
            glisten_ssl_ffi:send(Socket, Data)
    end.

-file("src/glisten/transport.gleam", 102).
-spec close(transport(), glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
close(Transport, Socket) ->
    case Transport of
        tcp ->
            glisten_tcp_ffi:close(Socket);

        ssl ->
            glisten_ssl_ffi:close(Socket)
    end.

-file("src/glisten/transport.gleam", 109).
-spec shutdown(transport(), glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
shutdown(Transport, Socket) ->
    case Transport of
        tcp ->
            glisten@tcp:shutdown(Socket);

        ssl ->
            glisten@ssl:shutdown(Socket)
    end.

-file("src/glisten/transport.gleam", 119).
-spec set_opts(
    transport(),
    glisten@socket:socket(),
    list(glisten@socket@options:tcp_option())
) -> {ok, nil} | {error, nil}.
set_opts(Transport, Socket, Opts) ->
    case Transport of
        tcp ->
            glisten@tcp:set_opts(Socket, Opts);

        ssl ->
            glisten@ssl:set_opts(Socket, Opts)
    end.

-file("src/glisten/transport.gleam", 130).
-spec negotiated_protocol(transport(), glisten@socket:socket()) -> {ok,
        binary()} |
    {error, binary()}.
negotiated_protocol(Transport, Socket) ->
    case Transport of
        tcp ->
            {error, <<"Can't negotiate protocol on tcp"/utf8>>};

        ssl ->
            glisten_ssl_ffi:negotiated_protocol(Socket)
    end.

-file("src/glisten/transport.gleam", 140).
-spec decode_ipv4() -> gleam@dynamic@decode:decoder(glisten@socket@options:ip_address()).
decode_ipv4() ->
    gleam@dynamic@decode:field(
        0,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        fun(A) ->
            gleam@dynamic@decode:field(
                1,
                {decoder, fun gleam@dynamic@decode:decode_int/1},
                fun(B) ->
                    gleam@dynamic@decode:field(
                        2,
                        {decoder, fun gleam@dynamic@decode:decode_int/1},
                        fun(C) ->
                            gleam@dynamic@decode:field(
                                3,
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(D) ->
                                    gleam@dynamic@decode:success(
                                        {ip_v4, A, B, C, D}
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glisten/transport.gleam", 148).
-spec decode_ipv6() -> gleam@dynamic@decode:decoder(glisten@socket@options:ip_address()).
decode_ipv6() ->
    gleam@dynamic@decode:field(
        0,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        fun(A) ->
            gleam@dynamic@decode:field(
                1,
                {decoder, fun gleam@dynamic@decode:decode_int/1},
                fun(B) ->
                    gleam@dynamic@decode:field(
                        2,
                        {decoder, fun gleam@dynamic@decode:decode_int/1},
                        fun(C) ->
                            gleam@dynamic@decode:field(
                                3,
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(D) ->
                                    gleam@dynamic@decode:field(
                                        4,
                                        {decoder,
                                            fun gleam@dynamic@decode:decode_int/1},
                                        fun(E) ->
                                            gleam@dynamic@decode:field(
                                                5,
                                                {decoder,
                                                    fun gleam@dynamic@decode:decode_int/1},
                                                fun(F) ->
                                                    gleam@dynamic@decode:field(
                                                        6,
                                                        {decoder,
                                                            fun gleam@dynamic@decode:decode_int/1},
                                                        fun(G) ->
                                                            gleam@dynamic@decode:field(
                                                                7,
                                                                {decoder,
                                                                    fun gleam@dynamic@decode:decode_int/1},
                                                                fun(H) ->
                                                                    case {A,
                                                                        B,
                                                                        C,
                                                                        D,
                                                                        E,
                                                                        F,
                                                                        G,
                                                                        H} of
                                                                        {0,
                                                                            0,
                                                                            0,
                                                                            0,
                                                                            0,
                                                                            65535,
                                                                            A@1,
                                                                            B@1} ->
                                                                            {A@2,
                                                                                B@2,
                                                                                C@1,
                                                                                D@1} = inet:ipv4_mapped_ipv6_address(
                                                                                {A@1,
                                                                                    B@1,
                                                                                    C,
                                                                                    D,
                                                                                    E,
                                                                                    F,
                                                                                    G,
                                                                                    H}
                                                                            ),
                                                                            gleam@dynamic@decode:success(
                                                                                {ip_v4,
                                                                                    A@2,
                                                                                    B@2,
                                                                                    C@1,
                                                                                    D@1}
                                                                            );

                                                                        {_,
                                                                            _,
                                                                            _,
                                                                            _,
                                                                            _,
                                                                            _,
                                                                            _,
                                                                            _} ->
                                                                            gleam@dynamic@decode:success(
                                                                                {ip_v6,
                                                                                    A,
                                                                                    B,
                                                                                    C,
                                                                                    D,
                                                                                    E,
                                                                                    F,
                                                                                    G,
                                                                                    H}
                                                                            )
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
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glisten/transport.gleam", 168).
-spec decode_ip() -> gleam@dynamic@decode:decoder(glisten@socket@options:ip_address()).
decode_ip() ->
    gleam@dynamic@decode:one_of(decode_ipv6(), [decode_ipv4()]).

-file("src/glisten/transport.gleam", 172).
-spec peername(transport(), glisten@socket:socket()) -> {ok,
        {glisten@socket@options:ip_address(), integer()}} |
    {error, nil}.
peername(Transport, Socket) ->
    _pipe = case Transport of
        tcp ->
            inet:peername(Socket);

        ssl ->
            ssl:peername(Socket)
    end,
    gleam@result:'try'(
        _pipe,
        fun(Pair) ->
            {Ip_address, Port} = Pair,
            _pipe@1 = gleam@dynamic@decode:run(Ip_address, decode_ip()),
            _pipe@2 = gleam@result:map(_pipe@1, fun(Ip) -> {Ip, Port} end),
            gleam@result:replace_error(_pipe@2, nil)
        end
    ).

-file("src/glisten/transport.gleam", 192).
-spec socket_info(glisten@socket:socket()) -> gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()).
socket_info(Socket) ->
    socket:info(Socket).

-file("src/glisten/transport.gleam", 194).
-spec get_socket_opts(
    transport(),
    glisten@socket:socket(),
    list(gleam@erlang@atom:atom_())
) -> {ok, list({gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()})} |
    {error, nil}.
get_socket_opts(Transport, Socket, Opts) ->
    case Transport of
        tcp ->
            inet:getopts(Socket, Opts);

        ssl ->
            ssl:getopts(Socket, Opts)
    end.

-file("src/glisten/transport.gleam", 205).
-spec set_buffer_size(transport(), glisten@socket:socket()) -> {ok, nil} |
    {error, nil}.
set_buffer_size(Transport, Socket) ->
    _pipe = get_socket_opts(
        Transport,
        Socket,
        [erlang:binary_to_atom(<<"recbuf"/utf8>>)]
    ),
    _pipe@3 = gleam@result:'try'(_pipe, fun(P) -> case P of
                [{_, Value}] ->
                    _pipe@1 = Value,
                    _pipe@2 = gleam@dynamic@decode:run(
                        _pipe@1,
                        {decoder, fun gleam@dynamic@decode:decode_int/1}
                    ),
                    gleam@result:replace_error(_pipe@2, nil);

                _ ->
                    {error, nil}
            end end),
    gleam@result:'try'(
        _pipe@3,
        fun(Value@1) -> set_opts(Transport, Socket, [{buffer, Value@1}]) end
    ).

-file("src/glisten/transport.gleam", 221).
-spec sockname(transport(), glisten@socket:listen_socket()) -> {ok,
        {glisten@socket@options:ip_address(), integer()}} |
    {error, glisten@socket:socket_reason()}.
sockname(Transport, Socket) ->
    _pipe = case Transport of
        tcp ->
            inet:sockname(Socket);

        ssl ->
            ssl:sockname(Socket)
    end,
    gleam@result:'try'(
        _pipe,
        fun(Pair) ->
            {Maybe_ip, Port} = Pair,
            _pipe@1 = Maybe_ip,
            _pipe@2 = gleam@dynamic@decode:run(_pipe@1, decode_ip()),
            _pipe@3 = gleam@result:map(_pipe@2, fun(Ip) -> {Ip, Port} end),
            gleam@result:replace_error(_pipe@3, badarg)
        end
    ).
