-module(glisten@tcp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/glisten/tcp.gleam").
-export([controlling_process/2, accept_timeout/2, accept/1, receive_timeout/3, 'receive'/2, send/2, socket_info/1, close/1, do_shutdown/2, shutdown/1, set_opts/2, listen/2, handshake/1, negotiated_protocol/1, peername/1, get_socket_opts/2, sockname/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/glisten/tcp.gleam", 11).
-spec controlling_process(glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok,
        nil} |
    {error, gleam@erlang@atom:atom_()}.
controlling_process(Socket, Pid) ->
    glisten_tcp_ffi:controlling_process(Socket, Pid).

-file("src/glisten/tcp.gleam", 20).
-spec accept_timeout(glisten@socket:listen_socket(), integer()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept_timeout(Socket, Timeout) ->
    gen_tcp:accept(Socket, Timeout).

-file("src/glisten/tcp.gleam", 26).
-spec accept(glisten@socket:listen_socket()) -> {ok, glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept(Socket) ->
    gen_tcp:accept(Socket).

-file("src/glisten/tcp.gleam", 29).
-spec receive_timeout(glisten@socket:socket(), integer(), integer()) -> {ok,
        bitstring()} |
    {error, glisten@socket:socket_reason()}.
receive_timeout(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

-file("src/glisten/tcp.gleam", 36).
-spec 'receive'(glisten@socket:socket(), integer()) -> {ok, bitstring()} |
    {error, glisten@socket:socket_reason()}.
'receive'(Socket, Length) ->
    gen_tcp:recv(Socket, Length).

-file("src/glisten/tcp.gleam", 39).
-spec send(glisten@socket:socket(), gleam@bytes_tree:bytes_tree()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
send(Socket, Packet) ->
    glisten_tcp_ffi:send(Socket, Packet).

-file("src/glisten/tcp.gleam", 42).
-spec socket_info(glisten@socket:socket()) -> gleam@dict:dict(any(), any()).
socket_info(Socket) ->
    socket:info(Socket).

-file("src/glisten/tcp.gleam", 45).
-spec close(any()) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
close(Socket) ->
    glisten_tcp_ffi:close(Socket).

-file("src/glisten/tcp.gleam", 48).
-spec do_shutdown(glisten@socket:socket(), gleam@erlang@atom:atom_()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
do_shutdown(Socket, Write) ->
    glisten_tcp_ffi:shutdown(Socket, Write).

-file("src/glisten/tcp.gleam", 50).
-spec shutdown(glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
shutdown(Socket) ->
    glisten_tcp_ffi:shutdown(Socket, erlang:binary_to_atom(<<"write"/utf8>>)).

-file("src/glisten/tcp.gleam", 61).
?DOC(" Update the optons for a socket (mutates the socket)\n").
-spec set_opts(
    glisten@socket:socket(),
    list(glisten@socket@options:tcp_option())
) -> {ok, nil} | {error, nil}.
set_opts(Socket, Opts) ->
    _pipe = Opts,
    _pipe@1 = glisten@socket@options:to_dict(_pipe),
    _pipe@2 = maps:to_list(_pipe@1),
    _pipe@3 = gleam@list:map(_pipe@2, fun gleam@function:identity/1),
    glisten_tcp_ffi:set_opts(Socket, _pipe@3).

-file("src/glisten/tcp.gleam", 70).
?DOC(" Start listening over TCP on a port with the given options\n").
-spec listen(integer(), list(glisten@socket@options:tcp_option())) -> {ok,
        glisten@socket:listen_socket()} |
    {error, glisten@socket:socket_reason()}.
listen(Port, Opts) ->
    _pipe = Opts,
    _pipe@1 = glisten@socket@options:merge_with_defaults(_pipe),
    gen_tcp:listen(Port, _pipe@1).

-file("src/glisten/tcp.gleam", 79).
-spec handshake(glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
    {error, nil}.
handshake(Socket) ->
    {ok, Socket}.

-file("src/glisten/tcp.gleam", 84).
-spec negotiated_protocol(glisten@socket:socket()) -> any().
negotiated_protocol(Socket) ->
    tcp:negotiated_protocol(Socket).

-file("src/glisten/tcp.gleam", 87).
-spec peername(glisten@socket:socket()) -> {ok,
        {gleam@dynamic:dynamic_(), integer()}} |
    {error, nil}.
peername(Socket) ->
    inet:peername(Socket).

-file("src/glisten/tcp.gleam", 90).
-spec get_socket_opts(glisten@socket:socket(), list(gleam@erlang@atom:atom_())) -> {ok,
        list({gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()})} |
    {error, nil}.
get_socket_opts(Socket, Opts) ->
    inet:getopts(Socket, Opts).

-file("src/glisten/tcp.gleam", 96).
-spec sockname(glisten@socket:listen_socket()) -> {ok,
        {gleam@dynamic:dynamic_(), integer()}} |
    {error, glisten@socket:socket_reason()}.
sockname(Socket) ->
    inet:sockname(Socket).
