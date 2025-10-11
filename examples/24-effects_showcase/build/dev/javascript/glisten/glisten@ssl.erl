-module(glisten@ssl).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/glisten/ssl.gleam").
-export([controlling_process/2, accept_timeout/2, accept/1, receive_timeout/3, 'receive'/2, send/2, close/1, do_shutdown/2, shutdown/1, set_opts/2, handshake/1, listen/2, negotiated_protocol/1, peername/1, sockname/1, get_socket_opts/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/glisten/ssl.gleam", 11).
-spec controlling_process(glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok,
        nil} |
    {error, gleam@erlang@atom:atom_()}.
controlling_process(Socket, Pid) ->
    glisten_ssl_ffi:controlling_process(Socket, Pid).

-file("src/glisten/ssl.gleam", 20).
-spec accept_timeout(glisten@socket:listen_socket(), integer()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept_timeout(Socket, Timeout) ->
    ssl:transport_accept(Socket, Timeout).

-file("src/glisten/ssl.gleam", 26).
-spec accept(glisten@socket:listen_socket()) -> {ok, glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept(Socket) ->
    ssl:transport_accept(Socket).

-file("src/glisten/ssl.gleam", 29).
-spec receive_timeout(glisten@socket:socket(), integer(), integer()) -> {ok,
        bitstring()} |
    {error, glisten@socket:socket_reason()}.
receive_timeout(Socket, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout).

-file("src/glisten/ssl.gleam", 36).
-spec 'receive'(glisten@socket:socket(), integer()) -> {ok, bitstring()} |
    {error, glisten@socket:socket_reason()}.
'receive'(Socket, Length) ->
    ssl:recv(Socket, Length).

-file("src/glisten/ssl.gleam", 39).
-spec send(glisten@socket:socket(), gleam@bytes_tree:bytes_tree()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
send(Socket, Packet) ->
    glisten_ssl_ffi:send(Socket, Packet).

-file("src/glisten/ssl.gleam", 42).
-spec close(glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
close(Socket) ->
    glisten_ssl_ffi:close(Socket).

-file("src/glisten/ssl.gleam", 45).
-spec do_shutdown(glisten@socket:socket(), gleam@erlang@atom:atom_()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
do_shutdown(Socket, Write) ->
    glisten_ssl_ffi:shutdown(Socket, Write).

-file("src/glisten/ssl.gleam", 47).
-spec shutdown(glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
shutdown(Socket) ->
    glisten_ssl_ffi:shutdown(Socket, erlang:binary_to_atom(<<"write"/utf8>>)).

-file("src/glisten/ssl.gleam", 58).
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
    glisten_ssl_ffi:set_opts(Socket, _pipe@3).

-file("src/glisten/ssl.gleam", 70).
-spec handshake(glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
    {error, nil}.
handshake(Socket) ->
    ssl:handshake(Socket).

-file("src/glisten/ssl.gleam", 73).
?DOC(" Start listening over TLS on a port with the given options\n").
-spec listen(integer(), list(glisten@socket@options:tcp_option())) -> {ok,
        glisten@socket:listen_socket()} |
    {error, glisten@socket:socket_reason()}.
listen(Port, Options) ->
    _pipe = Options,
    _pipe@1 = glisten@socket@options:merge_with_defaults(_pipe),
    ssl:listen(Port, _pipe@1).

-file("src/glisten/ssl.gleam", 83).
-spec negotiated_protocol(glisten@socket:socket()) -> {ok, binary()} |
    {error, binary()}.
negotiated_protocol(Socket) ->
    glisten_ssl_ffi:negotiated_protocol(Socket).

-file("src/glisten/ssl.gleam", 86).
-spec peername(glisten@socket:socket()) -> {ok,
        {gleam@dynamic:dynamic_(), integer()}} |
    {error, nil}.
peername(Socket) ->
    ssl:peername(Socket).

-file("src/glisten/ssl.gleam", 89).
-spec sockname(glisten@socket:listen_socket()) -> {ok,
        {gleam@dynamic:dynamic_(), integer()}} |
    {error, glisten@socket:socket_reason()}.
sockname(Socket) ->
    ssl:sockname(Socket).

-file("src/glisten/ssl.gleam", 92).
-spec get_socket_opts(glisten@socket:socket(), list(gleam@erlang@atom:atom_())) -> {ok,
        list({gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()})} |
    {error, nil}.
get_socket_opts(Socket, Opts) ->
    ssl:getopts(Socket, Opts).
