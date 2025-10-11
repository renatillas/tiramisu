-module(mist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist.gleam").
-export([continue/1, with_selector/2, stop/0, stop_abnormal/1, ip_address_to_string/1, get_client_info/1, send_file/3, read_body/2, stream/1, new/1, port/2, read_request_body/3, after_start/2, bind/2, with_ipv6/1, with_tls/3, start/1, supervised/1, websocket/4, send_binary_frame/2, send_text_frame/2, event/1, event_id/2, event_name/2, event_retry/2, server_sent_events/4, send_event/2]).
-export_type([next/2, ip_address/0, connection_info/0, response_data/0, file_error/0, read_error/0, chunk/0, chunk_state/0, tls_options/0, builder/2, port_/0, websocket_message/1, s_s_e_connection/0, s_s_e_event/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque next(NGK, NGL) :: {continue,
        NGK,
        gleam@option:option(gleam@erlang@process:selector(NGL))} |
    normal_stop |
    {abnormal_stop, binary()}.

-type ip_address() :: {ip_v4, integer(), integer(), integer(), integer()} |
    {ip_v6,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-type connection_info() :: {connection_info, integer(), ip_address()}.

-type response_data() :: {websocket,
        gleam@erlang@process:selector(gleam@erlang@process:down())} |
    {bytes, gleam@bytes_tree:bytes_tree()} |
    {chunked, gleam@yielder:yielder(gleam@bytes_tree:bytes_tree())} |
    {file, mist@internal@file:file_descriptor(), integer(), integer()} |
    {server_sent_events,
        gleam@erlang@process:selector(gleam@erlang@process:down())}.

-type file_error() :: is_dir | no_access | no_entry | unknown_file_error.

-type read_error() :: excess_body | malformed_body.

-type chunk() :: {chunk,
        bitstring(),
        fun((integer()) -> {ok, chunk()} | {error, read_error()})} |
    done.

-type chunk_state() :: {chunk_state,
        mist@internal@buffer:buffer(),
        mist@internal@buffer:buffer(),
        boolean()}.

-type tls_options() :: {cert_key_files, binary(), binary()}.

-opaque builder(NGM, NGN) :: {builder,
        integer(),
        fun((gleam@http@request:request(NGM)) -> gleam@http@response:response(NGN)),
        fun((integer(), gleam@http:scheme(), ip_address()) -> nil),
        binary(),
        boolean(),
        gleam@option:option(tls_options())}.

-type port_() :: assigned | {provided, integer()}.

-type websocket_message(NGO) :: {text, binary()} |
    {binary, bitstring()} |
    closed |
    shutdown |
    {custom, NGO}.

-opaque s_s_e_connection() :: {s_s_e_connection,
        mist@internal@http:connection()}.

-opaque s_s_e_event() :: {s_s_e_event,
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        gleam@string_tree:string_tree()}.

-file("src/mist.gleam", 54).
-spec continue(NGP) -> next(NGP, any()).
continue(State) ->
    {continue, State, none}.

-file("src/mist.gleam", 58).
-spec with_selector(next(NGT, NGU), gleam@erlang@process:selector(NGU)) -> next(NGT, NGU).
with_selector(Next, Selector) ->
    case Next of
        {continue, State, _} ->
            {continue, State, {some, Selector}};

        _ ->
            Next
    end.

-file("src/mist.gleam", 68).
-spec stop() -> next(any(), any()).
stop() ->
    normal_stop.

-file("src/mist.gleam", 72).
-spec stop_abnormal(binary()) -> next(any(), any()).
stop_abnormal(Reason) ->
    {abnormal_stop, Reason}.

-file("src/mist.gleam", 76).
-spec convert_next(next(NHI, NHJ)) -> mist@internal@next:next(NHI, NHJ).
convert_next(Next) ->
    case Next of
        {continue, State, Selector} ->
            {continue, State, Selector};

        normal_stop ->
            normal_stop;

        {abnormal_stop, Reason} ->
            {abnormal_stop, Reason}
    end.

-file("src/mist.gleam", 100).
-spec to_mist_ip_address(glisten:ip_address()) -> ip_address().
to_mist_ip_address(Ip) ->
    case Ip of
        {ip_v4, A, B, C, D} ->
            {ip_v4, A, B, C, D};

        {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H} ->
            {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H}
    end.

-file("src/mist.gleam", 107).
-spec to_glisten_ip_address(ip_address()) -> glisten:ip_address().
to_glisten_ip_address(Ip) ->
    case Ip of
        {ip_v4, A, B, C, D} ->
            {ip_v4, A, B, C, D};

        {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H} ->
            {ip_v6, A@1, B@1, C@1, D@1, E, F, G, H}
    end.

-file("src/mist.gleam", 96).
?DOC(
    " Convenience function for printing the `IpAddress` type. It will convert the\n"
    " IPv6 loopback to the short-hand `::1`.\n"
).
-spec ip_address_to_string(ip_address()) -> binary().
ip_address_to_string(Address) ->
    glisten:ip_address_to_string(to_glisten_ip_address(Address)).

-file("src/mist.gleam", 119).
?DOC(" Tries to get the IP address and port of a connected client.\n").
-spec get_client_info(mist@internal@http:connection()) -> {ok,
        connection_info()} |
    {error, nil}.
get_client_info(Conn) ->
    _pipe = glisten@transport:peername(
        erlang:element(4, Conn),
        erlang:element(3, Conn)
    ),
    gleam@result:map(
        _pipe,
        fun(Pair) ->
            {connection_info,
                erlang:element(2, Pair),
                begin
                    _pipe@1 = erlang:element(1, Pair),
                    _pipe@2 = glisten:convert_ip_address(_pipe@1),
                    to_mist_ip_address(_pipe@2)
                end}
        end
    ).

-file("src/mist.gleam", 155).
-spec convert_file_errors(mist@internal@file:file_error()) -> file_error().
convert_file_errors(Err) ->
    case Err of
        is_dir ->
            is_dir;

        no_access ->
            no_access;

        no_entry ->
            no_entry;

        unknown_file_error ->
            unknown_file_error
    end.

-file("src/mist.gleam", 169).
?DOC(
    " To respond with a file using Erlang's `sendfile`, use this function\n"
    " with the specified offset and limit (optional). It will attempt to open the\n"
    " file for reading, get its file size, and then send the file.  If the read\n"
    " errors, this will return the relevant `FileError`. Generally, this will be\n"
    " more memory efficient than manually doing this process with `mist.Bytes`.\n"
).
-spec send_file(binary(), integer(), gleam@option:option(integer())) -> {ok,
        response_data()} |
    {error, file_error()}.
send_file(Path, Offset, Limit) ->
    _pipe = Path,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = mist@internal@file:stat(_pipe@1),
    _pipe@3 = gleam@result:map_error(_pipe@2, fun convert_file_errors/1),
    gleam@result:map(
        _pipe@3,
        fun(Stat) ->
            {file,
                erlang:element(2, Stat),
                Offset,
                gleam@option:unwrap(Limit, erlang:element(3, Stat))}
        end
    ).

-file("src/mist.gleam", 199).
?DOC(
    " The request body is not pulled from the socket until requested. The\n"
    " `content-length` header is used to determine whether the socket is read\n"
    " from or not. The read may also fail, and a `ReadError` is raised.\n"
).
-spec read_body(
    gleam@http@request:request(mist@internal@http:connection()),
    integer()
) -> {ok, gleam@http@request:request(bitstring())} | {error, read_error()}.
read_body(Req, Max_body_limit) ->
    _pipe = Req,
    _pipe@1 = gleam@http@request:get_header(_pipe, <<"content-length"/utf8>>),
    _pipe@2 = gleam@result:'try'(_pipe@1, fun gleam_stdlib:parse_int/1),
    _pipe@3 = gleam@result:unwrap(_pipe@2, 0),
    (fun(Content_length) -> case Content_length of
            Value when Value =< Max_body_limit ->
                _pipe@4 = mist@internal@http:read_body(Req),
                gleam@result:replace_error(_pipe@4, malformed_body);

            _ ->
                {error, excess_body}
        end end)(_pipe@3).

-file("src/mist.gleam", 228).
-spec do_stream(
    gleam@http@request:request(mist@internal@http:connection()),
    mist@internal@buffer:buffer()
) -> fun((integer()) -> {ok, chunk()} | {error, read_error()}).
do_stream(Req, Buffer) ->
    fun(Size) ->
        Socket = erlang:element(3, erlang:element(4, Req)),
        Transport = erlang:element(4, erlang:element(4, Req)),
        Byte_size = erlang:byte_size(erlang:element(3, Buffer)),
        case {erlang:element(2, Buffer), Byte_size} of
            {0, 0} ->
                {ok, done};

            {0, _} ->
                {Data, Rest} = mist@internal@buffer:slice(Buffer, Size),
                {ok,
                    {chunk,
                        Data,
                        do_stream(Req, mist@internal@buffer:new(Rest))}};

            {_, Buffer_size} when Buffer_size >= Size ->
                {Data@1, Rest@1} = mist@internal@buffer:slice(Buffer, Size),
                New_buffer = {buffer, erlang:element(2, Buffer), Rest@1},
                {ok, {chunk, Data@1, do_stream(Req, New_buffer)}};

            {_, _} ->
                _pipe = mist@internal@http:read_data(
                    Socket,
                    Transport,
                    mist@internal@buffer:empty(),
                    invalid_body
                ),
                _pipe@1 = gleam@result:replace_error(_pipe, malformed_body),
                gleam@result:map(
                    _pipe@1,
                    fun(Data@2) ->
                        Fetched_data = erlang:byte_size(Data@2),
                        New_buffer@1 = {buffer,
                            gleam@int:max(
                                0,
                                erlang:element(2, Buffer) - Fetched_data
                            ),
                            gleam@bit_array:append(
                                erlang:element(3, Buffer),
                                Data@2
                            )},
                        {New_data, Rest@2} = mist@internal@buffer:slice(
                            New_buffer@1,
                            Size
                        ),
                        {chunk,
                            New_data,
                            do_stream(
                                Req,
                                {buffer,
                                    erlang:element(2, New_buffer@1),
                                    Rest@2}
                            )}
                    end
                )
        end
    end.

-file("src/mist.gleam", 293).
-spec fetch_chunks_until(
    glisten@socket:socket(),
    glisten@transport:transport(),
    chunk_state(),
    integer()
) -> {ok, {bitstring(), chunk_state()}} | {error, read_error()}.
fetch_chunks_until(Socket, Transport, State, Byte_size) ->
    Data_size = erlang:byte_size(erlang:element(3, erlang:element(2, State))),
    case {erlang:element(4, State), Data_size} of
        {_, Size} when Size >= Byte_size ->
            {Value, Rest} = mist@internal@buffer:slice(
                erlang:element(2, State),
                Byte_size
            ),
            {ok,
                {Value,
                    {chunk_state,
                        mist@internal@buffer:new(Rest),
                        erlang:element(3, State),
                        erlang:element(4, State)}}};

        {true, _} ->
            {ok,
                {erlang:element(3, erlang:element(2, State)),
                    {chunk_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        true}}};

        {false, _} ->
            case mist@internal@http:parse_chunk(
                erlang:element(3, erlang:element(3, State))
            ) of
                complete ->
                    Updated_state = {chunk_state,
                        erlang:element(2, State),
                        mist@internal@buffer:empty(),
                        true},
                    fetch_chunks_until(
                        Socket,
                        Transport,
                        Updated_state,
                        Byte_size
                    );

                {chunk, <<>>, Next_buffer} ->
                    _pipe = mist@internal@http:read_data(
                        Socket,
                        Transport,
                        Next_buffer,
                        invalid_body
                    ),
                    _pipe@1 = gleam@result:replace_error(_pipe, malformed_body),
                    gleam@result:'try'(
                        _pipe@1,
                        fun(New_data) ->
                            Updated_state@1 = {chunk_state,
                                erlang:element(2, State),
                                mist@internal@buffer:new(New_data),
                                erlang:element(4, State)},
                            fetch_chunks_until(
                                Socket,
                                Transport,
                                Updated_state@1,
                                Byte_size
                            )
                        end
                    );

                {chunk, Data, Next_buffer@1} ->
                    Updated_state@2 = {chunk_state,
                        mist@internal@buffer:append(
                            erlang:element(2, State),
                            Data
                        ),
                        Next_buffer@1,
                        erlang:element(4, State)},
                    fetch_chunks_until(
                        Socket,
                        Transport,
                        Updated_state@2,
                        Byte_size
                    )
            end
    end.

-file("src/mist.gleam", 273).
-spec do_stream_chunked(
    gleam@http@request:request(mist@internal@http:connection()),
    chunk_state()
) -> fun((integer()) -> {ok, chunk()} | {error, read_error()}).
do_stream_chunked(Req, State) ->
    Socket = erlang:element(3, erlang:element(4, Req)),
    Transport = erlang:element(4, erlang:element(4, Req)),
    fun(Size) -> case fetch_chunks_until(Socket, Transport, State, Size) of
            {ok, {Data, {chunk_state, _, _, true}}} ->
                {ok, {chunk, Data, fun(_) -> {ok, done} end}};

            {ok, {Data@1, State@1}} ->
                {ok, {chunk, Data@1, do_stream_chunked(Req, State@1)}};

            {error, _} ->
                {error, malformed_body}
        end end.

-file("src/mist.gleam", 345).
?DOC(
    " Rather than explicitly reading either the whole body (optionally up to\n"
    " `N` bytes), this function allows you to consume a stream of the request\n"
    " body. Any errors reading the body will propagate out, or `Chunk`s will be\n"
    " emitted. This provides a `consume` method to attempt to grab the next\n"
    " `size` chunk from the socket.\n"
).
-spec stream(gleam@http@request:request(mist@internal@http:connection())) -> {ok,
        fun((integer()) -> {ok, chunk()} | {error, read_error()})} |
    {error, read_error()}.
stream(Req) ->
    Continue = begin
        _pipe = Req,
        _pipe@1 = mist@internal@http:handle_continue(_pipe),
        gleam@result:replace_error(_pipe@1, malformed_body)
    end,
    gleam@result:map(
        Continue,
        fun(_) ->
            Is_chunked = case gleam@http@request:get_header(
                Req,
                <<"transfer-encoding"/utf8>>
            ) of
                {ok, <<"chunked"/utf8>>} ->
                    true;

                _ ->
                    false
            end,
            Data@1 = case erlang:element(2, erlang:element(4, Req)) of
                {initial, Data} -> Data;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist"/utf8>>,
                                function => <<"stream"/utf8>>,
                                line => 360,
                                value => _assert_fail,
                                start => 11285,
                                'end' => 11330,
                                pattern_start => 11296,
                                pattern_end => 11314})
            end,
            case Is_chunked of
                true ->
                    State = {chunk_state,
                        mist@internal@buffer:new(<<>>),
                        mist@internal@buffer:new(Data@1),
                        false},
                    do_stream_chunked(Req, State);

                false ->
                    Content_length = begin
                        _pipe@2 = Req,
                        _pipe@3 = gleam@http@request:get_header(
                            _pipe@2,
                            <<"content-length"/utf8>>
                        ),
                        _pipe@4 = gleam@result:'try'(
                            _pipe@3,
                            fun gleam_stdlib:parse_int/1
                        ),
                        gleam@result:unwrap(_pipe@4, 0)
                    end,
                    Initial_size = erlang:byte_size(Data@1),
                    Buffer = {buffer,
                        gleam@int:max(0, Content_length - Initial_size),
                        Data@1},
                    do_stream(Req, Buffer)
            end
        end
    ).

-file("src/mist.gleam", 401).
?DOC(
    " Create a new `mist` handler with a given function. The default port is\n"
    " 4000.\n"
).
-spec new(
    fun((gleam@http@request:request(NIK)) -> gleam@http@response:response(NIM))
) -> builder(NIK, NIM).
new(Handler) ->
    {builder,
        4000,
        Handler,
        fun(Port, Scheme, Interface) ->
            Address = case Interface of
                {ip_v6, _, _, _, _, _, _, _, _} ->
                    <<<<"["/utf8, (ip_address_to_string(Interface))/binary>>/binary,
                        "]"/utf8>>;

                _ ->
                    ip_address_to_string(Interface)
            end,
            Message = <<<<<<<<<<"Listening on "/utf8,
                                (gleam@http:scheme_to_string(Scheme))/binary>>/binary,
                            "://"/utf8>>/binary,
                        Address/binary>>/binary,
                    ":"/utf8>>/binary,
                (erlang:integer_to_binary(Port))/binary>>,
            gleam_stdlib:println(Message)
        end,
        <<"localhost"/utf8>>,
        false,
        none}.

-file("src/mist.gleam", 426).
?DOC(" Assign a different listening port to the service.\n").
-spec port(builder(NIQ, NIR), integer()) -> builder(NIQ, NIR).
port(Builder, Port) ->
    {builder,
        Port,
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder)}.

-file("src/mist.gleam", 433).
?DOC(
    " This function allows for implicitly reading the body of requests up\n"
    " to a given size. If the size is too large, or the read fails, the provided\n"
    " `failure_response` will be sent back as the response.\n"
).
-spec read_request_body(
    builder(bitstring(), NIW),
    integer(),
    gleam@http@response:response(NIW)
) -> builder(mist@internal@http:connection(), NIW).
read_request_body(Builder, Bytes_limit, Failure_response) ->
    Handler = fun(Request) -> case read_body(Request, Bytes_limit) of
            {ok, Request@1} ->
                (erlang:element(3, Builder))(Request@1);

            {error, _} ->
                Failure_response
        end end,
    {builder,
        erlang:element(2, Builder),
        Handler,
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder)}.

-file("src/mist.gleam", 449).
?DOC(
    " Override the default function to be called after the service starts. The\n"
    " default is to log a message with the listening port.\n"
).
-spec after_start(
    builder(NJC, NJD),
    fun((integer(), gleam@http:scheme(), ip_address()) -> nil)
) -> builder(NJC, NJD).
after_start(Builder, After_start) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        After_start,
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder)}.

-file("src/mist.gleam", 460).
?DOC(
    " Specify an interface to listen on. This is a string that can have the\n"
    " following values: \"localhost\", a valid IPv4 address (i.e. \"127.0.0.1\"), or\n"
    " a valid IPv6 address (i.e. \"::1\"). An invalid value will cause the\n"
    " application to crash.\n"
).
-spec bind(builder(NJI, NJJ), binary()) -> builder(NJI, NJJ).
bind(Builder, Interface) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        Interface,
        erlang:element(6, Builder),
        erlang:element(7, Builder)}.

-file("src/mist.gleam", 469).
?DOC(
    " By default, `mist` will listen on `localhost` over IPv4. If you specify an\n"
    " IPv4 address to bind to, it will still only serve over IPv4. Calling this\n"
    " function will listen on both IPv4 and IPv6 for the given interface. If it is\n"
    " not supported, your application will crash. If you provide an IPv6 address\n"
    " to `mist.bind`, this function will have no effect.\n"
).
-spec with_ipv6(builder(NJO, NJP)) -> builder(NJO, NJP).
with_ipv6(Builder) ->
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        true,
        erlang:element(7, Builder)}.

-file("src/mist.gleam", 474).
?DOC(" Use HTTPS with the provided certificate and key files.\n").
-spec with_tls(builder(NJU, NJV), binary(), binary()) -> builder(NJU, NJV).
with_tls(Builder, Cert, Key) ->
    Certfile = mist_ffi:file_open(gleam_stdlib:identity(Cert)),
    Keyfile = mist_ffi:file_open(gleam_stdlib:identity(Key)),
    _ = case {Certfile, Keyfile} of
        {{error, _}, {error, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Certificate and key file not found"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist"/utf8>>,
                    function => <<"with_tls"/utf8>>,
                    line => 483});

        {{ok, _}, {error, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Key file not found"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist"/utf8>>,
                    function => <<"with_tls"/utf8>>,
                    line => 484});

        {{error, _}, {ok, _}} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Certificate file not found"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist"/utf8>>,
                    function => <<"with_tls"/utf8>>,
                    line => 485});

        {{ok, _}, {ok, _}} ->
            nil
    end,
    {builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        {some, {cert_key_files, Cert, Key}}}.

-file("src/mist.gleam", 492).
-spec convert_body_types(gleam@http@response:response(response_data())) -> gleam@http@response:response(mist@internal@http:response_data()).
convert_body_types(Resp) ->
    New_body = case erlang:element(4, Resp) of
        {websocket, Selector} ->
            {websocket, Selector};

        {bytes, Data} ->
            {bytes, Data};

        {file, Descriptor, Offset, Length} ->
            {file, Descriptor, Offset, Length};

        {chunked, Iter} ->
            {chunked, Iter};

        {server_sent_events, Selector@1} ->
            {server_sent_events, Selector@1}
    end,
    gleam@http@response:set_body(Resp, New_body).

-file("src/mist.gleam", 511).
?DOC(" Start a `mist` service with the provided builder.\n").
-spec start(builder(mist@internal@http:connection(), response_data())) -> {ok,
        gleam@otp@actor:started(gleam@otp@static_supervisor:supervisor())} |
    {error, gleam@otp@actor:start_error()}.
start(Builder) ->
    Listener_name = gleam_erlang_ffi:new_name(<<"glisten_listener"/utf8>>),
    _pipe = fun(Req) ->
        convert_body_types((erlang:element(3, Builder))(Req))
    end,
    _pipe@1 = mist@internal@handler:with_func(_pipe),
    _pipe@2 = glisten:new(fun mist@internal@handler:init/1, _pipe@1),
    _pipe@3 = glisten:bind(_pipe@2, erlang:element(5, Builder)),
    _pipe@4 = (fun(Handler) -> case erlang:element(6, Builder) of
            true ->
                glisten:with_ipv6(Handler);

            false ->
                Handler
        end end)(_pipe@3),
    _pipe@6 = (fun(Handler@1) -> case erlang:element(7, Builder) of
            {some, {cert_key_files, Certfile, Keyfile}} ->
                _pipe@5 = Handler@1,
                glisten:with_tls(_pipe@5, Certfile, Keyfile);

            _ ->
                Handler@1
        end end)(_pipe@4),
    _pipe@7 = glisten:start_with_listener_name(
        _pipe@6,
        erlang:element(2, Builder),
        Listener_name
    ),
    gleam@result:map(
        _pipe@7,
        fun(Server) ->
            Info = glisten:get_server_info(Listener_name, 5000),
            Ip_address = to_mist_ip_address(erlang:element(3, Info)),
            Scheme = case gleam@option:is_some(erlang:element(7, Builder)) of
                true ->
                    https;

                false ->
                    http
            end,
            (erlang:element(4, Builder))(
                erlang:element(2, Info),
                Scheme,
                Ip_address
            ),
            Server
        end
    ).

-file("src/mist.gleam", 547).
?DOC(" Start the `mist` supervisor as a child of a supervision tree.\n").
-spec supervised(builder(mist@internal@http:connection(), response_data())) -> gleam@otp@supervision:child_specification(gleam@otp@static_supervisor:supervisor()).
supervised(Builder) ->
    gleam@otp@supervision:supervisor(fun() -> start(Builder) end).

-file("src/mist.gleam", 562).
-spec internal_to_public_ws_message(
    mist@internal@websocket:handler_message(NKK)
) -> {ok, websocket_message(NKK)} | {error, nil}.
internal_to_public_ws_message(Msg) ->
    case Msg of
        {internal, {data, {text_frame, Data}}} ->
            _pipe = Data,
            _pipe@1 = gleam@bit_array:to_string(_pipe),
            gleam@result:map(_pipe@1, fun(Field@0) -> {text, Field@0} end);

        {internal, {data, {binary_frame, Data@1}}} ->
            {ok, {binary, Data@1}};

        {user, Msg@1} ->
            {ok, {custom, Msg@1}};

        _ ->
            {error, nil}
    end.

-file("src/mist.gleam", 587).
?DOC(
    " Upgrade a request to handle websockets. If the request is\n"
    " malformed, or the websocket process fails to initialize, an empty\n"
    " 400 response will be sent to the client.\n"
    "\n"
    " The `on_init` method will be called when the actual WebSocket process\n"
    " is started, and the return value is the initial state and an optional\n"
    " selector for receiving user messages.\n"
    "\n"
    " The `on_close` method is called when the WebSocket process shuts down\n"
    " for any reason, valid or otherwise.\n"
).
-spec websocket(
    gleam@http@request:request(mist@internal@http:connection()),
    fun((NKQ, websocket_message(NKR), mist@internal@websocket:websocket_connection()) -> next(NKQ, NKR)),
    fun((mist@internal@websocket:websocket_connection()) -> {NKQ,
        gleam@option:option(gleam@erlang@process:selector(NKR))}),
    fun((NKQ) -> nil)
) -> gleam@http@response:response(response_data()).
websocket(Request, Handler, On_init, On_close) ->
    Handler@1 = fun(State, Message, Connection) -> _pipe = Message,
        _pipe@1 = internal_to_public_ws_message(_pipe),
        _pipe@2 = gleam@result:map(
            _pipe@1,
            fun(_capture) -> Handler(State, _capture, Connection) end
        ),
        _pipe@3 = gleam@result:unwrap(_pipe@2, continue(State)),
        convert_next(_pipe@3) end,
    Extensions = begin
        _pipe@4 = Request,
        _pipe@5 = gleam@http@request:get_header(
            _pipe@4,
            <<"sec-websocket-extensions"/utf8>>
        ),
        _pipe@6 = gleam@result:map(
            _pipe@5,
            fun(Header) -> gleam@string:split(Header, <<";"/utf8>>) end
        ),
        gleam@result:unwrap(_pipe@6, [])
    end,
    Socket = erlang:element(3, erlang:element(4, Request)),
    Transport = erlang:element(4, erlang:element(4, Request)),
    _pipe@7 = Request,
    _pipe@8 = mist@internal@http:upgrade(Socket, Transport, Extensions, _pipe@7),
    _pipe@9 = gleam@result:'try'(
        _pipe@8,
        fun(_) ->
            mist@internal@websocket:initialize_connection(
                On_init,
                On_close,
                Handler@1,
                Socket,
                Transport,
                Extensions
            )
        end
    ),
    _pipe@12 = gleam@result:map(
        _pipe@9,
        fun(Subj) ->
            Ws_process@1 = case gleam@erlang@process:subject_owner(
                erlang:element(3, Subj)
            ) of
                {ok, Ws_process} -> Ws_process;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist"/utf8>>,
                                function => <<"websocket"/utf8>>,
                                line => 623,
                                value => _assert_fail,
                                start => 19307,
                                'end' => 19367,
                                pattern_start => 19318,
                                pattern_end => 19332})
            end,
            Monitor = gleam@erlang@process:monitor(Ws_process@1),
            Selector = begin
                _pipe@10 = gleam_erlang_ffi:new_selector(),
                gleam@erlang@process:select_specific_monitor(
                    _pipe@10,
                    Monitor,
                    fun gleam@function:identity/1
                )
            end,
            _pipe@11 = gleam@http@response:new(200),
            gleam@http@response:set_body(_pipe@11, {websocket, Selector})
        end
    ),
    gleam@result:lazy_unwrap(
        _pipe@12,
        fun() -> _pipe@13 = gleam@http@response:new(400),
            gleam@http@response:set_body(
                _pipe@13,
                {bytes, gleam@bytes_tree:new()}
            ) end
    ).

-file("src/mist.gleam", 641).
?DOC(" Sends a binary frame across the websocket.\n").
-spec send_binary_frame(
    mist@internal@websocket:websocket_connection(),
    bitstring()
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
send_binary_frame(Connection, Frame) ->
    Binary_frame = exception_ffi:rescue(
        fun() ->
            gramps@websocket:encode_binary_frame(
                Frame,
                erlang:element(4, Connection),
                none
            )
        end
    ),
    case Binary_frame of
        {ok, Binary_frame@1} ->
            glisten@transport:send(
                erlang:element(3, Connection),
                erlang:element(2, Connection),
                Binary_frame@1
            );

        {error, Reason} ->
            logging:log(
                error,
                <<"Cannot send messages from a different process than the WebSocket: "/utf8,
                    (gleam@string:inspect(Reason))/binary>>
            ),
            erlang:error(#{gleam_error => panic,
                    message => <<"Exiting due to sending WebSocket message from non-owning process"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist"/utf8>>,
                    function => <<"send_binary_frame"/utf8>>,
                    line => 659})
    end.

-file("src/mist.gleam", 665).
?DOC(" Sends a text frame across the websocket.\n").
-spec send_text_frame(mist@internal@websocket:websocket_connection(), binary()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
send_text_frame(Connection, Frame) ->
    Text_frame = exception_ffi:rescue(
        fun() ->
            gramps@websocket:encode_text_frame(
                Frame,
                erlang:element(4, Connection),
                none
            )
        end
    ),
    case Text_frame of
        {ok, Text_frame@1} ->
            glisten@transport:send(
                erlang:element(3, Connection),
                erlang:element(2, Connection),
                Text_frame@1
            );

        {error, Reason} ->
            logging:log(
                error,
                <<"Cannot send messages from a different process than the WebSocket: "/utf8,
                    (gleam@string:inspect(Reason))/binary>>
            ),
            erlang:error(#{gleam_error => panic,
                    message => <<"Exiting due to sending WebSocket message from non-owning process"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"mist"/utf8>>,
                    function => <<"send_text_frame"/utf8>>,
                    line => 683})
    end.

-file("src/mist.gleam", 709).
-spec event(gleam@string_tree:string_tree()) -> s_s_e_event().
event(Data) ->
    {s_s_e_event, none, none, none, Data}.

-file("src/mist.gleam", 714).
-spec event_id(s_s_e_event(), binary()) -> s_s_e_event().
event_id(Event, Id) ->
    {s_s_e_event,
        {some, Id},
        erlang:element(3, Event),
        erlang:element(4, Event),
        erlang:element(5, Event)}.

-file("src/mist.gleam", 719).
-spec event_name(s_s_e_event(), binary()) -> s_s_e_event().
event_name(Event, Name) ->
    {s_s_e_event,
        erlang:element(2, Event),
        {some, Name},
        erlang:element(4, Event),
        erlang:element(5, Event)}.

-file("src/mist.gleam", 724).
-spec event_retry(s_s_e_event(), integer()) -> s_s_e_event().
event_retry(Event, Retry) ->
    {s_s_e_event,
        erlang:element(2, Event),
        erlang:element(3, Event),
        {some, Retry},
        erlang:element(5, Event)}.

-file("src/mist.gleam", 737).
?DOC(
    " Sets up the connection for server-sent events. The initial response provided\n"
    " here will have its headers included in the SSE setup. The body is discarded.\n"
    " The `init` and `loop` parameters follow the same shape as the\n"
    " `gleam/otp/actor` module.\n"
    "\n"
    " NOTE:  There is no proper way within the spec for the server to \"close\" the\n"
    " SSE connection. There are ways around it.\n"
    "\n"
    " See:  `examples/eventz` for a sample usage.\n"
).
-spec server_sent_events(
    gleam@http@request:request(mist@internal@http:connection()),
    gleam@http@response:response(any()),
    fun((gleam@erlang@process:subject(NLF)) -> {ok,
            gleam@otp@actor:initialised(NLH, NLF, any())} |
        {error, binary()}),
    fun((NLH, NLF, s_s_e_connection()) -> gleam@otp@actor:next(NLH, NLF))
) -> gleam@http@response:response(response_data()).
server_sent_events(Req, Resp, Init, Loop) ->
    With_default_headers = begin
        _pipe = Resp,
        _pipe@1 = gleam@http@response:set_header(
            _pipe,
            <<"content-type"/utf8>>,
            <<"text/event-stream"/utf8>>
        ),
        _pipe@2 = gleam@http@response:set_header(
            _pipe@1,
            <<"cache-control"/utf8>>,
            <<"no-cache"/utf8>>
        ),
        gleam@http@response:set_header(
            _pipe@2,
            <<"connection"/utf8>>,
            <<"keep-alive"/utf8>>
        )
    end,
    _pipe@3 = glisten@transport:send(
        erlang:element(4, erlang:element(4, Req)),
        erlang:element(3, erlang:element(4, Req)),
        mist@internal@encoder:response_builder(
            200,
            erlang:element(3, With_default_headers),
            <<"1.1"/utf8>>
        )
    ),
    _pipe@4 = gleam@result:replace_error(_pipe@3, nil),
    _pipe@9 = gleam@result:'try'(
        _pipe@4,
        fun(_) ->
            _pipe@6 = gleam@otp@actor:new_with_initialiser(
                1000,
                fun(Subj) -> _pipe@5 = Init(Subj),
                    gleam@result:map(
                        _pipe@5,
                        fun(Return) ->
                            gleam@otp@actor:returning(Return, Subj)
                        end
                    ) end
            ),
            _pipe@7 = gleam@otp@actor:on_message(
                _pipe@6,
                fun(State, Message) ->
                    Loop(
                        State,
                        Message,
                        {s_s_e_connection, erlang:element(4, Req)}
                    )
                end
            ),
            _pipe@8 = gleam@otp@actor:start(_pipe@7),
            gleam@result:replace_error(_pipe@8, nil)
        end
    ),
    _pipe@12 = gleam@result:map(
        _pipe@9,
        fun(Subj@1) ->
            Sse_process@1 = case gleam@erlang@process:subject_owner(
                erlang:element(3, Subj@1)
            ) of
                {ok, Sse_process} -> Sse_process;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"mist"/utf8>>,
                                function => <<"server_sent_events"/utf8>>,
                                line => 768,
                                value => _assert_fail,
                                start => 23927,
                                'end' => 23988,
                                pattern_start => 23938,
                                pattern_end => 23953})
            end,
            Monitor = gleam@erlang@process:monitor(Sse_process@1),
            Selector = begin
                _pipe@10 = gleam_erlang_ffi:new_selector(),
                gleam@erlang@process:select_specific_monitor(
                    _pipe@10,
                    Monitor,
                    fun gleam@function:identity/1
                )
            end,
            _pipe@11 = gleam@http@response:new(200),
            gleam@http@response:set_body(
                _pipe@11,
                {server_sent_events, Selector}
            )
        end
    ),
    gleam@result:lazy_unwrap(
        _pipe@12,
        fun() -> _pipe@13 = gleam@http@response:new(400),
            gleam@http@response:set_body(
                _pipe@13,
                {bytes, gleam@bytes_tree:new()}
            ) end
    ).

-file("src/mist.gleam", 786).
-spec send_event(s_s_e_connection(), s_s_e_event()) -> {ok, nil} | {error, nil}.
send_event(Conn, Event) ->
    {s_s_e_connection, Conn@1} = Conn,
    Id@1 = begin
        _pipe = erlang:element(2, Event),
        _pipe@1 = gleam@option:map(
            _pipe,
            fun(Id) -> <<<<"id: "/utf8, Id/binary>>/binary, "\n"/utf8>> end
        ),
        gleam@option:unwrap(_pipe@1, <<""/utf8>>)
    end,
    Event_name = begin
        _pipe@2 = erlang:element(3, Event),
        _pipe@3 = gleam@option:map(
            _pipe@2,
            fun(Name) ->
                <<<<"event: "/utf8, Name/binary>>/binary, "\n"/utf8>>
            end
        ),
        gleam@option:unwrap(_pipe@3, <<""/utf8>>)
    end,
    Retry@1 = begin
        _pipe@4 = erlang:element(4, Event),
        _pipe@5 = gleam@option:map(
            _pipe@4,
            fun(Retry) ->
                <<<<"retry: "/utf8, (erlang:integer_to_binary(Retry))/binary>>/binary,
                    "\n"/utf8>>
            end
        ),
        gleam@option:unwrap(_pipe@5, <<""/utf8>>)
    end,
    Data = begin
        _pipe@6 = erlang:element(5, Event),
        _pipe@7 = gleam@string_tree:split(_pipe@6, <<"\n"/utf8>>),
        _pipe@8 = gleam@list:map(
            _pipe@7,
            fun(Row) -> gleam@string_tree:prepend(Row, <<"data: "/utf8>>) end
        ),
        gleam@string_tree:join(_pipe@8, <<"\n"/utf8>>)
    end,
    Message = begin
        _pipe@9 = Data,
        _pipe@10 = gleam@string_tree:prepend(_pipe@9, Event_name),
        _pipe@11 = gleam@string_tree:prepend(_pipe@10, Id@1),
        _pipe@12 = gleam@string_tree:prepend(_pipe@11, Retry@1),
        _pipe@13 = gleam@string_tree:append(_pipe@12, <<"\n\n"/utf8>>),
        gleam_stdlib:wrap_list(_pipe@13)
    end,
    _pipe@14 = glisten@transport:send(
        erlang:element(4, Conn@1),
        erlang:element(3, Conn@1),
        Message
    ),
    _pipe@15 = gleam@result:replace(_pipe@14, nil),
    gleam@result:replace_error(_pipe@15, nil).
