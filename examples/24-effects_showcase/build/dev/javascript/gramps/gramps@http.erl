-module(gramps@http).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gramps/http.gleam").
-export([read_response/1, read_request/1, status_to_bit_array/1, encode_headers/1, response_builder/2, to_bytes_tree/1]).
-export_type([packet_type/0, decode_packet_error/0, uri_packet/0, decoded_packet/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type packet_type() :: httph_bin | http_bin.

-type decode_packet_error() :: {more, integer()} | {http_error, binary()}.

-type uri_packet() :: {abs_path, binary()} |
    {absolute_uri, gleam@http:scheme(), binary(), integer(), binary()}.

-type decoded_packet() :: {http_request,
        binary(),
        uri_packet(),
        {integer(), integer()}} |
    {http_response, {integer(), integer()}, integer(), binary()} |
    {http_header, integer(), gleam@dynamic:dynamic_(), binary(), binary()} |
    http_eoh.

-file("src/gramps/http.gleam", 41).
-spec get_headers(bitstring(), list({binary(), binary()})) -> {ok,
        {list({binary(), binary()}), bitstring()}} |
    {error, decode_packet_error()}.
get_headers(Data, Headers) ->
    case gramps_ffi:decode_packet(httph_bin, Data, []) of
        {ok, {http_eoh, Rest}} ->
            {ok, {Headers, Rest}};

        {ok, {{http_header, _, _, Field, Value}, Rest@1}} ->
            get_headers(Rest@1, [{string:lowercase(Field), Value} | Headers]);

        {ok, Val} ->
            {error,
                {http_error,
                    <<"Expected only headers but got request/response: "/utf8,
                        (gleam@string:inspect(Val))/binary>>}};

        {error, Reason} ->
            {error, Reason}
    end.

-file("src/gramps/http.gleam", 59).
-spec read_response(bitstring()) -> {ok,
        {gleam@http@response:response(nil), bitstring()}} |
    {error, decode_packet_error()}.
read_response(Data) ->
    case gramps_ffi:decode_packet(http_bin, Data, []) of
        {ok, {{http_response, _, Status, _}, Rest}} ->
            case get_headers(Rest, []) of
                {ok, {Headers, Rest@1}} ->
                    {ok, {{response, Status, Headers, nil}, Rest@1}};

                {error, Reason} ->
                    {error, Reason}
            end;

        {error, Reason@1} ->
            {error, Reason@1};

        _ ->
            {error, {http_error, <<"Unexpected data"/utf8>>}}
    end.

-file("src/gramps/http.gleam", 76).
-spec read_request(bitstring()) -> {ok,
        {gleam@http@request:request(nil), bitstring()}} |
    {error, decode_packet_error()}.
read_request(Data) ->
    case gramps_ffi:decode_packet(http_bin, Data, []) of
        {ok, {{http_request, Method, Uri, _}, Rest}} ->
            case get_headers(Rest, []) of
                {ok, {Headers, Rest@1}} ->
                    Host = begin
                        _pipe = Headers,
                        _pipe@1 = gleam@list:key_find(_pipe, <<"host"/utf8>>),
                        gleam@result:unwrap(_pipe@1, <<""/utf8>>)
                    end,
                    {Scheme@1, Host@2, Port@1, Path@2} = case Uri of
                        {absolute_uri, Scheme, Host@1, Port, Path} ->
                            {Scheme, Host@1, {some, Port}, Path};

                        {abs_path, Path@1} ->
                            {http, Host, none, Path@1}
                    end,
                    {Path@3, Query} = begin
                        _pipe@2 = Path@2,
                        _pipe@3 = gleam@string:split_once(_pipe@2, <<"?"/utf8>>),
                        _pipe@4 = gleam@result:map(
                            _pipe@3,
                            fun(Pair) ->
                                {erlang:element(1, Pair),
                                    {some, erlang:element(2, Pair)}}
                            end
                        ),
                        gleam@result:unwrap(_pipe@4, {Path@2, none})
                    end,
                    Method@1 = gleam@http:parse_method(Method),
                    {ok,
                        {{request,
                                gleam@result:unwrap(Method@1, get),
                                Headers,
                                nil,
                                Scheme@1,
                                Host@2,
                                Port@1,
                                Path@3,
                                Query},
                            Rest@1}};

                {error, Reason} ->
                    {error, Reason}
            end;

        {error, Reason@1} ->
            {error, Reason@1};

        Err ->
            {error,
                {http_error,
                    <<"Unexpected data: "/utf8,
                        (gleam@string:inspect(Err))/binary>>}}
    end.

-file("src/gramps/http.gleam", 148).
-spec status_to_bit_array(integer()) -> bitstring().
status_to_bit_array(Status) ->
    case Status of
        100 ->
            <<"Continue"/utf8>>;

        101 ->
            <<"Switching Protocols"/utf8>>;

        103 ->
            <<"Early Hints"/utf8>>;

        200 ->
            <<"OK"/utf8>>;

        201 ->
            <<"Created"/utf8>>;

        202 ->
            <<"Accepted"/utf8>>;

        203 ->
            <<"Non-Authoritative Information"/utf8>>;

        204 ->
            <<"No Content"/utf8>>;

        205 ->
            <<"Reset Content"/utf8>>;

        206 ->
            <<"Partial Content"/utf8>>;

        300 ->
            <<"Multiple Choices"/utf8>>;

        301 ->
            <<"Moved Permanently"/utf8>>;

        302 ->
            <<"Found"/utf8>>;

        303 ->
            <<"See Other"/utf8>>;

        304 ->
            <<"Not Modified"/utf8>>;

        307 ->
            <<"Temporary Redirect"/utf8>>;

        308 ->
            <<"Permanent Redirect"/utf8>>;

        400 ->
            <<"Bad Request"/utf8>>;

        401 ->
            <<"Unauthorized"/utf8>>;

        402 ->
            <<"Payment Required"/utf8>>;

        403 ->
            <<"Forbidden"/utf8>>;

        404 ->
            <<"Not Found"/utf8>>;

        405 ->
            <<"Method Not Allowed"/utf8>>;

        406 ->
            <<"Not Acceptable"/utf8>>;

        407 ->
            <<"Proxy Authentication Required"/utf8>>;

        408 ->
            <<"Request Timeout"/utf8>>;

        409 ->
            <<"Conflict"/utf8>>;

        410 ->
            <<"Gone"/utf8>>;

        411 ->
            <<"Length Required"/utf8>>;

        412 ->
            <<"Precondition Failed"/utf8>>;

        413 ->
            <<"Payload Too Large"/utf8>>;

        414 ->
            <<"URI Too Long"/utf8>>;

        415 ->
            <<"Unsupported Media Type"/utf8>>;

        416 ->
            <<"Range Not Satisfiable"/utf8>>;

        417 ->
            <<"Expectation Failed"/utf8>>;

        418 ->
            <<"I'm a teapot"/utf8>>;

        422 ->
            <<"Unprocessable Entity"/utf8>>;

        425 ->
            <<"Too Early"/utf8>>;

        426 ->
            <<"Upgrade Required"/utf8>>;

        428 ->
            <<"Precondition Required"/utf8>>;

        429 ->
            <<"Too Many Requests"/utf8>>;

        431 ->
            <<"Request Header Fields Too Large"/utf8>>;

        451 ->
            <<"Unavailable For Legal Reasons"/utf8>>;

        500 ->
            <<"Internal Server Error"/utf8>>;

        501 ->
            <<"Not Implemented"/utf8>>;

        502 ->
            <<"Bad Gateway"/utf8>>;

        503 ->
            <<"Service Unavailable"/utf8>>;

        504 ->
            <<"Gateway Timeout"/utf8>>;

        505 ->
            <<"HTTP Version Not Supported"/utf8>>;

        506 ->
            <<"Variant Also Negotiates"/utf8>>;

        507 ->
            <<"Insufficient Storage"/utf8>>;

        508 ->
            <<"Loop Detected"/utf8>>;

        510 ->
            <<"Not Extended"/utf8>>;

        511 ->
            <<"Network Authentication Required"/utf8>>;

        _ ->
            <<"Unknown HTTP Status"/utf8>>
    end.

-file("src/gramps/http.gleam", 209).
-spec encode_headers(list({binary(), binary()})) -> gleam@bytes_tree:bytes_tree().
encode_headers(Headers) ->
    gleam@list:fold(
        Headers,
        gleam@bytes_tree:new(),
        fun(Builder, Tup) ->
            {Header, Value} = Tup,
            _pipe = Builder,
            _pipe@1 = gleam@bytes_tree:append_string(_pipe, Header),
            _pipe@2 = gleam@bytes_tree:append(_pipe@1, <<": "/utf8>>),
            _pipe@3 = gleam@bytes_tree:append_string(_pipe@2, Value),
            gleam@bytes_tree:append(_pipe@3, <<"\r\n"/utf8>>)
        end
    ).

-file("src/gramps/http.gleam", 132).
-spec response_builder(integer(), list({binary(), binary()})) -> gleam@bytes_tree:bytes_tree().
response_builder(Status, Headers) ->
    Status_string = begin
        _pipe = Status,
        _pipe@1 = erlang:integer_to_binary(_pipe),
        _pipe@2 = gleam_stdlib:wrap_list(_pipe@1),
        _pipe@3 = gleam@bytes_tree:append(_pipe@2, <<" "/utf8>>),
        gleam@bytes_tree:append(_pipe@3, status_to_bit_array(Status))
    end,
    _pipe@4 = gleam@bytes_tree:new(),
    _pipe@5 = gleam@bytes_tree:append(_pipe@4, <<"HTTP/1.1 "/utf8>>),
    _pipe@6 = gleam_stdlib:iodata_append(_pipe@5, Status_string),
    _pipe@7 = gleam@bytes_tree:append(_pipe@6, <<"\r\n"/utf8>>),
    _pipe@8 = gleam_stdlib:iodata_append(_pipe@7, encode_headers(Headers)),
    gleam@bytes_tree:append(_pipe@8, <<"\r\n"/utf8>>).

-file("src/gramps/http.gleam", 126).
?DOC(" Turns an HTTP response into a TCP message\n").
-spec to_bytes_tree(gleam@http@response:response(gleam@bytes_tree:bytes_tree())) -> gleam@bytes_tree:bytes_tree().
to_bytes_tree(Resp) ->
    _pipe = erlang:element(2, Resp),
    _pipe@1 = response_builder(_pipe, erlang:element(3, Resp)),
    gleam_stdlib:iodata_append(_pipe@1, erlang:element(4, Resp)).
