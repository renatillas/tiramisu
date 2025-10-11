-module(wisp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/wisp.gleam").
-export([response/1, set_body/2, file_download/3, file_download_from_memory/3, html_response/2, json_response/2, html_body/2, json_body/2, string_tree_body/2, string_body/2, escape_html/1, method_not_allowed/1, no_content/0, set_max_body_size/2, get_max_body_size/1, set_secret_key_base/2, get_secret_key_base/1, set_max_files_size/2, get_max_files_size/1, set_read_chunk_size/2, get_read_chunk_size/1, require_method/3, get_query/1, method_override/1, read_body_bits/1, parse_range_header/1, handle_head/2, new_temporary_file/1, delete_temporary_files/1, configure_logger/0, set_logger_level/1, log_emergency/1, log_alert/1, log_critical/1, log_error/1, log_warning/1, log_notice/1, log_info/1, log_request/2, log_debug/1, random_string/1, sign_message/3, verify_signed_message/2, set_cookie/6, get_cookie/3, create_canned_connection/2, ok/0, created/0, accepted/0, redirect/1, permanent_redirect/1, not_found/0, bad_request/1, content_too_large/0, unsupported_media_type/1, unprocessable_content/0, internal_server_error/0, require_bit_array_body/2, require_content_type/3, rescue_crashes/1, require_string_body/2, require_json/2, csrf_known_header_protection/2, require_form/2, serve_static/4]).
-export_type([body/0, buffered_reader/0, quotas/0, form_data/0, uploaded_file/0, do_not_leak/0, error_kind/0, range/0, log_level/0, security/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type body() :: {text, binary()} |
    {bytes, gleam@bytes_tree:bytes_tree()} |
    {file, binary(), integer(), gleam@option:option(integer())}.

-type buffered_reader() :: {buffered_reader,
        fun((integer()) -> {ok, wisp@internal:read()} | {error, nil}),
        bitstring()}.

-type quotas() :: {quotas, integer(), integer()}.

-type form_data() :: {form_data,
        list({binary(), binary()}),
        list({binary(), uploaded_file()})}.

-type uploaded_file() :: {uploaded_file, binary(), binary()}.

-type do_not_leak() :: any().

-type error_kind() :: errored | thrown | exited.

-type range() :: {range, integer(), gleam@option:option(integer())}.

-type log_level() :: emergency_level |
    alert_level |
    critical_level |
    error_level |
    warning_level |
    notice_level |
    info_level |
    debug_level.

-type security() :: plain_text | signed.

-file("src/wisp.gleam", 84).
?DOC(
    " Create a response with the given status code.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " response(200)\n"
    " // -> Response(200, [], Text(\"\"))\n"
    " ```\n"
).
-spec response(integer()) -> gleam@http@response:response(body()).
response(Status) ->
    {response, Status, [], {text, <<""/utf8>>}}.

-file("src/wisp.gleam", 98).
?DOC(
    " Set the body of a response.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " response(200)\n"
    " |> set_body(File(\"/tmp/myfile.txt\", option.None))\n"
    " // -> Response(200, [], File(\"/tmp/myfile.txt\", option.None))\n"
    " ```\n"
).
-spec set_body(gleam@http@response:response(body()), body()) -> gleam@http@response:response(body()).
set_body(Response, Body) ->
    _pipe = Response,
    gleam@http@response:set_body(_pipe, Body).

-file("src/wisp.gleam", 128).
?DOC(
    " Send a file from the disc as a file download.\n"
    "\n"
    " The operating system `send_file` function is used to efficiently send the\n"
    " file over the network socket without reading the entire file into memory.\n"
    "\n"
    " The `content-disposition` header will be set to `attachment;\n"
    " filename=\"name\"` to ensure the file is downloaded by the browser. This is\n"
    " especially good for files that the browser would otherwise attempt to open\n"
    " as this can result in cross-site scripting vulnerabilities.\n"
    "\n"
    " If you wish to not set the `content-disposition` header you could use the\n"
    " `set_body` function with the `File` body variant.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " response(200)\n"
    " |> file_download(named: \"myfile.txt\", from: \"/tmp/myfile.txt\")\n"
    " // -> Response(\n"
    " //   200,\n"
    " //   [#(\"content-disposition\", \"attachment; filename=\\\"myfile.txt\\\"\")],\n"
    " //   File(\"/tmp/myfile.txt\", option.None),\n"
    " // )\n"
    " ```\n"
).
-spec file_download(gleam@http@response:response(body()), binary(), binary()) -> gleam@http@response:response(body()).
file_download(Response, Name, Path) ->
    Name@1 = gleam_stdlib:percent_encode(Name),
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_header(
        _pipe,
        <<"content-disposition"/utf8>>,
        <<<<"attachment; filename=\""/utf8, Name@1/binary>>/binary, "\""/utf8>>
    ),
    gleam@http@response:set_body(_pipe@1, {file, Path, 0, none}).

-file("src/wisp.gleam", 165).
?DOC(
    " Send a file from memory as a file download.\n"
    "\n"
    " If your file is already on the disc use `file_download` instead, to avoid\n"
    " having to read the file into memory to send it.\n"
    "\n"
    " The `content-disposition` header will be set to `attachment;\n"
    " filename=\"name\"` to ensure the file is downloaded by the browser. This is\n"
    " especially good for files that the browser would otherwise attempt to open\n"
    " as this can result in cross-site scripting vulnerabilities.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let content = bytes_tree.from_string(\"Hello, Joe!\")\n"
    " response(200)\n"
    " |> file_download_from_memory(named: \"myfile.txt\", containing: content)\n"
    " // -> Response(\n"
    " //   200,\n"
    " //   [#(\"content-disposition\", \"attachment; filename=\\\"myfile.txt\\\"\")],\n"
    " //   File(\"/tmp/myfile.txt\", option.None),\n"
    " // )\n"
    " ```\n"
).
-spec file_download_from_memory(
    gleam@http@response:response(body()),
    binary(),
    gleam@bytes_tree:bytes_tree()
) -> gleam@http@response:response(body()).
file_download_from_memory(Response, Name, Data) ->
    Name@1 = gleam_stdlib:percent_encode(Name),
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_header(
        _pipe,
        <<"content-disposition"/utf8>>,
        <<<<"attachment; filename=\""/utf8, Name@1/binary>>/binary, "\""/utf8>>
    ),
    gleam@http@response:set_body(_pipe@1, {bytes, Data}).

-file("src/wisp.gleam", 191).
?DOC(
    " Create a HTML response.\n"
    "\n"
    " The body is expected to be valid HTML, though this is not validated.\n"
    " The `content-type` header will be set to `text/html; charset=utf-8`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " html_response(\"<h1>Hello, Joe!</h1>\", 200)\n"
    " // -> Response(200, [#(\"content-type\", \"text/html; charset=utf-8\")], Text(body))\n"
    " ```\n"
).
-spec html_response(binary(), integer()) -> gleam@http@response:response(body()).
html_response(Html, Status) ->
    {response,
        Status,
        [{<<"content-type"/utf8>>, <<"text/html; charset=utf-8"/utf8>>}],
        {text, Html}}.

-file("src/wisp.gleam", 211).
?DOC(
    " Create a JSON response.\n"
    "\n"
    " The body is expected to be valid JSON, though this is not validated.\n"
    " The `content-type` header will be set to `application/json`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " json_response(\"{\\\"name\\\": \\\"Joe\\\"}\", 200)\n"
    " // -> Response(200, [#(\"content-type\", \"application/json\")], Text(body))\n"
    " ```\n"
).
-spec json_response(binary(), integer()) -> gleam@http@response:response(body()).
json_response(Json, Status) ->
    {response,
        Status,
        [{<<"content-type"/utf8>>, <<"application/json; charset=utf-8"/utf8>>}],
        {text, Json}}.

-file("src/wisp.gleam", 232).
?DOC(
    " Set the body of a response to a given HTML document, and set the\n"
    " `content-type` header to `text/html`.\n"
    "\n"
    " The body is expected to be valid HTML, though this is not validated.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " response(201)\n"
    " |> html_body(\"<h1>Hello, Joe!</h1>\")\n"
    " // -> Response(201, [#(\"content-type\", \"text/html; charset=utf-8\")], Text(body))\n"
    " ```\n"
).
-spec html_body(gleam@http@response:response(body()), binary()) -> gleam@http@response:response(body()).
html_body(Response, Html) ->
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_body(_pipe, {text, Html}),
    gleam@http@response:set_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        <<"text/html; charset=utf-8"/utf8>>
    ).

-file("src/wisp.gleam", 251).
?DOC(
    " Set the body of a response to a given JSON document, and set the\n"
    " `content-type` header to `application/json`.\n"
    "\n"
    " The body is expected to be valid JSON, though this is not validated.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " response(201)\n"
    " |> json_body(\"{\\\"name\\\": \\\"Joe\\\"}\")\n"
    " // -> Response(201, [#(\"content-type\", \"application/json; charset=utf-8\")], Text(body))\n"
    " ```\n"
).
-spec json_body(gleam@http@response:response(body()), binary()) -> gleam@http@response:response(body()).
json_body(Response, Json) ->
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_body(_pipe, {text, Json}),
    gleam@http@response:set_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        <<"application/json; charset=utf-8"/utf8>>
    ).

-file("src/wisp.gleam", 271).
?DOC(
    " Set the body of a response to a given string tree.\n"
    "\n"
    " You likely want to also set the request `content-type` header to an\n"
    " appropriate value for the format of the content.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let body = string_tree.from_string(\"Hello, Joe!\")\n"
    " response(201)\n"
    " |> string_tree_body(body)\n"
    " // -> Response(201, [], Text(body))\n"
    " ```\n"
).
-spec string_tree_body(
    gleam@http@response:response(body()),
    gleam@string_tree:string_tree()
) -> gleam@http@response:response(body()).
string_tree_body(Response, Content) ->
    _pipe = Response,
    gleam@http@response:set_body(
        _pipe,
        {bytes, gleam_stdlib:wrap_list(Content)}
    ).

-file("src/wisp.gleam", 290).
?DOC(
    " Set the body of a response to a given string.\n"
    "\n"
    " You likely want to also set the request `content-type` header to an\n"
    " appropriate value for the format of the content.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let body =\n"
    " response(201)\n"
    " |> string_body(\"Hello, Joe!\")\n"
    " // -> Response(201, [], Text(\"Hello, Joe\"))\n"
    " ```\n"
).
-spec string_body(gleam@http@response:response(body()), binary()) -> gleam@http@response:response(body()).
string_body(Response, Content) ->
    _pipe = Response,
    gleam@http@response:set_body(_pipe, {text, Content}).

-file("src/wisp.gleam", 307).
?DOC(
    " Escape a string so that it can be safely included in a HTML document.\n"
    "\n"
    " Any content provided by the user should be escaped before being included in\n"
    " a HTML document to prevent cross-site scripting attacks.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " escape_html(\"<h1>Hello, Joe!</h1>\")\n"
    " // -> \"&lt;h1&gt;Hello, Joe!&lt;/h1&gt;\"\n"
    " ```\n"
).
-spec escape_html(binary()) -> binary().
escape_html(Content) ->
    houdini:escape(Content).

-file("src/wisp.gleam", 324).
?DOC(
    " Create a response with status code 405: Method Not Allowed. Use this\n"
    " when a request does not have an appropriate method to be handled.\n"
    "\n"
    " The `allow` header will be set to a comma separated list of the permitted\n"
    " methods.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " method_not_allowed(allowed: [Get, Post])\n"
    " // -> Response(405, [#(\"allow\", \"GET, POST\")], Text(\"Method not allowed\"))\n"
    " ```\n"
).
-spec method_not_allowed(list(gleam@http:method())) -> gleam@http@response:response(body()).
method_not_allowed(Methods) ->
    Allowed = begin
        _pipe = Methods,
        _pipe@1 = gleam@list:map(_pipe, fun gleam@http:method_to_string/1),
        _pipe@2 = gleam@list:sort(_pipe@1, fun gleam@string:compare/2),
        _pipe@3 = gleam@string:join(_pipe@2, <<", "/utf8>>),
        string:uppercase(_pipe@3)
    end,
    {response,
        405,
        [{<<"allow"/utf8>>, Allowed}],
        {text, <<"Method not allowed"/utf8>>}}.

-file("src/wisp.gleam", 430).
?DOC(
    " Create a response with status code 204: No content.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " no_content()\n"
    " // -> Response(204, [], Text(\"\"))\n"
    " ```\n"
).
-spec no_content() -> gleam@http@response:response(body()).
no_content() ->
    {response, 204, [], {text, <<""/utf8>>}}.

-file("src/wisp.gleam", 577).
-spec buffered_read(buffered_reader(), integer()) -> {ok, wisp@internal:read()} |
    {error, nil}.
buffered_read(Reader, Chunk_size) ->
    case erlang:element(3, Reader) of
        <<>> ->
            (erlang:element(2, Reader))(Chunk_size);

        _ ->
            {ok, {chunk, erlang:element(3, Reader), erlang:element(2, Reader)}}
    end.

-file("src/wisp.gleam", 597).
?DOC(
    " Set the maximum permitted size of a request body of the request in bytes.\n"
    "\n"
    " If a body is larger than this size attempting to read the body will result\n"
    " in a response with status code 413: Content too large will be returned to the\n"
    " client.\n"
    "\n"
    " This limit only applies for headers and bodies that get read into memory.\n"
    " Part of a multipart body that contain files and so are streamed to disc\n"
    " instead use the `max_files_size` limit.\n"
).
-spec set_max_body_size(
    gleam@http@request:request(wisp@internal:connection()),
    integer()
) -> gleam@http@request:request(wisp@internal:connection()).
set_max_body_size(Request, Size) ->
    _pipe = begin
        _record = erlang:element(4, Request),
        {connection,
            erlang:element(2, _record),
            Size,
            erlang:element(4, _record),
            erlang:element(5, _record),
            erlang:element(6, _record),
            erlang:element(7, _record)}
    end,
    gleam@http@request:set_body(Request, _pipe).

-file("src/wisp.gleam", 604).
?DOC(" Get the maximum permitted size of a request body of the request in bytes.\n").
-spec get_max_body_size(gleam@http@request:request(wisp@internal:connection())) -> integer().
get_max_body_size(Request) ->
    erlang:element(3, erlang:element(4, Request)).

-file("src/wisp.gleam", 618).
?DOC(
    " Set the secret key base used to sign cookies and other sensitive data.\n"
    "\n"
    " This key must be at least 64 bytes long and should be kept secret. Anyone\n"
    " with this secret will be able to manipulate signed cookies and other sensitive\n"
    " data.\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic if the key is less than 64 bytes long.\n"
).
-spec set_secret_key_base(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> gleam@http@request:request(wisp@internal:connection()).
set_secret_key_base(Request, Key) ->
    case erlang:byte_size(Key) < 64 of
        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Secret key base must be at least 64 bytes long"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"wisp"/utf8>>,
                    function => <<"set_secret_key_base"/utf8>>,
                    line => 620});

        false ->
            _pipe = begin
                _record = erlang:element(4, Request),
                {connection,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    Key,
                    erlang:element(7, _record)}
            end,
            gleam@http@request:set_body(Request, _pipe)
    end.

-file("src/wisp.gleam", 629).
?DOC(" Get the secret key base used to sign cookies and other sensitive data.\n").
-spec get_secret_key_base(
    gleam@http@request:request(wisp@internal:connection())
) -> binary().
get_secret_key_base(Request) ->
    erlang:element(6, erlang:element(4, Request)).

-file("src/wisp.gleam", 643).
?DOC(
    " Set the maximum permitted size of all files uploaded by a request, in bytes.\n"
    "\n"
    " If a request contains fails which are larger in total than this size\n"
    " then attempting to read the body will result in a response with status code\n"
    " 413: Content too large will be returned to the client.\n"
    "\n"
    " This limit only applies for files in a multipart body that get streamed to\n"
    " disc. For headers and other content that gets read into memory use the\n"
    " `max_body_size` limit.\n"
).
-spec set_max_files_size(
    gleam@http@request:request(wisp@internal:connection()),
    integer()
) -> gleam@http@request:request(wisp@internal:connection()).
set_max_files_size(Request, Size) ->
    _pipe = begin
        _record = erlang:element(4, Request),
        {connection,
            erlang:element(2, _record),
            erlang:element(3, _record),
            Size,
            erlang:element(5, _record),
            erlang:element(6, _record),
            erlang:element(7, _record)}
    end,
    gleam@http@request:set_body(Request, _pipe).

-file("src/wisp.gleam", 651).
?DOC(
    " Get the maximum permitted total size of a files uploaded by a request in\n"
    " bytes.\n"
).
-spec get_max_files_size(gleam@http@request:request(wisp@internal:connection())) -> integer().
get_max_files_size(Request) ->
    erlang:element(4, erlang:element(4, Request)).

-file("src/wisp.gleam", 663).
?DOC(
    " The the size limit for each chunk of the request body when read from the\n"
    " client.\n"
    "\n"
    " This value is passed to the underlying web server when reading the body and\n"
    " the exact size of chunks read depends on the server implementation. It most\n"
    " likely will read chunks smaller than this size if not yet enough data has\n"
    " been received from the client.\n"
).
-spec set_read_chunk_size(
    gleam@http@request:request(wisp@internal:connection()),
    integer()
) -> gleam@http@request:request(wisp@internal:connection()).
set_read_chunk_size(Request, Size) ->
    _pipe = begin
        _record = erlang:element(4, Request),
        {connection,
            erlang:element(2, _record),
            erlang:element(3, _record),
            erlang:element(4, _record),
            Size,
            erlang:element(6, _record),
            erlang:element(7, _record)}
    end,
    gleam@http@request:set_body(Request, _pipe).

-file("src/wisp.gleam", 671).
?DOC(
    " Get the size limit for each chunk of the request body when read from the\n"
    " client.\n"
).
-spec get_read_chunk_size(
    gleam@http@request:request(wisp@internal:connection())
) -> integer().
get_read_chunk_size(Request) ->
    erlang:element(5, erlang:element(4, Request)).

-file("src/wisp.gleam", 693).
?DOC(
    " This middleware function ensures that the request has a specific HTTP\n"
    " method, returning a response with status code 405: Method not allowed\n"
    " if the method is not correct.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   use <- wisp.require_method(request, http.Patch)\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec require_method(
    gleam@http@request:request(any()),
    gleam@http:method(),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_method(Request, Method, Next) ->
    case erlang:element(2, Request) =:= Method of
        true ->
            Next();

        false ->
            method_not_allowed([Method])
    end.

-file("src/wisp.gleam", 738).
?DOC(
    " Parse the query parameters of a request into a list of key-value pairs. The\n"
    " `key_find` function in the `gleam/list` stdlib module may be useful for\n"
    " finding values in the list.\n"
    "\n"
    " Query parameter names do not have to be unique and so may appear multiple\n"
    " times in the list.\n"
).
-spec get_query(gleam@http@request:request(wisp@internal:connection())) -> list({binary(),
    binary()}).
get_query(Request) ->
    _pipe = gleam@http@request:get_query(Request),
    gleam@result:unwrap(_pipe, []).

-file("src/wisp.gleam", 768).
?DOC(
    " This function overrides an incoming POST request with a method given in\n"
    " the request's `_method` query paramerter. This is useful as web browsers\n"
    " typically only support GET and POST requests, but our application may\n"
    " expect other HTTP methods that are more semantically correct.\n"
    "\n"
    " The methods PUT, PATCH, and DELETE are accepted for overriding, all others\n"
    " are ignored.\n"
    "\n"
    " The `_method` query paramerter can be specified in a HTML form like so:\n"
    "\n"
    " ```html\n"
    " <form method=\"POST\" action=\"/item/1?_method=DELETE\">\n"
    "   <button type=\"submit\">Delete item</button>\n"
    " </form>\n"
    " ```\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   let request = wisp.method_override(request)\n"
    "   // The method has now been overridden if appropriate\n"
    " }\n"
    " ```\n"
).
-spec method_override(gleam@http@request:request(AEVE)) -> gleam@http@request:request(AEVE).
method_override(Request) ->
    gleam@bool:guard(
        erlang:element(2, Request) /= post,
        Request,
        fun() ->
            _pipe = begin
                gleam@result:'try'(
                    gleam@http@request:get_query(Request),
                    fun(Query) ->
                        gleam@result:'try'(
                            gleam@list:key_find(Query, <<"_method"/utf8>>),
                            fun(Value) ->
                                gleam@result:map(
                                    gleam@http:parse_method(Value),
                                    fun(Method) -> case Method of
                                            put ->
                                                gleam@http@request:set_method(
                                                    Request,
                                                    Method
                                                );

                                            patch ->
                                                gleam@http@request:set_method(
                                                    Request,
                                                    Method
                                                );

                                            delete ->
                                                gleam@http@request:set_method(
                                                    Request,
                                                    Method
                                                );

                                            _ ->
                                                Request
                                        end end
                                )
                            end
                        )
                    end
                )
            end,
            gleam@result:unwrap(_pipe, Request)
        end
    ).

-file("src/wisp.gleam", 881).
-spec read_body_loop(
    fun((integer()) -> {ok, wisp@internal:read()} | {error, nil}),
    integer(),
    integer(),
    bitstring()
) -> {ok, bitstring()} | {error, nil}.
read_body_loop(Reader, Read_chunk_size, Max_body_size, Accumulator) ->
    gleam@result:'try'(Reader(Read_chunk_size), fun(Chunk) -> case Chunk of
                reading_finished ->
                    {ok, Accumulator};

                {chunk, Chunk@1, Next} ->
                    Accumulator@1 = gleam@bit_array:append(Accumulator, Chunk@1),
                    case erlang:byte_size(Accumulator@1) > Max_body_size of
                        true ->
                            {error, nil};

                        false ->
                            read_body_loop(
                                Next,
                                Read_chunk_size,
                                Max_body_size,
                                Accumulator@1
                            )
                    end
            end end).

-file("src/wisp.gleam", 871).
?DOC(
    " Read the entire body of the request as a bit array.\n"
    "\n"
    " You may instead wish to use the `require_bit_array_body` or the\n"
    " `require_string_body` middleware functions instead.\n"
    "\n"
    " This function does not cache the body in any way, so if you call this\n"
    " function (or any other body reading function) more than once it may hang or\n"
    " return an incorrect value, depending on the underlying web server. It is the\n"
    " responsibility of the caller to cache the body if it is needed multiple\n"
    " times.\n"
    "\n"
    " If the body is larger than the `max_body_size` limit then a response\n"
    " with status code 413: Content too large will be returned to the client.\n"
).
-spec read_body_bits(gleam@http@request:request(wisp@internal:connection())) -> {ok,
        bitstring()} |
    {error, nil}.
read_body_bits(Request) ->
    Connection = erlang:element(4, Request),
    read_body_loop(
        erlang:element(2, Connection),
        erlang:element(5, Connection),
        erlang:element(3, Connection),
        <<>>
    ).

-file("src/wisp.gleam", 1250).
-spec sort_keys(list({binary(), AEWV})) -> list({binary(), AEWV}).
sort_keys(Pairs) ->
    gleam@list:sort(
        Pairs,
        fun(A, B) ->
            gleam@string:compare(erlang:element(1, A), erlang:element(1, B))
        end
    ).

-file("src/wisp.gleam", 1318).
-spec atom_dict_decoder() -> gleam@dynamic@decode:decoder(gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_())).
atom_dict_decoder() ->
    Atom@1 = gleam@dynamic@decode:new_primitive_decoder(
        <<"Atom"/utf8>>,
        fun(Data) -> case wisp_ffi:atom_from_dynamic(Data) of
                {ok, Atom} ->
                    {ok, Atom};

                {error, _} ->
                    {error, erlang:binary_to_atom(<<"nil"/utf8>>)}
            end end
    ),
    gleam@dynamic@decode:dict(
        Atom@1,
        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
    ).

-file("src/wisp.gleam", 1484).
?DOC(
    " Parses the content of a range header.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " wisp.parse_range_header(\"bytes=-64\")\n"
    " // -> Ok(Range(offset: -64, limit: option.None))\n"
    " ```\n"
).
-spec parse_range_header(binary()) -> {ok, range()} | {error, nil}.
parse_range_header(Range_header) ->
    case Range_header of
        <<"bytes="/utf8, Range/binary>> ->
            gleam@result:'try'(
                begin
                    _pipe = Range,
                    gleam@string:split_once(_pipe, <<"-"/utf8>>)
                end,
                fun(_use0) ->
                    {Start_str, End_str} = _use0,
                    case {Start_str, End_str} of
                        {<<""/utf8>>, _} ->
                            _pipe@1 = gleam_stdlib:parse_int(End_str),
                            gleam@result:map(
                                _pipe@1,
                                fun(Tail_offset) ->
                                    {range, - Tail_offset, none}
                                end
                            );

                        {_, <<""/utf8>>} ->
                            _pipe@2 = gleam_stdlib:parse_int(Start_str),
                            gleam@result:map(
                                _pipe@2,
                                fun(Offset) -> {range, Offset, none} end
                            );

                        {_, _} ->
                            gleam@result:'try'(
                                gleam_stdlib:parse_int(Start_str),
                                fun(Offset@1) ->
                                    gleam@result:'try'(
                                        gleam_stdlib:parse_int(End_str),
                                        fun(End) ->
                                            {ok,
                                                {range,
                                                    Offset@1,
                                                    {some, (End - Offset@1) + 1}}}
                                        end
                                    )
                                end
                            )
                    end
                end
            );

        _ ->
            {error, nil}
    end.

-file("src/wisp.gleam", 1632).
?DOC(
    " A middleware function that converts `HEAD` requests to `GET` requests,\n"
    " handles the request, and then discards the response body. This is useful so\n"
    " that your application can handle `HEAD` requests without having to implement\n"
    " handlers for them.\n"
    "\n"
    " The `x-original-method` header is set to `\"HEAD\"` for requests that were\n"
    " originally `HEAD` requests.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(req: Request) -> Response {\n"
    "   use req <- wisp.handle_head(req)\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec handle_head(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
handle_head(Req, Handler) ->
    case erlang:element(2, Req) of
        head ->
            _pipe = Req,
            _pipe@1 = gleam@http@request:set_method(_pipe, get),
            _pipe@2 = gleam@http@request:prepend_header(
                _pipe@1,
                <<"x-original-method"/utf8>>,
                <<"HEAD"/utf8>>
            ),
            Handler(_pipe@2);

        _ ->
            Handler(Req)
    end.

-file("src/wisp.gleam", 1657).
?DOC(
    " Create a new temporary directory for the given request.\n"
    "\n"
    " If you are using the Mist adapter or another compliant web server\n"
    " adapter then this file will be deleted for you when the request is complete.\n"
    " Otherwise you will need to call the `delete_temporary_files` function\n"
    " yourself.\n"
).
-spec new_temporary_file(gleam@http@request:request(wisp@internal:connection())) -> {ok,
        binary()} |
    {error, simplifile:file_error()}.
new_temporary_file(Request) ->
    Directory = erlang:element(7, erlang:element(4, Request)),
    gleam@result:'try'(
        simplifile:create_directory_all(Directory),
        fun(_) ->
            Path = filepath:join(Directory, wisp@internal:random_slug()),
            gleam@result:map(simplifile:create_file(Path), fun(_) -> Path end)
        end
    ).

-file("src/wisp.gleam", 1673).
?DOC(
    " Delete any temporary files created for the given request.\n"
    "\n"
    " If you are using the Mist adapter or another compliant web server\n"
    " adapter then this file will be deleted for you when the request is complete.\n"
    " Otherwise you will need to call this function yourself.\n"
).
-spec delete_temporary_files(
    gleam@http@request:request(wisp@internal:connection())
) -> {ok, nil} | {error, simplifile:file_error()}.
delete_temporary_files(Request) ->
    case simplifile_erl:delete(erlang:element(7, erlang:element(4, Request))) of
        {error, enoent} ->
            {ok, nil};

        Other ->
            Other
    end.

-file("src/wisp.gleam", 1708).
?DOC(
    " Configure the Erlang logger, setting the minimum log level to `info`, to be\n"
    " called when your application starts.\n"
    "\n"
    " You may wish to use an alternative for this such as one provided by a more\n"
    " sophisticated logging library.\n"
    "\n"
    " In future this function may be extended to change the output format.\n"
).
-spec configure_logger() -> nil.
configure_logger() ->
    logging_ffi:configure().

-file("src/wisp.gleam", 1729).
-spec log_level_to_logging_log_level(log_level()) -> logging:log_level().
log_level_to_logging_log_level(Log_level) ->
    case Log_level of
        emergency_level ->
            emergency;

        alert_level ->
            alert;

        critical_level ->
            critical;

        error_level ->
            error;

        warning_level ->
            warning;

        notice_level ->
            notice;

        info_level ->
            info;

        debug_level ->
            debug
    end.

-file("src/wisp.gleam", 1748).
?DOC(
    " Set the log level of the Erlang logger to `log_level`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec set_logger_level(log_level()) -> nil.
set_logger_level(Log_level) ->
    logging:set_level(log_level_to_logging_log_level(Log_level)).

-file("src/wisp.gleam", 1758).
?DOC(
    " Log a message to the Erlang logger with the level of `emergency`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_emergency(binary()) -> nil.
log_emergency(Message) ->
    logging:log(emergency, Message).

-file("src/wisp.gleam", 1768).
?DOC(
    " Log a message to the Erlang logger with the level of `alert`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_alert(binary()) -> nil.
log_alert(Message) ->
    logging:log(alert, Message).

-file("src/wisp.gleam", 1778).
?DOC(
    " Log a message to the Erlang logger with the level of `critical`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_critical(binary()) -> nil.
log_critical(Message) ->
    logging:log(critical, Message).

-file("src/wisp.gleam", 1788).
?DOC(
    " Log a message to the Erlang logger with the level of `error`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_error(binary()) -> nil.
log_error(Message) ->
    logging:log(error, Message).

-file("src/wisp.gleam", 1798).
?DOC(
    " Log a message to the Erlang logger with the level of `warning`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_warning(binary()) -> nil.
log_warning(Message) ->
    logging:log(warning, Message).

-file("src/wisp.gleam", 1808).
?DOC(
    " Log a message to the Erlang logger with the level of `notice`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_notice(binary()) -> nil.
log_notice(Message) ->
    logging:log(notice, Message).

-file("src/wisp.gleam", 1818).
?DOC(
    " Log a message to the Erlang logger with the level of `info`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_info(binary()) -> nil.
log_info(Message) ->
    logging:log(info, Message).

-file("src/wisp.gleam", 1358).
?DOC(
    " A middleware function that logs details about the request and response.\n"
    "\n"
    " The format used logged by this middleware may change in future versions of\n"
    " Wisp.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(req: Request) -> Response {\n"
    "   use <- wisp.log_request(req)\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec log_request(
    gleam@http@request:request(wisp@internal:connection()),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
log_request(Req, Handler) ->
    Response = Handler(),
    _pipe = [erlang:integer_to_binary(erlang:element(2, Response)),
        <<" "/utf8>>,
        string:uppercase(gleam@http:method_to_string(erlang:element(2, Req))),
        <<" "/utf8>>,
        erlang:element(8, Req)],
    _pipe@1 = erlang:list_to_binary(_pipe),
    log_info(_pipe@1),
    Response.

-file("src/wisp.gleam", 1828).
?DOC(
    " Log a message to the Erlang logger with the level of `debug`.\n"
    "\n"
    " See the [Erlang logger documentation][1] for more information.\n"
    "\n"
    " [1]: https://www.erlang.org/doc/man/logger\n"
).
-spec log_debug(binary()) -> nil.
log_debug(Message) ->
    logging:log(debug, Message).

-file("src/wisp.gleam", 1838).
?DOC(" Generate a random string of the given length.\n").
-spec random_string(integer()) -> binary().
random_string(Length) ->
    wisp@internal:random_string(Length).

-file("src/wisp.gleam", 1851).
?DOC(
    " Sign a message which can later be verified using the `verify_signed_message`\n"
    " function to detect if the message has been tampered with.\n"
    "\n"
    " Signed messages are not encrypted and can be read by anyone. They are not\n"
    " suitable for storing sensitive information.\n"
    "\n"
    " This function uses the secret key base from the request. If the secret\n"
    " changes then the signature will no longer be verifiable.\n"
).
-spec sign_message(
    gleam@http@request:request(wisp@internal:connection()),
    bitstring(),
    gleam@crypto:hash_algorithm()
) -> binary().
sign_message(Request, Message, Algorithm) ->
    gleam@crypto:sign_message(
        Message,
        <<(erlang:element(6, erlang:element(4, Request)))/binary>>,
        Algorithm
    ).

-file("src/wisp.gleam", 1867).
?DOC(
    " Verify a signed message which was signed using the `sign_message` function.\n"
    "\n"
    " Returns the content of the message if the signature is valid, otherwise\n"
    " returns an error.\n"
    "\n"
    " This function uses the secret key base from the request. If the secret\n"
    " changes then the signature will no longer be verifiable.\n"
).
-spec verify_signed_message(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> {ok, bitstring()} | {error, nil}.
verify_signed_message(Request, Message) ->
    gleam@crypto:verify_signed_message(
        Message,
        <<(erlang:element(6, erlang:element(4, Request)))/binary>>
    ).

-file("src/wisp.gleam", 1924).
?DOC(
    " Set a cookie on the response. After `max_age` seconds the cookie will be\n"
    " expired by the client.\n"
    "\n"
    " If you wish for more control over the cookie attributes then you may want\n"
    " to use the `gleam/http/cookie` module from the `gleam_http` package.\n"
    "\n"
    " # Security\n"
    "\n"
    " - `PlainText`: the cookie value is base64 encoded. This permits use of any\n"
    "    characters in the cookie, but it is possible for the client to edit the\n"
    "    cookie, potentially maliciously.\n"
    " - `Signed`: the cookie value will be signed with `sign_message` and so\n"
    "    cannot be tampered with by the client.\n"
    "\n"
    " # `Secure` cookie attribute\n"
    "\n"
    " This function sets the `Secure` cookie attribute (with one exception detailed\n"
    " below), ensuring that browsers will only send the cookie over HTTPS\n"
    " connections.\n"
    "\n"
    " Most browsers consider localhost to be secure and permit `Secure` cookies\n"
    " for those requests as well, but Safari does not. For cookies to work in\n"
    " development for programmers using a browser like Safari the `Secure`\n"
    " attribute will not be set if all these conditions are met:\n"
    "\n"
    " - The request scheme is `http://`.\n"
    " - The request host is `localhost`, `127.0.0.1`, or `[::1]`.\n"
    " - The `x-forwarded-proto` header has not been set, indicating that the\n"
    "   request is not from a reverse proxy such as Caddy or Nginx.\n"
    "\n"
    " # Examples\n"
    "\n"
    " Setting a plain text cookie that the client can read and modify:\n"
    "\n"
    " ```gleam\n"
    " wisp.ok()\n"
    " |> wisp.set_cookie(request, \"id\", \"123\", wisp.PlainText, 60 * 60)\n"
    " ```\n"
    "\n"
    " Setting a signed cookie that the client can read but not modify:\n"
    "\n"
    " ```gleam\n"
    " wisp.ok()\n"
    " |> wisp.set_cookie(request, \"id\", value, wisp.Signed, 60 * 60)\n"
    " ```\n"
).
-spec set_cookie(
    gleam@http@response:response(body()),
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    security(),
    integer()
) -> gleam@http@response:response(body()).
set_cookie(Response, Request, Name, Value, Security, Max_age) ->
    Scheme = case erlang:element(6, Request) of
        <<"localhost"/utf8>> when erlang:element(5, Request) =:= http ->
            case gleam@http@request:get_header(
                Request,
                <<"x-forwarded-proto"/utf8>>
            ) of
                {ok, _} ->
                    https;

                {error, _} ->
                    http
            end;

        <<"127.0.0.1"/utf8>> when erlang:element(5, Request) =:= http ->
            case gleam@http@request:get_header(
                Request,
                <<"x-forwarded-proto"/utf8>>
            ) of
                {ok, _} ->
                    https;

                {error, _} ->
                    http
            end;

        <<"[::1]"/utf8>> when erlang:element(5, Request) =:= http ->
            case gleam@http@request:get_header(
                Request,
                <<"x-forwarded-proto"/utf8>>
            ) of
                {ok, _} ->
                    https;

                {error, _} ->
                    http
            end;

        _ ->
            https
    end,
    Attributes = begin
        _record = gleam@http@cookie:defaults(Scheme),
        {attributes,
            {some, Max_age},
            erlang:element(3, _record),
            erlang:element(4, _record),
            erlang:element(5, _record),
            erlang:element(6, _record),
            erlang:element(7, _record)}
    end,
    Value@1 = case Security of
        plain_text ->
            gleam_stdlib:base64_encode(<<Value/binary>>, false);

        signed ->
            sign_message(Request, <<Value/binary>>, sha512)
    end,
    _pipe = Response,
    gleam@http@response:set_cookie(_pipe, Name, Value@1, Attributes).

-file("src/wisp.gleam", 1977).
?DOC(
    " Get a cookie from the request.\n"
    "\n"
    " If a cookie is missing, found to be malformed, or the signature is invalid\n"
    " for a signed cookie, then `Error(Nil)` is returned.\n"
    "\n"
    " # Security\n"
    "\n"
    " - `PlainText`: the cookie value is expected to be base64 encoded.\n"
    " - `Signed`: the cookie value is expected to be signed with `sign_message`.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " wisp.get_cookie(request, \"group\", wisp.PlainText)\n"
    " // -> Ok(\"A\")\n"
    " ```\n"
).
-spec get_cookie(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    security()
) -> {ok, binary()} | {error, nil}.
get_cookie(Request, Name, Security) ->
    gleam@result:'try'(
        begin
            _pipe = Request,
            _pipe@1 = gleam@http@request:get_cookies(_pipe),
            gleam@list:key_find(_pipe@1, Name)
        end,
        fun(Value) -> gleam@result:'try'(case Security of
                    plain_text ->
                        gleam@bit_array:base64_decode(Value);

                    signed ->
                        verify_signed_message(Request, Value)
                end, fun(Value@1) -> gleam@bit_array:to_string(Value@1) end) end
    ).

-file("src/wisp.gleam", 2000).
?DOC(false).
-spec create_canned_connection(bitstring(), binary()) -> wisp@internal:connection().
create_canned_connection(Body, Secret_key_base) ->
    wisp@internal:make_connection(
        fun(_) -> {ok, {chunk, Body, fun(_) -> {ok, reading_finished} end}} end,
        Secret_key_base
    ).

-file("src/wisp.gleam", 343).
?DOC(
    " Create a response with status code 200: OK.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " ok()\n"
    " // -> Response(200, [#(\"content-type\", \"text/plain\")], Text(\"OK\"))\n"
    " ```\n"
).
-spec ok() -> gleam@http@response:response(body()).
ok() ->
    {response,
        200,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"OK"/utf8>>}}.

-file("src/wisp.gleam", 356).
?DOC(
    " Create a response with status code 201: Created.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " created()\n"
    " // -> Response(201, [#(\"content-type\", \"text/plain\")], Text(\"Created\"))\n"
    " ```\n"
).
-spec created() -> gleam@http@response:response(body()).
created() ->
    {response,
        201,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Created"/utf8>>}}.

-file("src/wisp.gleam", 369).
?DOC(
    " Create a response with status code 202: Accepted.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " accepted()\n"
    " // -> Response(202, [#(\"content-type\", \"text/plain\")], Text(\"Accepted\"))\n"
    " ```\n"
).
-spec accepted() -> gleam@http@response:response(body()).
accepted() ->
    {response,
        202,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Accepted"/utf8>>}}.

-file("src/wisp.gleam", 387).
?DOC(
    " Create a response with status code 303: See Other, and the `location`\n"
    " header set to the given URL. Used to redirect the client to another page.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " redirect(to: \"https://example.com\")\n"
    " // -> Response(\n"
    " //   303,\n"
    " //   [#(\"location\", \"https://example.com\"), #(\"context-type\", \"text/plain\")],\n"
    " //   Text(\"You are being redirected: https://example.com\"),\n"
    " // )\n"
    " ```\n"
).
-spec redirect(binary()) -> gleam@http@response:response(body()).
redirect(Url) ->
    {response,
        303,
        [{<<"location"/utf8>>, Url},
            {<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"You are being redirected: "/utf8, Url/binary>>}}.

-file("src/wisp.gleam", 413).
?DOC(
    " Create a response with status code 308: Permanent redirect, and the\n"
    " `location` header set to the given URL. Used to redirect the client to\n"
    " another page.\n"
    "\n"
    " This redirect is permanent and the client is expected to cache the new\n"
    " location, using it for future requests.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " moved_permanently(to: \"https://example.com\")\n"
    " // -> Response(\n"
    " //   303,\n"
    " //   [#(\"location\", \"https://example.com\")],\n"
    " //   Text(\"You are being redirected: https://example.com\"),\n"
    " // )\n"
    " ```\n"
).
-spec permanent_redirect(binary()) -> gleam@http@response:response(body()).
permanent_redirect(Url) ->
    {response,
        308,
        [{<<"location"/utf8>>, Url},
            {<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"You are being redirected: "/utf8, Url/binary>>}}.

-file("src/wisp.gleam", 443).
?DOC(
    " Create a response with status code 404: Not found.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " not_found()\n"
    " // -> Response(404, [#(\"content-type\", \"text/plain\")], Text(\"Not found\"))\n"
    " ```\n"
).
-spec not_found() -> gleam@http@response:response(body()).
not_found() ->
    {response,
        404,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Not found"/utf8>>}}.

-file("src/wisp.gleam", 456).
?DOC(
    " Create a response with status code 400: Bad request.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " bad_request(\"Invalid JSON\")\n"
    " // -> Response(400, [#(\"content-type\", \"text/plain\")], Text(\"Bad request: Invalid JSON\"))\n"
    " ```\n"
).
-spec bad_request(binary()) -> gleam@http@response:response(body()).
bad_request(Detail) ->
    Body = case Detail of
        <<""/utf8>> ->
            <<"Bad request"/utf8>>;

        _ ->
            <<"Bad request: "/utf8, Detail/binary>>
    end,
    {response,
        400,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, Body}}.

-file("src/wisp.gleam", 473).
?DOC(
    " Create a response with status code 413: Content too large.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " content_too_large()\n"
    " // -> Response(413, [#(\"content-type\", \"text/plain\")], Text(\"Content too large\"))\n"
    " ```\n"
).
-spec content_too_large() -> gleam@http@response:response(body()).
content_too_large() ->
    {response,
        413,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Content too large"/utf8>>}}.

-file("src/wisp.gleam", 489).
?DOC(
    " Create a response with status code 415: Unsupported media type.\n"
    "\n"
    " The `allow` header will be set to a comma separated list of the permitted\n"
    " content-types.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " unsupported_media_type(accept: [\"application/json\", \"text/plain\"])\n"
    " // -> Response(415, [#(\"allow\", \"application/json, text/plain\")], Text(\"Unsupported media type\"))\n"
    " ```\n"
).
-spec unsupported_media_type(list(binary())) -> gleam@http@response:response(body()).
unsupported_media_type(Acceptable) ->
    Acceptable@1 = gleam@string:join(Acceptable, <<", "/utf8>>),
    {response,
        415,
        [{<<"accept"/utf8>>, Acceptable@1},
            {<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Unsupported media type"/utf8>>}}.

-file("src/wisp.gleam", 507).
?DOC(
    " Create a response with status code 422: Unprocessable content.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " unprocessable_content()\n"
    " // -> Response(422, [#(\"content-type\", \"text/plain\")], Text(\"Unprocessable content\"))\n"
    " ```\n"
).
-spec unprocessable_content() -> gleam@http@response:response(body()).
unprocessable_content() ->
    {response,
        422,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Unprocessable content"/utf8>>}}.

-file("src/wisp.gleam", 520).
?DOC(
    " Create a response with status code 500: Internal server error.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " internal_server_error()\n"
    " // -> Response(500, [#(\"content-type\", \"text/plain\")], Text(\"Internal server error\"))\n"
    " ```\n"
).
-spec internal_server_error() -> gleam@http@response:response(body()).
internal_server_error() ->
    {response,
        500,
        [{<<"content-type"/utf8>>, <<"text/plain"/utf8>>}],
        {text, <<"Internal server error"/utf8>>}}.

-file("src/wisp.gleam", 562).
-spec decrement_body_quota(quotas(), integer()) -> {ok, quotas()} |
    {error, gleam@http@response:response(body())}.
decrement_body_quota(Quotas, Size) ->
    Quotas@1 = {quotas,
        erlang:element(2, Quotas) - Size,
        erlang:element(3, Quotas)},
    case erlang:element(2, Quotas@1) < 0 of
        true ->
            {error, content_too_large()};

        false ->
            {ok, Quotas@1}
    end.

-file("src/wisp.gleam", 570).
-spec decrement_quota(integer(), integer()) -> {ok, integer()} |
    {error, gleam@http@response:response(body())}.
decrement_quota(Quota, Size) ->
    case Quota - Size of
        Quota@1 when Quota@1 < 0 ->
            {error, content_too_large()};

        Quota@2 ->
            {ok, Quota@2}
    end.

-file("src/wisp.gleam", 845).
?DOC(
    " A middleware function which reads the entire body of the request as a bit\n"
    " string.\n"
    "\n"
    " This function does not cache the body in any way, so if you call this\n"
    " function (or any other body reading function) more than once it may hang or\n"
    " return an incorrect value, depending on the underlying web server. It is the\n"
    " responsibility of the caller to cache the body if it is needed multiple\n"
    " times.\n"
    "\n"
    " If the body is larger than the `max_body_size` limit then a response\n"
    " with status code 413: Content too large will be returned to the client.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   use body <- wisp.require_string_body(request)\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec require_bit_array_body(
    gleam@http@request:request(wisp@internal:connection()),
    fun((bitstring()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_bit_array_body(Request, Next) ->
    case read_body_bits(Request) of
        {ok, Body} ->
            Next(Body);

        {error, _} ->
            content_too_large()
    end.

-file("src/wisp.gleam", 971).
?DOC(
    " This middleware function ensures that the request has a value for the\n"
    " `content-type` header, returning a response with status code 415:\n"
    " Unsupported media type if the header is not the expected value\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   use <- wisp.require_content_type(request, \"application/json\")\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec require_content_type(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_content_type(Request, Expected, Next) ->
    case gleam@list:key_find(
        erlang:element(3, Request),
        <<"content-type"/utf8>>
    ) of
        {ok, Content_type} ->
            case gleam@string:split_once(Content_type, <<";"/utf8>>) of
                {ok, {Content_type@1, _}} when Content_type@1 =:= Expected ->
                    Next();

                _ when Content_type =:= Expected ->
                    Next();

                _ ->
                    unsupported_media_type([Expected])
            end;

        _ ->
            unsupported_media_type([Expected])
    end.

-file("src/wisp.gleam", 1129).
-spec or_500({ok, AEVP} | {error, any()}) -> {ok, AEVP} |
    {error, gleam@http@response:response(body())}.
or_500(Result) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            log_error(gleam@string:inspect(Error)),
            {error, internal_server_error()}
    end.

-file("src/wisp.gleam", 1120).
-spec multipart_file_append(binary(), bitstring()) -> {ok, binary()} |
    {error, gleam@http@response:response(body())}.
multipart_file_append(Path, Chunk) ->
    _pipe = simplifile_erl:append_bits(Path, Chunk),
    _pipe@1 = or_500(_pipe),
    gleam@result:replace(_pipe@1, Path).

-file("src/wisp.gleam", 1181).
-spec fn_with_bad_request_error(
    fun((AEWD) -> {ok, AEWE} | {error, any()}),
    binary()
) -> fun((AEWD) -> {ok, AEWE} | {error, gleam@http@response:response(body())}).
fn_with_bad_request_error(F, Error) ->
    fun(A) -> case F(A) of
            {ok, X} ->
                {ok, X};

            {error, _} ->
                {error, bad_request(Error)}
        end end.

-file("src/wisp.gleam", 1293).
?DOC(
    " A middleware function that rescues crashes and returns a response\n"
    " with status code 500: Internal server error.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(req: Request) -> Response {\n"
    "   use <- wisp.rescue_crashes\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec rescue_crashes(fun(() -> gleam@http@response:response(body()))) -> gleam@http@response:response(body()).
rescue_crashes(Handler) ->
    case exception_ffi:rescue(Handler) of
        {ok, Response} ->
            Response;

        {error, Error} ->
            {Kind, Detail@3} = case Error of
                {errored, Detail} ->
                    {errored, Detail};

                {thrown, Detail@1} ->
                    {thrown, Detail@1};

                {exited, Detail@2} ->
                    {exited, Detail@2}
            end,
            case gleam@dynamic@decode:run(Detail@3, atom_dict_decoder()) of
                {ok, Details} ->
                    C = erlang:binary_to_atom(<<"class"/utf8>>),
                    logger:error(
                        gleam@dict:insert(
                            Details,
                            C,
                            gleam@function:identity(Kind)
                        )
                    ),
                    nil;

                {error, _} ->
                    log_error(gleam@string:inspect(Error))
            end,
            internal_server_error()
    end.

-file("src/wisp.gleam", 808).
?DOC(
    " A middleware function which reads the entire body of the request as a string.\n"
    "\n"
    " This function does not cache the body in any way, so if you call this\n"
    " function (or any other body reading function) more than once it may hang or\n"
    " return an incorrect value, depending on the underlying web server. It is the\n"
    " responsibility of the caller to cache the body if it is needed multiple\n"
    " times.\n"
    "\n"
    " If the body is larger than the `max_body_size` limit then a response\n"
    " with status code 413: Content too large will be returned to the client.\n"
    "\n"
    " If the body is found not to be valid UTF-8 then a response with\n"
    " status code 400: Bad request will be returned to the client.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   use body <- wisp.require_string_body(request)\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec require_string_body(
    gleam@http@request:request(wisp@internal:connection()),
    fun((binary()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_string_body(Request, Next) ->
    case read_body_bits(Request) of
        {ok, Body} ->
            case gleam@bit_array:to_string(Body) of
                {ok, Body@1} ->
                    Next(Body@1);

                {error, _} ->
                    bad_request(<<"Invalid UTF-8"/utf8>>)
            end;

        {error, _} ->
            content_too_large()
    end.

-file("src/wisp.gleam", 1013).
?DOC(
    " A middleware which extracts JSON from the body of a request.\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   use json <- wisp.require_json(request)\n"
    "   // decode and use JSON here...\n"
    " }\n"
    " ```\n"
    "\n"
    " The `set_max_body_size` and `set_read_chunk_size` can be used to configure\n"
    " the reading of the request body.\n"
    "\n"
    " If the request does not have the `content-type` set to `application/json` a\n"
    " response with status code 415: Unsupported media type will be returned\n"
    " to the client.\n"
    "\n"
    " If the request body is larger than the `max_body_size` or `max_files_size`\n"
    " limits then a response with status code 413: Content too large will be\n"
    " returned to the client.\n"
    "\n"
    " If the body cannot be parsed successfully then a response with status\n"
    " code 400: Bad request will be returned to the client.\n"
).
-spec require_json(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@dynamic:dynamic_()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_json(Request, Next) ->
    require_content_type(
        Request,
        <<"application/json"/utf8>>,
        fun() ->
            require_string_body(
                Request,
                fun(Body) ->
                    case gleam@json:parse(
                        Body,
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1}
                    ) of
                        {ok, Json} ->
                            Next(Json);

                        {error, _} ->
                            bad_request(<<"Invalid JSON"/utf8>>)
                    end
                end
            )
        end
    ).

-file("src/wisp.gleam", 1022).
-spec require_urlencoded_form(
    gleam@http@request:request(wisp@internal:connection()),
    fun((form_data()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_urlencoded_form(Request, Next) ->
    require_string_body(
        Request,
        fun(Body) -> case gleam_stdlib:parse_query(Body) of
                {ok, Pairs} ->
                    Pairs@1 = sort_keys(Pairs),
                    Next({form_data, Pairs@1, []});

                {error, _} ->
                    bad_request(<<"Invalid form encoding"/utf8>>)
            end end
    ).

-file("src/wisp.gleam", 1523).
?DOC(
    " Checks for the `range` header and handles partial file reads.\n"
    "\n"
    " If the range request header is present, it will set the `accept-ranges`,\n"
    " `content-range`, and `content-length` response headers. If the range\n"
    " request header has a range that is out of bounds of the file, it will\n"
    " respond with a `416 Range Not Satisfiable`.\n"
    "\n"
    " If the header isn't present, it returns the input response without changes.\n"
).
-spec handle_file_range_header(
    gleam@http@response:response(body()),
    gleam@http@request:request(wisp@internal:connection()),
    simplifile:file_info(),
    binary()
) -> gleam@http@response:response(body()).
handle_file_range_header(Resp, Req, File_info, Path) ->
    Result = begin
        gleam@result:'try'(
            begin
                _pipe = gleam@http@request:get_header(Req, <<"range"/utf8>>),
                gleam@result:replace_error(_pipe, Resp)
            end,
            fun(Raw_range) ->
                gleam@result:'try'(
                    begin
                        _pipe@1 = parse_range_header(Raw_range),
                        gleam@result:replace_error(
                            _pipe@1,
                            bad_request(<<"Invalid range"/utf8>>)
                        )
                    end,
                    fun(Range) ->
                        Range@1 = case erlang:element(2, Range) < 0 of
                            true ->
                                {range,
                                    erlang:element(2, File_info) + erlang:element(
                                        2,
                                        Range
                                    ),
                                    erlang:element(3, Range)};

                            false ->
                                Range
                        end,
                        End_is_invalid = begin
                            _pipe@2 = erlang:element(3, Range@1),
                            _pipe@3 = gleam@option:map(
                                _pipe@2,
                                fun(End) ->
                                    ((End < 0) orelse (End >= erlang:element(
                                        2,
                                        File_info
                                    )))
                                    orelse (End < erlang:element(2, Range@1))
                                end
                            ),
                            gleam@option:unwrap(_pipe@3, false)
                        end,
                        gleam@bool:guard(
                            ((erlang:element(2, Range@1) < 0) orelse (erlang:element(
                                2,
                                Range@1
                            )
                            >= erlang:element(2, File_info)))
                            orelse End_is_invalid,
                            {error,
                                begin
                                    _pipe@4 = response(416),
                                    gleam@http@response:prepend_header(
                                        _pipe@4,
                                        <<"range"/utf8>>,
                                        <<"bytes=*"/utf8>>
                                    )
                                end},
                            fun() ->
                                Content_range = begin
                                    End@1 = case erlang:element(3, Range@1) of
                                        {some, L} ->
                                            _pipe@5 = ((erlang:element(
                                                2,
                                                Range@1
                                            )
                                            + L)
                                            - 1),
                                            erlang:integer_to_binary(_pipe@5);

                                        none ->
                                            _pipe@6 = (erlang:element(
                                                2,
                                                File_info
                                            )
                                            - 1),
                                            _pipe@7 = gleam@int:max(_pipe@6, 0),
                                            erlang:integer_to_binary(_pipe@7)
                                    end,
                                    <<<<<<<<<<"bytes "/utf8,
                                                        (erlang:integer_to_binary(
                                                            erlang:element(
                                                                2,
                                                                Range@1
                                                            )
                                                        ))/binary>>/binary,
                                                    "-"/utf8>>/binary,
                                                End@1/binary>>/binary,
                                            "/"/utf8>>/binary,
                                        (erlang:integer_to_binary(
                                            erlang:element(2, File_info)
                                        ))/binary>>
                                end,
                                Content_length = case erlang:element(3, Range@1) of
                                    {some, L@1} ->
                                        erlang:integer_to_binary(L@1);

                                    none ->
                                        erlang:integer_to_binary(
                                            erlang:element(2, File_info) - erlang:element(
                                                2,
                                                Range@1
                                            )
                                        )
                                end,
                                _pipe@8 = {response,
                                    206,
                                    erlang:element(3, Resp),
                                    {file,
                                        Path,
                                        erlang:element(2, Range@1),
                                        erlang:element(3, Range@1)}},
                                _pipe@9 = gleam@http@response:set_header(
                                    _pipe@8,
                                    <<"content-length"/utf8>>,
                                    Content_length
                                ),
                                _pipe@10 = gleam@http@response:set_header(
                                    _pipe@9,
                                    <<"accept-ranges"/utf8>>,
                                    <<"bytes"/utf8>>
                                ),
                                _pipe@11 = gleam@http@response:set_header(
                                    _pipe@10,
                                    <<"content-range"/utf8>>,
                                    Content_range
                                ),
                                {ok, _pipe@11}
                            end
                        )
                    end
                )
            end
        )
    end,
    case Result of
        {error, Response} ->
            Response;

        {ok, Response@1} ->
            Response@1
    end.

-file("src/wisp.gleam", 1193).
-spec multipart_content_disposition(list({binary(), binary()})) -> {ok,
        {binary(), gleam@option:option(binary())}} |
    {error, gleam@http@response:response(body())}.
multipart_content_disposition(Headers) ->
    _pipe = begin
        gleam@result:'try'(
            gleam@list:key_find(Headers, <<"content-disposition"/utf8>>),
            fun(Header) ->
                gleam@result:'try'(
                    gleam@http:parse_content_disposition(Header),
                    fun(Header@1) ->
                        gleam@result:map(
                            gleam@list:key_find(
                                erlang:element(3, Header@1),
                                <<"name"/utf8>>
                            ),
                            fun(Name) ->
                                Filename = gleam@option:from_result(
                                    gleam@list:key_find(
                                        erlang:element(3, Header@1),
                                        <<"filename"/utf8>>
                                    )
                                ),
                                {Name, Filename}
                            end
                        )
                    end
                )
            end
        )
    end,
    gleam@result:replace_error(
        _pipe,
        bad_request(<<"Invalid content-disposition"/utf8>>)
    ).

-file("src/wisp.gleam", 2037).
?DOC(
    " Cross-Site Request Forgery (CSRF) attacks by checking the `host` request\n"
    " header against the `origin` header or `referer` header.\n"
    "\n"
    " - Requests with the `Get` and `Head` methods are accepted.\n"
    " - Requests with no `host` header are rejected with status 400: Bad Request.\n"
    " - Requests with no `origin` or `referer` headers are accepted, but have the\n"
    "   `cookie` header removed to prevent CSRF attacks against cookie based\n"
    "   sessions.\n"
    " - Requests with origin/referer headers that match their host header are\n"
    "   accepted.\n"
    " - Requests with headers that don't match are rejected with status 400: Bad\n"
    "   Request.\n"
    "\n"
    " This middleware implements the [OWASP Verifying Origin With Standard Headers][1]\n"
    " CSRF defense-in-depth technique. **Do not** allow `Get` or `Head` requests \n"
    " to trigger side effects if relying only on this function and the SameSite\n"
    " cookies feature for CSRF protection.\n"
    "\n"
    " This middleware and SameSite cookies typically is sufficient to protect\n"
    " against CSRF attacks, but you may decide to employ [token based mitigation][2]\n"
    " for more complete CSRF defence-in-depth.\n"
    "\n"
    " [1]: https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#using-standard-headers-to-verify-origin\n"
    " [2]: https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#token-based-mitigation\n"
).
-spec csrf_known_header_protection(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
csrf_known_header_protection(Request, Next) ->
    Is_pure_method = case erlang:element(2, Request) of
        head ->
            true;

        get ->
            true;

        _ ->
            false
    end,
    gleam@bool:lazy_guard(
        Is_pure_method,
        fun() -> Next(Request) end,
        fun() ->
            Origin = case gleam@http@request:get_header(
                Request,
                <<"origin"/utf8>>
            ) of
                {error, _} ->
                    gleam@http@request:get_header(Request, <<"referer"/utf8>>);

                {ok, _} = O ->
                    O
            end,
            Host = gleam@http@request:get_header(Request, <<"host"/utf8>>),
            case {Origin, Host} of
                {_, {error, _}} ->
                    log_warning(<<"Host header missing from request"/utf8>>),
                    bad_request(<<"Invalid host"/utf8>>);

                {{error, _}, _} ->
                    Headers = gleam@list:filter(
                        erlang:element(3, Request),
                        fun(H) -> erlang:element(1, H) /= <<"cookie"/utf8>> end
                    ),
                    Request@1 = {request,
                        erlang:element(2, Request),
                        Headers,
                        erlang:element(4, Request),
                        erlang:element(5, Request),
                        erlang:element(6, Request),
                        erlang:element(7, Request),
                        erlang:element(8, Request),
                        erlang:element(9, Request)},
                    Next(Request@1);

                {{ok, Origin@1}, {ok, Host@1}} ->
                    {Host_host, Host_port} = case gleam@string:split_once(
                        Host@1,
                        <<":"/utf8>>
                    ) of
                        {ok, {Host@2, Port}} ->
                            Port@1 = begin
                                _pipe = Port,
                                _pipe@1 = gleam_stdlib:parse_int(_pipe),
                                gleam@option:from_result(_pipe@1)
                            end,
                            {{some, Host@2}, Port@1};

                        _ ->
                            {{some, Host@1}, none}
                    end,
                    {uri, _, _, Origin_host, Origin_port, _, _, _} = begin
                        _pipe@2 = gleam_stdlib:uri_parse(Origin@1),
                        gleam@result:unwrap(
                            _pipe@2,
                            {uri,
                                none,
                                none,
                                none,
                                none,
                                <<""/utf8>>,
                                none,
                                none}
                        )
                    end,
                    case (Host_host =:= Origin_host) andalso (Host_port =:= Origin_port) of
                        true ->
                            Next(Request);

                        false ->
                            log_warning(
                                <<<<<<"Origin-host mismatch: "/utf8,
                                            Host@1/binary>>/binary,
                                        " "/utf8>>/binary,
                                    Origin@1/binary>>
                            ),
                            bad_request(<<"Invalid origin"/utf8>>)
                    end
            end
        end
    ).

-file("src/wisp.gleam", 1207).
-spec read_chunk(buffered_reader(), integer()) -> {ok,
        {bitstring(),
            fun((integer()) -> {ok, wisp@internal:read()} | {error, nil})}} |
    {error, gleam@http@response:response(body())}.
read_chunk(Reader, Chunk_size) ->
    case buffered_read(Reader, Chunk_size) of
        {error, _} ->
            {error, bad_request(<<"Unexpected end of request body"/utf8>>)};

        {ok, Chunk} ->
            case Chunk of
                {chunk, Chunk@1, Next} ->
                    {ok, {Chunk@1, Next}};

                reading_finished ->
                    {error,
                        bad_request(<<"Unexpected end of request body"/utf8>>)}
            end
    end.

-file("src/wisp.gleam", 1139).
-spec multipart_body(
    buffered_reader(),
    fun((bitstring()) -> {ok, gleam@http:multipart_body()} |
        {error, gleam@http@response:response(body())}),
    binary(),
    integer(),
    integer(),
    fun((AEVX, bitstring()) -> {ok, AEVX} |
        {error, gleam@http@response:response(body())}),
    AEVX
) -> {ok, {gleam@option:option(buffered_reader()), integer(), AEVX}} |
    {error, gleam@http@response:response(body())}.
multipart_body(Reader, Parse, Boundary, Chunk_size, Quota, Append, Data) ->
    gleam@result:'try'(
        read_chunk(Reader, Chunk_size),
        fun(_use0) ->
            {Chunk, Reader@1} = _use0,
            Size_read = erlang:byte_size(Chunk),
            gleam@result:'try'(Parse(Chunk), fun(Output) -> case Output of
                        {multipart_body, Parsed, Done, Remaining} ->
                            Used = (Size_read - erlang:byte_size(Remaining)) - 2,
                            Used@1 = case Done of
                                true ->
                                    (Used - 4) - erlang:byte_size(Boundary);

                                false ->
                                    Used
                            end,
                            gleam@result:'try'(
                                decrement_quota(Quota, Used@1),
                                fun(Quota@1) ->
                                    Reader@2 = {buffered_reader,
                                        Reader@1,
                                        Remaining},
                                    Reader@3 = case Done of
                                        true ->
                                            none;

                                        false ->
                                            {some, Reader@2}
                                    end,
                                    gleam@result:map(
                                        Append(Data, Parsed),
                                        fun(Value) ->
                                            {Reader@3, Quota@1, Value}
                                        end
                                    )
                                end
                            );

                        {more_required_for_body, Chunk@1, Parse@1} ->
                            Parse@2 = fn_with_bad_request_error(
                                Parse@1,
                                <<"Invalid form encoding"/utf8>>
                            ),
                            Reader@4 = {buffered_reader, Reader@1, <<>>},
                            gleam@result:'try'(
                                Append(Data, Chunk@1),
                                fun(Data@1) ->
                                    multipart_body(
                                        Reader@4,
                                        Parse@2,
                                        Boundary,
                                        Chunk_size,
                                        Quota,
                                        Append,
                                        Data@1
                                    )
                                end
                            )
                    end end)
        end
    ).

-file("src/wisp.gleam", 1221).
-spec multipart_headers(
    buffered_reader(),
    fun((bitstring()) -> {ok, gleam@http:multipart_headers()} |
        {error, gleam@http@response:response(body())}),
    integer(),
    quotas()
) -> {ok, {list({binary(), binary()}), buffered_reader(), quotas()}} |
    {error, gleam@http@response:response(body())}.
multipart_headers(Reader, Parse, Chunk_size, Quotas) ->
    gleam@result:'try'(
        read_chunk(Reader, Chunk_size),
        fun(_use0) ->
            {Chunk, Reader@1} = _use0,
            gleam@result:'try'(Parse(Chunk), fun(Headers) -> case Headers of
                        {multipart_headers, Headers@1, Remaining} ->
                            Used = erlang:byte_size(Chunk) - erlang:byte_size(
                                Remaining
                            ),
                            gleam@result:map(
                                decrement_body_quota(Quotas, Used),
                                fun(Quotas@1) ->
                                    Reader@2 = {buffered_reader,
                                        Reader@1,
                                        Remaining},
                                    {Headers@1, Reader@2, Quotas@1}
                                end
                            );

                        {more_required_for_headers, Parse@1} ->
                            Parse@2 = fun(Chunk@1) -> case Parse@1(Chunk@1) of
                                    {ok, Parsed} ->
                                        {ok, Parsed};

                                    {error, _} ->
                                        {error,
                                            bad_request(
                                                <<"Invalid form encoding"/utf8>>
                                            )}
                                end end,
                            Reader@3 = {buffered_reader, Reader@1, <<>>},
                            multipart_headers(
                                Reader@3,
                                Parse@2,
                                Chunk_size,
                                Quotas
                            )
                    end end)
        end
    ).

-file("src/wisp.gleam", 1053).
-spec read_multipart(
    gleam@http@request:request(wisp@internal:connection()),
    buffered_reader(),
    binary(),
    quotas(),
    form_data()
) -> {ok, form_data()} | {error, gleam@http@response:response(body())}.
read_multipart(Request, Reader, Boundary, Quotas, Data) ->
    Read_size = erlang:element(5, erlang:element(4, Request)),
    Header_parser = fn_with_bad_request_error(
        fun(_capture) ->
            gleam@http:parse_multipart_headers(_capture, Boundary)
        end,
        <<"Invalid form encoding"/utf8>>
    ),
    Result = multipart_headers(Reader, Header_parser, Read_size, Quotas),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Headers, Reader@1, Quotas@1} = _use0,
            gleam@result:'try'(
                multipart_content_disposition(Headers),
                fun(_use0@1) ->
                    {Name, Filename} = _use0@1,
                    Parse = fn_with_bad_request_error(
                        fun(_capture@1) ->
                            gleam@http:parse_multipart_body(
                                _capture@1,
                                Boundary
                            )
                        end,
                        <<"Invalid form encoding"/utf8>>
                    ),
                    gleam@result:'try'(case Filename of
                            {some, File_name} ->
                                gleam@result:'try'(
                                    or_500(new_temporary_file(Request)),
                                    fun(Path) ->
                                        Append = fun multipart_file_append/2,
                                        Q = erlang:element(3, Quotas@1),
                                        Result@1 = multipart_body(
                                            Reader@1,
                                            Parse,
                                            Boundary,
                                            Read_size,
                                            Q,
                                            Append,
                                            Path
                                        ),
                                        gleam@result:map(
                                            Result@1,
                                            fun(_use0@2) ->
                                                {Reader@2, Quota, _} = _use0@2,
                                                Quotas@2 = {quotas,
                                                    erlang:element(2, Quotas@1),
                                                    Quota},
                                                File = {uploaded_file,
                                                    File_name,
                                                    Path},
                                                Data@1 = {form_data,
                                                    erlang:element(2, Data),
                                                    [{Name, File} |
                                                        erlang:element(3, Data)]},
                                                {Data@1, Reader@2, Quotas@2}
                                            end
                                        )
                                    end
                                );

                            none ->
                                Append@1 = fun(Data@2, Chunk) ->
                                    {ok, gleam@bit_array:append(Data@2, Chunk)}
                                end,
                                Q@1 = erlang:element(2, Quotas@1),
                                Result@2 = multipart_body(
                                    Reader@1,
                                    Parse,
                                    Boundary,
                                    Read_size,
                                    Q@1,
                                    Append@1,
                                    <<>>
                                ),
                                gleam@result:'try'(
                                    Result@2,
                                    fun(_use0@3) ->
                                        {Reader@3, Quota@1, Value} = _use0@3,
                                        Quotas@3 = {quotas,
                                            Quota@1,
                                            erlang:element(3, Quotas@1)},
                                        gleam@result:map(
                                            case gleam@bit_array:to_string(
                                                Value
                                            ) of
                                                {ok, String} ->
                                                    {ok, String};

                                                {error, _} ->
                                                    {error,
                                                        bad_request(
                                                            <<"Invalid UTF-8"/utf8>>
                                                        )}
                                            end,
                                            fun(Value@1) ->
                                                Data@3 = {form_data,
                                                    [{Name, Value@1} |
                                                        erlang:element(2, Data)],
                                                    erlang:element(3, Data)},
                                                {Data@3, Reader@3, Quotas@3}
                                            end
                                        )
                                    end
                                )
                        end, fun(_use0@4) ->
                            {Data@4, Reader@4, Quotas@4} = _use0@4,
                            case Reader@4 of
                                {some, Reader@5} ->
                                    read_multipart(
                                        Request,
                                        Reader@5,
                                        Boundary,
                                        Quotas@4,
                                        Data@4
                                    );

                                none ->
                                    {ok,
                                        {form_data,
                                            sort_keys(erlang:element(2, Data@4)),
                                            sort_keys(erlang:element(3, Data@4))}}
                            end
                        end)
                end
            )
        end
    ).

-file("src/wisp.gleam", 1036).
-spec require_multipart_form(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    fun((form_data()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_multipart_form(Request, Boundary, Next) ->
    Quotas = {quotas,
        erlang:element(3, erlang:element(4, Request)),
        erlang:element(4, erlang:element(4, Request))},
    Reader = {buffered_reader,
        erlang:element(2, erlang:element(4, Request)),
        <<>>},
    Result = read_multipart(
        Request,
        Reader,
        Boundary,
        Quotas,
        {form_data, [], []}
    ),
    case Result of
        {ok, Form_data} ->
            Next(Form_data);

        {error, Response} ->
            Response
    end.

-file("src/wisp.gleam", 937).
?DOC(
    " A middleware which extracts form data from the body of a request that is\n"
    " encoded as either `application/x-www-form-urlencoded` or\n"
    " `multipart/form-data`.\n"
    "\n"
    " Extracted fields are sorted into alphabetical order by key, so if you wish\n"
    " to use pattern matching the order can be relied upon.\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(request: Request) -> Response {\n"
    "   use form <- wisp.require_form(request)\n"
    "   case form.values {\n"
    "     [#(\"password\", pass), #(\"username\", username)] -> // ...\n"
    "     _ -> // ...\n"
    "   }\n"
    " }\n"
    " ```\n"
    "\n"
    " The `set_max_body_size`, `set_max_files_size`, and `set_read_chunk_size` can\n"
    " be used to configure the reading of the request body.\n"
    "\n"
    " Any file uploads will streamed into temporary files on disc. These files are\n"
    " automatically deleted when the request handler returns, so if you wish to\n"
    " use them after the request has completed you will need to move them to a new\n"
    " location.\n"
    "\n"
    " If the request does not have a recognised `content-type` header then a\n"
    " response with status code 415: Unsupported media type will be returned\n"
    " to the client.\n"
    "\n"
    " If the request body is larger than the `max_body_size` or `max_files_size`\n"
    " limits then a response with status code 413: Content too large will be\n"
    " returned to the client.\n"
    "\n"
    " If the body cannot be parsed successfully then a response with status\n"
    " code 400: Bad request will be returned to the client.\n"
).
-spec require_form(
    gleam@http@request:request(wisp@internal:connection()),
    fun((form_data()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_form(Request, Next) ->
    case gleam@list:key_find(
        erlang:element(3, Request),
        <<"content-type"/utf8>>
    ) of
        {ok, <<"application/x-www-form-urlencoded"/utf8>>} ->
            require_urlencoded_form(Request, Next);

        {ok, <<"application/x-www-form-urlencoded;"/utf8, _/binary>>} ->
            require_urlencoded_form(Request, Next);

        {ok, <<"multipart/form-data; boundary="/utf8, Boundary/binary>>} ->
            require_multipart_form(Request, Boundary, Next);

        {ok, <<"multipart/form-data"/utf8>>} ->
            bad_request(<<"Invalid form encoding"/utf8>>);

        _ ->
            unsupported_media_type(
                [<<"application/x-www-form-urlencoded"/utf8>>,
                    <<"multipart/form-data"/utf8>>]
            )
    end.

-file("src/wisp.gleam", 1600).
?DOC(
    " Calculates etag for requested file and then checks for the request header `if-none-match`.\n"
    "\n"
    " If the header isn't present or the value doesn't match the newly generated etag, it returns the file with the newly generated etag.\n"
    " Otherwise if the etag matches, it returns status 304 without the file, allowing the browser to use the cached version.\n"
).
-spec handle_etag(
    gleam@http@response:response(body()),
    gleam@http@request:request(wisp@internal:connection()),
    simplifile:file_info()
) -> gleam@http@response:response(body()).
handle_etag(Resp, Req, File_info) ->
    Etag = wisp@internal:generate_etag(
        erlang:element(2, File_info),
        erlang:element(10, File_info)
    ),
    case gleam@http@request:get_header(Req, <<"if-none-match"/utf8>>) of
        {ok, Old_etag} when Old_etag =:= Etag ->
            _pipe = response(304),
            gleam@http@response:set_header(_pipe, <<"etag"/utf8>>, Etag);

        _ ->
            gleam@http@response:set_header(Resp, <<"etag"/utf8>>, Etag)
    end.

-file("src/wisp.gleam", 1413).
?DOC(
    " A middleware function that serves files from a directory, along with a\n"
    " suitable `content-type` header for known file extensions.\n"
    "\n"
    " Files are sent using the `File` response body type, so they will be sent\n"
    " directly to the client from the disc, without being read into memory.\n"
    "\n"
    " The `under` parameter is the request path prefix that must match for the\n"
    " file to be served.\n"
    "\n"
    " | `under`   | `from`  | `request.path`     | `file`                  |\n"
    " |-----------|---------|--------------------|-------------------------|\n"
    " | `/static` | `/data` | `/static/file.txt` | `/data/file.txt`        |\n"
    " | ``        | `/data` | `/static/file.txt` | `/data/static/file.txt` |\n"
    " | `/static` | ``      | `/static/file.txt` | `file.txt`              |\n"
    "\n"
    " This middleware will discard any `..` path segments in the request path to\n"
    " prevent the client from accessing files outside of the directory. It is\n"
    " advised not to serve a directory that contains your source code, application\n"
    " configuration, database, or other private files.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(req: Request) -> Response {\n"
    "   use <- wisp.serve_static(req, under: \"/static\", from: \"/public\")\n"
    "   // ...\n"
    " }\n"
    " ```\n"
    "\n"
    " Typically you static assets may be kept in your project in a directory\n"
    " called `priv`. The `priv_directory` function can be used to get a path to\n"
    " this directory.\n"
    "\n"
    " ```gleam\n"
    " fn handle_request(req: Request) -> Response {\n"
    "   let assert Ok(priv) = priv_directory(\"my_application\")\n"
    "   use <- wisp.serve_static(req, under: \"/static\", from: priv)\n"
    "   // ...\n"
    " }\n"
    " ```\n"
).
-spec serve_static(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
serve_static(Req, Prefix, Directory, Handler) ->
    Path = wisp@internal:remove_preceeding_slashes(erlang:element(8, Req)),
    Prefix@1 = wisp@internal:remove_preceeding_slashes(Prefix),
    case {erlang:element(2, Req),
        gleam_stdlib:string_starts_with(Path, Prefix@1)} of
        {get, true} ->
            Path@1 = begin
                _pipe = Path,
                _pipe@1 = gleam@string:drop_start(
                    _pipe,
                    string:length(Prefix@1)
                ),
                _pipe@2 = gleam@string:replace(
                    _pipe@1,
                    <<".."/utf8>>,
                    <<""/utf8>>
                ),
                filepath:join(Directory, _pipe@2)
            end,
            File_type = begin
                _pipe@3 = erlang:element(8, Req),
                _pipe@4 = gleam@string:split(_pipe@3, <<"."/utf8>>),
                _pipe@5 = gleam@list:last(_pipe@4),
                gleam@result:unwrap(_pipe@5, <<""/utf8>>)
            end,
            Mime_type = marceau:extension_to_mime_type(File_type),
            Content_type = case Mime_type of
                <<"application/json"/utf8>> ->
                    <<Mime_type/binary, "; charset=utf-8"/utf8>>;

                <<"text/"/utf8, _/binary>> ->
                    <<Mime_type/binary, "; charset=utf-8"/utf8>>;

                _ ->
                    Mime_type
            end,
            case simplifile_erl:file_info(Path@1) of
                {ok, File_info} ->
                    case simplifile:file_info_type(File_info) of
                        file ->
                            _pipe@6 = gleam@http@response:new(200),
                            _pipe@7 = gleam@http@response:set_header(
                                _pipe@6,
                                <<"content-type"/utf8>>,
                                Content_type
                            ),
                            _pipe@8 = gleam@http@response:set_body(
                                _pipe@7,
                                {file, Path@1, 0, none}
                            ),
                            _pipe@9 = handle_etag(_pipe@8, Req, File_info),
                            handle_file_range_header(
                                _pipe@9,
                                Req,
                                File_info,
                                Path@1
                            );

                        _ ->
                            Handler()
                    end;

                _ ->
                    Handler()
            end;

        {_, _} ->
            Handler()
    end.
