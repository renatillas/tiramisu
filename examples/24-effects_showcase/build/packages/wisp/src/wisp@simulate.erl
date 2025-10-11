-module(wisp@simulate).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/wisp/simulate.gleam").
-export([read_body/1, read_body_bits/1, header/3, session/3, cookie/4, string_body/2, bit_array_body/2, html_body/2, form_body/2, json_body/2, multipart_body/3, request/2, browser_request/2]).
-export_type([file_upload/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type file_upload() :: {file_upload, binary(), binary(), bitstring()}.

-file("src/wisp/simulate.gleam", 200).
-spec build_multipart_body(
    list({binary(), binary()}),
    list({binary(), file_upload()}),
    binary()
) -> bitstring().
build_multipart_body(Form_values, Files, Boundary) ->
    Body = begin
        _pipe = gleam@list:fold(
            Form_values,
            <<>>,
            fun(Acc, Field) ->
                {Name, Value} = Field,
                <<Acc/bitstring,
                    "--"/utf8,
                    Boundary/binary,
                    "\r\n"/utf8,
                    "Content-Disposition: form-data; name=\""/utf8,
                    Name/binary,
                    "\"\r\n"/utf8,
                    "\r\n"/utf8,
                    Value/binary,
                    "\r\n"/utf8>>
            end
        ),
        gleam@list:fold(
            Files,
            _pipe,
            fun(Acc@1, File) ->
                <<Acc@1/bitstring,
                    "--"/utf8,
                    Boundary/binary,
                    "\r\n"/utf8,
                    "Content-Disposition: form-data; name=\""/utf8,
                    (erlang:element(1, File))/binary,
                    "\"; filename=\""/utf8,
                    (erlang:element(2, (erlang:element(2, File))))/binary,
                    "\"\r\n"/utf8,
                    "Content-Type: "/utf8,
                    (erlang:element(3, (erlang:element(2, File))))/binary,
                    "\r\n"/utf8,
                    "\r\n"/utf8,
                    (erlang:element(4, (erlang:element(2, File))))/bitstring,
                    "\r\n"/utf8>>
            end
        )
    end,
    <<Body/bitstring, "--"/utf8, Boundary/binary, "--\r\n"/utf8>>.

-file("src/wisp/simulate.gleam", 255).
?DOC(
    " Read a text body from a response.\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic if the response body is a file and the file cannot\n"
    " be read, or if it does not contain valid UTF-8.\n"
).
-spec read_body(gleam@http@response:response(wisp:body())) -> binary().
read_body(Response) ->
    case erlang:element(4, Response) of
        {text, Tree} ->
            Tree;

        {bytes, Bytes} ->
            Data = erlang:list_to_bitstring(Bytes),
            String@1 = case gleam@bit_array:to_string(Data) of
                {ok, String} -> String;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the response body was non-UTF8 binary data"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body"/utf8>>,
                                line => 260,
                                value => _assert_fail,
                                start => 7570,
                                'end' => 7619,
                                pattern_start => 7581,
                                pattern_end => 7591})
            end,
            String@1;

        {file, Path, 0, none} ->
            Data@2 = case simplifile_erl:read_bits(Path) of
                {ok, Data@1} -> Data@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body was a file, but the file could not be read"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body"/utf8>>,
                                line => 265,
                                value => _assert_fail@1,
                                start => 7746,
                                'end' => 7794,
                                pattern_start => 7757,
                                pattern_end => 7765})
            end,
            Contents@1 = case gleam@bit_array:to_string(Data@2) of
                {ok, Contents} -> Contents;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body file was not valid UTF-8"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body"/utf8>>,
                                line => 267,
                                value => _assert_fail@2,
                                start => 7866,
                                'end' => 7917,
                                pattern_start => 7877,
                                pattern_end => 7889})
            end,
            Contents@1;

        {file, Path@1, Offset, Limit} ->
            Data@4 = case simplifile_erl:read_bits(Path@1) of
                {ok, Data@3} -> Data@3;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body was a file, but the file could not be read"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body"/utf8>>,
                                line => 272,
                                value => _assert_fail@3,
                                start => 8030,
                                'end' => 8078,
                                pattern_start => 8041,
                                pattern_end => 8049})
            end,
            Byte_length = begin
                _pipe = Limit,
                gleam@option:unwrap(_pipe, erlang:byte_size(Data@4) - Offset)
            end,
            Slice@1 = case gleam_stdlib:bit_array_slice(
                Data@4,
                Offset,
                Byte_length
            ) of
                {ok, Slice} -> Slice;
                _assert_fail@4 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body was a file, but the limit and offset were invalid"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body"/utf8>>,
                                line => 276,
                                value => _assert_fail@4,
                                start => 8241,
                                'end' => 8306,
                                pattern_start => 8252,
                                pattern_end => 8261})
            end,
            String@3 = case gleam@bit_array:to_string(Slice@1) of
                {ok, String@2} -> String@2;
                _assert_fail@5 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body file range was not valid UTF-8"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body"/utf8>>,
                                line => 278,
                                value => _assert_fail@5,
                                start => 8385,
                                'end' => 8435,
                                pattern_start => 8396,
                                pattern_end => 8406})
            end,
            String@3
    end.

-file("src/wisp/simulate.gleam", 292).
?DOC(
    " Read a binary data body from a response.\n"
    "\n"
    " # Panics\n"
    "\n"
    " This function will panic if the response body is a file and the file cannot\n"
    " be read.\n"
).
-spec read_body_bits(gleam@http@response:response(wisp:body())) -> bitstring().
read_body_bits(Response) ->
    case erlang:element(4, Response) of
        {bytes, Tree} ->
            erlang:list_to_bitstring(Tree);

        {text, Tree@1} ->
            <<Tree@1/binary>>;

        {file, Path, 0, none} ->
            Contents@1 = case simplifile_erl:read_bits(Path) of
                {ok, Contents} -> Contents;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the response body was a file, but the file could not be read"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body_bits"/utf8>>,
                                line => 297,
                                value => _assert_fail,
                                start => 8889,
                                'end' => 8941,
                                pattern_start => 8900,
                                pattern_end => 8912})
            end,
            Contents@1;

        {file, Path@1, Offset, Limit} ->
            Contents@3 = case simplifile_erl:read_bits(Path@1) of
                {ok, Contents@2} -> Contents@2;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body was a file, but the file could not be read"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body_bits"/utf8>>,
                                line => 302,
                                value => _assert_fail@1,
                                start => 9081,
                                'end' => 9133,
                                pattern_start => 9092,
                                pattern_end => 9104})
            end,
            Limit@1 = begin
                _pipe = Limit,
                gleam@option:unwrap(_pipe, erlang:byte_size(Contents@3))
            end,
            Sliced@1 = case begin
                _pipe@1 = Contents@3,
                gleam_stdlib:bit_array_slice(_pipe@1, Offset, Limit@1)
            end of
                {ok, Sliced} -> Sliced;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"the body was a file, but the limit and offset were invalid"/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"wisp/simulate"/utf8>>,
                                function => <<"read_body_bits"/utf8>>,
                                line => 305,
                                value => _assert_fail@2,
                                start => 9277,
                                'end' => 9343,
                                pattern_start => 9288,
                                pattern_end => 9298})
            end,
            Sliced@1
    end.

-file("src/wisp/simulate.gleam", 314).
?DOC(" Set a header on a request.\n").
-spec header(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary()
) -> gleam@http@request:request(wisp@internal:connection()).
header(Request, Name, Value) ->
    gleam@http@request:set_header(Request, Name, Value).

-file("src/wisp/simulate.gleam", 51).
?DOC(
    " Continue a browser session from a previous request and response, adopting\n"
    " the request cookies, and updating the cookies as specified by the response.\n"
).
-spec session(
    gleam@http@request:request(wisp@internal:connection()),
    gleam@http@request:request(wisp@internal:connection()),
    gleam@http@response:response(wisp:body())
) -> gleam@http@request:request(wisp@internal:connection()).
session(Next_request, Previous_request, Previous_response) ->
    Request = case gleam@list:key_find(
        erlang:element(3, Previous_request),
        <<"cookie"/utf8>>
    ) of
        {ok, Cookies} ->
            header(Next_request, <<"cookie"/utf8>>, Cookies);

        {error, _} ->
            Next_request
    end,
    Set_cookies = begin
        _pipe = gleam@list:key_filter(
            erlang:element(3, Previous_response),
            <<"set-cookie"/utf8>>
        ),
        _pipe@2 = gleam@list:map(
            _pipe,
            fun(Cookie) -> case gleam@string:split_once(Cookie, <<";"/utf8>>) of
                    {ok, {Cookie@1, Attributes}} ->
                        Attributes@1 = begin
                            _pipe@1 = gleam@string:split(
                                Attributes,
                                <<";"/utf8>>
                            ),
                            gleam@list:map(_pipe@1, fun gleam@string:trim/1)
                        end,
                        {Cookie@1, Attributes@1};

                    {error, nil} ->
                        {Cookie, []}
                end end
        ),
        gleam@list:filter_map(
            _pipe@2,
            fun(Cookie@2) ->
                _pipe@3 = gleam@string:split_once(
                    erlang:element(1, Cookie@2),
                    <<"="/utf8>>
                ),
                gleam@result:map(
                    _pipe@3,
                    fun(Split) ->
                        {erlang:element(1, Split),
                            erlang:element(2, Split),
                            erlang:element(2, Cookie@2)}
                    end
                )
            end
        )
    end,
    gleam@list:fold(
        Set_cookies,
        Request,
        fun(Request@1, Cookie@3) ->
            case gleam@list:contains(
                erlang:element(3, Cookie@3),
                <<"Max-Age=0"/utf8>>
            ) of
                true ->
                    gleam@http@request:remove_cookie(
                        Request@1,
                        erlang:element(1, Cookie@3)
                    );

                false ->
                    gleam@http@request:set_cookie(
                        Request@1,
                        erlang:element(1, Cookie@3),
                        erlang:element(2, Cookie@3)
                    )
            end
        end
    ).

-file("src/wisp/simulate.gleam", 320).
?DOC(" Set a cookie on the request.\n").
-spec cookie(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    wisp:security()
) -> gleam@http@request:request(wisp@internal:connection()).
cookie(Request, Name, Value, Security) ->
    Value@1 = case Security of
        plain_text ->
            gleam_stdlib:base64_encode(<<Value/binary>>, false);

        signed ->
            wisp:sign_message(Request, <<Value/binary>>, sha512)
    end,
    gleam@http@request:set_cookie(Request, Name, Value@1).

-file("src/wisp/simulate.gleam", 94).
?DOC(
    " Add a text body to the request.\n"
    " \n"
    " The `content-type` header is set to `text/plain`. You may want to override\n"
    " this with `request.set_header`.\n"
).
-spec string_body(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> gleam@http@request:request(wisp@internal:connection()).
string_body(Request, Text) ->
    Body = begin
        _pipe = Text,
        _pipe@1 = gleam_stdlib:identity(_pipe),
        wisp:create_canned_connection(
            _pipe@1,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        )
    end,
    _pipe@2 = Request,
    _pipe@3 = gleam@http@request:set_body(_pipe@2, Body),
    gleam@http@request:set_header(
        _pipe@3,
        <<"content-type"/utf8>>,
        <<"text/plain"/utf8>>
    ).

-file("src/wisp/simulate.gleam", 109).
?DOC(
    " Add a binary body to the request.\n"
    " \n"
    " The `content-type` header is set to `application/octet-stream`. You may\n"
    " want to override/ this with `request.set_header`.\n"
).
-spec bit_array_body(
    gleam@http@request:request(wisp@internal:connection()),
    bitstring()
) -> gleam@http@request:request(wisp@internal:connection()).
bit_array_body(Request, Data) ->
    Body = wisp:create_canned_connection(
        Data,
        <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
    ),
    _pipe = Request,
    _pipe@1 = gleam@http@request:set_body(_pipe, Body),
    gleam@http@request:set_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        <<"application/octet-stream"/utf8>>
    ).

-file("src/wisp/simulate.gleam", 120).
?DOC(
    " Add HTML body to the request.\n"
    " \n"
    " The `content-type` header is set to `text/html; charset=utf-8`.\n"
).
-spec html_body(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> gleam@http@request:request(wisp@internal:connection()).
html_body(Request, Html) ->
    Body = begin
        _pipe = Html,
        _pipe@1 = gleam_stdlib:identity(_pipe),
        wisp:create_canned_connection(
            _pipe@1,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        )
    end,
    _pipe@2 = Request,
    _pipe@3 = gleam@http@request:set_body(_pipe@2, Body),
    gleam@http@request:set_header(
        _pipe@3,
        <<"content-type"/utf8>>,
        <<"text/html; charset=utf-8"/utf8>>
    ).

-file("src/wisp/simulate.gleam", 134).
?DOC(
    " Add a form data body to the request.\n"
    " \n"
    " The `content-type` header is set to `application/x-www-form-urlencoded`.\n"
).
-spec form_body(
    gleam@http@request:request(wisp@internal:connection()),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
form_body(Request, Data) ->
    Body = begin
        _pipe = gleam@uri:query_to_string(Data),
        _pipe@1 = gleam_stdlib:identity(_pipe),
        wisp:create_canned_connection(
            _pipe@1,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        )
    end,
    _pipe@2 = Request,
    _pipe@3 = gleam@http@request:set_body(_pipe@2, Body),
    gleam@http@request:set_header(
        _pipe@3,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-file("src/wisp/simulate.gleam", 148).
?DOC(
    " Add a JSON body to the request.\n"
    " \n"
    " The `content-type` header is set to `application/json`.\n"
).
-spec json_body(
    gleam@http@request:request(wisp@internal:connection()),
    gleam@json:json()
) -> gleam@http@request:request(wisp@internal:connection()).
json_body(Request, Data) ->
    Body = begin
        _pipe = gleam@json:to_string(Data),
        _pipe@1 = gleam_stdlib:identity(_pipe),
        wisp:create_canned_connection(
            _pipe@1,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        )
    end,
    _pipe@2 = Request,
    _pipe@3 = gleam@http@request:set_body(_pipe@2, Body),
    gleam@http@request:set_header(
        _pipe@3,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-file("src/wisp/simulate.gleam", 183).
?DOC(
    " Add a multipart/form-data body to the request for testing file uploads\n"
    " and form submissions.\n"
    " \n"
    " The `content-type` header is set to `multipart/form-data` with an\n"
    " appropriate boundary.\n"
    " \n"
    " # Examples\n"
    " \n"
    " ```gleam\n"
    " let file = UploadedFile(\n"
    "   file_name: \"test.txt\", \n"
    "   content_type: \"text/plain\",\n"
    "   content: <<\"Hello, world!\":utf8>>\n"
    " )\n"
    " \n"
    " simulate.request(http.Post, \"/upload\")\n"
    " |> simulate.multipart_body([#(\"user\", \"joe\")], [#(\"file\", file)])\n"
    " ```\n"
).
-spec multipart_body(
    gleam@http@request:request(wisp@internal:connection()),
    list({binary(), binary()}),
    list({binary(), file_upload()})
) -> gleam@http@request:request(wisp@internal:connection()).
multipart_body(Request, Values, Files) ->
    Boundary = begin
        _pipe = crypto:strong_rand_bytes(16),
        gleam_stdlib:base16_encode(_pipe)
    end,
    Body_data = build_multipart_body(Values, Files, Boundary),
    Body = wisp:create_canned_connection(
        Body_data,
        <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
    ),
    _pipe@1 = Request,
    _pipe@2 = gleam@http@request:set_body(_pipe@1, Body),
    gleam@http@request:set_header(
        _pipe@2,
        <<"content-type"/utf8>>,
        <<"multipart/form-data; boundary="/utf8, Boundary/binary>>
    ).

-file("src/wisp/simulate.gleam", 21).
?DOC(
    " Create a test request that can be used to test your request handler\n"
    " functions.\n"
    "\n"
    " If you are testing handlers that are intended to be accessed from a browser\n"
    " (such as those that use cookies) consider using `browser_request` instead.\n"
).
-spec request(gleam@http:method(), binary()) -> gleam@http@request:request(wisp@internal:connection()).
request(Method, Path) ->
    {Path@2, Query@1} = case gleam@string:split_once(Path, <<"?"/utf8>>) of
        {ok, {Path@1, Query}} ->
            {Path@1, {some, Query}};

        _ ->
            {Path, none}
    end,
    Connection = wisp:create_canned_connection(
        <<>>,
        <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
    ),
    {request,
        Method,
        [{<<"host"/utf8>>, <<"wisp.example.com"/utf8>>}],
        Connection,
        https,
        <<"wisp.example.com"/utf8>>,
        none,
        Path@2,
        Query@1}.

-file("src/wisp/simulate.gleam", 44).
?DOC(
    " Create a test request with browser-set headers that can be used to test\n"
    " your request handler functions.\n"
    "\n"
    " The `origin` header is set when using this function.\n"
).
-spec browser_request(gleam@http:method(), binary()) -> gleam@http@request:request(wisp@internal:connection()).
browser_request(Method, Path) ->
    _record = request(Method, Path),
    {request,
        erlang:element(2, _record),
        [{<<"origin"/utf8>>, <<"https://"/utf8, "wisp.example.com"/utf8>>},
            {<<"host"/utf8>>, <<"wisp.example.com"/utf8>>}],
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record),
        erlang:element(7, _record),
        erlang:element(8, _record),
        erlang:element(9, _record)}.
