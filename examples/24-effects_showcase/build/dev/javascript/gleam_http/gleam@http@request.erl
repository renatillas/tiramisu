-module(gleam@http@request).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/http/request.gleam").
-export([to_uri/1, from_uri/1, get_header/2, set_header/3, prepend_header/3, set_body/2, map/2, path_segments/1, get_query/1, set_query/2, set_method/2, new/0, to/1, set_scheme/2, set_host/2, set_port/2, set_path/2, set_cookie/3, get_cookies/1, remove_cookie/2]).
-export_type([request/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type request(ECH) :: {request,
        gleam@http:method(),
        list({binary(), binary()}),
        ECH,
        gleam@http:scheme(),
        binary(),
        gleam@option:option(integer()),
        binary(),
        gleam@option:option(binary())}.

-file("src/gleam/http/request.gleam", 30).
?DOC(" Return the uri that a request was sent to.\n").
-spec to_uri(request(any())) -> gleam@uri:uri().
to_uri(Request) ->
    {uri,
        {some, gleam@http:scheme_to_string(erlang:element(5, Request))},
        none,
        {some, erlang:element(6, Request)},
        erlang:element(7, Request),
        erlang:element(8, Request),
        erlang:element(9, Request),
        none}.

-file("src/gleam/http/request.gleam", 44).
?DOC(" Construct a request from a URI.\n").
-spec from_uri(gleam@uri:uri()) -> {ok, request(binary())} | {error, nil}.
from_uri(Uri) ->
    gleam@result:'try'(
        begin
            _pipe = erlang:element(2, Uri),
            _pipe@1 = gleam@option:unwrap(_pipe, <<""/utf8>>),
            gleam@http:scheme_from_string(_pipe@1)
        end,
        fun(Scheme) ->
            gleam@result:'try'(
                begin
                    _pipe@2 = erlang:element(4, Uri),
                    gleam@option:to_result(_pipe@2, nil)
                end,
                fun(Host) ->
                    Req = {request,
                        get,
                        [],
                        <<""/utf8>>,
                        Scheme,
                        Host,
                        erlang:element(5, Uri),
                        erlang:element(6, Uri),
                        erlang:element(7, Uri)},
                    {ok, Req}
                end
            )
        end
    ).

-file("src/gleam/http/request.gleam", 75).
?DOC(
    " Get the value for a given header.\n"
    "\n"
    " If the request does not have that header then `Error(Nil)` is returned.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec get_header(request(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Request, Key) ->
    gleam@list:key_find(erlang:element(3, Request), string:lowercase(Key)).

-file("src/gleam/http/request.gleam", 86).
?DOC(
    " Set the header with the given value under the given header key.\n"
    "\n"
    " If already present, it is replaced.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec set_header(request(ECR), binary(), binary()) -> request(ECR).
set_header(Request, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Request),
        string:lowercase(Key),
        Value
    ),
    {request,
        erlang:element(2, Request),
        Headers,
        erlang:element(4, Request),
        erlang:element(5, Request),
        erlang:element(6, Request),
        erlang:element(7, Request),
        erlang:element(8, Request),
        erlang:element(9, Request)}.

-file("src/gleam/http/request.gleam", 103).
?DOC(
    " Prepend the header with the given value under the given header key.\n"
    "\n"
    " Similar to `set_header` except if the header already exists it prepends\n"
    " another header with the same key.\n"
    "\n"
    " Header keys are always lowercase in `gleam_http`. To use any uppercase\n"
    " letter is invalid.\n"
).
-spec prepend_header(request(ECU), binary(), binary()) -> request(ECU).
prepend_header(Request, Key, Value) ->
    Headers = [{string:lowercase(Key), Value} | erlang:element(3, Request)],
    {request,
        erlang:element(2, Request),
        Headers,
        erlang:element(4, Request),
        erlang:element(5, Request),
        erlang:element(6, Request),
        erlang:element(7, Request),
        erlang:element(8, Request),
        erlang:element(9, Request)}.

-file("src/gleam/http/request.gleam", 114).
?DOC(" Set the body of the request, overwriting any existing body.\n").
-spec set_body(request(any()), ECZ) -> request(ECZ).
set_body(Req, Body) ->
    {request,
        erlang:element(2, Req),
        erlang:element(3, Req),
        Body,
        erlang:element(5, Req),
        erlang:element(6, Req),
        erlang:element(7, Req),
        erlang:element(8, Req),
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 120).
?DOC(" Update the body of a request using a given function.\n").
-spec map(request(EDB), fun((EDB) -> EDD)) -> request(EDD).
map(Request, Transform) ->
    _pipe = erlang:element(4, Request),
    _pipe@1 = Transform(_pipe),
    set_body(Request, _pipe@1).

-file("src/gleam/http/request.gleam", 140).
?DOC(
    " Return the non-empty segments of a request path.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " > new()\n"
    " > |> set_path(\"/one/two/three\")\n"
    " > |> path_segments\n"
    " [\"one\", \"two\", \"three\"]\n"
    " ```\n"
).
-spec path_segments(request(any())) -> list(binary()).
path_segments(Request) ->
    _pipe = erlang:element(8, Request),
    gleam@uri:path_segments(_pipe).

-file("src/gleam/http/request.gleam", 146).
?DOC(" Decode the query of a request.\n").
-spec get_query(request(any())) -> {ok, list({binary(), binary()})} |
    {error, nil}.
get_query(Request) ->
    case erlang:element(9, Request) of
        {some, Query_string} ->
            gleam_stdlib:parse_query(Query_string);

        none ->
            {ok, []}
    end.

-file("src/gleam/http/request.gleam", 156).
?DOC(
    " Set the query of the request.\n"
    " Query params will be percent encoded before being added to the Request.\n"
).
-spec set_query(request(EDN), list({binary(), binary()})) -> request(EDN).
set_query(Req, Query) ->
    Query@1 = begin
        _pipe = gleam@list:map(
            Query,
            fun(Pair) ->
                {Key, Value} = Pair,
                <<<<(gleam_stdlib:percent_encode(Key))/binary, "="/utf8>>/binary,
                    (gleam_stdlib:percent_encode(Value))/binary>>
            end
        ),
        _pipe@1 = gleam@string:join(_pipe, <<"&"/utf8>>),
        {some, _pipe@1}
    end,
    {request,
        erlang:element(2, Req),
        erlang:element(3, Req),
        erlang:element(4, Req),
        erlang:element(5, Req),
        erlang:element(6, Req),
        erlang:element(7, Req),
        erlang:element(8, Req),
        Query@1}.

-file("src/gleam/http/request.gleam", 173).
?DOC(" Set the method of the request.\n").
-spec set_method(request(EDR), gleam@http:method()) -> request(EDR).
set_method(Req, Method) ->
    {request,
        Method,
        erlang:element(3, Req),
        erlang:element(4, Req),
        erlang:element(5, Req),
        erlang:element(6, Req),
        erlang:element(7, Req),
        erlang:element(8, Req),
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 180).
?DOC(
    " A request with commonly used default values. This request can be used as\n"
    " an initial value and then update to create the desired request.\n"
).
-spec new() -> request(binary()).
new() ->
    {request,
        get,
        [],
        <<""/utf8>>,
        https,
        <<"localhost"/utf8>>,
        none,
        <<""/utf8>>,
        none}.

-file("src/gleam/http/request.gleam", 195).
?DOC(" Construct a request from a URL string\n").
-spec to(binary()) -> {ok, request(binary())} | {error, nil}.
to(Url) ->
    _pipe = Url,
    _pipe@1 = gleam_stdlib:uri_parse(_pipe),
    gleam@result:'try'(_pipe@1, fun from_uri/1).

-file("src/gleam/http/request.gleam", 203).
?DOC(" Set the scheme (protocol) of the request.\n").
-spec set_scheme(request(EDY), gleam@http:scheme()) -> request(EDY).
set_scheme(Req, Scheme) ->
    {request,
        erlang:element(2, Req),
        erlang:element(3, Req),
        erlang:element(4, Req),
        Scheme,
        erlang:element(6, Req),
        erlang:element(7, Req),
        erlang:element(8, Req),
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 209).
?DOC(" Set the host of the request.\n").
-spec set_host(request(EEB), binary()) -> request(EEB).
set_host(Req, Host) ->
    {request,
        erlang:element(2, Req),
        erlang:element(3, Req),
        erlang:element(4, Req),
        erlang:element(5, Req),
        Host,
        erlang:element(7, Req),
        erlang:element(8, Req),
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 215).
?DOC(" Set the port of the request.\n").
-spec set_port(request(EEE), integer()) -> request(EEE).
set_port(Req, Port) ->
    {request,
        erlang:element(2, Req),
        erlang:element(3, Req),
        erlang:element(4, Req),
        erlang:element(5, Req),
        erlang:element(6, Req),
        {some, Port},
        erlang:element(8, Req),
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 221).
?DOC(" Set the path of the request.\n").
-spec set_path(request(EEH), binary()) -> request(EEH).
set_path(Req, Path) ->
    {request,
        erlang:element(2, Req),
        erlang:element(3, Req),
        erlang:element(4, Req),
        erlang:element(5, Req),
        erlang:element(6, Req),
        erlang:element(7, Req),
        Path,
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 232).
?DOC(
    " Set a cookie on a request, replacing any previous cookie with that name.\n"
    "\n"
    " All cookies should be stored in a single header named `cookie`.\n"
    " There should be at most one header with the name `cookie`, otherwise this\n"
    " function cannot guarentee that previous cookies with the same name are\n"
    " replaced.\n"
).
-spec set_cookie(request(EEK), binary(), binary()) -> request(EEK).
set_cookie(Req, Name, Value) ->
    {Cookies, Headers} = begin
        _pipe = gleam@list:key_pop(erlang:element(3, Req), <<"cookie"/utf8>>),
        gleam@result:unwrap(_pipe, {<<""/utf8>>, erlang:element(3, Req)})
    end,
    Cookies@1 = begin
        _pipe@1 = gleam@http@cookie:parse(Cookies),
        _pipe@2 = gleam@list:key_set(_pipe@1, Name, Value),
        _pipe@3 = gleam@list:map(
            _pipe@2,
            fun(Pair) ->
                <<<<(erlang:element(1, Pair))/binary, "="/utf8>>/binary,
                    (erlang:element(2, Pair))/binary>>
            end
        ),
        gleam@string:join(_pipe@3, <<"; "/utf8>>)
    end,
    {request,
        erlang:element(2, Req),
        [{<<"cookie"/utf8>>, Cookies@1} | Headers],
        erlang:element(4, Req),
        erlang:element(5, Req),
        erlang:element(6, Req),
        erlang:element(7, Req),
        erlang:element(8, Req),
        erlang:element(9, Req)}.

-file("src/gleam/http/request.gleam", 255).
?DOC(
    " Fetch the cookies sent in a request.\n"
    "\n"
    " Note badly formed cookie pairs will be ignored.\n"
    " RFC6265 specifies that invalid cookie names/attributes should be ignored.\n"
).
-spec get_cookies(request(any())) -> list({binary(), binary()}).
get_cookies(Req) ->
    {request, _, Headers, _, _, _, _, _, _} = Req,
    gleam@list:flat_map(Headers, fun(Header) -> case Header of
                {<<"cookie"/utf8>>, Value} ->
                    gleam@http@cookie:parse(Value);

                _ ->
                    []
            end end).

-file("src/gleam/http/request.gleam", 270).
?DOC(
    " Remove a cookie from a request\n"
    "\n"
    " Remove a cookie from the request. If no cookie is found return the request\n"
    " unchanged. This will not remove the cookie from the client.\n"
).
-spec remove_cookie(request(EEQ), binary()) -> request(EEQ).
remove_cookie(Req, Name) ->
    case gleam@list:key_pop(erlang:element(3, Req), <<"cookie"/utf8>>) of
        {ok, {Cookies_string, Headers}} ->
            New_cookies_string = begin
                _pipe = gleam@http@cookie:parse(Cookies_string),
                _pipe@1 = gleam@list:filter_map(
                    _pipe,
                    fun(Cookie) -> case Cookie of
                            {Cookie_name, _} when Cookie_name =:= Name ->
                                {error, nil};

                            {Name@1, Value} ->
                                {ok,
                                    <<<<Name@1/binary, "="/utf8>>/binary,
                                        Value/binary>>}
                        end end
                ),
                gleam@string:join(_pipe@1, <<"; "/utf8>>)
            end,
            {request,
                erlang:element(2, Req),
                [{<<"cookie"/utf8>>, New_cookies_string} | Headers],
                erlang:element(4, Req),
                erlang:element(5, Req),
                erlang:element(6, Req),
                erlang:element(7, Req),
                erlang:element(8, Req),
                erlang:element(9, Req)};

        {error, _} ->
            Req
    end.
