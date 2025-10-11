-module(lustre_dev_tools@dev@proxy).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/dev/proxy.gleam").
-export([new/2, handle/3]).
-export_type([proxy/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type proxy() :: {proxy, binary(), gleam@uri:uri()} | none.

-file("src/lustre_dev_tools/dev/proxy.gleam", 24).
?DOC(false).
-spec new(binary(), binary()) -> {ok, proxy()} |
    {error, lustre_dev_tools@error:error()}.
new(From, To) ->
    case {From, To} of
        {<<""/utf8>>, <<""/utf8>>} ->
            {ok, none};

        {<<""/utf8>>, _} ->
            {error, proxy_missing_from};

        {_, <<""/utf8>>} ->
            {error, proxy_missing_to};

        {<<"/"/utf8, _/binary>>, _} ->
            case gleam_stdlib:uri_parse(To) of
                {ok, Uri} ->
                    {ok, {proxy, From, Uri}};

                {error, _} ->
                    {error, proxy_invalid_to}
            end;

        {_, _} ->
            case gleam_stdlib:uri_parse(To) of
                {ok, Uri@1} ->
                    {ok, {proxy, <<"/"/utf8, From/binary>>, Uri@1}};

                {error, _} ->
                    {error, proxy_invalid_to}
            end
    end.

-file("src/lustre_dev_tools/dev/proxy.gleam", 44).
?DOC(false).
-spec handle(
    gleam@http@request:request(wisp@internal:connection()),
    proxy(),
    fun(() -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
handle(Request, Proxy, Next) ->
    case Proxy of
        none ->
            Next();

        {proxy, From, To} ->
            case gleam@string:split_once(erlang:element(8, Request), From) of
                {ok, {<<""/utf8>>, Path}} ->
                    Internal_error = begin
                        _pipe = gleam@http@response:new(500),
                        gleam@http@response:set_body(
                            _pipe,
                            {bytes, gleam@bytes_tree:new()}
                        )
                    end,
                    Path@1 = filepath:join(erlang:element(6, To), Path),
                    Host@1 = case erlang:element(4, To) of
                        {some, Host} -> Host;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"lustre_dev_tools/dev/proxy"/utf8>>,
                                        function => <<"handle"/utf8>>,
                                        line => 59,
                                        value => _assert_fail,
                                        start => 1604,
                                        'end' => 1635,
                                        pattern_start => 1615,
                                        pattern_end => 1625})
                    end,
                    Body@1 = case wisp:read_body_bits(Request) of
                        {ok, Body} -> Body;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"lustre_dev_tools/dev/proxy"/utf8>>,
                                        function => <<"handle"/utf8>>,
                                        line => 60,
                                        value => _assert_fail@1,
                                        start => 1646,
                                        'end' => 1696,
                                        pattern_start => 1657,
                                        pattern_end => 1665})
                    end,
                    _pipe@1 = {request,
                        erlang:element(2, Request),
                        erlang:element(3, Request),
                        Body@1,
                        erlang:element(5, Request),
                        Host@1,
                        erlang:element(5, To),
                        Path@1,
                        erlang:element(9, Request)},
                    _pipe@2 = gleam@httpc:send_bits(_pipe@1),
                    _pipe@3 = gleam@result:map(
                        _pipe@2,
                        fun(_capture) ->
                            gleam@http@response:map(
                                _capture,
                                fun gleam@bytes_tree:from_bit_array/1
                            )
                        end
                    ),
                    _pipe@4 = gleam@result:map(
                        _pipe@3,
                        fun(_capture@1) ->
                            gleam@http@response:map(
                                _capture@1,
                                fun(Field@0) -> {bytes, Field@0} end
                            )
                        end
                    ),
                    gleam@result:unwrap(_pipe@4, Internal_error);

                _ ->
                    Next()
            end
    end.
