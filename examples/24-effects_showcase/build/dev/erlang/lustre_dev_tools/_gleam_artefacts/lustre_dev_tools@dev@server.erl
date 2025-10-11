-module(lustre_dev_tools@dev@server).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/dev/server.gleam").
-export([start/8]).
-export_type([context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type context() :: {context,
        lustre_dev_tools@project:project(),
        binary(),
        gleam@option:option(binary()),
        binary(),
        lustre_dev_tools@dev@proxy:proxy()}.

-file("src/lustre_dev_tools/dev/server.gleam", 77).
?DOC(false).
-spec handle_wisp_request(
    gleam@http@request:request(wisp@internal:connection()),
    context()
) -> gleam@http@response:response(wisp:body()).
handle_wisp_request(Request, Context) ->
    wisp:rescue_crashes(
        fun() ->
            wisp:handle_head(
                Request,
                fun(Request@1) ->
                    wisp:csrf_known_header_protection(
                        Request@1,
                        fun(Request@2) ->
                            wisp:serve_static(
                                Request@2,
                                <<"/.lustre"/utf8>>,
                                erlang:element(5, Context),
                                fun() ->
                                    wisp:serve_static(
                                        Request@2,
                                        <<"/"/utf8>>,
                                        filepath:join(
                                            erlang:element(
                                                4,
                                                erlang:element(2, Context)
                                            ),
                                            <<"build/dev/javascript"/utf8>>
                                        ),
                                        fun() ->
                                            wisp:serve_static(
                                                Request@2,
                                                <<"/"/utf8>>,
                                                erlang:element(
                                                    7,
                                                    erlang:element(2, Context)
                                                ),
                                                fun() ->
                                                    lustre_dev_tools@dev@proxy:handle(
                                                        Request@2,
                                                        erlang:element(
                                                            6,
                                                            Context
                                                        ),
                                                        fun() ->
                                                            Html = lustre_dev_tools@build@html:dev(
                                                                erlang:element(
                                                                    2,
                                                                    Context
                                                                ),
                                                                erlang:element(
                                                                    3,
                                                                    Context
                                                                ),
                                                                erlang:element(
                                                                    4,
                                                                    Context
                                                                )
                                                            ),
                                                            wisp:html_body(
                                                                wisp:ok(),
                                                                Html
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

-file("src/lustre_dev_tools/dev/server.gleam", 42).
?DOC(false).
-spec start(
    lustre_dev_tools@project:project(),
    booklet:booklet(gleam@option:option(lustre_dev_tools@error:error())),
    group_registry:group_registry(lustre_dev_tools@dev@watcher:event()),
    lustre_dev_tools@dev@proxy:proxy(),
    binary(),
    gleam@option:option(binary()),
    binary(),
    integer()
) -> {ok, gleam@otp@actor:started(gleam@otp@static_supervisor:supervisor())} |
    {error, lustre_dev_tools@error:error()}.
start(Project, Error, Watcher, Proxy, Entry, Tailwind_entry, Host, Port) ->
    Priv@1 = case gleam_erlang_ffi:priv_directory(<<"lustre_dev_tools"/utf8>>) of
        {ok, Priv} -> Priv;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre_dev_tools/dev/server"/utf8>>,
                        function => <<"start"/utf8>>,
                        line => 52,
                        value => _assert_fail,
                        start => 1116,
                        'end' => 1184,
                        pattern_start => 1127,
                        pattern_end => 1135})
    end,
    Context = {context, Project, Entry, Tailwind_entry, Priv@1, Proxy},
    Handler = fun(Request) -> case gleam@http@request:path_segments(Request) of
            [<<".lustre"/utf8>>, <<"ws"/utf8>>] ->
                lustre_dev_tools@dev@live_reload:start(
                    Request,
                    Project,
                    Error,
                    Watcher,
                    Tailwind_entry
                );

            _ ->
                (wisp@wisp_mist:handler(
                    fun(_capture) -> handle_wisp_request(_capture, Context) end,
                    <<""/utf8>>
                ))(Request)
        end end,
    _pipe = mist:new(Handler),
    _pipe@1 = mist:port(_pipe, Port),
    _pipe@2 = mist:bind(_pipe@1, Host),
    _pipe@3 = mist:after_start(
        _pipe@2,
        fun(_, _, _) ->
            lustre_dev_tools@cli:success(
                <<<<<<"Server started on http://"/utf8, Host/binary>>/binary,
                        ":"/utf8>>/binary,
                    (erlang:integer_to_binary(Port))/binary>>,
                false
            )
        end
    ),
    _pipe@4 = mist:start(_pipe@3),
    gleam@result:map_error(
        _pipe@4,
        fun(Field@0) -> {could_not_start_dev_server, Field@0} end
    ).
