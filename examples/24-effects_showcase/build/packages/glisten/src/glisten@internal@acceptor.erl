-module(glisten@internal@acceptor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/glisten/internal/acceptor.gleam").
-export([start/2, start_pool/5]).
-export_type([acceptor_message/0, acceptor_error/0, acceptor_state/0, pool/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type acceptor_message() :: {accept_connection, glisten@socket:listen_socket()}.

-type acceptor_error() :: accept_error | handler_error | control_error.

-type acceptor_state() :: {acceptor_state,
        gleam@erlang@process:subject(acceptor_message()),
        gleam@option:option(glisten@socket:socket()),
        glisten@transport:transport()}.

-type pool(GIV, GIW) :: {pool,
        fun((GIV, glisten@internal@handler:loop_message(GIW), glisten@internal@handler:connection(GIW)) -> glisten@internal@handler:next(GIV, glisten@internal@handler:loop_message(GIW))),
        integer(),
        fun((glisten@internal@handler:connection(GIW)) -> {GIV,
            gleam@option:option(gleam@erlang@process:selector(GIW))}),
        gleam@option:option(fun((GIV) -> nil)),
        glisten@transport:transport()}.

-file("src/glisten/internal/acceptor.gleam", 38).
?DOC(false).
-spec start(
    pool(any(), any()),
    gleam@erlang@process:name(glisten@internal@listener:message())
) -> {ok,
        gleam@otp@actor:started(gleam@erlang@process:subject(acceptor_message()))} |
    {error, gleam@otp@actor:start_error()}.
start(Pool, Listener_name) ->
    _pipe@5 = gleam@otp@actor:new_with_initialiser(
        1000,
        fun(Subject) ->
            Listener = gleam@erlang@process:named_subject(Listener_name),
            State = gleam@erlang@process:call(
                Listener,
                750,
                fun(Field@0) -> {info, Field@0} end
            ),
            gleam@erlang@process:send(
                Subject,
                {accept_connection, erlang:element(2, State)}
            ),
            _pipe = {acceptor_state, Subject, none, erlang:element(6, Pool)},
            _pipe@1 = gleam@otp@actor:initialised(_pipe),
            _pipe@2 = gleam@otp@actor:returning(_pipe@1, Subject),
            _pipe@4 = gleam@otp@actor:selecting(
                _pipe@2,
                begin
                    _pipe@3 = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:select(_pipe@3, Subject)
                end
            ),
            {ok, _pipe@4}
        end
    ),
    _pipe@11 = gleam@otp@actor:on_message(
        _pipe@5,
        fun(State@1, Msg) ->
            {acceptor_state, Sender, _, _} = State@1,
            case Msg of
                {accept_connection, Listener@1} ->
                    Res = begin
                        gleam@result:'try'(
                            begin
                                _pipe@6 = glisten@transport:accept(
                                    erlang:element(4, State@1),
                                    Listener@1
                                ),
                                gleam@result:replace_error(
                                    _pipe@6,
                                    accept_error
                                )
                            end,
                            fun(Sock) ->
                                gleam@result:'try'(
                                    begin
                                        _pipe@7 = {handler,
                                            Sock,
                                            erlang:element(2, Pool),
                                            erlang:element(4, Pool),
                                            erlang:element(5, Pool),
                                            erlang:element(6, Pool)},
                                        _pipe@8 = glisten@internal@handler:start(
                                            _pipe@7
                                        ),
                                        gleam@result:replace_error(
                                            _pipe@8,
                                            handler_error
                                        )
                                    end,
                                    fun(Start) ->
                                        _pipe@9 = glisten@transport:controlling_process(
                                            erlang:element(4, State@1),
                                            Sock,
                                            erlang:element(2, Start)
                                        ),
                                        _pipe@10 = gleam@result:replace_error(
                                            _pipe@9,
                                            control_error
                                        ),
                                        gleam@result:map(
                                            _pipe@10,
                                            fun(_) ->
                                                gleam@erlang@process:send(
                                                    erlang:element(3, Start),
                                                    {internal, ready}
                                                )
                                            end
                                        )
                                    end
                                )
                            end
                        )
                    end,
                    case Res of
                        {error, Reason} ->
                            logging:log(
                                error,
                                <<"Failed to accept/start handler: "/utf8,
                                    (gleam@string:inspect(Reason))/binary>>
                            ),
                            gleam@otp@actor:stop_abnormal(
                                <<"Failed to accept/start handler"/utf8>>
                            );

                        _ ->
                            gleam@otp@actor:send(
                                Sender,
                                {accept_connection, Listener@1}
                            ),
                            gleam@otp@actor:continue(State@1)
                    end
            end
        end
    ),
    gleam@otp@actor:start(_pipe@11).

-file("src/glisten/internal/acceptor.gleam", 114).
?DOC(false).
-spec start_pool(
    pool(any(), any()),
    glisten@transport:transport(),
    integer(),
    list(glisten@socket@options:tcp_option()),
    gleam@erlang@process:name(glisten@internal@listener:message())
) -> {ok, gleam@otp@actor:started(gleam@otp@static_supervisor:supervisor())} |
    {error, gleam@otp@actor:start_error()}.
start_pool(Pool, Transport, Port, Options, Listener_name) ->
    Acceptors = gleam@list:range(0, erlang:element(3, Pool)),
    _pipe = gleam@otp@static_supervisor:new(one_for_one),
    _pipe@1 = gleam@otp@static_supervisor:add(
        _pipe,
        gleam@otp@supervision:worker(
            fun() ->
                glisten@internal@listener:start(
                    Port,
                    Transport,
                    Options,
                    Listener_name
                )
            end
        )
    ),
    _pipe@4 = gleam@otp@static_supervisor:add(
        _pipe@1,
        gleam@otp@supervision:supervisor(
            fun() -> _pipe@2 = gleam@otp@static_supervisor:new(one_for_one),
                _pipe@3 = gleam@list:fold(
                    Acceptors,
                    _pipe@2,
                    fun(Sup, _) ->
                        gleam@otp@static_supervisor:add(
                            Sup,
                            gleam@otp@supervision:worker(
                                fun() -> start(Pool, Listener_name) end
                            )
                        )
                    end
                ),
                gleam@otp@static_supervisor:start(_pipe@3) end
        )
    ),
    gleam@otp@static_supervisor:start(_pipe@4).
