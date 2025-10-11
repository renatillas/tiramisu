-module(glisten@internal@listener).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/glisten/internal/listener.gleam").
-export([start/4]).
-export_type([message/0, state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type message() :: {info, gleam@erlang@process:subject(state())}.

-type state() :: {state,
        glisten@socket:listen_socket(),
        integer(),
        glisten@socket@options:ip_address()}.

-file("src/glisten/internal/listener.gleam", 17).
?DOC(false).
-spec start(
    integer(),
    glisten@transport:transport(),
    list(glisten@socket@options:tcp_option()),
    gleam@erlang@process:name(message())
) -> {ok, gleam@otp@actor:started(gleam@erlang@process:subject(message()))} |
    {error, gleam@otp@actor:start_error()}.
start(Port, Transport, Options, Name) ->
    _pipe@8 = gleam@otp@actor:new_with_initialiser(
        5000,
        fun(Subject) ->
            _pipe = glisten@transport:listen(Transport, Port, Options),
            _pipe@2 = gleam@result:'try'(
                _pipe,
                fun(Socket) ->
                    _pipe@1 = glisten@transport:sockname(Transport, Socket),
                    gleam@result:map(
                        _pipe@1,
                        fun(Info) ->
                            {state,
                                Socket,
                                erlang:element(2, Info),
                                erlang:element(1, Info)}
                        end
                    )
                end
            ),
            _pipe@7 = gleam@result:map(_pipe@2, fun(State) -> _pipe@3 = State,
                    _pipe@4 = gleam@otp@actor:initialised(_pipe@3),
                    _pipe@6 = gleam@otp@actor:selecting(
                        _pipe@4,
                        begin
                            _pipe@5 = gleam_erlang_ffi:new_selector(),
                            gleam@erlang@process:select(_pipe@5, Subject)
                        end
                    ),
                    gleam@otp@actor:returning(_pipe@6, Subject) end),
            gleam@result:map_error(
                _pipe@7,
                fun(Err) ->
                    <<"Failed to start socket listener: "/utf8,
                        (gleam@string:inspect(Err))/binary>>
                end
            )
        end
    ),
    _pipe@9 = gleam@otp@actor:on_message(
        _pipe@8,
        fun(State@1, Msg) -> case Msg of
                {info, Caller} ->
                    gleam@erlang@process:send(Caller, State@1),
                    gleam@otp@actor:continue(State@1)
            end end
    ),
    _pipe@10 = gleam@otp@actor:named(_pipe@9, Name),
    gleam@otp@actor:start(_pipe@10).
