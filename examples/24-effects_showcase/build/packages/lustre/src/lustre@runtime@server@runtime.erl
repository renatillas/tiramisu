-module(lustre@runtime@server@runtime).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/runtime/server/runtime.gleam").
-export([start/4]).
-export_type([state/2, config/1, message/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type state(QXG, QXH) :: {state,
        gleam@erlang@process:subject(message(QXH)),
        gleam@erlang@process:selector(message(QXH)),
        gleam@erlang@process:selector(message(QXH)),
        QXG,
        fun((QXG, QXH) -> {QXG, lustre@effect:effect(QXH)}),
        fun((QXG) -> lustre@vdom@vnode:element(QXH)),
        config(QXH),
        lustre@vdom@vnode:element(QXH),
        lustre@vdom@events:events(QXH),
        gleam@dict:dict(binary(), gleam@json:json()),
        gleam@dict:dict(gleam@erlang@process:subject(lustre@runtime@transport:client_message(QXH)), gleam@erlang@process:monitor()),
        gleam@set:set(fun((lustre@runtime@transport:client_message(QXH)) -> nil))}.

-type config(QXI) :: {config,
        boolean(),
        boolean(),
        gleam@dict:dict(binary(), fun((binary()) -> {ok, QXI} | {error, nil})),
        gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(QXI)),
        gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(QXI))}.

-type message(QXJ) :: {client_dispatched_message,
        lustre@runtime@transport:server_message()} |
    {client_registered_subject,
        gleam@erlang@process:subject(lustre@runtime@transport:client_message(QXJ))} |
    {client_deregistered_subject,
        gleam@erlang@process:subject(lustre@runtime@transport:client_message(QXJ))} |
    {client_registered_callback,
        fun((lustre@runtime@transport:client_message(QXJ)) -> nil)} |
    {client_deregistered_callback,
        fun((lustre@runtime@transport:client_message(QXJ)) -> nil)} |
    {effect_added_selector, gleam@erlang@process:selector(message(QXJ))} |
    {effect_dispatched_message, QXJ} |
    {effect_emit_event, binary(), gleam@json:json()} |
    {effect_provided_value, binary(), gleam@json:json()} |
    {monitor_reported_down, gleam@erlang@process:monitor()} |
    system_requested_shutdown.

-file("src/lustre/runtime/server/runtime.gleam", 328).
?DOC(false).
-spec handle_attribute_change(
    gleam@dict:dict(binary(), fun((binary()) -> {ok, QYN} | {error, nil})),
    binary(),
    binary()
) -> {ok, QYN} | {error, nil}.
handle_attribute_change(Attributes, Name, Value) ->
    case gleam_stdlib:map_get(Attributes, Name) of
        {error, _} ->
            {error, nil};

        {ok, Handler} ->
            Handler(Value)
    end.

-file("src/lustre/runtime/server/runtime.gleam", 339).
?DOC(false).
-spec handle_property_change(
    gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(QYU)),
    binary(),
    gleam@dynamic:dynamic_()
) -> {ok, QYU} | {error, nil}.
handle_property_change(Properties, Name, Value) ->
    case gleam_stdlib:map_get(Properties, Name) of
        {error, _} ->
            {error, nil};

        {ok, Decoder} ->
            _pipe = gleam@dynamic@decode:run(Value, Decoder),
            gleam@result:replace_error(_pipe, nil)
    end.

-file("src/lustre/runtime/server/runtime.gleam", 351).
?DOC(false).
-spec handle_effect(
    gleam@erlang@process:subject(message(QZA)),
    lustre@effect:effect(QZA)
) -> nil.
handle_effect(Self, Effect) ->
    Send = fun(_capture) -> gleam@erlang@process:send(Self, _capture) end,
    Dispatch = fun(Message) -> Send({effect_dispatched_message, Message}) end,
    Emit = fun(Name, Data) -> Send({effect_emit_event, Name, Data}) end,
    Provide = fun(Key, Value) -> Send({effect_provided_value, Key, Value}) end,
    Select = fun(Selector) -> _pipe = Selector,
        _pipe@1 = gleam_erlang_ffi:map_selector(
            _pipe,
            fun(Field@0) -> {effect_dispatched_message, Field@0} end
        ),
        _pipe@2 = {effect_added_selector, _pipe@1},
        Send(_pipe@2) end,
    Internals = fun() -> gleam@dynamic:nil() end,
    lustre@effect:perform(Effect, Dispatch, Emit, Select, Internals, Provide).

-file("src/lustre/runtime/server/runtime.gleam", 260).
?DOC(false).
-spec handle_client_message(
    state(QYH, QYI),
    lustre@runtime@transport:server_message()
) -> state(QYH, QYI).
handle_client_message(State, Message) ->
    case Message of
        {batch, _, Messages} ->
            gleam@list:fold(Messages, State, fun handle_client_message/2);

        {attribute_changed, _, Name, Value} ->
            case handle_attribute_change(
                erlang:element(4, erlang:element(8, State)),
                Name,
                Value
            ) of
                {error, _} ->
                    State;

                {ok, Msg} ->
                    {Model, Effect} = (erlang:element(6, State))(
                        erlang:element(5, State),
                        Msg
                    ),
                    Vdom = (erlang:element(7, State))(Model),
                    handle_effect(erlang:element(2, State), Effect),
                    {state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        Model,
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        Vdom,
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State),
                        erlang:element(13, State)}
            end;

        {property_changed, _, Name@1, Value@1} ->
            case handle_property_change(
                erlang:element(5, erlang:element(8, State)),
                Name@1,
                Value@1
            ) of
                {error, _} ->
                    State;

                {ok, Msg@1} ->
                    {Model@1, Effect@1} = (erlang:element(6, State))(
                        erlang:element(5, State),
                        Msg@1
                    ),
                    Vdom@1 = (erlang:element(7, State))(Model@1),
                    handle_effect(erlang:element(2, State), Effect@1),
                    {state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        Model@1,
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        Vdom@1,
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State),
                        erlang:element(13, State)}
            end;

        {event_fired, _, Path, Name@2, Event} ->
            case lustre@vdom@events:handle(
                erlang:element(10, State),
                Path,
                Name@2,
                Event
            ) of
                {Events, {error, _}} ->
                    {state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        erlang:element(5, State),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        erlang:element(9, State),
                        Events,
                        erlang:element(11, State),
                        erlang:element(12, State),
                        erlang:element(13, State)};

                {Events@1, {ok, Handler}} ->
                    {Model@2, Effect@2} = (erlang:element(6, State))(
                        erlang:element(5, State),
                        erlang:element(4, Handler)
                    ),
                    Vdom@2 = (erlang:element(7, State))(Model@2),
                    handle_effect(erlang:element(2, State), Effect@2),
                    {state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        Model@2,
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        Vdom@2,
                        Events@1,
                        erlang:element(11, State),
                        erlang:element(12, State),
                        erlang:element(13, State)}
            end;

        {context_provided, _, Key, Value@2} ->
            case gleam_stdlib:map_get(
                erlang:element(6, erlang:element(8, State)),
                Key
            ) of
                {error, _} ->
                    State;

                {ok, Decoder} ->
                    case gleam@dynamic@decode:run(Value@2, Decoder) of
                        {error, _} ->
                            State;

                        {ok, Context} ->
                            {Model@3, Effect@3} = (erlang:element(6, State))(
                                erlang:element(5, State),
                                Context
                            ),
                            Vdom@3 = (erlang:element(7, State))(Model@3),
                            handle_effect(erlang:element(2, State), Effect@3),
                            {state,
                                erlang:element(2, State),
                                erlang:element(3, State),
                                erlang:element(4, State),
                                Model@3,
                                erlang:element(6, State),
                                erlang:element(7, State),
                                erlang:element(8, State),
                                Vdom@3,
                                erlang:element(10, State),
                                erlang:element(11, State),
                                erlang:element(12, State),
                                erlang:element(13, State)}
                    end
            end
    end.

-file("src/lustre/runtime/server/runtime.gleam", 370).
?DOC(false).
-spec broadcast(
    gleam@dict:dict(gleam@erlang@process:subject(lustre@runtime@transport:client_message(QZE)), gleam@erlang@process:monitor()),
    gleam@set:set(fun((lustre@runtime@transport:client_message(QZE)) -> nil)),
    lustre@runtime@transport:client_message(QZE)
) -> nil.
broadcast(Clients, Callbacks, Message) ->
    gleam@dict:each(
        Clients,
        fun(Client, _) -> gleam@erlang@process:send(Client, Message) end
    ),
    gleam@set:each(Callbacks, fun(Callback) -> Callback(Message) end).

-file("src/lustre/runtime/server/runtime.gleam", 127).
?DOC(false).
-spec loop(state(QXX, QXY), message(QXY)) -> gleam@otp@actor:next(state(QXX, QXY), message(QXY)).
loop(State, Message) ->
    case Message of
        {client_dispatched_message, Message@1} ->
            Next = handle_client_message(State, Message@1),
            {diff, Patch, Events} = lustre@vdom@diff:diff(
                erlang:element(10, State),
                erlang:element(9, State),
                erlang:element(9, Next)
            ),
            broadcast(
                erlang:element(12, State),
                erlang:element(13, State),
                lustre@runtime@transport:reconcile(Patch)
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, Next),
                    erlang:element(3, Next),
                    erlang:element(4, Next),
                    erlang:element(5, Next),
                    erlang:element(6, Next),
                    erlang:element(7, Next),
                    erlang:element(8, Next),
                    erlang:element(9, Next),
                    Events,
                    erlang:element(11, Next),
                    erlang:element(12, Next),
                    erlang:element(13, Next)}
            );

        {client_registered_subject, Client} ->
            case gleam@dict:has_key(erlang:element(12, State), Client) of
                true ->
                    gleam@otp@actor:continue(State);

                false ->
                    case gleam@erlang@process:subject_owner(Client) of
                        {error, _} ->
                            gleam@otp@actor:continue(State);

                        {ok, Pid} ->
                            Monitor = gleam@erlang@process:monitor(Pid),
                            Subscribers = gleam@dict:insert(
                                erlang:element(12, State),
                                Client,
                                Monitor
                            ),
                            gleam@erlang@process:send(
                                Client,
                                lustre@runtime@transport:mount(
                                    erlang:element(2, erlang:element(8, State)),
                                    erlang:element(3, erlang:element(8, State)),
                                    maps:keys(
                                        erlang:element(
                                            4,
                                            erlang:element(8, State)
                                        )
                                    ),
                                    maps:keys(
                                        erlang:element(
                                            5,
                                            erlang:element(8, State)
                                        )
                                    ),
                                    maps:keys(
                                        erlang:element(
                                            6,
                                            erlang:element(8, State)
                                        )
                                    ),
                                    erlang:element(11, State),
                                    erlang:element(9, State)
                                )
                            ),
                            gleam@otp@actor:continue(
                                {state,
                                    erlang:element(2, State),
                                    erlang:element(3, State),
                                    erlang:element(4, State),
                                    erlang:element(5, State),
                                    erlang:element(6, State),
                                    erlang:element(7, State),
                                    erlang:element(8, State),
                                    erlang:element(9, State),
                                    erlang:element(10, State),
                                    erlang:element(11, State),
                                    Subscribers,
                                    erlang:element(13, State)}
                            )
                    end
            end;

        {client_deregistered_subject, Client@1} ->
            Subscribers@1 = gleam@dict:delete(
                erlang:element(12, State),
                Client@1
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    erlang:element(9, State),
                    erlang:element(10, State),
                    erlang:element(11, State),
                    Subscribers@1,
                    erlang:element(13, State)}
            );

        {client_registered_callback, Callback} ->
            case gleam@set:contains(erlang:element(13, State), Callback) of
                true ->
                    gleam@otp@actor:continue(State);

                false ->
                    Callbacks = gleam@set:insert(
                        erlang:element(13, State),
                        Callback
                    ),
                    Callback(
                        lustre@runtime@transport:mount(
                            erlang:element(2, erlang:element(8, State)),
                            erlang:element(3, erlang:element(8, State)),
                            maps:keys(
                                erlang:element(4, erlang:element(8, State))
                            ),
                            maps:keys(
                                erlang:element(5, erlang:element(8, State))
                            ),
                            maps:keys(
                                erlang:element(6, erlang:element(8, State))
                            ),
                            erlang:element(11, State),
                            erlang:element(9, State)
                        )
                    ),
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            erlang:element(3, State),
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State),
                            erlang:element(7, State),
                            erlang:element(8, State),
                            erlang:element(9, State),
                            erlang:element(10, State),
                            erlang:element(11, State),
                            erlang:element(12, State),
                            Callbacks}
                    )
            end;

        {client_deregistered_callback, Callback@1} ->
            case gleam@set:contains(erlang:element(13, State), Callback@1) of
                false ->
                    gleam@otp@actor:continue(State);

                true ->
                    Callbacks@1 = gleam@set:delete(
                        erlang:element(13, State),
                        Callback@1
                    ),
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            erlang:element(3, State),
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State),
                            erlang:element(7, State),
                            erlang:element(8, State),
                            erlang:element(9, State),
                            erlang:element(10, State),
                            erlang:element(11, State),
                            erlang:element(12, State),
                            Callbacks@1}
                    )
            end;

        {effect_added_selector, Selector} ->
            Base_selector = gleam_erlang_ffi:merge_selector(
                erlang:element(4, State),
                Selector
            ),
            Selector@1 = gleam_erlang_ffi:merge_selector(
                erlang:element(3, State),
                Selector
            ),
            _pipe = gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    Selector@1,
                    Base_selector,
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    erlang:element(9, State),
                    erlang:element(10, State),
                    erlang:element(11, State),
                    erlang:element(12, State),
                    erlang:element(13, State)}
            ),
            gleam@otp@actor:with_selector(_pipe, Selector@1);

        {effect_dispatched_message, Message@2} ->
            {Model, Effect} = (erlang:element(6, State))(
                erlang:element(5, State),
                Message@2
            ),
            Vdom = (erlang:element(7, State))(Model),
            {diff, Patch@1, Events@1} = lustre@vdom@diff:diff(
                erlang:element(10, State),
                erlang:element(9, State),
                Vdom
            ),
            handle_effect(erlang:element(2, State), Effect),
            broadcast(
                erlang:element(12, State),
                erlang:element(13, State),
                lustre@runtime@transport:reconcile(Patch@1)
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    Model,
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    Vdom,
                    Events@1,
                    erlang:element(11, State),
                    erlang:element(12, State),
                    erlang:element(13, State)}
            );

        {effect_emit_event, Name, Data} ->
            broadcast(
                erlang:element(12, State),
                erlang:element(13, State),
                lustre@runtime@transport:emit(Name, Data)
            ),
            gleam@otp@actor:continue(State);

        {effect_provided_value, Key, Value} ->
            Providers = gleam@dict:insert(erlang:element(11, State), Key, Value),
            broadcast(
                erlang:element(12, State),
                erlang:element(13, State),
                lustre@runtime@transport:provide(Key, Value)
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    erlang:element(9, State),
                    erlang:element(10, State),
                    Providers,
                    erlang:element(12, State),
                    erlang:element(13, State)}
            );

        {monitor_reported_down, Monitor@1} ->
            Subscribers@2 = gleam@dict:filter(
                erlang:element(12, State),
                fun(_, M) -> M /= Monitor@1 end
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    erlang:element(9, State),
                    erlang:element(10, State),
                    erlang:element(11, State),
                    Subscribers@2,
                    erlang:element(13, State)}
            );

        system_requested_shutdown ->
            gleam@dict:each(
                erlang:element(12, State),
                fun(_, Monitor@2) ->
                    gleam@erlang@process:demonitor_process(Monitor@2)
                end
            ),
            gleam@otp@actor:stop()
    end.

-file("src/lustre/runtime/server/runtime.gleam", 55).
?DOC(false).
-spec start(
    {QXN, lustre@effect:effect(QXO)},
    fun((QXN, QXO) -> {QXN, lustre@effect:effect(QXO)}),
    fun((QXN) -> lustre@vdom@vnode:element(QXO)),
    config(QXO)
) -> {ok, gleam@erlang@process:subject(message(QXO))} |
    {error, gleam@otp@actor:start_error()}.
start(Init, Update, View, Config) ->
    Result = begin
        _pipe@5 = gleam@otp@actor:new_with_initialiser(
            1000,
            fun(Self) ->
                Vdom = View(erlang:element(1, Init)),
                Events = lustre@vdom@events:from_node(Vdom),
                Base_selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    _pipe@1 = gleam@erlang@process:select(_pipe, Self),
                    gleam@erlang@process:select_monitors(
                        _pipe@1,
                        fun(Down) ->
                            {monitor_reported_down, erlang:element(2, Down)}
                        end
                    )
                end,
                State = {state,
                    Self,
                    Base_selector,
                    Base_selector,
                    erlang:element(1, Init),
                    Update,
                    View,
                    Config,
                    Vdom,
                    Events,
                    maps:new(),
                    maps:new(),
                    gleam@set:new()},
                handle_effect(Self, erlang:element(2, Init)),
                _pipe@2 = gleam@otp@actor:initialised(State),
                _pipe@3 = gleam@otp@actor:selecting(_pipe@2, Base_selector),
                _pipe@4 = gleam@otp@actor:returning(_pipe@3, Self),
                {ok, _pipe@4}
            end
        ),
        _pipe@6 = gleam@otp@actor:on_message(_pipe@5, fun loop/2),
        gleam@otp@actor:start(_pipe@6)
    end,
    case Result of
        {ok, Started} ->
            {ok, erlang:element(3, Started)};

        {error, Error} ->
            {error, Error}
    end.
