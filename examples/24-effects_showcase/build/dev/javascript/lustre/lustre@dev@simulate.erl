-module(lustre@dev@simulate).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/dev/simulate.gleam").
-export([simple/3, application/3, start/2, message/2, problem/3, model/1, view/1, history/1, event/4, click/2, input/3, submit/3]).
-export_type([app/3, simulation/2, event/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque app(THP, THQ, THR) :: {app,
        fun((THP) -> {THQ, lustre@effect:effect(THR)}),
        fun((THQ, THR) -> {THQ, lustre@effect:effect(THR)}),
        fun((THQ) -> lustre@vdom@vnode:element(THR))}.

-opaque simulation(THS, THT) :: {simulation,
        fun((THS, THT) -> {THS, lustre@effect:effect(THT)}),
        fun((THS) -> lustre@vdom@vnode:element(THT)),
        list(event(THT)),
        THS,
        lustre@vdom@vnode:element(THT)}.

-type event(THU) :: {dispatch, THU} |
    {event, lustre@dev@query:'query'(), binary(), gleam@json:json()} |
    {problem, binary(), binary()}.

-file("src/lustre/dev/simulate.gleam", 81).
?DOC(
    " Construct a simulated simple Lustre application. The simulation can be started\n"
    " with the [`start`](#start) function by providing the initial arguments for\n"
    " your app's `init` function.\n"
    "\n"
    " DOM events and messages dispatched by effects can be simulated using the\n"
    " [`event`](#event) and [`messgae`](#message) functions.\n"
).
-spec simple(
    fun((THV) -> THW),
    fun((THW, THX) -> THW),
    fun((THW) -> lustre@vdom@vnode:element(THX))
) -> app(THV, THW, THX).
simple(Init, Update, View) ->
    {app,
        fun(Args) -> {Init(Args), lustre@effect:none()} end,
        fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
        View}.

-file("src/lustre/dev/simulate.gleam", 104).
?DOC(
    " Construct a simulated Lustre application. The simulation can be started\n"
    " with the [`start`](#start) function by providing the initial arguments for\n"
    " your app's `init` function.\n"
    "\n"
    " DOM events and messages dispatched by effects can be simulated using the\n"
    " [`event`](#event) and [`messgae`](#message) functions.\n"
    "\n"
    " > **Note**: simulated apps do not run any effects! You can simulate the result\n"
    " > of an effect by using the [`message`](#message) function, but to test side\n"
    " > effects you should test your application in a real environment.\n"
).
-spec application(
    fun((TIC) -> {TID, lustre@effect:effect(TIE)}),
    fun((TID, TIE) -> {TID, lustre@effect:effect(TIE)}),
    fun((TID) -> lustre@vdom@vnode:element(TIE))
) -> app(TIC, TID, TIE).
application(Init, Update, View) ->
    {app, Init, Update, View}.

-file("src/lustre/dev/simulate.gleam", 118).
?DOC(
    " Start a simulated Lustre application. Once a simulation is running you can\n"
    " use the [`message`](#message) and [`event`](#event) functions to simulate\n"
    " events\n"
).
-spec start(app(TIL, TIM, TIN), TIL) -> simulation(TIM, TIN).
start(App, Args) ->
    {Model, _} = (erlang:element(2, App))(Args),
    Html = (erlang:element(4, App))(Model),
    {simulation,
        erlang:element(3, App),
        erlang:element(4, App),
        [],
        Model,
        Html}.

-file("src/lustre/dev/simulate.gleam", 157).
?DOC(
    " Simulate a message sent directly to the runtime. This is often used to mimic\n"
    " the result of some effect you would have run in a real environment. For example,\n"
    " you might simulate a click event on a login button and then simulate the\n"
    " successful response from the server by calling this function with the message\n"
    " you would dispatch from the effect:\n"
    "\n"
    " ```gleam\n"
    " import birdie\n"
    " import lustre/dev/simulate\n"
    " import lustre/dev/query\n"
    " import lustre/element\n"
    "\n"
    " pub fn login_test() {\n"
    "   let app = simulate.application(init:, update:, view:)\n"
    "   let login_button = query.element(matching: query.id(\"login\"))\n"
    "   let user = User(name: \"Lucy\")\n"
    "\n"
    "   simulate.start(app, Nil)\n"
    "   |> simulate.event(on: login_button, name: \"click\", data: [])\n"
    "   // Simulate a successful response from the server\n"
    "   |> simulate.message(ApiReturnedUser(Ok(user)))\n"
    "   |> simulate.view\n"
    "   |> element.to_readable_string\n"
    "   |> birdie.snap(\"Successful login\")\n"
    " }\n"
    " ```\n"
    "\n"
    " > **Note**: your app's `view` function will probably be rendering quite a lot\n"
    " > of HTML! To make your snapshots more meaningful, you might want to couple\n"
    " > this with the [`query`](./query.html) module to only snapshot parts of the\n"
    " > page that are relevant to the test.\n"
).
-spec message(simulation(TIT, TIU), TIU) -> simulation(TIT, TIU).
message(Simulation, Msg) ->
    {Model, _} = (erlang:element(2, Simulation))(
        erlang:element(5, Simulation),
        Msg
    ),
    Html = (erlang:element(3, Simulation))(Model),
    History = [{dispatch, Msg} | erlang:element(4, Simulation)],
    {simulation,
        erlang:element(2, Simulation),
        erlang:element(3, Simulation),
        History,
        Model,
        Html}.

-file("src/lustre/dev/simulate.gleam", 318).
?DOC(
    " Log a problem that occured during the simulation. This function is useful for\n"
    " external packages that want to provide functions to simulate certain effects\n"
    " that may fail in the real world. For example, a routing package may log a\n"
    " problem if a link has an invalid `href` attribute that would cause no message\n"
    " to be dispatched.\n"
    "\n"
    " > **Note**: logging a problem will not stop the simulation from running, just\n"
    " > like a real application!\n"
).
-spec problem(simulation(TJZ, TKA), binary(), binary()) -> simulation(TJZ, TKA).
problem(Simulation, Name, Message) ->
    History = [{problem, Name, Message} | erlang:element(4, Simulation)],
    {simulation,
        erlang:element(2, Simulation),
        erlang:element(3, Simulation),
        History,
        erlang:element(5, Simulation),
        erlang:element(6, Simulation)}.

-file("src/lustre/dev/simulate.gleam", 333).
?DOC(
    " Introspect the current `model` of a running simulation. This can be useful\n"
    " to debug why a simulation is not producing the view you expect.\n"
).
-spec model(simulation(TKF, any())) -> TKF.
model(Simulation) ->
    erlang:element(5, Simulation).

-file("src/lustre/dev/simulate.gleam", 342).
?DOC(
    " Introspect the current `view` of a running simulation. Typically you would\n"
    " use this with a snapshot testing library like [`birdie`](https://hexdocs.pm/birdie/index.html)\n"
    " and/or with the [`query`](./query.html) api to make assertions about the state\n"
    " of the page.\n"
).
-spec view(simulation(any(), TKK)) -> lustre@vdom@vnode:element(TKK).
view(Simulation) ->
    erlang:element(6, Simulation).

-file("src/lustre/dev/simulate.gleam", 354).
?DOC(
    " Receive the current [`Event`](#Event) log of a running simulation. You can\n"
    " use this to produce more detailed snapshots by also rendering the sequence of\n"
    " events that produced the given view.\n"
    "\n"
    " In addition to simulated DOM events and message dispatch, the event log will\n"
    " also include entries for when the queried event target could not be found in\n"
    " the view and cases where an event was fired but not handled by your application.\n"
).
-spec history(simulation(any(), TKP)) -> list(event(TKP)).
history(Simulation) ->
    _pipe = erlang:element(4, Simulation),
    lists:reverse(_pipe).

-file("src/lustre/dev/simulate.gleam", 181).
?DOC(
    " Simulate a DOM event on the first element that matches the given query. The\n"
    " payload represents a simulated event object, and should be used to pass data\n"
    " you expect your event handlers to decode.\n"
    "\n"
    " If no element matches the query, an [`EventTargetNotFound`](#Event) event is\n"
    " logged in the simulation history. If an element is found, but the application\n"
    " has no handler for the event, the [`EventHandlerNotFound`](#Event) event is\n"
    " logged instead.\n"
    "\n"
    " > **Note**: this is not a perfect simulation of a real DOM event. There is no\n"
    " > capture phase of a simulated event and simulated events will not bubble up\n"
    " > to parent elements.\n"
).
-spec event(
    simulation(TIZ, TJA),
    lustre@dev@query:'query'(),
    binary(),
    list({binary(), gleam@json:json()})
) -> simulation(TIZ, TJA).
event(Simulation, Query, Event, Payload) ->
    gleam@result:unwrap_both(
        begin
            gleam@result:'try'(
                gleam@result:replace_error(
                    lustre@dev@query:find_path(
                        erlang:element(6, Simulation),
                        Query,
                        0,
                        root
                    ),
                    problem(
                        Simulation,
                        <<"EventTargetNotFound"/utf8>>,
                        <<"No element matching "/utf8,
                            (lustre@dev@query:to_readable_string(Query))/binary>>
                    )
                ),
                fun(_use0) ->
                    {_, Path} = _use0,
                    Events = lustre@vdom@events:from_node(
                        erlang:element(6, Simulation)
                    ),
                    Data = gleam@json:object(Payload),
                    gleam@result:'try'(
                        gleam@result:replace_error(
                            gleam@pair:second(
                                lustre@vdom@events:handle(
                                    Events,
                                    lustre@vdom@path:to_string(Path),
                                    Event,
                                    begin
                                        _pipe = Data,
                                        _pipe@1 = gleam@json:to_string(_pipe),
                                        _pipe@2 = gleam@json:parse(
                                            _pipe@1,
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_dynamic/1}
                                        ),
                                        gleam@result:unwrap(
                                            _pipe@2,
                                            gleam@function:identity(nil)
                                        )
                                    end
                                )
                            ),
                            problem(
                                Simulation,
                                <<"EventHandlerNotFound"/utf8>>,
                                <<<<<<"No "/utf8, Event/binary>>/binary,
                                        " handler for element matching "/utf8>>/binary,
                                    (lustre@dev@query:to_readable_string(Query))/binary>>
                            )
                        ),
                        fun(Handler) ->
                            {Model, _} = (erlang:element(2, Simulation))(
                                erlang:element(5, Simulation),
                                erlang:element(4, Handler)
                            ),
                            Html = (erlang:element(3, Simulation))(Model),
                            History = [{event, Query, Event, Data} |
                                erlang:element(4, Simulation)],
                            {ok,
                                {simulation,
                                    erlang:element(2, Simulation),
                                    erlang:element(3, Simulation),
                                    History,
                                    Model,
                                    Html}}
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre/dev/simulate.gleam", 241).
?DOC(
    " A convenience function that simulates a click event on the first element\n"
    " matching the given query. This event will have no payload and is only\n"
    " appropriate for event handlers that use Lustre's `on_click` handler or custom\n"
    " handlers that do not decode the event payload.\n"
).
-spec click(simulation(TJG, TJH), lustre@dev@query:'query'()) -> simulation(TJG, TJH).
click(Simulation, Query) ->
    event(Simulation, Query, <<"click"/utf8>>, []).

-file("src/lustre/dev/simulate.gleam", 262).
?DOC(
    " Simulate an input event on the first element matching the given query. This\n"
    " helper has an event payload that looks like this:\n"
    "\n"
    " ```json\n"
    " {\n"
    "   \"target\": {\n"
    "     \"value\": value\n"
    "   }\n"
    " }\n"
    " ```\n"
    "\n"
    " and is appropriate for event handlers that use Lustre's `on_input` handler\n"
    " or custom handlers that only decode the event target value.\n"
).
-spec input(simulation(TJM, TJN), lustre@dev@query:'query'(), binary()) -> simulation(TJM, TJN).
input(Simulation, Query, Value) ->
    event(
        Simulation,
        Query,
        <<"input"/utf8>>,
        [{<<"target"/utf8>>,
                gleam@json:object(
                    [{<<"value"/utf8>>, gleam@json:string(Value)}]
                )}]
    ).

-file("src/lustre/dev/simulate.gleam", 289).
?DOC(
    " Simulate a submit event on the first element matching the given query. The\n"
    " simulated event payload looks like this:\n"
    "\n"
    " ```json\n"
    " {\n"
    "   \"detail\": {\n"
    "     \"formData\": [\n"
    "       ...\n"
    "     ]\n"
    "   }\n"
    " }\n"
    " ```\n"
    "\n"
    " and is appropriate for event handlers that use Lustre's `on_submit` handler\n"
    " or custom handlers that only decode the non-standard `detail.formData`\n"
    " property.\n"
).
-spec submit(
    simulation(TJS, TJT),
    lustre@dev@query:'query'(),
    list({binary(), binary()})
) -> simulation(TJS, TJT).
submit(Simulation, Query, Form_data) ->
    event(
        Simulation,
        Query,
        <<"submit"/utf8>>,
        [{<<"detail"/utf8>>,
                gleam@json:object(
                    [{<<"formData"/utf8>>,
                            gleam@json:array(
                                Form_data,
                                fun(Entry) ->
                                    gleam@json:preprocessed_array(
                                        [gleam@json:string(
                                                erlang:element(1, Entry)
                                            ),
                                            gleam@json:string(
                                                erlang:element(2, Entry)
                                            )]
                                    )
                                end
                            )}]
                )}]
    ).
