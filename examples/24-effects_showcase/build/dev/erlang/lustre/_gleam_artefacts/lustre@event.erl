-module(lustre@event).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/event.gleam").
-export([emit/2, handler/3, on/2, advanced/2, prevent_default/1, stop_propagation/1, debounce/2, throttle/2, on_click/1, on_mouse_down/1, on_mouse_up/1, on_mouse_enter/1, on_mouse_leave/1, on_mouse_over/1, on_mouse_out/1, on_keypress/1, on_keydown/1, on_keyup/1, on_input/1, on_change/1, on_check/1, on_submit/1, on_focus/1, on_blur/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/event.gleam", 31).
?DOC(
    " Dispatches a custom message from a Lustre component. This lets components\n"
    " communicate with their parents the same way native DOM elements do.\n"
    "\n"
    " Any JSON-serialisable payload can be attached as additional data for any\n"
    " event listeners to decode. This data will be on the event's `detail` property.\n"
).
-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-file("src/lustre/event.gleam", 94).
?DOC(
    " Construct a [`Handler`](#Handler) that can be used with [`advanced`](#advanced)\n"
    " to conditionally stop propagation or prevent the default behaviour of an event.\n"
).
-spec handler(YKI, boolean(), boolean()) -> lustre@vdom@vattr:handler(YKI).
handler(Message, Prevent_default, Stop_propagation) ->
    {handler, Prevent_default, Stop_propagation, Message}.

-file("src/lustre/event.gleam", 102).
-spec is_immediate_event(binary()) -> boolean().
is_immediate_event(Name) ->
    case Name of
        <<"input"/utf8>> ->
            true;

        <<"change"/utf8>> ->
            true;

        <<"focus"/utf8>> ->
            true;

        <<"focusin"/utf8>> ->
            true;

        <<"focusout"/utf8>> ->
            true;

        <<"blur"/utf8>> ->
            true;

        <<"select"/utf8>> ->
            true;

        _ ->
            false
    end.

-file("src/lustre/event.gleam", 49).
?DOC(
    " Listens for the given event and then runs the given decoder on the event\n"
    " object. If the decoder succeeds, the decoded event is dispatched to your\n"
    " application's `update` function. If it fails, the event is silently ignored.\n"
    "\n"
    " The event name is typically an all-lowercase string such as \"click\" or \"mousemove\".\n"
    " If you're listening for non-standard events (like those emitted by a custom\n"
    " element) their event names might be slightly different.\n"
    "\n"
    " > **Note**: if you are developing a server component, it is important to also\n"
    " > use [`server_component.include`](./server_component.html#include) to state\n"
    " > which properties of the event you need to be sent to the server.\n"
).
-spec on(binary(), gleam@dynamic@decode:decoder(YKB)) -> lustre@vdom@vattr:attribute(YKB).
on(Name, Handler) ->
    lustre@vdom@vattr:event(
        Name,
        gleam@dynamic@decode:map(
            Handler,
            fun(Msg) -> {handler, false, false, Msg} end
        ),
        [],
        {never, 0},
        {never, 0},
        is_immediate_event(Name),
        0,
        0
    ).

-file("src/lustre/event.gleam", 78).
?DOC(
    " Listens for the given event and then runs the given decoder on the event\n"
    " object. This decoder is capable of _conditionally_ stopping propagation or\n"
    " preventing the default behaviour of the event by returning a `Handler` record\n"
    " with the appropriate flags set. This makes it possible to write event handlers\n"
    " for more-advanced scenarios such as handling specific key presses.\n"
    "\n"
    " > **Note**: it is not possible to conditionally stop propagation or prevent\n"
    " > the default behaviour of an event when using _server components_. Your event\n"
    " > handler runs on the server, far away from the browser!\n"
    "\n"
    " > **Note**: if you are developing a server component, it is important to also\n"
    " > use [`server_component.include`](./server_component.html#include) to state\n"
    " > which properties of the event you need to be sent to the server.\n"
).
-spec advanced(
    binary(),
    gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(YKE))
) -> lustre@vdom@vattr:attribute(YKE).
advanced(Name, Handler) ->
    lustre@vdom@vattr:event(
        Name,
        Handler,
        [],
        {possible, 1},
        {possible, 1},
        is_immediate_event(Name),
        0,
        0
    ).

-file("src/lustre/event.gleam", 116).
?DOC(
    " Indicate that the event should have its default behaviour cancelled. This is\n"
    " equivalent to calling `event.preventDefault()` in JavaScript.\n"
    "\n"
    " > **Note**: this will override the conditional behaviour of an event handler\n"
    " > created with [`advanced`](#advanced).\n"
).
-spec prevent_default(lustre@vdom@vattr:attribute(YKK)) -> lustre@vdom@vattr:attribute(YKK).
prevent_default(Event) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            {event,
                erlang:element(2, Event),
                erlang:element(3, Event),
                erlang:element(4, Event),
                erlang:element(5, Event),
                {always, 2},
                erlang:element(7, Event),
                erlang:element(8, Event),
                erlang:element(9, Event),
                erlang:element(10, Event)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 129).
?DOC(
    " Indicate that the event should not propagate to parent elements. This is\n"
    " equivalent to calling `event.stopPropagation()` in JavaScript.\n"
    "\n"
    " > **Note**: this will override the conditional behaviour of an event handler\n"
    " > created with [`advanced`](#advanced).\n"
).
-spec stop_propagation(lustre@vdom@vattr:attribute(YKN)) -> lustre@vdom@vattr:attribute(YKN).
stop_propagation(Event) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            {event,
                erlang:element(2, Event),
                erlang:element(3, Event),
                erlang:element(4, Event),
                erlang:element(5, Event),
                erlang:element(6, Event),
                {always, 2},
                erlang:element(8, Event),
                erlang:element(9, Event),
                erlang:element(10, Event)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 164).
?DOC(
    " Use Lustre's built-in event debouncing to wait a delay after a burst of\n"
    " events before dispatching the most recent one. You can visualise debounced\n"
    " events like so:\n"
    "\n"
    " ```\n"
    "  original : --a-b-cd--e----------f--------\n"
    " debounced : ---------------e----------f---\n"
    " ```\n"
    "\n"
    " This is particularly useful for server components where many events in quick\n"
    " succession can introduce problems because of network latency.\n"
    "\n"
    " The unit of `delay` is millisecond, same as JavaScript's `setTimeout`.\n"
    "\n"
    " ### Example:\n"
    "\n"
    " ```gleam\n"
    " type Msg {\n"
    "     UserInputText(String)\n"
    " }\n"
    " \n"
    " html.input([event.debounce(event.on_input(fn(v) { UserInputText(v) }), 200)])\n"
    " ```\n"
    "\n"
    " > **Note**: debounced events inherently introduce latency. Try to consider\n"
    " > typical interaction patterns and experiment with different delays to balance\n"
    " > responsiveness and update frequency.\n"
).
-spec debounce(lustre@vdom@vattr:attribute(YKQ), integer()) -> lustre@vdom@vattr:attribute(YKQ).
debounce(Event, Delay) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            {event,
                erlang:element(2, Event),
                erlang:element(3, Event),
                erlang:element(4, Event),
                erlang:element(5, Event),
                erlang:element(6, Event),
                erlang:element(7, Event),
                erlang:element(8, Event),
                gleam@int:max(0, Delay),
                erlang:element(10, Event)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 189).
?DOC(
    " Use Lustre's built-in event throttling to restrict the number of events\n"
    " that can be dispatched in a given time period. You can visualise throttled\n"
    " events like so:\n"
    "\n"
    " ```\n"
    " original : --a-b-cd--e----------f--------\n"
    " throttled : -a------ e----------e--------\n"
    " ```\n"
    "\n"
    " This is particularly useful for server components where many events in quick\n"
    " succession can introduce problems because of network latency.\n"
    "\n"
    " The unit of `delay` is millisecond, same as JavaScript's `setTimeout`.\n"
    "\n"
    " > **Note**: throttled events inherently reduce precision. Try to consider\n"
    " > typical interaction patterns and experiment with different delays to balance\n"
    " > responsiveness and update frequency.\n"
).
-spec throttle(lustre@vdom@vattr:attribute(YKT), integer()) -> lustre@vdom@vattr:attribute(YKT).
throttle(Event, Delay) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            {event,
                erlang:element(2, Event),
                erlang:element(3, Event),
                erlang:element(4, Event),
                erlang:element(5, Event),
                erlang:element(6, Event),
                erlang:element(7, Event),
                erlang:element(8, Event),
                erlang:element(9, Event),
                gleam@int:max(0, Delay)};

        _ ->
            Event
    end.

-file("src/lustre/event.gleam", 199).
?DOC("\n").
-spec on_click(YKW) -> lustre@vdom@vattr:attribute(YKW).
on_click(Msg) ->
    on(<<"click"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 204).
?DOC("\n").
-spec on_mouse_down(YKY) -> lustre@vdom@vattr:attribute(YKY).
on_mouse_down(Msg) ->
    on(<<"mousedown"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 209).
?DOC("\n").
-spec on_mouse_up(YLA) -> lustre@vdom@vattr:attribute(YLA).
on_mouse_up(Msg) ->
    on(<<"mouseup"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 214).
?DOC("\n").
-spec on_mouse_enter(YLC) -> lustre@vdom@vattr:attribute(YLC).
on_mouse_enter(Msg) ->
    on(<<"mouseenter"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 219).
?DOC("\n").
-spec on_mouse_leave(YLE) -> lustre@vdom@vattr:attribute(YLE).
on_mouse_leave(Msg) ->
    on(<<"mouseleave"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 224).
?DOC("\n").
-spec on_mouse_over(YLG) -> lustre@vdom@vattr:attribute(YLG).
on_mouse_over(Msg) ->
    on(<<"mouseover"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 229).
?DOC("\n").
-spec on_mouse_out(YLI) -> lustre@vdom@vattr:attribute(YLI).
on_mouse_out(Msg) ->
    on(<<"mouseout"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 238).
?DOC(
    " Listens for key presses on an element, and dispatches a message with the\n"
    " current key being pressed.\n"
).
-spec on_keypress(fun((binary()) -> YLK)) -> lustre@vdom@vattr:attribute(YLK).
on_keypress(Msg) ->
    on(
        <<"keypress"/utf8>>,
        begin
            gleam@dynamic@decode:field(
                <<"key"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Key) -> _pipe = Key,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ).

-file("src/lustre/event.gleam", 249).
?DOC(
    " Listens for key down events on an element, and dispatches a message with the\n"
    " current key being pressed.\n"
).
-spec on_keydown(fun((binary()) -> YLM)) -> lustre@vdom@vattr:attribute(YLM).
on_keydown(Msg) ->
    on(
        <<"keydown"/utf8>>,
        begin
            gleam@dynamic@decode:field(
                <<"key"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Key) -> _pipe = Key,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ).

-file("src/lustre/event.gleam", 260).
?DOC(
    " Listens for key up events on an element, and dispatches a message with the\n"
    " current key being released.\n"
).
-spec on_keyup(fun((binary()) -> YLO)) -> lustre@vdom@vattr:attribute(YLO).
on_keyup(Msg) ->
    on(
        <<"keyup"/utf8>>,
        begin
            gleam@dynamic@decode:field(
                <<"key"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Key) -> _pipe = Key,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ).

-file("src/lustre/event.gleam", 275).
?DOC(
    " Listens for input events on elements such as `<input>`, `<textarea>` and\n"
    " `<select>`. This handler automatically decodes the string value of the input\n"
    " and passes it to the given message function. This is commonly used to\n"
    " implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
).
-spec on_input(fun((binary()) -> YLQ)) -> lustre@vdom@vattr:attribute(YLQ).
on_input(Msg) ->
    on(
        <<"input"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"target"/utf8>>, <<"value"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Value) -> gleam@dynamic@decode:success(Msg(Value)) end
            )
        end
    ).

-file("src/lustre/event.gleam", 288).
?DOC(
    " Listens for change events on elements such as `<input>`, `<textarea>` and\n"
    " `<select>`. This handler automatically decodes the string value of the input\n"
    " and passes it to the given message function. This is commonly used to\n"
    " implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
).
-spec on_change(fun((binary()) -> YLS)) -> lustre@vdom@vattr:attribute(YLS).
on_change(Msg) ->
    on(
        <<"change"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"target"/utf8>>, <<"value"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Value) -> gleam@dynamic@decode:success(Msg(Value)) end
            )
        end
    ).

-file("src/lustre/event.gleam", 301).
?DOC(
    " Listens for change events on `<input type=\"checkbox\">` elements. This handler\n"
    " automatically decodes the boolean value of the checkbox and passes it to\n"
    " the given message function. This is commonly used to implement\n"
    " [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
).
-spec on_check(fun((boolean()) -> YLU)) -> lustre@vdom@vattr:attribute(YLU).
on_check(Msg) ->
    on(
        <<"change"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"target"/utf8>>, <<"checked"/utf8>>],
                {decoder, fun gleam@dynamic@decode:decode_bool/1},
                fun(Checked) -> gleam@dynamic@decode:success(Msg(Checked)) end
            )
        end
    ).

-file("src/lustre/event.gleam", 332).
-spec formdata_decoder() -> gleam@dynamic@decode:decoder(list({binary(),
    binary()})).
formdata_decoder() ->
    String_value_decoder = begin
        gleam@dynamic@decode:field(
            0,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Key) ->
                gleam@dynamic@decode:field(
                    1,
                    gleam@dynamic@decode:one_of(
                        gleam@dynamic@decode:map(
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            fun(Field@0) -> {ok, Field@0} end
                        ),
                        [gleam@dynamic@decode:success({error, nil})]
                    ),
                    fun(Value) -> _pipe = Value,
                        _pipe@1 = gleam@result:map(
                            _pipe,
                            fun(_capture) -> gleam@pair:new(Key, _capture) end
                        ),
                        gleam@dynamic@decode:success(_pipe@1) end
                )
            end
        )
    end,
    _pipe@2 = String_value_decoder,
    _pipe@3 = gleam@dynamic@decode:list(_pipe@2),
    gleam@dynamic@decode:map(_pipe@3, fun gleam@result:values/1).

-file("src/lustre/event.gleam", 321).
?DOC(
    " Listens for submit events on a `<form>` element and receives a list of\n"
    " name/value pairs for each field in the form. Files are not included in this\n"
    " list: if you need them, you can write your own handler for the `\"submit\"`\n"
    " event and decode the non-standard `detail.formData` property manually.\n"
    "\n"
    " This handler is best paired with the [`formal`](https://hexdocs.pm/formal/)\n"
    " package which lets you process form submissions in a type-safe way.\n"
    "\n"
    " This will automatically call [`prevent_default`](#prevent_default) to stop\n"
    " the browser's native form submission. In a Lustre app you'll want to handle\n"
    " that yourself as an [`Effect`](./effect.html#Effect).\n"
).
-spec on_submit(fun((list({binary(), binary()})) -> YLX)) -> lustre@vdom@vattr:attribute(YLX).
on_submit(Msg) ->
    _pipe@2 = on(
        <<"submit"/utf8>>,
        begin
            gleam@dynamic@decode:subfield(
                [<<"detail"/utf8>>, <<"formData"/utf8>>],
                formdata_decoder(),
                fun(Formdata) -> _pipe = Formdata,
                    _pipe@1 = Msg(_pipe),
                    gleam@dynamic@decode:success(_pipe@1) end
            )
        end
    ),
    prevent_default(_pipe@2).

-file("src/lustre/event.gleam", 356).
-spec on_focus(YMB) -> lustre@vdom@vattr:attribute(YMB).
on_focus(Msg) ->
    on(<<"focus"/utf8>>, gleam@dynamic@decode:success(Msg)).

-file("src/lustre/event.gleam", 360).
-spec on_blur(YMD) -> lustre@vdom@vattr:attribute(YMD).
on_blur(Msg) ->
    on(<<"blur"/utf8>>, gleam@dynamic@decode:success(Msg)).
