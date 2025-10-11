-module(lustre@vdom@events).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/vdom/events.gleam").
-export([new/0, tick/1, remove_event/3, handle/4, has_dispatched_events/2, add_event/5, compose_mapper/2, remove_child/4, add_child/5, from_node/1, add_children/5]).
-export_type([events/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-opaque events(SHN) :: {events,
        lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SHN))),
        list(binary()),
        list(binary())}.

-file("src/lustre/vdom/events.gleam", 43).
?DOC(false).
-spec new() -> events(any()).
new() ->
    {events, gleam@dict:new(), [], []}.

-file("src/lustre/vdom/events.gleam", 55).
?DOC(false).
-spec tick(events(SHT)) -> events(SHT).
tick(Events) ->
    {events, erlang:element(2, Events), erlang:element(4, Events), []}.

-file("src/lustre/vdom/events.gleam", 101).
?DOC(false).
-spec do_remove_event(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SIP))),
    lustre@vdom@path:path(),
    binary()
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SIP))).
do_remove_event(Handlers, Path, Name) ->
    gleam@dict:delete(Handlers, lustre@vdom@path:event(Path, Name)).

-file("src/lustre/vdom/events.gleam", 92).
?DOC(false).
-spec remove_event(events(SIM), lustre@vdom@path:path(), binary()) -> events(SIM).
remove_event(Events, Path, Name) ->
    Handlers = do_remove_event(erlang:element(2, Events), Path, Name),
    {events, Handlers, erlang:element(3, Events), erlang:element(4, Events)}.

-file("src/lustre/vdom/events.gleam", 238).
?DOC(false).
-spec remove_attributes(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SLB))),
    lustre@vdom@path:path(),
    list(lustre@vdom@vattr:attribute(SLB))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SLB))).
remove_attributes(Handlers, Path, Attributes) ->
    gleam@list:fold(
        Attributes,
        Handlers,
        fun(Events, Attribute) -> case Attribute of
                {event, _, Name, _, _, _, _, _, _, _} ->
                    do_remove_event(Events, Path, Name);

                _ ->
                    Events
            end end
    ).

-file("src/lustre/vdom/events.gleam", 270).
?DOC(false).
-spec handle(events(SLX), binary(), binary(), gleam@dynamic:dynamic_()) -> {events(SLX),
    {ok, lustre@vdom@vattr:handler(SLX)} |
        {error, list(gleam@dynamic@decode:decode_error())}}.
handle(Events, Path, Name, Event) ->
    Next_dispatched_paths = [Path | erlang:element(4, Events)],
    Events@1 = {events,
        erlang:element(2, Events),
        erlang:element(3, Events),
        Next_dispatched_paths},
    case gleam@dict:get(
        erlang:element(2, Events@1),
        <<<<Path/binary, (<<"\n"/utf8>>)/binary>>/binary, Name/binary>>
    ) of
        {ok, Handler} ->
            {Events@1, gleam@dynamic@decode:run(Event, Handler)};

        {error, _} ->
            {Events@1, {error, []}}
    end.

-file("src/lustre/vdom/events.gleam", 285).
?DOC(false).
-spec has_dispatched_events(events(any()), lustre@vdom@path:path()) -> boolean().
has_dispatched_events(Events, Path) ->
    lustre@vdom@path:matches(Path, erlang:element(3, Events)).

-file("src/lustre/vdom/events.gleam", 76).
?DOC(false).
-spec do_add_event(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SIB))),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    binary(),
    gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SIB))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SIB))).
do_add_event(Handlers, Mapper, Path, Name, Handler) ->
    gleam@dict:insert(
        Handlers,
        lustre@vdom@path:event(Path, Name),
        gleam@dynamic@decode:map(
            Handler,
            fun(Handler@1) ->
                {handler,
                    erlang:element(2, Handler@1),
                    erlang:element(3, Handler@1),
                    (gleam@function:identity(Mapper))(
                        erlang:element(4, Handler@1)
                    )}
            end
        )
    ).

-file("src/lustre/vdom/events.gleam", 65).
?DOC(false).
-spec add_event(
    events(SHW),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    binary(),
    gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SHW))
) -> events(SHW).
add_event(Events, Mapper, Path, Name, Handler) ->
    Handlers = do_add_event(
        erlang:element(2, Events),
        Mapper,
        Path,
        Name,
        Handler
    ),
    {events, Handlers, erlang:element(3, Events), erlang:element(4, Events)}.

-file("src/lustre/vdom/events.gleam", 154).
?DOC(false).
-spec add_attributes(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SJM))),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    list(lustre@vdom@vattr:attribute(SJM))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SJM))).
add_attributes(Handlers, Mapper, Path, Attributes) ->
    gleam@list:fold(
        Attributes,
        Handlers,
        fun(Events, Attribute) -> case Attribute of
                {event, _, Name, Handler, _, _, _, _, _, _} ->
                    do_add_event(Events, Mapper, Path, Name, Handler);

                _ ->
                    Events
            end end
    ).

-file("src/lustre/vdom/events.gleam", 294).
?DOC(false).
-spec is_reference_equal(SMJ, SMJ) -> boolean().
is_reference_equal(A, B) ->
    A =:= B.

-file("src/lustre/vdom/events.gleam", 28).
?DOC(false).
-spec compose_mapper(
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_())
) -> fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()).
compose_mapper(Mapper, Child_mapper) ->
    case {is_reference_equal(Mapper, fun gleam@function:identity/1),
        is_reference_equal(Child_mapper, fun gleam@function:identity/1)} of
        {_, true} ->
            Mapper;

        {true, false} ->
            Child_mapper;

        {false, false} ->
            fun(Msg) -> Mapper(Child_mapper(Msg)) end
    end.

-file("src/lustre/vdom/events.gleam", 250).
?DOC(false).
-spec do_remove_children(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SLM))),
    lustre@vdom@path:path(),
    integer(),
    list(lustre@vdom@vnode:element(SLM))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SLM))).
do_remove_children(Handlers, Path, Child_index, Children) ->
    case Children of
        [] ->
            Handlers;

        [Child | Rest] ->
            _pipe = Handlers,
            _pipe@1 = do_remove_child(_pipe, Path, Child_index, Child),
            do_remove_children(_pipe@1, Path, Child_index + 1, Rest)
    end.

-file("src/lustre/vdom/events.gleam", 209).
?DOC(false).
-spec do_remove_child(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SKR))),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(SKR)
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SKR))).
do_remove_child(Handlers, Parent, Child_index, Child) ->
    case Child of
        {element, _, _, _, _, _, Attributes, Children, _, _, _} ->
            Path = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            _pipe = Handlers,
            _pipe@1 = remove_attributes(_pipe, Path, Attributes),
            do_remove_children(_pipe@1, Path, 0, Children);

        {fragment, _, _, _, Children@1, _} ->
            Path@1 = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            do_remove_children(Handlers, Path@1, 0, Children@1);

        {unsafe_inner_html, _, _, _, _, _, Attributes@1, _} ->
            Path@2 = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            remove_attributes(Handlers, Path@2, Attributes@1);

        {text, _, _, _, _} ->
            Handlers
    end.

-file("src/lustre/vdom/events.gleam", 199).
?DOC(false).
-spec remove_child(
    events(SKN),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(SKN)
) -> events(SKN).
remove_child(Events, Parent, Child_index, Child) ->
    Handlers = do_remove_child(
        erlang:element(2, Events),
        Parent,
        Child_index,
        Child
    ),
    {events, Handlers, erlang:element(3, Events), erlang:element(4, Events)}.

-file("src/lustre/vdom/events.gleam", 182).
?DOC(false).
-spec do_add_children(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SKC))),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    list(lustre@vdom@vnode:element(SKC))
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SKC))).
do_add_children(Handlers, Mapper, Path, Child_index, Children) ->
    case Children of
        [] ->
            Handlers;

        [Child | Rest] ->
            _pipe = Handlers,
            _pipe@1 = do_add_child(_pipe, Mapper, Path, Child_index, Child),
            do_add_children(_pipe@1, Mapper, Path, Child_index + 1, Rest)
    end.

-file("src/lustre/vdom/events.gleam", 120).
?DOC(false).
-spec do_add_child(
    lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SJC))),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(SJC)
) -> lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(lustre@vdom@vattr:handler(SJC))).
do_add_child(Handlers, Mapper, Parent, Child_index, Child) ->
    case Child of
        {element, _, _, _, _, _, Attributes, Children, _, _, _} ->
            Path = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            Composed_mapper = compose_mapper(Mapper, erlang:element(4, Child)),
            _pipe = Handlers,
            _pipe@1 = add_attributes(_pipe, Composed_mapper, Path, Attributes),
            do_add_children(_pipe@1, Composed_mapper, Path, 0, Children);

        {fragment, _, _, _, Children@1, _} ->
            Path@1 = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            Composed_mapper@1 = compose_mapper(Mapper, erlang:element(4, Child)),
            do_add_children(Handlers, Composed_mapper@1, Path@1, 0, Children@1);

        {unsafe_inner_html, _, _, _, _, _, Attributes@1, _} ->
            Path@2 = lustre@vdom@path:add(
                Parent,
                Child_index,
                erlang:element(3, Child)
            ),
            Composed_mapper@2 = compose_mapper(Mapper, erlang:element(4, Child)),
            add_attributes(Handlers, Composed_mapper@2, Path@2, Attributes@1);

        {text, _, _, _, _} ->
            Handlers
    end.

-file("src/lustre/vdom/events.gleam", 109).
?DOC(false).
-spec add_child(
    events(SIY),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    lustre@vdom@vnode:element(SIY)
) -> events(SIY).
add_child(Events, Mapper, Parent, Index, Child) ->
    Handlers = do_add_child(
        erlang:element(2, Events),
        Mapper,
        Parent,
        Index,
        Child
    ),
    {events, Handlers, erlang:element(3, Events), erlang:element(4, Events)}.

-file("src/lustre/vdom/events.gleam", 51).
?DOC(false).
-spec from_node(lustre@vdom@vnode:element(SHQ)) -> events(SHQ).
from_node(Root) ->
    add_child(new(), fun gleam@function:identity/1, root, 0, Root).

-file("src/lustre/vdom/events.gleam", 170).
?DOC(false).
-spec add_children(
    events(SJX),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@path:path(),
    integer(),
    list(lustre@vdom@vnode:element(SJX))
) -> events(SJX).
add_children(Events, Mapper, Path, Child_index, Children) ->
    Handlers = do_add_children(
        erlang:element(2, Events),
        Mapper,
        Path,
        Child_index,
        Children
    ),
    {events, Handlers, erlang:element(3, Events), erlang:element(4, Events)}.
