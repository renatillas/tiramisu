-module(lustre@vdom@diff).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/vdom/diff.gleam").
-export([diff/3]).
-export_type([diff/1, attribute_change/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type diff(QMT) :: {diff,
        lustre@vdom@patch:patch(QMT),
        lustre@vdom@events:events(QMT)}.

-type attribute_change(QMU) :: {attribute_change,
        list(lustre@vdom@vattr:attribute(QMU)),
        list(lustre@vdom@vattr:attribute(QMU)),
        lustre@vdom@events:events(QMU)}.

-file("src/lustre/vdom/diff.gleam", 557).
?DOC(false).
-spec is_controlled(
    lustre@vdom@events:events(any()),
    binary(),
    binary(),
    lustre@vdom@path:path()
) -> boolean().
is_controlled(Events, Namespace, Tag, Path) ->
    case Tag of
        <<"input"/utf8>> when Namespace =:= <<""/utf8>> ->
            lustre@vdom@events:has_dispatched_events(Events, Path);

        <<"select"/utf8>> when Namespace =:= <<""/utf8>> ->
            lustre@vdom@events:has_dispatched_events(Events, Path);

        <<"textarea"/utf8>> when Namespace =:= <<""/utf8>> ->
            lustre@vdom@events:has_dispatched_events(Events, Path);

        _ ->
            false
    end.

-file("src/lustre/vdom/diff.gleam", 842).
?DOC(false).
-spec property_value_equal(gleam@json:json(), gleam@json:json()) -> boolean().
property_value_equal(A, B) ->
    A =:= B.

-file("src/lustre/vdom/diff.gleam", 578).
?DOC(false).
-spec diff_attributes(
    boolean(),
    lustre@vdom@path:path(),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@events:events(QNW),
    list(lustre@vdom@vattr:attribute(QNW)),
    list(lustre@vdom@vattr:attribute(QNW)),
    list(lustre@vdom@vattr:attribute(QNW)),
    list(lustre@vdom@vattr:attribute(QNW))
) -> attribute_change(QNW).
diff_attributes(Controlled, Path, Mapper, Events, Old, New, Added, Removed) ->
    case {Old, New} of
        {[], []} ->
            {attribute_change, Added, Removed, Events};

        {[{event, _, Name, _, _, _, _, _, _, _} = Prev | Old@1], []} ->
            Removed@1 = [Prev | Removed],
            Events@1 = lustre@vdom@events:remove_event(Events, Path, Name),
            diff_attributes(
                Controlled,
                Path,
                Mapper,
                Events@1,
                Old@1,
                New,
                Added,
                Removed@1
            );

        {[Prev@1 | Old@2], []} ->
            Removed@2 = [Prev@1 | Removed],
            diff_attributes(
                Controlled,
                Path,
                Mapper,
                Events,
                Old@2,
                New,
                Added,
                Removed@2
            );

        {[], [{event, _, Name@1, Handler, _, _, _, _, _, _} = Next | New@1]} ->
            Added@1 = [Next | Added],
            Events@2 = lustre@vdom@events:add_event(
                Events,
                Mapper,
                Path,
                Name@1,
                Handler
            ),
            diff_attributes(
                Controlled,
                Path,
                Mapper,
                Events@2,
                Old,
                New@1,
                Added@1,
                Removed
            );

        {[], [Next@1 | New@2]} ->
            Added@2 = [Next@1 | Added],
            diff_attributes(
                Controlled,
                Path,
                Mapper,
                Events,
                Old,
                New@2,
                Added@2,
                Removed
            );

        {[Prev@2 | Remaining_old], [Next@2 | Remaining_new]} ->
            case {Prev@2, lustre@vdom@vattr:compare(Prev@2, Next@2), Next@2} of
                {{attribute, _, _, _}, eq, {attribute, _, _, _}} ->
                    Has_changes = case erlang:element(3, Next@2) of
                        <<"value"/utf8>> ->
                            Controlled orelse (erlang:element(4, Prev@2) /= erlang:element(
                                4,
                                Next@2
                            ));

                        <<"checked"/utf8>> ->
                            Controlled orelse (erlang:element(4, Prev@2) /= erlang:element(
                                4,
                                Next@2
                            ));

                        <<"selected"/utf8>> ->
                            Controlled orelse (erlang:element(4, Prev@2) /= erlang:element(
                                4,
                                Next@2
                            ));

                        _ ->
                            erlang:element(4, Prev@2) /= erlang:element(
                                4,
                                Next@2
                            )
                    end,
                    Added@3 = case Has_changes of
                        true ->
                            [Next@2 | Added];

                        false ->
                            Added
                    end,
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events,
                        Remaining_old,
                        Remaining_new,
                        Added@3,
                        Removed
                    );

                {{property, _, _, _}, eq, {property, _, _, _}} ->
                    Has_changes@1 = case erlang:element(3, Next@2) of
                        <<"scrollLeft"/utf8>> ->
                            true;

                        <<"scrollRight"/utf8>> ->
                            true;

                        <<"value"/utf8>> ->
                            Controlled orelse not property_value_equal(
                                erlang:element(4, Prev@2),
                                erlang:element(4, Next@2)
                            );

                        <<"checked"/utf8>> ->
                            Controlled orelse not property_value_equal(
                                erlang:element(4, Prev@2),
                                erlang:element(4, Next@2)
                            );

                        <<"selected"/utf8>> ->
                            Controlled orelse not property_value_equal(
                                erlang:element(4, Prev@2),
                                erlang:element(4, Next@2)
                            );

                        _ ->
                            not property_value_equal(
                                erlang:element(4, Prev@2),
                                erlang:element(4, Next@2)
                            )
                    end,
                    Added@4 = case Has_changes@1 of
                        true ->
                            [Next@2 | Added];

                        false ->
                            Added
                    end,
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events,
                        Remaining_old,
                        Remaining_new,
                        Added@4,
                        Removed
                    );

                {{event, _, _, _, _, _, _, _, _, _},
                    eq,
                    {event, _, Name@2, Handler@1, _, _, _, _, _, _}} ->
                    Has_changes@2 = ((((erlang:element(
                        2,
                        erlang:element(6, Prev@2)
                    )
                    /= erlang:element(2, erlang:element(6, Next@2)))
                    orelse (erlang:element(2, erlang:element(7, Prev@2)) /= erlang:element(
                        2,
                        erlang:element(7, Next@2)
                    )))
                    orelse (erlang:element(8, Prev@2) /= erlang:element(
                        8,
                        Next@2
                    )))
                    orelse (erlang:element(9, Prev@2) /= erlang:element(
                        9,
                        Next@2
                    )))
                    orelse (erlang:element(10, Prev@2) /= erlang:element(
                        10,
                        Next@2
                    )),
                    Added@5 = case Has_changes@2 of
                        true ->
                            [Next@2 | Added];

                        false ->
                            Added
                    end,
                    Events@3 = lustre@vdom@events:add_event(
                        Events,
                        Mapper,
                        Path,
                        Name@2,
                        Handler@1
                    ),
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events@3,
                        Remaining_old,
                        Remaining_new,
                        Added@5,
                        Removed
                    );

                {{event, _, Name@3, _, _, _, _, _, _, _}, eq, _} ->
                    Added@6 = [Next@2 | Added],
                    Removed@3 = [Prev@2 | Removed],
                    Events@4 = lustre@vdom@events:remove_event(
                        Events,
                        Path,
                        Name@3
                    ),
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events@4,
                        Remaining_old,
                        Remaining_new,
                        Added@6,
                        Removed@3
                    );

                {_, eq, {event, _, Name@4, Handler@2, _, _, _, _, _, _}} ->
                    Added@7 = [Next@2 | Added],
                    Removed@4 = [Prev@2 | Removed],
                    Events@5 = lustre@vdom@events:add_event(
                        Events,
                        Mapper,
                        Path,
                        Name@4,
                        Handler@2
                    ),
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events@5,
                        Remaining_old,
                        Remaining_new,
                        Added@7,
                        Removed@4
                    );

                {_, eq, _} ->
                    Added@8 = [Next@2 | Added],
                    Removed@5 = [Prev@2 | Removed],
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events,
                        Remaining_old,
                        Remaining_new,
                        Added@8,
                        Removed@5
                    );

                {_, gt, {event, _, Name@5, Handler@3, _, _, _, _, _, _}} ->
                    Added@9 = [Next@2 | Added],
                    Events@6 = lustre@vdom@events:add_event(
                        Events,
                        Mapper,
                        Path,
                        Name@5,
                        Handler@3
                    ),
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events@6,
                        Old,
                        Remaining_new,
                        Added@9,
                        Removed
                    );

                {_, gt, _} ->
                    Added@10 = [Next@2 | Added],
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events,
                        Old,
                        Remaining_new,
                        Added@10,
                        Removed
                    );

                {{event, _, Name@6, _, _, _, _, _, _, _}, lt, _} ->
                    Removed@6 = [Prev@2 | Removed],
                    Events@7 = lustre@vdom@events:remove_event(
                        Events,
                        Path,
                        Name@6
                    ),
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events@7,
                        Remaining_old,
                        New,
                        Added,
                        Removed@6
                    );

                {_, lt, _} ->
                    Removed@7 = [Prev@2 | Removed],
                    diff_attributes(
                        Controlled,
                        Path,
                        Mapper,
                        Events,
                        Remaining_old,
                        New,
                        Added,
                        Removed@7
                    )
            end
    end.

-file("src/lustre/vdom/diff.gleam", 49).
?DOC(false).
-spec do_diff(
    list(lustre@vdom@vnode:element(QNA)),
    lustre@internals@mutable_map:mutable_map(binary(), lustre@vdom@vnode:element(QNA)),
    list(lustre@vdom@vnode:element(QNA)),
    lustre@internals@mutable_map:mutable_map(binary(), lustre@vdom@vnode:element(QNA)),
    lustre@internals@mutable_map:mutable_map(binary(), nil),
    integer(),
    integer(),
    integer(),
    integer(),
    lustre@vdom@path:path(),
    list(lustre@vdom@patch:change(QNA)),
    list(lustre@vdom@patch:patch(QNA)),
    fun((gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_()),
    lustre@vdom@events:events(QNA)
) -> diff(QNA).
do_diff(
    Old,
    Old_keyed,
    New,
    New_keyed,
    Moved,
    Moved_offset,
    Removed,
    Node_index,
    Patch_index,
    Path,
    Changes,
    Children,
    Mapper,
    Events
) ->
    case {Old, New} of
        {[], []} ->
            {diff, {patch, Patch_index, Removed, Changes, Children}, Events};

        {[Prev | Old@1], []} ->
            Removed@1 = case (erlang:element(3, Prev) =:= <<""/utf8>>) orelse not gleam@dict:has_key(
                Moved,
                erlang:element(3, Prev)
            ) of
                true ->
                    Removed + 1;

                false ->
                    Removed
            end,
            Events@1 = lustre@vdom@events:remove_child(
                Events,
                Path,
                Node_index,
                Prev
            ),
            do_diff(
                Old@1,
                Old_keyed,
                New,
                New_keyed,
                Moved,
                Moved_offset,
                Removed@1,
                Node_index,
                Patch_index,
                Path,
                Changes,
                Children,
                Mapper,
                Events@1
            );

        {[], [_ | _]} ->
            Events@2 = lustre@vdom@events:add_children(
                Events,
                Mapper,
                Path,
                Node_index,
                New
            ),
            Insert = lustre@vdom@patch:insert(New, Node_index - Moved_offset),
            Changes@1 = [Insert | Changes],
            {diff, {patch, Patch_index, Removed, Changes@1, Children}, Events@2};

        {[Prev@1 | Old_remaining], [Next | New_remaining]} when erlang:element(
            3,
            Prev@1
        ) =/= erlang:element(3, Next) ->
            Next_did_exist = gleam@dict:get(Old_keyed, erlang:element(3, Next)),
            Prev_does_exist = gleam@dict:has_key(
                New_keyed,
                erlang:element(3, Prev@1)
            ),
            case {Prev_does_exist, Next_did_exist} of
                {true, {ok, Match}} ->
                    case gleam@dict:has_key(Moved, erlang:element(3, Prev@1)) of
                        true ->
                            do_diff(
                                Old_remaining,
                                Old_keyed,
                                New,
                                New_keyed,
                                Moved,
                                Moved_offset - 1,
                                Removed,
                                Node_index,
                                Patch_index,
                                Path,
                                Changes,
                                Children,
                                Mapper,
                                Events
                            );

                        false ->
                            Before = Node_index - Moved_offset,
                            Changes@2 = [lustre@vdom@patch:move(
                                    erlang:element(3, Next),
                                    Before
                                ) |
                                Changes],
                            Moved@1 = gleam@dict:insert(
                                Moved,
                                erlang:element(3, Next),
                                nil
                            ),
                            Moved_offset@1 = Moved_offset + 1,
                            do_diff(
                                [Match | Old],
                                Old_keyed,
                                New,
                                New_keyed,
                                Moved@1,
                                Moved_offset@1,
                                Removed,
                                Node_index,
                                Patch_index,
                                Path,
                                Changes@2,
                                Children,
                                Mapper,
                                Events
                            )
                    end;

                {false, {ok, _}} ->
                    Index = Node_index - Moved_offset,
                    Changes@3 = [lustre@vdom@patch:remove(Index) | Changes],
                    Events@3 = lustre@vdom@events:remove_child(
                        Events,
                        Path,
                        Node_index,
                        Prev@1
                    ),
                    Moved_offset@2 = Moved_offset - 1,
                    do_diff(
                        Old_remaining,
                        Old_keyed,
                        New,
                        New_keyed,
                        Moved,
                        Moved_offset@2,
                        Removed,
                        Node_index,
                        Patch_index,
                        Path,
                        Changes@3,
                        Children,
                        Mapper,
                        Events@3
                    );

                {true, {error, _}} ->
                    Before@1 = Node_index - Moved_offset,
                    Events@4 = lustre@vdom@events:add_child(
                        Events,
                        Mapper,
                        Path,
                        Node_index,
                        Next
                    ),
                    Insert@1 = lustre@vdom@patch:insert([Next], Before@1),
                    Changes@4 = [Insert@1 | Changes],
                    do_diff(
                        Old,
                        Old_keyed,
                        New_remaining,
                        New_keyed,
                        Moved,
                        Moved_offset + 1,
                        Removed,
                        Node_index + 1,
                        Patch_index,
                        Path,
                        Changes@4,
                        Children,
                        Mapper,
                        Events@4
                    );

                {false, {error, _}} ->
                    Change = lustre@vdom@patch:replace(
                        Node_index - Moved_offset,
                        Next
                    ),
                    Events@5 = begin
                        _pipe = Events,
                        _pipe@1 = lustre@vdom@events:remove_child(
                            _pipe,
                            Path,
                            Node_index,
                            Prev@1
                        ),
                        lustre@vdom@events:add_child(
                            _pipe@1,
                            Mapper,
                            Path,
                            Node_index,
                            Next
                        )
                    end,
                    do_diff(
                        Old_remaining,
                        Old_keyed,
                        New_remaining,
                        New_keyed,
                        Moved,
                        Moved_offset,
                        Removed,
                        Node_index + 1,
                        Patch_index,
                        Path,
                        [Change | Changes],
                        Children,
                        Mapper,
                        Events@5
                    )
            end;

        {[{fragment, _, _, _, _, _} = Prev@2 | Old@2],
            [{fragment, _, _, _, _, _} = Next@1 | New@1]} ->
            Composed_mapper = lustre@vdom@events:compose_mapper(
                Mapper,
                erlang:element(4, Next@1)
            ),
            Child_path = lustre@vdom@path:add(
                Path,
                Node_index,
                erlang:element(3, Next@1)
            ),
            Child = do_diff(
                erlang:element(5, Prev@2),
                erlang:element(6, Prev@2),
                erlang:element(5, Next@1),
                erlang:element(6, Next@1),
                gleam@dict:new(),
                0,
                0,
                0,
                Node_index,
                Child_path,
                [],
                [],
                Composed_mapper,
                Events
            ),
            Children@1 = case erlang:element(2, Child) of
                {patch, _, 0, [], []} ->
                    Children;

                _ ->
                    [erlang:element(2, Child) | Children]
            end,
            do_diff(
                Old@2,
                Old_keyed,
                New@1,
                New_keyed,
                Moved,
                Moved_offset,
                Removed,
                Node_index + 1,
                Patch_index,
                Path,
                Changes,
                Children@1,
                Mapper,
                erlang:element(3, Child)
            );

        {[{element, _, _, _, _, _, _, _, _, _, _} = Prev@3 | Old@3],
            [{element, _, _, _, _, _, _, _, _, _, _} = Next@2 | New@2]} when (erlang:element(
            5,
            Prev@3
        ) =:= erlang:element(5, Next@2)) andalso (erlang:element(6, Prev@3) =:= erlang:element(
            6,
            Next@2
        )) ->
            Composed_mapper@1 = lustre@vdom@events:compose_mapper(
                Mapper,
                erlang:element(4, Next@2)
            ),
            Child_path@1 = lustre@vdom@path:add(
                Path,
                Node_index,
                erlang:element(3, Next@2)
            ),
            Controlled = is_controlled(
                Events,
                erlang:element(5, Next@2),
                erlang:element(6, Next@2),
                Child_path@1
            ),
            {attribute_change, Added_attrs, Removed_attrs, Events@6} = diff_attributes(
                Controlled,
                Child_path@1,
                Composed_mapper@1,
                Events,
                erlang:element(7, Prev@3),
                erlang:element(7, Next@2),
                [],
                []
            ),
            Initial_child_changes = case {Added_attrs, Removed_attrs} of
                {[], []} ->
                    [];

                {_, _} ->
                    [lustre@vdom@patch:update(Added_attrs, Removed_attrs)]
            end,
            Child@1 = do_diff(
                erlang:element(8, Prev@3),
                erlang:element(9, Prev@3),
                erlang:element(8, Next@2),
                erlang:element(9, Next@2),
                gleam@dict:new(),
                0,
                0,
                0,
                Node_index,
                Child_path@1,
                Initial_child_changes,
                [],
                Composed_mapper@1,
                Events@6
            ),
            Children@2 = case erlang:element(2, Child@1) of
                {patch, _, 0, [], []} ->
                    Children;

                _ ->
                    [erlang:element(2, Child@1) | Children]
            end,
            do_diff(
                Old@3,
                Old_keyed,
                New@2,
                New_keyed,
                Moved,
                Moved_offset,
                Removed,
                Node_index + 1,
                Patch_index,
                Path,
                Changes,
                Children@2,
                Mapper,
                erlang:element(3, Child@1)
            );

        {[{text, _, _, _, _} = Prev@4 | Old@4],
            [{text, _, _, _, _} = Next@3 | New@3]} when erlang:element(
            5,
            Prev@4
        ) =:= erlang:element(5, Next@3) ->
            do_diff(
                Old@4,
                Old_keyed,
                New@3,
                New_keyed,
                Moved,
                Moved_offset,
                Removed,
                Node_index + 1,
                Patch_index,
                Path,
                Changes,
                Children,
                Mapper,
                Events
            );

        {[{text, _, _, _, _} | Old@5], [{text, _, _, _, _} = Next@4 | New@4]} ->
            Child@2 = lustre@vdom@patch:new(
                Node_index,
                0,
                [lustre@vdom@patch:replace_text(erlang:element(5, Next@4))],
                []
            ),
            do_diff(
                Old@5,
                Old_keyed,
                New@4,
                New_keyed,
                Moved,
                Moved_offset,
                Removed,
                Node_index + 1,
                Patch_index,
                Path,
                Changes,
                [Child@2 | Children],
                Mapper,
                Events
            );

        {[{unsafe_inner_html, _, _, _, _, _, _, _} = Prev@5 | Old@6],
            [{unsafe_inner_html, _, _, _, _, _, _, _} = Next@5 | New@5]} ->
            Composed_mapper@2 = lustre@vdom@events:compose_mapper(
                Mapper,
                erlang:element(4, Next@5)
            ),
            Child_path@2 = lustre@vdom@path:add(
                Path,
                Node_index,
                erlang:element(3, Next@5)
            ),
            {attribute_change, Added_attrs@1, Removed_attrs@1, Events@7} = diff_attributes(
                false,
                Child_path@2,
                Composed_mapper@2,
                Events,
                erlang:element(7, Prev@5),
                erlang:element(7, Next@5),
                [],
                []
            ),
            Child_changes = case {Added_attrs@1, Removed_attrs@1} of
                {[], []} ->
                    [];

                {_, _} ->
                    [lustre@vdom@patch:update(Added_attrs@1, Removed_attrs@1)]
            end,
            Child_changes@1 = case erlang:element(8, Prev@5) =:= erlang:element(
                8,
                Next@5
            ) of
                true ->
                    Child_changes;

                false ->
                    [lustre@vdom@patch:replace_inner_html(
                            erlang:element(8, Next@5)
                        ) |
                        Child_changes]
            end,
            Children@3 = case Child_changes@1 of
                [] ->
                    Children;

                _ ->
                    [lustre@vdom@patch:new(Node_index, 0, Child_changes@1, []) |
                        Children]
            end,
            do_diff(
                Old@6,
                Old_keyed,
                New@5,
                New_keyed,
                Moved,
                Moved_offset,
                Removed,
                Node_index + 1,
                Patch_index,
                Path,
                Changes,
                Children@3,
                Mapper,
                Events@7
            );

        {[Prev@6 | Old_remaining@1], [Next@6 | New_remaining@1]} ->
            Change@1 = lustre@vdom@patch:replace(
                Node_index - Moved_offset,
                Next@6
            ),
            Events@8 = begin
                _pipe@2 = Events,
                _pipe@3 = lustre@vdom@events:remove_child(
                    _pipe@2,
                    Path,
                    Node_index,
                    Prev@6
                ),
                lustre@vdom@events:add_child(
                    _pipe@3,
                    Mapper,
                    Path,
                    Node_index,
                    Next@6
                )
            end,
            do_diff(
                Old_remaining@1,
                Old_keyed,
                New_remaining@1,
                New_keyed,
                Moved,
                Moved_offset,
                Removed,
                Node_index + 1,
                Patch_index,
                Path,
                [Change@1 | Changes],
                Children,
                Mapper,
                Events@8
            )
    end.

-file("src/lustre/vdom/diff.gleam", 24).
?DOC(false).
-spec diff(
    lustre@vdom@events:events(QMV),
    lustre@vdom@vnode:element(QMV),
    lustre@vdom@vnode:element(QMV)
) -> diff(QMV).
diff(Events, Old, New) ->
    do_diff(
        [Old],
        gleam@dict:new(),
        [New],
        gleam@dict:new(),
        gleam@dict:new(),
        0,
        0,
        0,
        0,
        root,
        [],
        [],
        fun gleam@function:identity/1,
        lustre@vdom@events:tick(Events)
    ).
