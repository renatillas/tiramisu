-module(tiramisu@spatial).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/spatial.gleam").
-export([aabb/2, aabb_from_center/2, aabb_contains_point/2, aabb_intersects/2, aabb_center/1, aabb_size/1, octree_new/2, octree_query/2, octree_query_radius/3, octree_query_all/1, octree_count/1, octree_bounds/1, octree_insert/3]).
-export_type([a_a_b_b/0, octree/1, octree_children/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Spatial partitioning data structures for efficient spatial queries.\n"
    "\n"
    " Provides octree and AABB (Axis-Aligned Bounding Box) for:\n"
    " - Finding objects within a region (10-100x faster than linear search)\n"
    " - Finding nearby objects\n"
    " - Frustum culling optimization\n"
    " - Broad-phase collision detection\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/spatial\n"
    "\n"
    " // Create octree for world bounds\n"
    " let world_bounds = spatial.aabb(\n"
    "   min: vec3.Vec3(-100.0, -100.0, -100.0),\n"
    "   max: vec3.Vec3(100.0, 100.0, 100.0),\n"
    " )\n"
    " let tree = spatial.new_octree(bounds: world_bounds, capacity: 8)\n"
    "\n"
    " // Insert enemies\n"
    " let tree = spatial.insert(tree, enemy_pos, enemy_id)\n"
    "\n"
    " // Find enemies near player\n"
    " let nearby = spatial.query_radius(tree, player_pos, radius: 10.0)\n"
    " ```\n"
).

-type a_a_b_b() :: {a_a_b_b, vec@vec3:vec3(float()), vec@vec3:vec3(float())}.

-opaque octree(AVJS) :: {octree_node,
        a_a_b_b(),
        integer(),
        list({vec@vec3:vec3(float()), AVJS}),
        gleam@option:option(octree_children(AVJS))}.

-type octree_children(AVJT) :: {octree_children,
        octree(AVJT),
        octree(AVJT),
        octree(AVJT),
        octree(AVJT),
        octree(AVJT),
        octree(AVJT),
        octree(AVJT),
        octree(AVJT)}.

-file("src/tiramisu/spatial.gleam", 69).
?DOC(" Create an AABB from min and max points\n").
-spec aabb(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> a_a_b_b().
aabb(Min, Max) ->
    {a_a_b_b, Min, Max}.

-file("src/tiramisu/spatial.gleam", 74).
?DOC(" Create an AABB from center and half-extents\n").
-spec aabb_from_center(vec@vec3:vec3(float()), vec@vec3:vec3(float())) -> a_a_b_b().
aabb_from_center(Center, Half_extents) ->
    {a_a_b_b,
        vec@vec3f:subtract(Center, Half_extents),
        vec@vec3f:add(Center, Half_extents)}.

-file("src/tiramisu/spatial.gleam", 82).
?DOC(" Check if a point is inside an AABB\n").
-spec aabb_contains_point(a_a_b_b(), vec@vec3:vec3(float())) -> boolean().
aabb_contains_point(Bounds, Point) ->
    (((((erlang:element(2, Point) >= erlang:element(
        2,
        erlang:element(2, Bounds)
    ))
    andalso (erlang:element(2, Point) =< erlang:element(
        2,
        erlang:element(3, Bounds)
    )))
    andalso (erlang:element(3, Point) >= erlang:element(
        3,
        erlang:element(2, Bounds)
    )))
    andalso (erlang:element(3, Point) =< erlang:element(
        3,
        erlang:element(3, Bounds)
    )))
    andalso (erlang:element(4, Point) >= erlang:element(
        4,
        erlang:element(2, Bounds)
    )))
    andalso (erlang:element(4, Point) =< erlang:element(
        4,
        erlang:element(3, Bounds)
    )).

-file("src/tiramisu/spatial.gleam", 92).
?DOC(" Check if two AABBs intersect\n").
-spec aabb_intersects(a_a_b_b(), a_a_b_b()) -> boolean().
aabb_intersects(A, B) ->
    (((((erlang:element(2, erlang:element(2, A)) =< erlang:element(
        2,
        erlang:element(3, B)
    ))
    andalso (erlang:element(2, erlang:element(3, A)) >= erlang:element(
        2,
        erlang:element(2, B)
    )))
    andalso (erlang:element(3, erlang:element(2, A)) =< erlang:element(
        3,
        erlang:element(3, B)
    )))
    andalso (erlang:element(3, erlang:element(3, A)) >= erlang:element(
        3,
        erlang:element(2, B)
    )))
    andalso (erlang:element(4, erlang:element(2, A)) =< erlang:element(
        4,
        erlang:element(3, B)
    )))
    andalso (erlang:element(4, erlang:element(3, A)) >= erlang:element(
        4,
        erlang:element(2, B)
    )).

-file("src/tiramisu/spatial.gleam", 102).
?DOC(" Get the center of an AABB\n").
-spec aabb_center(a_a_b_b()) -> vec@vec3:vec3(float()).
aabb_center(Bounds) ->
    {vec3,
        (erlang:element(2, erlang:element(2, Bounds)) + erlang:element(
            2,
            erlang:element(3, Bounds)
        ))
        / 2.0,
        (erlang:element(3, erlang:element(2, Bounds)) + erlang:element(
            3,
            erlang:element(3, Bounds)
        ))
        / 2.0,
        (erlang:element(4, erlang:element(2, Bounds)) + erlang:element(
            4,
            erlang:element(3, Bounds)
        ))
        / 2.0}.

-file("src/tiramisu/spatial.gleam", 111).
?DOC(" Get the size (dimensions) of an AABB\n").
-spec aabb_size(a_a_b_b()) -> vec@vec3:vec3(float()).
aabb_size(Bounds) ->
    vec@vec3f:subtract(erlang:element(3, Bounds), erlang:element(2, Bounds)).

-file("src/tiramisu/spatial.gleam", 131).
?DOC(
    " Create a new empty octree\n"
    "\n"
    " ## Parameters\n"
    " - `bounds`: The spatial region this octree covers\n"
    " - `capacity`: Maximum items per node before subdividing (typically 8-16)\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " let bounds = aabb(\n"
    "   min: vec3.Vec3(-100.0, -100.0, -100.0),\n"
    "   max: vec3.Vec3(100.0, 100.0, 100.0)\n"
    " )\n"
    " let tree = octree_new(bounds, capacity: 8)\n"
    " ```\n"
).
-spec octree_new(a_a_b_b(), integer()) -> octree(any()).
octree_new(Bounds, Capacity) ->
    {octree_node, Bounds, Capacity, [], none}.

-file("src/tiramisu/spatial.gleam", 179).
?DOC(" Subdivide an octree node into 8 octants\n").
-spec subdivide(octree(AVKH)) -> octree(AVKH).
subdivide(Tree) ->
    case Tree of
        {octree_node, Bounds, Capacity, _, _} ->
            Center = aabb_center(Bounds),
            _ = vec@vec3f:scale(aabb_size(Bounds), 0.5),
            Bottom_nw = octree_new(
                {a_a_b_b,
                    erlang:element(2, Bounds),
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, Center),
                        erlang:element(4, Center)}},
                Capacity
            ),
            Bottom_ne = octree_new(
                {a_a_b_b,
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, erlang:element(2, Bounds)),
                        erlang:element(4, erlang:element(2, Bounds))},
                    {vec3,
                        erlang:element(2, erlang:element(3, Bounds)),
                        erlang:element(3, Center),
                        erlang:element(4, Center)}},
                Capacity
            ),
            Bottom_sw = octree_new(
                {a_a_b_b,
                    {vec3,
                        erlang:element(2, erlang:element(2, Bounds)),
                        erlang:element(3, erlang:element(2, Bounds)),
                        erlang:element(4, Center)},
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, Center),
                        erlang:element(4, erlang:element(3, Bounds))}},
                Capacity
            ),
            Bottom_se = octree_new(
                {a_a_b_b,
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, erlang:element(2, Bounds)),
                        erlang:element(4, Center)},
                    {vec3,
                        erlang:element(2, erlang:element(3, Bounds)),
                        erlang:element(3, Center),
                        erlang:element(4, erlang:element(3, Bounds))}},
                Capacity
            ),
            Top_nw = octree_new(
                {a_a_b_b,
                    {vec3,
                        erlang:element(2, erlang:element(2, Bounds)),
                        erlang:element(3, Center),
                        erlang:element(4, erlang:element(2, Bounds))},
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, erlang:element(3, Bounds)),
                        erlang:element(4, Center)}},
                Capacity
            ),
            Top_ne = octree_new(
                {a_a_b_b,
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, Center),
                        erlang:element(4, erlang:element(2, Bounds))},
                    {vec3,
                        erlang:element(2, erlang:element(3, Bounds)),
                        erlang:element(3, erlang:element(3, Bounds)),
                        erlang:element(4, Center)}},
                Capacity
            ),
            Top_sw = octree_new(
                {a_a_b_b,
                    {vec3,
                        erlang:element(2, erlang:element(2, Bounds)),
                        erlang:element(3, Center),
                        erlang:element(4, Center)},
                    {vec3,
                        erlang:element(2, Center),
                        erlang:element(3, erlang:element(3, Bounds)),
                        erlang:element(4, erlang:element(3, Bounds))}},
                Capacity
            ),
            Top_se = octree_new(
                {a_a_b_b, Center, erlang:element(3, Bounds)},
                Capacity
            ),
            {octree_node,
                Bounds,
                Capacity,
                [],
                {some,
                    {octree_children,
                        Bottom_nw,
                        Bottom_ne,
                        Bottom_sw,
                        Bottom_se,
                        Top_nw,
                        Top_ne,
                        Top_sw,
                        Top_se}}}
    end.

-file("src/tiramisu/spatial.gleam", 333).
?DOC(" Query all items within a bounding box region\n").
-spec octree_query(octree(AVKO), a_a_b_b()) -> list({vec@vec3:vec3(float()),
    AVKO}).
octree_query(Tree, Query_bounds) ->
    case Tree of
        {octree_node, Bounds, _, Items, Children} ->
            case aabb_intersects(Bounds, Query_bounds) of
                false ->
                    [];

                true ->
                    Local_matches = gleam@list:filter(
                        Items,
                        fun(Item_pair) ->
                            {Pos, _} = Item_pair,
                            aabb_contains_point(Query_bounds, Pos)
                        end
                    ),
                    case Children of
                        none ->
                            Local_matches;

                        {some,
                            {octree_children,
                                Bottom_nw,
                                Bottom_ne,
                                Bottom_sw,
                                Bottom_se,
                                Top_nw,
                                Top_ne,
                                Top_sw,
                                Top_se}} ->
                            lists:append(
                                [Local_matches,
                                    octree_query(Bottom_nw, Query_bounds),
                                    octree_query(Bottom_ne, Query_bounds),
                                    octree_query(Bottom_sw, Query_bounds),
                                    octree_query(Bottom_se, Query_bounds),
                                    octree_query(Top_nw, Query_bounds),
                                    octree_query(Top_ne, Query_bounds),
                                    octree_query(Top_sw, Query_bounds),
                                    octree_query(Top_se, Query_bounds)]
                            )
                    end
            end
    end.

-file("src/tiramisu/spatial.gleam", 382).
?DOC(" Query all items within a radius of a point\n").
-spec octree_query_radius(octree(AVKS), vec@vec3:vec3(float()), float()) -> list({vec@vec3:vec3(float()),
    AVKS}).
octree_query_radius(Tree, Center, Radius) ->
    Half_extents = {vec3, Radius, Radius, Radius},
    Query_bounds = aabb_from_center(Center, Half_extents),
    _pipe = octree_query(Tree, Query_bounds),
    gleam@list:filter(
        _pipe,
        fun(Item_pair) ->
            {Pos, _} = Item_pair,
            vec@vec3f:distance(Center, Pos) =< Radius
        end
    ).

-file("src/tiramisu/spatial.gleam", 400).
?DOC(" Query all items in the octree (useful for iteration)\n").
-spec octree_query_all(octree(AVKX)) -> list({vec@vec3:vec3(float()), AVKX}).
octree_query_all(Tree) ->
    case Tree of
        {octree_node, _, _, Items, Children} ->
            case Children of
                none ->
                    Items;

                {some,
                    {octree_children,
                        Bottom_nw,
                        Bottom_ne,
                        Bottom_sw,
                        Bottom_se,
                        Top_nw,
                        Top_ne,
                        Top_sw,
                        Top_se}} ->
                    lists:append(
                        [Items,
                            octree_query_all(Bottom_nw),
                            octree_query_all(Bottom_ne),
                            octree_query_all(Bottom_sw),
                            octree_query_all(Bottom_se),
                            octree_query_all(Top_nw),
                            octree_query_all(Top_ne),
                            octree_query_all(Top_sw),
                            octree_query_all(Top_se)]
                    )
            end
    end.

-file("src/tiramisu/spatial.gleam", 431).
?DOC(" Count total items in the octree\n").
-spec octree_count(octree(any())) -> integer().
octree_count(Tree) ->
    _pipe = octree_query_all(Tree),
    erlang:length(_pipe).

-file("src/tiramisu/spatial.gleam", 437).
?DOC(" Get the bounds of the octree\n").
-spec octree_bounds(octree(any())) -> a_a_b_b().
octree_bounds(Tree) ->
    case Tree of
        {octree_node, Bounds, _, _, _} ->
            Bounds
    end.

-file("src/tiramisu/spatial.gleam", 261).
?DOC(" Insert item into the appropriate child octant\n").
-spec insert_into_child(
    a_a_b_b(),
    octree_children(AVKK),
    vec@vec3:vec3(float()),
    AVKK
) -> octree_children(AVKK).
insert_into_child(Parent_bounds, Children, Position, Item) ->
    case Children of
        {octree_children,
            Bottom_nw,
            Bottom_ne,
            Bottom_sw,
            Bottom_se,
            Top_nw,
            Top_ne,
            Top_sw,
            Top_se} ->
            Center = aabb_center(Parent_bounds),
            case {erlang:element(2, Position) < erlang:element(2, Center),
                erlang:element(3, Position) < erlang:element(3, Center),
                erlang:element(4, Position) < erlang:element(4, Center)} of
                {true, true, true} ->
                    {octree_children,
                        octree_insert(Bottom_nw, Position, Item),
                        erlang:element(3, Children),
                        erlang:element(4, Children),
                        erlang:element(5, Children),
                        erlang:element(6, Children),
                        erlang:element(7, Children),
                        erlang:element(8, Children),
                        erlang:element(9, Children)};

                {false, true, true} ->
                    {octree_children,
                        erlang:element(2, Children),
                        octree_insert(Bottom_ne, Position, Item),
                        erlang:element(4, Children),
                        erlang:element(5, Children),
                        erlang:element(6, Children),
                        erlang:element(7, Children),
                        erlang:element(8, Children),
                        erlang:element(9, Children)};

                {true, true, false} ->
                    {octree_children,
                        erlang:element(2, Children),
                        erlang:element(3, Children),
                        octree_insert(Bottom_sw, Position, Item),
                        erlang:element(5, Children),
                        erlang:element(6, Children),
                        erlang:element(7, Children),
                        erlang:element(8, Children),
                        erlang:element(9, Children)};

                {false, true, false} ->
                    {octree_children,
                        erlang:element(2, Children),
                        erlang:element(3, Children),
                        erlang:element(4, Children),
                        octree_insert(Bottom_se, Position, Item),
                        erlang:element(6, Children),
                        erlang:element(7, Children),
                        erlang:element(8, Children),
                        erlang:element(9, Children)};

                {true, false, true} ->
                    {octree_children,
                        erlang:element(2, Children),
                        erlang:element(3, Children),
                        erlang:element(4, Children),
                        erlang:element(5, Children),
                        octree_insert(Top_nw, Position, Item),
                        erlang:element(7, Children),
                        erlang:element(8, Children),
                        erlang:element(9, Children)};

                {false, false, true} ->
                    {octree_children,
                        erlang:element(2, Children),
                        erlang:element(3, Children),
                        erlang:element(4, Children),
                        erlang:element(5, Children),
                        erlang:element(6, Children),
                        octree_insert(Top_ne, Position, Item),
                        erlang:element(8, Children),
                        erlang:element(9, Children)};

                {true, false, false} ->
                    {octree_children,
                        erlang:element(2, Children),
                        erlang:element(3, Children),
                        erlang:element(4, Children),
                        erlang:element(5, Children),
                        erlang:element(6, Children),
                        erlang:element(7, Children),
                        octree_insert(Top_sw, Position, Item),
                        erlang:element(9, Children)};

                {false, false, false} ->
                    {octree_children,
                        erlang:element(2, Children),
                        erlang:element(3, Children),
                        erlang:element(4, Children),
                        erlang:element(5, Children),
                        erlang:element(6, Children),
                        erlang:element(7, Children),
                        erlang:element(8, Children),
                        octree_insert(Top_se, Position, Item)}
            end
    end.

-file("src/tiramisu/spatial.gleam", 136).
?DOC(" Insert an item at a position into the octree\n").
-spec octree_insert(octree(AVKD), vec@vec3:vec3(float()), AVKD) -> octree(AVKD).
octree_insert(Tree, Position, Item) ->
    case Tree of
        {octree_node, Bounds, Capacity, Items, Children} ->
            case aabb_contains_point(Bounds, Position) of
                false ->
                    Tree;

                true ->
                    case Children of
                        none ->
                            New_items = [{Position, Item} | Items],
                            case erlang:length(New_items) > Capacity of
                                false ->
                                    {octree_node,
                                        erlang:element(2, Tree),
                                        erlang:element(3, Tree),
                                        New_items,
                                        erlang:element(5, Tree)};

                                true ->
                                    Subdivided = subdivide(Tree),
                                    gleam@list:fold(
                                        New_items,
                                        Subdivided,
                                        fun(Acc, Item_pair) ->
                                            {Pos, It} = Item_pair,
                                            octree_insert(Acc, Pos, It)
                                        end
                                    )
                            end;

                        {some, Octants} ->
                            New_children = insert_into_child(
                                Bounds,
                                Octants,
                                Position,
                                Item
                            ),
                            {octree_node,
                                erlang:element(2, Tree),
                                erlang:element(3, Tree),
                                erlang:element(4, Tree),
                                {some, New_children}}
                    end
            end
    end.
