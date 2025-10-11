-module(tiramisu@debug).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/debug.gleam").
-export([bounding_box/4, sphere/4, line/4, ray/5, axes/3, grid/4, point/4, box_from_transform/3, path/3, cross/4, with_collider_wireframes/2, collider/4]).
-export_type([performance_stats/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type performance_stats() :: {performance_stats,
        float(),
        float(),
        integer(),
        integer(),
        float()}.

-file("src/tiramisu/debug.gleam", 10).
-spec bounding_box(
    AUSB,
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    integer()
) -> tiramisu@scene:node_(AUSB).
bounding_box(Id, Min, Max, Color) ->
    {debug_box, Id, Min, Max, Color}.

-file("src/tiramisu/debug.gleam", 19).
-spec sphere(AUSF, vec@vec3:vec3(float()), float(), integer()) -> tiramisu@scene:node_(AUSF).
sphere(Id, Center, Radius, Color) ->
    {debug_sphere, Id, Center, Radius, Color}.

-file("src/tiramisu/debug.gleam", 28).
-spec line(AUSI, vec@vec3:vec3(float()), vec@vec3:vec3(float()), integer()) -> tiramisu@scene:node_(AUSI).
line(Id, From, To, Color) ->
    {debug_line, Id, From, To, Color}.

-file("src/tiramisu/debug.gleam", 37).
-spec ray(
    AUSM,
    vec@vec3:vec3(float()),
    vec@vec3:vec3(float()),
    float(),
    integer()
) -> tiramisu@scene:node_(AUSM).
ray(Id, Origin, Direction, Length, Color) ->
    End = vec@vec3f:add(Origin, vec@vec3f:scale(Direction, Length)),
    {debug_line, Id, Origin, End, Color}.

-file("src/tiramisu/debug.gleam", 48).
-spec axes(AUSQ, vec@vec3:vec3(float()), float()) -> tiramisu@scene:node_(AUSQ).
axes(Id, Origin, Size) ->
    {debug_axes, Id, Origin, Size}.

-file("src/tiramisu/debug.gleam", 52).
-spec grid(AUST, float(), integer(), integer()) -> tiramisu@scene:node_(AUST).
grid(Id, Size, Divisions, Color) ->
    {debug_grid, Id, Size, Divisions, Color}.

-file("src/tiramisu/debug.gleam", 56).
-spec point(AUSV, vec@vec3:vec3(float()), float(), integer()) -> tiramisu@scene:node_(AUSV).
point(Id, Position, Size, Color) ->
    {debug_point, Id, Position, Size, Color}.

-file("src/tiramisu/debug.gleam", 65).
-spec box_from_transform(AUSY, tiramisu@transform:transform(), integer()) -> tiramisu@scene:node_(AUSY).
box_from_transform(Id, T, Color) ->
    Half_x = erlang:element(2, erlang:element(4, T)) / 2.0,
    Half_y = erlang:element(3, erlang:element(4, T)) / 2.0,
    Half_z = erlang:element(4, erlang:element(4, T)) / 2.0,
    Min = {vec3,
        erlang:element(2, erlang:element(2, T)) - Half_x,
        erlang:element(3, erlang:element(2, T)) - Half_y,
        erlang:element(4, erlang:element(2, T)) - Half_z},
    Max = {vec3,
        erlang:element(2, erlang:element(2, T)) + Half_x,
        erlang:element(3, erlang:element(2, T)) + Half_y,
        erlang:element(4, erlang:element(2, T)) + Half_z},
    bounding_box(Id, Min, Max, Color).

-file("src/tiramisu/debug.gleam", 100).
-spec create_path_lines(
    AUTF,
    list(vec@vec3:vec3(float())),
    integer(),
    integer(),
    list(tiramisu@scene:node_(AUTF))
) -> list(tiramisu@scene:node_(AUTF)).
create_path_lines(Id, Points, Color, Index, Acc) ->
    case Points of
        [] ->
            lists:reverse(Acc);

        [_] ->
            lists:reverse(Acc);

        [P1, P2 | Rest] ->
            Line_node = line(Id, P1, P2, Color),
            create_path_lines(
                Id,
                [P2 | Rest],
                Color,
                Index + 1,
                [Line_node | Acc]
            )
    end.

-file("src/tiramisu/debug.gleam", 92).
?DOC(" Create multiple lines forming a path through points\n").
-spec path(AUTA, list(vec@vec3:vec3(float())), integer()) -> list(tiramisu@scene:node_(AUTA)).
path(Id_prefix, Points, Color) ->
    create_path_lines(Id_prefix, Points, Color, 0, []).

-file("src/tiramisu/debug.gleam", 116).
-spec cross(AUTM, vec@vec3:vec3(float()), float(), integer()) -> list(tiramisu@scene:node_(AUTM)).
cross(Id, Position, Size, Color) ->
    Half_size = Size / 2.0,
    [line(
            Id,
            {vec3,
                erlang:element(2, Position) - Half_size,
                erlang:element(3, Position),
                erlang:element(4, Position)},
            {vec3,
                erlang:element(2, Position) + Half_size,
                erlang:element(3, Position),
                erlang:element(4, Position)},
            Color
        ),
        line(
            Id,
            {vec3,
                erlang:element(2, Position),
                erlang:element(3, Position) - Half_size,
                erlang:element(4, Position)},
            {vec3,
                erlang:element(2, Position),
                erlang:element(3, Position) + Half_size,
                erlang:element(4, Position)},
            Color
        ),
        line(
            Id,
            {vec3,
                erlang:element(2, Position),
                erlang:element(3, Position),
                erlang:element(4, Position) - Half_size},
            {vec3,
                erlang:element(2, Position),
                erlang:element(3, Position),
                erlang:element(4, Position) + Half_size},
            Color
        )].

-file("src/tiramisu/debug.gleam", 233).
?DOC(
    " @deprecated Use `show_collider_wireframes` with the physics world from context instead.\n"
    "\n"
    " This function is kept for backwards compatibility but will be removed in a future version.\n"
).
-spec with_collider_wireframes(list(tiramisu@scene:node_(AUTU)), integer()) -> list(tiramisu@scene:node_(AUTU)).
with_collider_wireframes(Nodes, _) ->
    Nodes.

-file("src/tiramisu/debug.gleam", 341).
?DOC(" Helper to get element at index from list (panics if out of bounds)\n").
-spec list_at(list(AUUD), integer()) -> AUUD.
list_at(List, Index) ->
    case gleam@list:drop(List, Index) of
        [First | _] ->
            First;

        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Index out of bounds"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"tiramisu/debug"/utf8>>,
                    function => <<"list_at"/utf8>>,
                    line => 344})
    end.

-file("src/tiramisu/debug.gleam", 349).
?DOC(" Visualize a sphere collider\n").
-spec collider_sphere(AUUF, float(), tiramisu@transform:transform(), integer()) -> tiramisu@scene:node_(AUUF).
collider_sphere(Id, Radius, Transform, Color) ->
    Center = erlang:element(2, Transform),
    sphere(Id, Center, Radius, Color).

-file("src/tiramisu/debug.gleam", 390).
?DOC(" Generate wireframe lines for a capsule in local space\n").
-spec generate_capsule_wireframe(float(), float(), integer()) -> list({vec@vec3:vec3(float()),
    vec@vec3:vec3(float())}).
generate_capsule_wireframe(Half_height, Radius, Segments) ->
    Pi = 3.14159265359,
    Neg_half_height = +0.0 - Half_height,
    Vertical_lines = begin
        _pipe = gleam@list:range(0, Segments - 1),
        gleam@list:map(
            _pipe,
            fun(I) ->
                Angle = case erlang:float(Segments) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> (2.0 * Pi) * erlang:float(I) / Gleam@denominator
                end,
                Cos = gleam_community@maths:cos(Angle),
                X = Radius * Cos,
                Sin = gleam_community@maths:sin(Angle),
                Z = Radius * Sin,
                Bottom = {vec3, X, Neg_half_height, Z},
                Top = {vec3, X, Half_height, Z},
                {Bottom, Top}
            end
        )
    end,
    Ring_lines = begin
        _pipe@1 = gleam@list:range(0, Segments - 1),
        gleam@list:flat_map(
            _pipe@1,
            fun(I@1) ->
                Angle1 = case erlang:float(Segments) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@1 -> (2.0 * Pi) * erlang:float(I@1) / Gleam@denominator@1
                end,
                Angle2 = case erlang:float(Segments) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@2 -> (2.0 * Pi) * erlang:float(I@1 + 1) / Gleam@denominator@2
                end,
                Cos1 = gleam_community@maths:cos(Angle1),
                X1 = Radius * Cos1,
                Sin1 = gleam_community@maths:sin(Angle1),
                Z1 = Radius * Sin1,
                Cos2 = gleam_community@maths:cos(Angle2),
                X2 = Radius * Cos2,
                Sin2 = gleam_community@maths:sin(Angle2),
                Z2 = Radius * Sin2,
                [{{vec3, X1, Neg_half_height, Z1},
                        {vec3, X2, Neg_half_height, Z2}},
                    {{vec3, X1, Half_height, Z1}, {vec3, X2, Half_height, Z2}}]
            end
        )
    end,
    Cap_arcs = begin
        _pipe@2 = gleam@list:range(0, 3),
        gleam@list:flat_map(
            _pipe@2,
            fun(I@2) ->
                Angle@1 = ((2.0 * Pi) * erlang:float(I@2)) / 4.0,
                Cos_a = gleam_community@maths:cos(Angle@1),
                X@1 = Radius * Cos_a,
                Sin_a = gleam_community@maths:sin(Angle@1),
                Z@1 = Radius * Sin_a,
                _pipe@3 = gleam@list:range(0, Segments div 4),
                gleam@list:map(
                    _pipe@3,
                    fun(J) ->
                        T1 = case erlang:float(Segments div 4) of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator@3 -> erlang:float(J) / Gleam@denominator@3
                        end,
                        T2 = case erlang:float(Segments div 4) of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator@4 -> erlang:float(J + 1) / Gleam@denominator@4
                        end,
                        Angle1@1 = Pi * ((+0.0 - 0.5) + (T1 / 2.0)),
                        Angle2@1 = Pi * ((+0.0 - 0.5) + (T2 / 2.0)),
                        Sin1@1 = gleam_community@maths:sin(Angle1@1),
                        Y1 = Neg_half_height + (Radius * Sin1@1),
                        Cos1@1 = gleam_community@maths:cos(Angle1@1),
                        R1 = Radius * Cos1@1,
                        Sin2@1 = gleam_community@maths:sin(Angle2@1),
                        Y2 = Neg_half_height + (Radius * Sin2@1),
                        Cos2@1 = gleam_community@maths:cos(Angle2@1),
                        R2 = Radius * Cos2@1,
                        Bottom_start = {vec3, case Radius of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator@5 -> X@1 * R1 / Gleam@denominator@5
                            end, Y1, case Radius of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator@6 -> Z@1 * R1 / Gleam@denominator@6
                            end},
                        Bottom_end = {vec3, case Radius of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator@7 -> X@1 * R2 / Gleam@denominator@7
                            end, Y2, case Radius of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator@8 -> Z@1 * R2 / Gleam@denominator@8
                            end},
                        {Bottom_start, Bottom_end}
                    end
                )
            end
        )
    end,
    lists:append([Vertical_lines, Ring_lines, Cap_arcs]).

-file("src/tiramisu/debug.gleam", 510).
?DOC(" Generate wireframe lines for a cylinder in local space\n").
-spec generate_cylinder_wireframe(float(), float(), integer()) -> list({vec@vec3:vec3(float()),
    vec@vec3:vec3(float())}).
generate_cylinder_wireframe(Half_height, Radius, Segments) ->
    Pi = 3.14159265359,
    Neg_half_height = +0.0 - Half_height,
    Vertical_lines = begin
        _pipe = gleam@list:range(0, Segments - 1),
        gleam@list:map(
            _pipe,
            fun(I) ->
                Angle = case erlang:float(Segments) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> (2.0 * Pi) * erlang:float(I) / Gleam@denominator
                end,
                Cos_val = gleam_community@maths:cos(Angle),
                X = Radius * Cos_val,
                Sin_val = gleam_community@maths:sin(Angle),
                Z = Radius * Sin_val,
                Bottom = {vec3, X, Neg_half_height, Z},
                Top = {vec3, X, Half_height, Z},
                {Bottom, Top}
            end
        )
    end,
    Ring_lines = begin
        _pipe@1 = gleam@list:range(0, Segments - 1),
        gleam@list:flat_map(
            _pipe@1,
            fun(I@1) ->
                Angle1 = case erlang:float(Segments) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@1 -> (2.0 * Pi) * erlang:float(I@1) / Gleam@denominator@1
                end,
                Angle2 = case erlang:float(Segments) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@2 -> (2.0 * Pi) * erlang:float(I@1 + 1) / Gleam@denominator@2
                end,
                Cos1 = gleam_community@maths:cos(Angle1),
                X1 = Radius * Cos1,
                Sin1 = gleam_community@maths:sin(Angle1),
                Z1 = Radius * Sin1,
                Cos2 = gleam_community@maths:cos(Angle2),
                X2 = Radius * Cos2,
                Sin2 = gleam_community@maths:sin(Angle2),
                Z2 = Radius * Sin2,
                [{{vec3, X1, Neg_half_height, Z1},
                        {vec3, X2, Neg_half_height, Z2}},
                    {{vec3, X1, Half_height, Z1}, {vec3, X2, Half_height, Z2}}]
            end
        )
    end,
    lists:append(Vertical_lines, Ring_lines).

-file("src/tiramisu/debug.gleam", 582).
?DOC(" Rotate a point around the Y axis\n").
-spec rotate_y(vec@vec3:vec3(float()), float()) -> vec@vec3:vec3(float()).
rotate_y(Point, Angle) ->
    Cos_a = gleam_community@maths:cos(Angle),
    Sin_a = gleam_community@maths:sin(Angle),
    {vec3,
        (erlang:element(2, Point) * Cos_a) + (erlang:element(4, Point) * Sin_a),
        erlang:element(3, Point),
        (+0.0 - (erlang:element(2, Point) * Sin_a)) + (erlang:element(4, Point)
        * Cos_a)}.

-file("src/tiramisu/debug.gleam", 564).
?DOC(" Transform a point by a transform (position + rotation + scale)\n").
-spec transform_point(vec@vec3:vec3(float()), tiramisu@transform:transform()) -> vec@vec3:vec3(float()).
transform_point(Point, Transform) ->
    Scaled = {vec3,
        erlang:element(2, Point) * erlang:element(
            2,
            erlang:element(4, Transform)
        ),
        erlang:element(3, Point) * erlang:element(
            3,
            erlang:element(4, Transform)
        ),
        erlang:element(4, Point) * erlang:element(
            4,
            erlang:element(4, Transform)
        )},
    Rotated = rotate_y(Scaled, erlang:element(3, erlang:element(3, Transform))),
    vec@vec3f:add(Rotated, erlang:element(2, Transform)).

-file("src/tiramisu/debug.gleam", 289).
?DOC(" Visualize a box collider\n").
-spec collider_box(
    AUUB,
    float(),
    float(),
    float(),
    tiramisu@transform:transform(),
    integer()
) -> tiramisu@scene:node_(AUUB).
collider_box(Id, Width, Height, Depth, Transform, Color) ->
    Half_w = Width / 2.0,
    Half_h = Height / 2.0,
    Half_d = Depth / 2.0,
    Neg_half_w = +0.0 - Half_w,
    Neg_half_h = +0.0 - Half_h,
    Neg_half_d = +0.0 - Half_d,
    Corners = [{vec3, Neg_half_w, Neg_half_h, Neg_half_d},
        {vec3, Half_w, Neg_half_h, Neg_half_d},
        {vec3, Half_w, Half_h, Neg_half_d},
        {vec3, Neg_half_w, Half_h, Neg_half_d},
        {vec3, Neg_half_w, Neg_half_h, Half_d},
        {vec3, Half_w, Neg_half_h, Half_d},
        {vec3, Half_w, Half_h, Half_d},
        {vec3, Neg_half_w, Half_h, Half_d}],
    World_corners = gleam@list:map(
        Corners,
        fun(_capture) -> transform_point(_capture, Transform) end
    ),
    {group,
        Id,
        {transform,
            {vec3, +0.0, +0.0, +0.0},
            {vec3, +0.0, +0.0, +0.0},
            {vec3, 1.0, 1.0, 1.0}},
        [line(Id, list_at(World_corners, 0), list_at(World_corners, 1), Color),
            line(
                Id,
                list_at(World_corners, 1),
                list_at(World_corners, 2),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 2),
                list_at(World_corners, 3),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 3),
                list_at(World_corners, 0),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 4),
                list_at(World_corners, 5),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 5),
                list_at(World_corners, 6),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 6),
                list_at(World_corners, 7),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 7),
                list_at(World_corners, 4),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 0),
                list_at(World_corners, 4),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 1),
                list_at(World_corners, 5),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 2),
                list_at(World_corners, 6),
                Color
            ),
            line(
                Id,
                list_at(World_corners, 3),
                list_at(World_corners, 7),
                Color
            )]}.

-file("src/tiramisu/debug.gleam", 360).
?DOC(" Visualize a capsule collider (cylinder with hemispherical caps)\n").
-spec collider_capsule(
    AUUH,
    float(),
    float(),
    tiramisu@transform:transform(),
    integer()
) -> tiramisu@scene:node_(AUUH).
collider_capsule(Id, Half_height, Radius, Transform, Color) ->
    Segments = 16,
    Lines = generate_capsule_wireframe(Half_height, Radius, Segments),
    World_lines = gleam@list:map(
        Lines,
        fun(Line_pair) ->
            {Start, End} = Line_pair,
            {transform_point(Start, Transform), transform_point(End, Transform)}
        end
    ),
    Children = gleam@list:index_map(
        World_lines,
        fun(Line_pair@1, _) ->
            {Start@1, End@1} = Line_pair@1,
            line(Id, Start@1, End@1, Color)
        end
    ),
    {group,
        Id,
        {transform,
            {vec3, +0.0, +0.0, +0.0},
            {vec3, +0.0, +0.0, +0.0},
            {vec3, 1.0, 1.0, 1.0}},
        Children}.

-file("src/tiramisu/debug.gleam", 480).
?DOC(" Visualize a cylinder collider\n").
-spec collider_cylinder(
    AUUM,
    float(),
    float(),
    tiramisu@transform:transform(),
    integer()
) -> tiramisu@scene:node_(AUUM).
collider_cylinder(Id, Half_height, Radius, Transform, Color) ->
    Segments = 16,
    Lines = generate_cylinder_wireframe(Half_height, Radius, Segments),
    World_lines = gleam@list:map(
        Lines,
        fun(Line_pair) ->
            {Start, End} = Line_pair,
            {transform_point(Start, Transform), transform_point(End, Transform)}
        end
    ),
    Children = gleam@list:index_map(
        World_lines,
        fun(Line_pair@1, _) ->
            {Start@1, End@1} = Line_pair@1,
            line(Id, Start@1, End@1, Color)
        end
    ),
    {group,
        Id,
        {transform,
            {vec3, +0.0, +0.0, +0.0},
            {vec3, +0.0, +0.0, +0.0},
            {vec3, 1.0, 1.0, 1.0}},
        Children}.

-file("src/tiramisu/debug.gleam", 271).
?DOC(
    " Visualize a physics collider shape at a given transform.\n"
    "\n"
    " This function converts a physics collider into debug visualization nodes\n"
    " that can be added to your scene for debugging physics shapes.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/debug\n"
    " import tiramisu/physics\n"
    " import tiramisu/transform\n"
    " import vec/vec3\n"
    "\n"
    " pub fn view(model: Model) {\n"
    "   let body_transform = transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))\n"
    "   let collider = physics.Box(width: 2.0, height: 2.0, depth: 2.0)\n"
    "\n"
    "   [\n"
    "     // Your normal scene nodes...\n"
    "     // Debug visualization for the collider\n"
    "     debug.collider(\n"
    "       id: \"player-collider-debug\",\n"
    "       shape: collider,\n"
    "       transform: body_transform,\n"
    "       color: debug.color_green,\n"
    "     ),\n"
    "   ]\n"
    " }\n"
    " ```\n"
).
-spec collider(
    AUTZ,
    tiramisu@physics:collider_shape(),
    tiramisu@transform:transform(),
    integer()
) -> tiramisu@scene:node_(AUTZ).
collider(Id, Shape, Transform, Color) ->
    case Shape of
        {box, Width, Height, Depth} ->
            collider_box(Id, Width, Height, Depth, Transform, Color);

        {sphere, Radius} ->
            collider_sphere(Id, Radius, Transform, Color);

        {capsule, Half_height, Radius@1} ->
            collider_capsule(Id, Half_height, Radius@1, Transform, Color);

        {cylinder, Half_height@1, Radius@2} ->
            collider_cylinder(Id, Half_height@1, Radius@2, Transform, Color)
    end.
