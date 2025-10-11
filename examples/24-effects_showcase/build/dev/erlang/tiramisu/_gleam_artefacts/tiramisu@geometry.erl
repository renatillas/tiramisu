-module(tiramisu@geometry).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/geometry.gleam").
-export([box/3, sphere/3, cone/3, plane/2, circle/2, custom_geometry/1, cylinder/4, torus/4, tetrahedron/2, icosahedron/2]).
-export_type([geometry/0, geometry_error/0, three_geometry/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque geometry() :: {box_geometry, float(), float(), float()} |
    {sphere_geometry, float(), integer(), integer()} |
    {cone_geometry, float(), float(), integer()} |
    {plane_geometry, float(), float()} |
    {circle_geometry, float(), integer()} |
    {cylinder_geometry, float(), float(), float(), integer()} |
    {torus_geometry, float(), float(), integer(), integer()} |
    {tetrahedron_geometry, float(), integer()} |
    {icosahedron_geometry, float(), integer()} |
    {custom_geometry, tiramisu@asset:buffer_geometry()}.

-type geometry_error() :: {non_positive_width, float()} |
    {non_positive_height, float()} |
    {non_positive_depth, float()} |
    {non_positive_radius, float()} |
    {invalid_geometry_tube, float()} |
    {less_than_three_segment_count_width, integer()} |
    {less_than_two_segment_count_height, integer()} |
    {negative_segment_count, integer()}.

-type three_geometry() :: any().

-file("src/tiramisu/geometry.gleam", 57).
?DOC(
    " Create a validated box geometry.\n"
    "\n"
    " All dimensions must be positive (> 0).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(cube) = scene.box(width: 1.0, height: 1.0, depth: 1.0)\n"
    " let assert Ok(wall) = scene.box(width: 10.0, height: 3.0, depth: 0.1)\n"
    " ```\n"
).
-spec box(float(), float(), float()) -> {ok, geometry()} |
    {error, geometry_error()}.
box(Width, Height, Depth) ->
    gleam@bool:guard(
        Width =< +0.0,
        {error, {non_positive_width, Width}},
        fun() ->
            gleam@bool:guard(
                Height =< +0.0,
                {error, {non_positive_height, Height}},
                fun() ->
                    gleam@bool:guard(
                        Depth =< +0.0,
                        {error, {non_positive_depth, Depth}},
                        fun() -> {ok, {box_geometry, Width, Height, Depth}} end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 80).
?DOC(
    " Create a validated sphere geometry.\n"
    "\n"
    " Radius must be positive. Width segments >= 3, height segments >= 2.\n"
    " More segments = smoother sphere but more triangles.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(ball) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 16)\n"
    " let assert Ok(low_poly) = scene.sphere(radius: 1.0, width_segments: 8, height_segments: 6)\n"
    " ```\n"
).
-spec sphere(float(), integer(), integer()) -> {ok, geometry()} |
    {error, geometry_error()}.
sphere(Radius, Width_segments, Height_segments) ->
    gleam@bool:guard(
        Radius =< +0.0,
        {error, {non_positive_radius, Radius}},
        fun() ->
            gleam@bool:guard(
                Width_segments < 3,
                {error, {less_than_three_segment_count_width, Width_segments}},
                fun() ->
                    gleam@bool:guard(
                        Height_segments < 2,
                        {error,
                            {less_than_two_segment_count_height,
                                Height_segments}},
                        fun() ->
                            {ok,
                                {sphere_geometry,
                                    Radius,
                                    Width_segments,
                                    Height_segments}}
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 98).
-spec cone(float(), float(), integer()) -> {ok, geometry()} |
    {error, geometry_error()}.
cone(Radius, Height, Segments) ->
    gleam@bool:guard(
        Radius =< +0.0,
        {error, {non_positive_radius, Radius}},
        fun() ->
            gleam@bool:guard(
                Height =< +0.0,
                {error, {non_positive_height, Height}},
                fun() ->
                    gleam@bool:guard(
                        Segments < 3,
                        {error, {negative_segment_count, Segments}},
                        fun() ->
                            {ok, {cone_geometry, Radius, Height, Segments}}
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 110).
-spec plane(float(), float()) -> {ok, geometry()} | {error, geometry_error()}.
plane(Width, Height) ->
    gleam@bool:guard(
        Width =< +0.0,
        {error, {non_positive_width, Width}},
        fun() ->
            gleam@bool:guard(
                Height =< +0.0,
                {error, {non_positive_height, Height}},
                fun() -> {ok, {plane_geometry, Width, Height}} end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 120).
-spec circle(float(), integer()) -> {ok, geometry()} | {error, geometry_error()}.
circle(Radius, Segments) ->
    gleam@bool:guard(
        Radius =< +0.0,
        {error, {non_positive_radius, Radius}},
        fun() ->
            gleam@bool:guard(
                Segments < 3,
                {error, {negative_segment_count, Segments}},
                fun() -> {ok, {circle_geometry, Radius, Segments}} end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 130).
-spec custom_geometry(tiramisu@asset:buffer_geometry()) -> geometry().
custom_geometry(Geometry) ->
    {custom_geometry, Geometry}.

-file("src/tiramisu/geometry.gleam", 145).
?DOC(
    " Create a validated cylinder geometry.\n"
    "\n"
    " Both radii must be non-negative, height positive, radial segments >= 3.\n"
    " Set one radius to 0 to create a cone shape.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(cylinder) = scene.cylinder(radius_top: 1.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)\n"
    " let assert Ok(cone) = scene.cylinder(radius_top: 0.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)\n"
    " ```\n"
).
-spec cylinder(float(), float(), float(), integer()) -> {ok, geometry()} |
    {error, geometry_error()}.
cylinder(Radius_top, Radius_bottom, Height, Radial_segments) ->
    gleam@bool:guard(
        Radius_top < +0.0,
        {error, {non_positive_radius, Radius_top}},
        fun() ->
            gleam@bool:guard(
                Radius_bottom < +0.0,
                {error, {non_positive_radius, Radius_bottom}},
                fun() ->
                    gleam@bool:guard(
                        Height =< +0.0,
                        {error, {non_positive_height, Height}},
                        fun() ->
                            gleam@bool:guard(
                                Radial_segments < 3,
                                {error,
                                    {negative_segment_count, Radial_segments}},
                                fun() ->
                                    {ok,
                                        {cylinder_geometry,
                                            Radius_top,
                                            Radius_bottom,
                                            Height,
                                            Radial_segments}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 172).
?DOC(
    " Create a validated torus (donut) geometry.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(donut) = scene.torus(radius: 2.0, tube: 0.5, radial_segments: 16, tubular_segments: 100)\n"
    " ```\n"
).
-spec torus(float(), float(), integer(), integer()) -> {ok, geometry()} |
    {error, geometry_error()}.
torus(Radius, Tube, Radial_segments, Tubular_segments) ->
    gleam@bool:guard(
        Radius =< +0.0,
        {error, {non_positive_radius, Radius}},
        fun() ->
            gleam@bool:guard(
                Tube =< +0.0,
                {error, {invalid_geometry_tube, Tube}},
                fun() ->
                    gleam@bool:guard(
                        Radial_segments < 3,
                        {error, {negative_segment_count, Radial_segments}},
                        fun() ->
                            gleam@bool:guard(
                                Tubular_segments < 3,
                                {error,
                                    {negative_segment_count, Tubular_segments}},
                                fun() ->
                                    {ok,
                                        {torus_geometry,
                                            Radius,
                                            Tube,
                                            Radial_segments,
                                            Tubular_segments}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 201).
?DOC(
    " Create a validated tetrahedron (4-sided polyhedron) geometry.\n"
    "\n"
    " Detail level controls subdivision (0 = no subdivision, higher = more triangles).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(shape) = scene.tetrahedron(radius: 1.0, detail: 0)\n"
    " ```\n"
).
-spec tetrahedron(float(), integer()) -> {ok, geometry()} |
    {error, geometry_error()}.
tetrahedron(Radius, Detail) ->
    gleam@bool:guard(
        Radius =< +0.0,
        {error, {non_positive_radius, Radius}},
        fun() ->
            gleam@bool:guard(
                Detail < 0,
                {error, {negative_segment_count, Detail}},
                fun() -> {ok, {tetrahedron_geometry, Radius, Detail}} end
            )
        end
    ).

-file("src/tiramisu/geometry.gleam", 220).
?DOC(
    " Create a validated icosahedron (20-sided polyhedron) geometry.\n"
    "\n"
    " Detail level controls subdivision. Good for creating spheres with flat faces.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(shape) = scene.icosahedron(radius: 1.0, detail: 2)\n"
    " ```\n"
).
-spec icosahedron(float(), integer()) -> {ok, geometry()} |
    {error, geometry_error()}.
icosahedron(Radius, Detail) ->
    gleam@bool:guard(
        Radius =< +0.0,
        {error, {non_positive_radius, Radius}},
        fun() ->
            gleam@bool:guard(
                Detail < 0,
                {error, {negative_segment_count, Detail}},
                fun() -> {ok, {icosahedron_geometry, Radius, Detail}} end
            )
        end
    ).
