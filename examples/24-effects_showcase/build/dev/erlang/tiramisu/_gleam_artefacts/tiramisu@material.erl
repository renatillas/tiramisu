-module(tiramisu@material).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/material.gleam").
-export([basic/4, standard/10, line/2, sprite/4, lambert/4, phong/5, toon/4, new/0, with_color/2, with_metalness/2, with_roughness/2, with_color_map/2, with_normal_map/2, with_ambient_oclusion_map/2, with_roughness_map/2, with_metalness_map/2, with_transparent/2, with_opacity/2, build/1]).
-export_type([material/0, material_error/0, standard_material_builder/0, three_material/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque material() :: {basic_material,
        integer(),
        gleam@option:option(tiramisu@asset:texture()),
        boolean(),
        float()} |
    {standard_material,
        integer(),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        float(),
        float(),
        boolean(),
        float()} |
    {phong_material,
        integer(),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        float()} |
    {lambert_material,
        integer(),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture())} |
    {toon_material,
        integer(),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture())} |
    {line_material, integer(), float()} |
    {sprite_material,
        integer(),
        gleam@option:option(tiramisu@asset:texture()),
        boolean(),
        float()}.

-type material_error() :: {out_of_bounds_color, integer()} |
    {out_of_bounds_opacity, float()} |
    {out_of_bounds_roughness, float()} |
    {out_of_bounds_metalness, float()} |
    {non_positive_linewidth, float()} |
    {non_positive_shininess, float()}.

-opaque standard_material_builder() :: {standard_material_builder,
        integer(),
        float(),
        float(),
        boolean(),
        float(),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture()),
        gleam@option:option(tiramisu@asset:texture())}.

-type three_material() :: any().

-file("src/tiramisu/material.gleam", 90).
?DOC(
    " Create a validated basic (unlit) material.\n"
    "\n"
    " Basic materials don't react to lights, making them very fast to render.\n"
    " Opacity must be between 0.0 (fully transparent) and 1.0 (fully opaque).\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(red) = material.basic(color: 0xff0000, transparent: False, opacity: 1.0, map: option.None)\n"
    " let assert Ok(glass) = material.basic(color: 0x88ccff, transparent: True, opacity: 0.5, map: option.None)\n"
    " ```\n"
).
-spec basic(
    integer(),
    boolean(),
    float(),
    gleam@option:option(tiramisu@asset:texture())
) -> {ok, material()} | {error, material_error()}.
basic(Color, Transparent, Opacity, Map) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            gleam@bool:guard(
                (Opacity < +0.0) orelse (Opacity > 1.0),
                {error, {out_of_bounds_opacity, Opacity}},
                fun() ->
                    {ok, {basic_material, Color, Map, Transparent, Opacity}}
                end
            )
        end
    ).

-file("src/tiramisu/material.gleam", 120).
?DOC(
    " Create a validated physically-based (PBR) standard material.\n"
    "\n"
    " Standard materials use metalness/roughness workflow for realistic rendering.\n"
    " - Metalness: 0.0 = dielectric (plastic, wood), 1.0 = metal\n"
    " - Roughness: 0.0 = mirror-smooth, 1.0 = completely rough\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(gold) = scene.standard_material(color: 0xffd700, metalness: 1.0, roughness: 0.3, transparent: False, opacity: 1.0, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)\n"
    " let assert Ok(plastic) = scene.standard_material(color: 0xff0000, metalness: 0.0, roughness: 0.5, transparent: False, opacity: 1.0, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)\n"
    " ```\n"
).
-spec standard(
    integer(),
    float(),
    float(),
    boolean(),
    float(),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture())
) -> {ok, material()} | {error, material_error()}.
standard(
    Color,
    Metalness,
    Roughness,
    Transparent,
    Opacity,
    Map,
    Normal_map,
    Ambient_oclusion_map,
    Roughness_map,
    Metalness_map
) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            gleam@bool:guard(
                (Metalness < +0.0) orelse (Metalness > 1.0),
                {error, {out_of_bounds_metalness, Metalness}},
                fun() ->
                    gleam@bool:guard(
                        (Roughness < +0.0) orelse (Roughness > 1.0),
                        {error, {out_of_bounds_roughness, Roughness}},
                        fun() ->
                            gleam@bool:guard(
                                (Opacity < +0.0) orelse (Opacity > 1.0),
                                {error, {out_of_bounds_opacity, Opacity}},
                                fun() ->
                                    {ok,
                                        {standard_material,
                                            Color,
                                            Map,
                                            Normal_map,
                                            Ambient_oclusion_map,
                                            Roughness_map,
                                            Metalness_map,
                                            Metalness,
                                            Roughness,
                                            Transparent,
                                            Opacity}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/material.gleam", 170).
?DOC(
    " Create a validated line material for rendering lines.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(line_mat) = scene.line_material(color: 0xff0000, linewidth: 2.0)\n"
    " ```\n"
).
-spec line(integer(), float()) -> {ok, material()} | {error, material_error()}.
line(Color, Linewidth) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            gleam@bool:guard(
                Linewidth =< +0.0,
                {error, {non_positive_linewidth, Linewidth}},
                fun() -> {ok, {line_material, Color, Linewidth}} end
            )
        end
    ).

-file("src/tiramisu/material.gleam", 192).
?DOC(
    " Create a validated sprite material for 2D billboards.\n"
    "\n"
    " Sprites always face the camera and are useful for particles, UI elements, etc.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(sprite_mat) = material.sprite(color: 0xffffff, transparent: True, opacity: 0.8, map: option.None)\n"
    " ```\n"
).
-spec sprite(
    integer(),
    boolean(),
    float(),
    gleam@option:option(tiramisu@asset:texture())
) -> {ok, material()} | {error, material_error()}.
sprite(Color, Transparent, Opacity, Map) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            gleam@bool:guard(
                (Opacity < +0.0) orelse (Opacity > 1.0),
                {error, {out_of_bounds_opacity, Opacity}},
                fun() ->
                    {ok, {sprite_material, Color, Map, Transparent, Opacity}}
                end
            )
        end
    ).

-file("src/tiramisu/material.gleam", 210).
-spec lambert(
    integer(),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture())
) -> {ok, material()} | {error, material_error()}.
lambert(Color, Map, Normal_map, Ambient_oclusion_map) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            {ok,
                {lambert_material, Color, Map, Normal_map, Ambient_oclusion_map}}
        end
    ).

-file("src/tiramisu/material.gleam", 223).
-spec phong(
    integer(),
    float(),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture())
) -> {ok, material()} | {error, material_error()}.
phong(Color, Shininess, Map, Normal_map, Ambient_oclusion_map) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            gleam@bool:guard(
                Shininess < +0.0,
                {error, {non_positive_shininess, Shininess}},
                fun() ->
                    {ok,
                        {phong_material,
                            Color,
                            Map,
                            Normal_map,
                            Ambient_oclusion_map,
                            Shininess}}
                end
            )
        end
    ).

-file("src/tiramisu/material.gleam", 238).
-spec toon(
    integer(),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture()),
    gleam@option:option(tiramisu@asset:texture())
) -> {ok, material()} | {error, material_error()}.
toon(Color, Map, Normal_map, Ambient_oclusion_map) ->
    gleam@bool:guard(
        (Color < 16#000000) orelse (Color > 16#ffffff),
        {error, {out_of_bounds_color, Color}},
        fun() ->
            {ok, {toon_material, Color, Map, Normal_map, Ambient_oclusion_map}}
        end
    ).

-file("src/tiramisu/material.gleam", 281).
-spec new() -> standard_material_builder().
new() ->
    {standard_material_builder,
        16#808080,
        0.5,
        0.5,
        false,
        1.0,
        none,
        none,
        none,
        none,
        none}.

-file("src/tiramisu/material.gleam", 296).
-spec with_color(standard_material_builder(), integer()) -> standard_material_builder().
with_color(Builder, Color) ->
    {standard_material_builder,
        Color,
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 303).
-spec with_metalness(standard_material_builder(), float()) -> standard_material_builder().
with_metalness(Builder, Metalness) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        Metalness,
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 310).
-spec with_roughness(standard_material_builder(), float()) -> standard_material_builder().
with_roughness(Builder, Roughness) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        Roughness,
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 317).
-spec with_color_map(standard_material_builder(), tiramisu@asset:texture()) -> standard_material_builder().
with_color_map(Builder, Map) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        {some, Map},
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 324).
-spec with_normal_map(standard_material_builder(), tiramisu@asset:texture()) -> standard_material_builder().
with_normal_map(Builder, Normal_map) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        {some, Normal_map},
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 331).
-spec with_ambient_oclusion_map(
    standard_material_builder(),
    tiramisu@asset:texture()
) -> standard_material_builder().
with_ambient_oclusion_map(Builder, Ambient_oclusion_map) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        {some, Ambient_oclusion_map},
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 341).
-spec with_roughness_map(standard_material_builder(), tiramisu@asset:texture()) -> standard_material_builder().
with_roughness_map(Builder, Roughness_map) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        {some, Roughness_map},
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 348).
-spec with_metalness_map(standard_material_builder(), tiramisu@asset:texture()) -> standard_material_builder().
with_metalness_map(Builder, Metalness_map) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        {some, Metalness_map}}.

-file("src/tiramisu/material.gleam", 355).
-spec with_transparent(standard_material_builder(), boolean()) -> standard_material_builder().
with_transparent(Builder, Transparent) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        Transparent,
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 362).
-spec with_opacity(standard_material_builder(), float()) -> standard_material_builder().
with_opacity(Builder, Opacity) ->
    {standard_material_builder,
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        Opacity,
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)}.

-file("src/tiramisu/material.gleam", 369).
-spec build(standard_material_builder()) -> {ok, material()} |
    {error, material_error()}.
build(Builder) ->
    standard(
        erlang:element(2, Builder),
        erlang:element(3, Builder),
        erlang:element(4, Builder),
        erlang:element(5, Builder),
        erlang:element(6, Builder),
        erlang:element(7, Builder),
        erlang:element(8, Builder),
        erlang:element(9, Builder),
        erlang:element(10, Builder),
        erlang:element(11, Builder)
    ).
