-module(tiramisu@light).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/light.gleam").
-export([ambient/2, directional/2, point/3, spot/5, hemisphere/3, with_shadows/2, with_shadow_resolution/2, with_shadow_bias/2]).
-export_type([light/0, light_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque light() :: {ambient, float(), integer()} |
    {directional, float(), integer(), boolean(), integer(), float()} |
    {point, float(), integer(), float(), boolean(), integer(), float()} |
    {spot,
        float(),
        integer(),
        float(),
        float(),
        float(),
        boolean(),
        integer(),
        float()} |
    {hemisphere, float(), integer(), integer()}.

-type light_error() :: {negative_intensity, float()} |
    {out_of_bounds_color, integer()} |
    {negative_distance, float()} |
    {invalid_shadow_resolution, integer()} |
    {invalid_shadow_bias, float()}.

-file("src/tiramisu/light.gleam", 52).
-spec ambient(float(), integer()) -> {ok, light()} | {error, light_error()}.
ambient(Intensity, Color) ->
    gleam@bool:guard(
        Intensity < +0.0,
        {error, {negative_intensity, Intensity}},
        fun() ->
            gleam@bool:guard(
                (Color < 0) andalso (Color > 16#ffffff),
                {error, {out_of_bounds_color, Color}},
                fun() -> {ok, {ambient, Intensity, Color}} end
            )
        end
    ).

-file("src/tiramisu/light.gleam", 64).
-spec directional(float(), integer()) -> {ok, light()} | {error, light_error()}.
directional(Intensity, Color) ->
    gleam@bool:guard(
        Intensity < +0.0,
        {error, {negative_intensity, Intensity}},
        fun() ->
            gleam@bool:guard(
                (Color < 0) andalso (Color > 16#ffffff),
                {error, {out_of_bounds_color, Color}},
                fun() ->
                    {ok, {directional, Intensity, Color, false, 1024, 0.0001}}
                end
            )
        end
    ).

-file("src/tiramisu/light.gleam", 82).
-spec point(float(), integer(), float()) -> {ok, light()} |
    {error, light_error()}.
point(Intensity, Color, Distance) ->
    gleam@bool:guard(
        Intensity < +0.0,
        {error, {negative_intensity, Intensity}},
        fun() ->
            gleam@bool:guard(
                (Color < 0) andalso (Color > 16#ffffff),
                {error, {out_of_bounds_color, Color}},
                fun() ->
                    gleam@bool:guard(
                        Distance < +0.0,
                        {error, {negative_distance, Distance}},
                        fun() ->
                            {ok,
                                {point,
                                    Intensity,
                                    Color,
                                    Distance,
                                    false,
                                    1024,
                                    0.0001}}
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/light.gleam", 103).
-spec spot(float(), integer(), float(), float(), float()) -> {ok, light()} |
    {error, light_error()}.
spot(Intensity, Color, Distance, Angle, Penumbra) ->
    gleam@bool:guard(
        Intensity < +0.0,
        {error, {negative_intensity, Intensity}},
        fun() ->
            gleam@bool:guard(
                (Color < 0) andalso (Color > 16#ffffff),
                {error, {out_of_bounds_color, Color}},
                fun() ->
                    gleam@bool:guard(
                        Distance < +0.0,
                        {error, {negative_distance, Distance}},
                        fun() ->
                            {ok,
                                {spot,
                                    Intensity,
                                    Color,
                                    Distance,
                                    Angle,
                                    Penumbra,
                                    false,
                                    1024,
                                    0.0001}}
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/light.gleam", 128).
-spec hemisphere(float(), integer(), integer()) -> {ok, light()} |
    {error, light_error()}.
hemisphere(Intensity, Sky_color, Ground_color) ->
    gleam@bool:guard(
        Intensity < +0.0,
        {error, {negative_intensity, Intensity}},
        fun() ->
            gleam@bool:guard(
                (Sky_color < 0) andalso (Sky_color > 16#ffffff),
                {error, {out_of_bounds_color, Sky_color}},
                fun() ->
                    gleam@bool:guard(
                        (Ground_color < 0) andalso (Ground_color > 16#ffffff),
                        {error, {out_of_bounds_color, Sky_color}},
                        fun() ->
                            {ok,
                                {hemisphere, Intensity, Sky_color, Ground_color}}
                        end
                    )
                end
            )
        end
    ).

-file("src/tiramisu/light.gleam", 158).
?DOC(
    " Enable shadow casting for a light.\n"
    "\n"
    " Only directional, point, and spot lights can cast shadows.\n"
    " Ambient and hemisphere lights are ignored.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)\n"
    "   |> light.with_shadows(True)\n"
    " ```\n"
).
-spec with_shadows(light(), boolean()) -> light().
with_shadows(Light, Cast_shadow) ->
    case Light of
        {directional, Intensity, Color, _, Shadow_resolution, Shadow_bias} ->
            {directional,
                Intensity,
                Color,
                Cast_shadow,
                Shadow_resolution,
                Shadow_bias};

        {point,
            Intensity@1,
            Color@1,
            Distance,
            _,
            Shadow_resolution@1,
            Shadow_bias@1} ->
            {point,
                Intensity@1,
                Color@1,
                Distance,
                Cast_shadow,
                Shadow_resolution@1,
                Shadow_bias@1};

        {spot,
            Intensity@2,
            Color@2,
            Distance@1,
            Angle,
            Penumbra,
            _,
            Shadow_resolution@2,
            Shadow_bias@2} ->
            {spot,
                Intensity@2,
                Color@2,
                Distance@1,
                Angle,
                Penumbra,
                Cast_shadow,
                Shadow_resolution@2,
                Shadow_bias@2};

        _ ->
            Light
    end.

-file("src/tiramisu/light.gleam", 214).
?DOC(
    " Set shadow map resolution (in pixels).\n"
    "\n"
    " Higher values produce sharper shadows but use more memory.\n"
    " Common values: 512, 1024 (default), 2048, 4096.\n"
    " Must be a power of 2.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)\n"
    "   |> light.with_shadows(True)\n"
    "   |> light.with_shadow_resolution(2048)\n"
    " ```\n"
).
-spec with_shadow_resolution(light(), integer()) -> {ok, light()} |
    {error, light_error()}.
with_shadow_resolution(Light, Resolution) ->
    gleam@bool:guard(
        (Resolution =< 0) orelse ((Resolution rem 2) /= 0),
        {error, {invalid_shadow_resolution, Resolution}},
        fun() -> case Light of
                {directional, Intensity, Color, Cast_shadow, _, Shadow_bias} ->
                    {ok,
                        {directional,
                            Intensity,
                            Color,
                            Cast_shadow,
                            Resolution,
                            Shadow_bias}};

                {point,
                    Intensity@1,
                    Color@1,
                    Distance,
                    Cast_shadow@1,
                    _,
                    Shadow_bias@1} ->
                    {ok,
                        {point,
                            Intensity@1,
                            Color@1,
                            Distance,
                            Cast_shadow@1,
                            Resolution,
                            Shadow_bias@1}};

                {spot,
                    Intensity@2,
                    Color@2,
                    Distance@1,
                    Angle,
                    Penumbra,
                    Cast_shadow@2,
                    _,
                    Shadow_bias@2} ->
                    {ok,
                        {spot,
                            Intensity@2,
                            Color@2,
                            Distance@1,
                            Angle,
                            Penumbra,
                            Cast_shadow@2,
                            Resolution,
                            Shadow_bias@2}};

                _ ->
                    {ok, Light}
            end end
    ).

-file("src/tiramisu/light.gleam", 278).
?DOC(
    " Set shadow bias to reduce shadow acne artifacts.\n"
    "\n"
    " Typical values: 0.00001 to 0.001 (default: 0.0001).\n"
    " Increase if you see shadow artifacts (shadow acne).\n"
    " Decrease if shadows appear detached from objects.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)\n"
    "   |> light.with_shadows(True)\n"
    "   |> light.with_shadow_bias(0.0005)\n"
    " ```\n"
).
-spec with_shadow_bias(light(), float()) -> {ok, light()} |
    {error, light_error()}.
with_shadow_bias(Light, Bias) ->
    gleam@bool:guard(
        Bias < +0.0,
        {error, {invalid_shadow_bias, Bias}},
        fun() -> case Light of
                {directional,
                    Intensity,
                    Color,
                    Cast_shadow,
                    Shadow_resolution,
                    _} ->
                    {ok,
                        {directional,
                            Intensity,
                            Color,
                            Cast_shadow,
                            Shadow_resolution,
                            Bias}};

                {point,
                    Intensity@1,
                    Color@1,
                    Distance,
                    Cast_shadow@1,
                    Shadow_resolution@1,
                    _} ->
                    {ok,
                        {point,
                            Intensity@1,
                            Color@1,
                            Distance,
                            Cast_shadow@1,
                            Shadow_resolution@1,
                            Bias}};

                {spot,
                    Intensity@2,
                    Color@2,
                    Distance@1,
                    Angle,
                    Penumbra,
                    Cast_shadow@2,
                    Shadow_resolution@2,
                    _} ->
                    {ok,
                        {spot,
                            Intensity@2,
                            Color@2,
                            Distance@1,
                            Angle,
                            Penumbra,
                            Cast_shadow@2,
                            Shadow_resolution@2,
                            Bias}};

                _ ->
                    {ok, Light}
            end end
    ).
