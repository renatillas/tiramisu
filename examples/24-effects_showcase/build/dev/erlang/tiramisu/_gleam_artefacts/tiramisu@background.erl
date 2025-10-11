-module(tiramisu@background).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/background.gleam").
-export_type([background/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Background type for scene rendering.\n"
    "\n"
    " Defines what is rendered behind all scene objects. Can be either a solid color,\n"
    " a texture image, or a cube texture (skybox).\n"
).

-type background() :: {color, integer()} |
    {texture, binary()} |
    {cube_texture, list(binary())}.


