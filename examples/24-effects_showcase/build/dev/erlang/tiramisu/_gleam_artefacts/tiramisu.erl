-module(tiramisu).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu.gleam").
-export_type([scene/0, dimensions/0, context/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Tiramisu game engine main module - immutable game loop with effect system.\n"
    "\n"
    " This module provides the core game loop following the Model-View-Update (MVU) architecture,\n"
    " inspired by Lustre. Your game state is immutable, and updates return new state along with effects.\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu\n"
    " import tiramisu/effect\n"
    " import tiramisu/scene\n"
    " import tiramisu/transform\n"
    "\n"
    " type Model {\n"
    "   Model(rotation: Float)\n"
    " }\n"
    "\n"
    " type Msg {\n"
    "   Frame\n"
    " }\n"
    "\n"
    " pub fn main() {\n"
    "   tiramisu.run(\n"
    "     dimensions: None,\n"
    "     background: background.Color(0x111111),\n"
    "     init: init,\n"
    "     update: update,\n"
    "     view: view,\n"
    "   )\n"
    " }\n"
    "\n"
    " fn init(_ctx: tiramisu.Context) {\n"
    "   #(Model(rotation: 0.0), effect.none())\n"
    " }\n"
    "\n"
    " fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {\n"
    "   case msg {\n"
    "     Frame -> {\n"
    "       let new_rotation = model.rotation +. ctx.delta_time\n"
    "       #(Model(rotation: new_rotation), effect.none())\n"
    "     }\n"
    "   }\n"
    " }\n"
    "\n"
    " fn view(model: Model) {\n"
    "   [\n"
    "     scene.Mesh(\n"
    "       id: \"cube\",\n"
    "       geometry: scene.BoxGeometry(1.0, 1.0, 1.0),\n"
    "       material: scene.BasicMaterial(0xff0000, False, 1.0, option.None),\n"
    "       transform: transform.identity\n"
    "         |> transform.set_rotation(vec3.Vec3(model.rotation, model.rotation, 0.0)),\n"
    "       physics: option.None,\n"
    "     ),\n"
    "   ]\n"
    " }\n"
    " ```\n"
).

-type scene() :: any().

-type dimensions() :: {dimensions, float(), float()}.

-type context(ENC) :: {context,
        float(),
        tiramisu@input:input_state(),
        float(),
        float(),
        gleam@option:option(tiramisu@physics:physics_world(ENC)),
        tiramisu@internal@managers:input_manager(),
        tiramisu@internal@managers:audio_manager()}.


