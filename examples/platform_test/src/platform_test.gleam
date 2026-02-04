/// Simple test of the new lustre_platform-based Tiramisu API.
///
/// This example demonstrates:
/// - Creating a platform with platform.platform()
/// - Using element constructors (mesh, camera, light)
/// - Using attribute constructors (position, color, etc.)
/// - Subscription-based game loop with Context
/// - Standard Lustre MVU pattern
import gleam/time/duration
import lustre
import lustre/effect
import tiramisu
import tiramisu/attribute
import tiramisu/context.{type Context}
import tiramisu/element
import tiramisu/platform
import vec/vec3

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(rotation: Float)
}

pub type Msg {
  Tick(Context)
}

// INIT ------------------------------------------------------------------------

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let model = Model(rotation: 0.0)
  // Subscribe to game ticks - no manual scheduling needed!
  #(model, tiramisu.subscribe_to_ticks(Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      // Use delta time from context for frame-rate independent animation
      let delta_seconds = duration.to_seconds(ctx.delta_time)
      let new_rotation = model.rotation +. delta_seconds
      #(Model(rotation: new_rotation), effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) {
  // Test with Three.js elements
  element.empty("root", [], [
    element.camera(
      "camera",
      [
        attribute.fov(75.0),
        attribute.position_xyz(0.0, 0.0, 10.0),
        attribute.look_at(vec3.Vec3(0.0, 0.0, 0.0)),
        attribute.active(True),
      ],
      [],
    ),
    element.light(
      "light",
      [
        attribute.light_type("directional"),
        attribute.intensity(1.0),
        attribute.color(0xffffff),
        attribute.position_xyz(10.0, 10.0, 10.0),
      ],
      [],
    ),
    element.mesh(
      "cube",
      [
        attribute.geometry_box(2.0, 2.0, 2.0),
        attribute.color(0xff6b6b),
        attribute.position_xyz(0.0, 0.0, 0.0),
        attribute.rotation_xyz(model.rotation, model.rotation *. 0.5, 0.0),
      ],
      [],
    ),
  ])
}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Create platform
  let assert Ok(my_platform) = platform.platform("#app")

  // Create Lustre application
  let app = lustre.application(init, update, view)

  // Start on platform
  let assert Ok(_) = lustre.start(app, my_platform, Nil)

  Nil
}
