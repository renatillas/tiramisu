//// Model Loading Example
////
//// Demonstrates loading and displaying GLTF models:
//// - Using attribute.src() to load external 3D models
//// - The mesh component handles loading automatically
//// - Rotation animation via tick updates

import gleam/float
import gleam/int
import gleam/time/duration
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element/html
import quaternion
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/scene
import tiramisu/tick.{type TickContext}
import tiramisu/transform
import vec/vec2
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Application state
pub type Model {
  Model(
    /// Rotation angle for the model
    rotation: Float,
    /// Smoothed frames per second (exponential moving average)
    fps: Float,
  )
}

/// Messages for state updates
pub type Msg {
  /// Animation tick
  Tick(TickContext)
  ModelLoaded(String)
}

// MODEL URL -------------------------------------------------------------------

// Using a public GLTF model from the Three.js examples
const model_url = "https://threejs.org/examples/models/gltf/Soldier.glb"

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  // Register all Tiramisu web components
  let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())

  // Start a Lustre app with effects support
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let initial_model = Model(rotation: 0.0, fps: 0.0)

  // Subscribe to tick updates for animation
  #(initial_model, tick.subscribe("main", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(m: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      let new_rotation = m.rotation +. dt *. 0.5
      let current_fps = case dt >. 0.0 {
        True -> 1.0 /. dt
        False -> m.fps
      }
      let smooth_fps = case m.fps == 0.0 {
        True -> current_fps
        False -> m.fps *. 0.9 +. current_fps *. 0.1
      }
      #(Model(rotation: new_rotation, fps: smooth_fps), effect.none())
    }
    ModelLoaded(mesh_id) -> {
      echo "Model loaded with id: " <> mesh_id
      #(m, effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(m: Model) {
  html.div([class("container")], [
    // 3D Scene
    tiramisu.scene(
      "main",
      [
        attribute.width(600),
        attribute.height(500),
        scene.background_color(0x1a1a2e),
      ],
      [
        // Camera
        tiramisu.camera(
          "main",
          [
            camera.fov(45.0),
            transform.transform(transform.at(vec3.Vec3(0.0, 1.5, 5.0))),
            camera.active(True),
          ],
          [],
        ),
        // Ground plane
        tiramisu.mesh(
          "ground",
          [
            material.metalness(0.1),
            material.roughness(0.9),
            mesh.plane(vec2.Vec2(10.0, 10.0)),
            mesh.color(0x2d3436),
            material.receive_shadow(True),
            transform.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Loaded GLTF model using attribute.src()
        tiramisu.mesh(
          "soldier",
          [
            attribute.src(model_url),
            transform.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(0.0, 0.0, 0.0)),
              ),
            ),
            material.cast_shadow(True),
            mesh.on_model_loaded(ModelLoaded),
          ],
          [],
        ),
        // Lights
        tiramisu.light(
          "ambient",
          [
            light.kind(light.Directional),
            light.color(0xffffff),
            light.intensity(1.0),
            light.cast_shadow(True),
            transform.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          ],
          [],
        ),
      ],
    ),
    // Info panel
    html.div([class("info")], [
      html.h3([], [html.text("Model Loading")]),
      html.div([class("status loaded")], [
        html.text("Using attribute.src() attribute"),
      ]),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Model Info"),
      ]),
      info_row("Source", "Soldier.glb"),
      info_row("Format", "GLTF/GLB"),
      info_row("Rotation", float_to_string_2(m.rotation) <> " rad"),
      info_row("FPS", int.to_string(float.round(m.fps))),
      html.div(
        [
          class("status"),
          attribute.style("margin-top", "15px"),
          attribute.style("background", "#333"),
        ],
        [html.text("Model rotates automatically")],
      ),
    ]),
  ])
}

// HELPERS ---------------------------------------------------------------------

fn info_row(label: String, value: String) {
  html.div([class("info-row")], [
    html.span([], [html.text(label)]),
    html.span([], [html.text(value)]),
  ])
}

fn float_to_string_2(f: Float) -> String {
  f
  |> float.to_precision(2)
  |> float.to_string()
}
