//// Model Loading Example
////
//// Demonstrates loading and displaying GLTF models:
//// - Using mesh.src() to load external 3D models
//// - The mesh component handles loading automatically
//// - Rotation animation via tick updates

import gleam/float
import gleam/time/duration
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element/html
import quaternion
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick.{type TickContext}
import tiramisu/transform
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Application state
pub type Model {
  Model(
    /// Rotation angle for the model
    rotation: Float,
  )
}

/// Messages for state updates
pub type Msg {
  /// Animation tick
  Tick(TickContext)
}

// MODEL URL -------------------------------------------------------------------

// Using a public GLTF model from the Three.js examples
const model_url = "https://threejs.org/examples/models/gltf/Soldier.glb"

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  // Register all Tiramisu web components
  let assert Ok(_) = tiramisu.register()

  // Start a Lustre app with effects support
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let initial_model = Model(rotation: 0.0)

  // Subscribe to tick updates for animation
  #(initial_model, tick.subscribe("", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(m: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)

      // Slowly rotate the model
      let new_rotation = m.rotation +. dt *. 0.5

      #(Model(rotation: new_rotation), effect.none())
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(m: Model) {
  html.div([class("container")], [
    // 3D Scene
    renderer.renderer(
      [
        renderer.width(600),
        renderer.height(500),
        renderer.background("#1a1a2e"),
      ],
      [
        // Camera
        camera.camera(
          "main",
          [
            camera.fov(45.0),
            camera.transform(transform.at(vec3.Vec3(0.0, 1.5, 5.0))),
            camera.active(True),
          ],
          [],
        ),
        // Ground plane
        mesh.mesh(
          "ground",
          [
            mesh.geometry_plane(10.0, 10.0),
            mesh.color(0x2d3436),
            mesh.metalness(0.1),
            mesh.roughness(0.9),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Loaded GLTF model using mesh.src()
        mesh.mesh(
          "soldier",
          [
            mesh.src(model_url),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(0.0, m.rotation, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Lights
        light.light(
          "ambient",
          [
            light.light_type("ambient"),
            light.color(0xffffff),
            light.intensity(0.6),
          ],
          [],
        ),
        light.light(
          "sun",
          [
            light.light_type("directional"),
            light.color(0xffffff),
            light.intensity(1.2),
            light.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          ],
          [],
        ),
      ],
    ),
    // Info panel
    html.div([class("info")], [
      html.h3([], [html.text("Model Loading")]),
      html.div([class("status loaded")], [
        html.text("Using mesh.src() attribute"),
      ]),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Model Info"),
      ]),
      info_row("Source", "Soldier.glb"),
      info_row("Format", "GLTF/GLB"),
      info_row("Rotation", float_to_string_2(m.rotation) <> " rad"),
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
