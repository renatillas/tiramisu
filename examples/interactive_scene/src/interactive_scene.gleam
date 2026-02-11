//// Interactive Scene Example
////
//// Demonstrates interactive Tiramisu scenes using lustre.application:
//// - Changing mesh colors dynamically
//// - Animating mesh rotation with requestAnimationFrame
//// - Toggling wireframe mode
//// - Adjusting material properties

import gleam/float
import gleam/int
import gleam/time/duration
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element/html
import lustre/event
import quaternion
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick
import tiramisu/transform
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Application state
pub type Model {
  Model(
    /// Current cube color (hex integer)
    cube_color: Int,
    /// Current rotation angle in radians
    rotation: Float,
    /// Rotation speed multiplier
    rotation_speed: Float,
    /// Whether wireframe mode is enabled
    wireframe: Bool,
    /// Metalness value (0.0 - 1.0)
    metalness: Float,
    /// Roughness value (0.0 - 1.0)
    roughness: Float,
    /// Whether animation is running
    animating: Bool,
  )
}

/// Messages for state updates
pub type Msg {
  /// Animation tick with timing context
  Tick(tick.TickContext)
  /// Change the cube color
  SetColor(Int)
  /// Toggle wireframe mode
  ToggleWireframe
  /// Toggle animation
  ToggleAnimation
  /// Set rotation speed
  SetRotationSpeed(Float)
  /// Set metalness
  SetMetalness(Float)
  /// Set roughness
  SetRoughness(Float)
}

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
  let model =
    Model(
      cube_color: 0xff6b6b,
      rotation: 0.0,
      rotation_speed: 1.0,
      wireframe: False,
      metalness: 0.3,
      roughness: 0.7,
      animating: True,
    )

  // Subscribe to tick updates for animation
  #(model, tick.subscribe("", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      case model.animating {
        True -> {
          // Get delta time in seconds from the tick context
          let dt = duration.to_seconds(ctx.delta_time)
          let new_rotation = model.rotation +. dt *. model.rotation_speed
          #(Model(..model, rotation: new_rotation), effect.none())
        }
        False -> #(model, effect.none())
      }
    }

    SetColor(color) -> #(Model(..model, cube_color: color), effect.none())

    ToggleWireframe -> #(
      Model(..model, wireframe: !model.wireframe),
      effect.none(),
    )

    ToggleAnimation -> #(
      Model(..model, animating: !model.animating),
      effect.none(),
    )

    SetRotationSpeed(speed) -> #(
      Model(..model, rotation_speed: speed),
      effect.none(),
    )

    SetMetalness(value) -> #(Model(..model, metalness: value), effect.none())

    SetRoughness(value) -> #(Model(..model, roughness: value), effect.none())
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) {
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
            camera.fov(75.0),
            camera.transform(transform.at(vec3.Vec3(0.0, 3.0, 6.0))),
            camera.active(True),
          ],
          [],
        ),
        // Rotating cube with dynamic properties
        mesh.mesh(
          "cube",
          [
            mesh.geometry_box(vec3.Vec3(2.0, 2.0, 2.0)),
            mesh.color(model.cube_color),
            mesh.metalness(model.metalness),
            mesh.roughness(model.roughness),
            mesh.wireframe(model.wireframe),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 1.5, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(0.0, model.rotation, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Ground plane
        mesh.mesh(
          "ground",
          [
            mesh.geometry_plane(15.0, 15.0),
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
        // Lights
        light.light(
          "ambient",
          [
            light.light_type("ambient"),
            light.color(0xffffff),
            light.intensity(0.4),
          ],
          [],
        ),
        light.light(
          "sun",
          [
            light.light_type("directional"),
            light.color(0xffffff),
            light.intensity(1.0),
            light.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          ],
          [],
        ),
      ],
    ),
    // Control panel
    html.div([class("controls")], [
      html.h3([], [html.text("Color")]),
      color_button("Red", 0xff6b6b, model.cube_color),
      color_button("Green", 0x4ecdc4, model.cube_color),
      color_button("Blue", 0x45b7d1, model.cube_color),
      color_button("Yellow", 0xf9ca24, model.cube_color),
      color_button("Purple", 0xa55eea, model.cube_color),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Rotation Speed"),
      ]),
      slider("speed", 0.0, 5.0, 0.1, model.rotation_speed, SetRotationSpeed),
      html.h3([attribute.style("margin-top", "15px")], [html.text("Material")]),
      slider("metalness", 0.0, 1.0, 0.05, model.metalness, SetMetalness),
      slider("roughness", 0.0, 1.0, 0.05, model.roughness, SetRoughness),
      html.h3([attribute.style("margin-top", "15px")], [html.text("Options")]),
      html.button(
        [
          event.on_click(ToggleWireframe),
          attribute.styles([
            #("background", case model.wireframe {
              True -> "#4ecdc4"
              False -> "#666"
            }),
            #("color", "white"),
          ]),
        ],
        [
          html.text(case model.wireframe {
            True -> "Wireframe: ON"
            False -> "Wireframe: OFF"
          }),
        ],
      ),
      html.button(
        [
          event.on_click(ToggleAnimation),
          attribute.styles([
            #("background", case model.animating {
              True -> "#f9ca24"
              False -> "#666"
            }),
            #("color", case model.animating {
              True -> "#111"
              False -> "white"
            }),
            #("margin-top", "5px"),
          ]),
        ],
        [
          html.text(case model.animating {
            True -> "Animation: ON"
            False -> "Animation: OFF"
          }),
        ],
      ),
    ]),
  ])
}

// HELPERS ---------------------------------------------------------------------

fn color_button(label: String, color: Int, current: Int) {
  let hex = "#" <> int.to_base16(color)
  let is_selected = color == current
  html.button(
    [
      class("color-btn"),
      event.on_click(SetColor(color)),
      attribute.styles([
        #("background", hex),
        #("border", case is_selected {
          True -> "3px solid white"
          False -> "3px solid transparent"
        }),
      ]),
    ],
    [html.text(label)],
  )
}

fn slider(
  id: String,
  min: Float,
  max: Float,
  step: Float,
  value: Float,
  on_change: fn(Float) -> Msg,
) {
  html.label([], [
    html.text(id <> ": " <> float.to_string(value)),
    html.input([
      attribute.type_("range"),
      attribute.id(id),
      attribute.min(float.to_string(min)),
      attribute.max(float.to_string(max)),
      attribute.step(float.to_string(step)),
      attribute.value(float.to_string(value)),
      event.on_input(fn(str) {
        case float.parse(str) {
          Ok(v) -> on_change(v)
          Error(_) -> on_change(value)
        }
      }),
    ]),
  ])
}
