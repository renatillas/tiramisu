//// Input Demo Example
////
//// Demonstrates the input system in Tiramisu:
//// - Keyboard input: WASD to move a cube, Space to jump
//// - Mouse input: position tracking, button states
//// - Just pressed/released detection for single-fire actions
//// - Real-time input state display

import gleam/float
import gleam/int
import gleam/list
import gleam/string
import gleam/time/duration
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element/html
import quaternion
import tiramisu
import tiramisu/camera
import tiramisu/input
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
    /// Cube position
    cube_x: Float,
    cube_y: Float,
    cube_z: Float,
    /// Cube color (changes on mouse click)
    cube_color: Int,
    /// Vertical velocity for jump
    velocity_y: Float,
    /// Whether cube is on ground
    on_ground: Bool,
    /// Space press count (demonstrates just_pressed)
    jump_count: Int,
    /// Mouse position
    mouse_x: Float,
    mouse_y: Float,
    /// Currently pressed keys (for display)
    pressed_keys: List(String),
    /// Mouse buttons held
    mouse_left: Bool,
    mouse_right: Bool,
  )
}

/// Messages for state updates
pub type Msg {
  /// Animation tick with timing and input context
  Tick(TickContext)
}

// CONSTANTS -------------------------------------------------------------------

const move_speed = 5.0

const jump_velocity = 8.0

const gravity = 20.0

const ground_y = 1.0

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
      cube_x: 0.0,
      cube_y: ground_y,
      cube_z: 0.0,
      cube_color: 0x4ecdc4,
      velocity_y: 0.0,
      on_ground: True,
      jump_count: 0,
      mouse_x: 0.0,
      mouse_y: 0.0,
      pressed_keys: [],
      mouse_left: False,
      mouse_right: False,
    )

  // Subscribe to tick updates for animation and input
  #(model, tick.subscribe("", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      let inp = ctx.input

      // Movement from WASD keys
      let move_x = case
        input.is_key_pressed(inp, input.KeyD),
        input.is_key_pressed(inp, input.KeyA)
      {
        True, False -> move_speed *. dt
        False, True -> move_speed *. dt *. -1.0
        _, _ -> 0.0
      }

      let move_z = case
        input.is_key_pressed(inp, input.KeyS),
        input.is_key_pressed(inp, input.KeyW)
      {
        True, False -> move_speed *. dt
        False, True -> move_speed *. dt *. -1.0
        _, _ -> 0.0
      }

      // Jump on Space (only when on ground and just pressed)
      let #(new_velocity_y, new_on_ground, jump_count) = case
        input.is_key_just_pressed(inp, input.Space),
        model.on_ground
      {
        True, True -> #(jump_velocity, False, model.jump_count + 1)
        _, _ -> #(model.velocity_y, model.on_ground, model.jump_count)
      }

      // Apply gravity
      let velocity_after_gravity = new_velocity_y -. gravity *. dt
      let new_y = model.cube_y +. velocity_after_gravity *. dt

      // Ground collision
      let #(final_y, final_velocity, final_on_ground) = case new_y <. ground_y {
        True -> #(ground_y, 0.0, True)
        False -> #(new_y, velocity_after_gravity, new_on_ground)
      }

      // Change color on mouse click (just pressed)
      let new_color = case
        input.is_mouse_button_just_pressed(inp, input.LeftButton)
      {
        True -> next_color(model.cube_color)
        False -> model.cube_color
      }

      // Get mouse position
      let #(mx, my) = input.mouse_position(inp)

      // Build list of pressed keys for display
      let pressed = build_pressed_keys_display(inp)

      let new_model =
        Model(
          cube_x: model.cube_x +. move_x,
          cube_y: final_y,
          cube_z: model.cube_z +. move_z,
          cube_color: new_color,
          velocity_y: final_velocity,
          on_ground: final_on_ground,
          jump_count: jump_count,
          mouse_x: mx,
          mouse_y: my,
          pressed_keys: pressed,
          mouse_left: input.is_mouse_button_pressed(inp, input.LeftButton),
          mouse_right: input.is_mouse_button_pressed(inp, input.RightButton),
        )

      #(new_model, effect.none())
    }
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
            camera.transform(transform.at(vec3.Vec3(0.0, 5.0, 10.0))),
            camera.active(True),
          ],
          [],
        ),
        // Player cube (controlled by input)
        mesh.mesh(
          "player",
          [
            mesh.geometry_box(1.5, 1.5, 1.5),
            mesh.color(model.cube_color),
            mesh.metalness(0.3),
            mesh.roughness(0.7),
            mesh.transform(
              transform.at(vec3.Vec3(model.cube_x, model.cube_y, model.cube_z)),
            ),
          ],
          [],
        ),
        // Ground plane
        mesh.mesh(
          "ground",
          [
            mesh.geometry_plane(20.0, 20.0),
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
            light.intensity(0.5),
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
    // Info panel
    html.div([class("info")], [
      html.h3([], [html.text("Input State")]),
      info_row("Mouse X", float_to_string_1(model.mouse_x)),
      info_row("Mouse Y", float_to_string_1(model.mouse_y)),
      info_row("Left Button", bool_to_string(model.mouse_left)),
      info_row("Right Button", bool_to_string(model.mouse_right)),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Keyboard"),
      ]),
      info_row("Pressed", case model.pressed_keys {
        [] -> "none"
        keys -> string.join(keys, ", ")
      }),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Game State"),
      ]),
      info_row("Position X", float_to_string_1(model.cube_x)),
      info_row("Position Y", float_to_string_1(model.cube_y)),
      info_row("Position Z", float_to_string_1(model.cube_z)),
      info_row("On Ground", bool_to_string(model.on_ground)),
      info_row("Jump Count", int.to_string(model.jump_count)),
      html.div([class("controls-help")], [
        html.text("Controls: "),
        html.kbd([], [html.text("W")]),
        html.kbd([], [html.text("A")]),
        html.kbd([], [html.text("S")]),
        html.kbd([], [html.text("D")]),
        html.text(" to move, "),
        html.kbd([], [html.text("Space")]),
        html.text(" to jump, "),
        html.text("Click to change color"),
      ]),
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

fn float_to_string_1(f: Float) -> String {
  f
  |> float.to_precision(1)
  |> float.to_string()
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "yes"
    False -> "no"
  }
}

fn next_color(current: Int) -> Int {
  case current {
    0x4ecdc4 -> 0xff6b6b
    0xff6b6b -> 0xf9ca24
    0xf9ca24 -> 0xa55eea
    0xa55eea -> 0x45b7d1
    _ -> 0x4ecdc4
  }
}

fn build_pressed_keys_display(inp: input.InputState) -> List(String) {
  let keys = []
  let keys = case input.is_key_pressed(inp, input.KeyW) {
    True -> ["W", ..keys]
    False -> keys
  }
  let keys = case input.is_key_pressed(inp, input.KeyA) {
    True -> ["A", ..keys]
    False -> keys
  }
  let keys = case input.is_key_pressed(inp, input.KeyS) {
    True -> ["S", ..keys]
    False -> keys
  }
  let keys = case input.is_key_pressed(inp, input.KeyD) {
    True -> ["D", ..keys]
    False -> keys
  }
  let keys = case input.is_key_pressed(inp, input.Space) {
    True -> ["Space", ..keys]
    False -> keys
  }
  let keys = case input.is_key_pressed(inp, input.ShiftLeft) {
    True -> ["Shift", ..keys]
    False -> keys
  }
  list.reverse(keys)
}
