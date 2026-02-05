//// Audio Demo Example
////
//// Demonstrates tiramisu's audio components:
//// - Global audio: Background music that plays at constant volume
//// - Positional audio: A sound attached to a moving ball
////   - Volume and panning change based on distance from camera
////
//// Click the buttons to toggle audio playback.
//// Watch how the positional audio changes as the ball moves!

import gleam/float
import gleam/time/duration
import gleam_community/maths
import lustre
import lustre/attribute.{class}
import lustre/effect.{type Effect}
import lustre/element/html
import lustre/event
import quaternion
import tiramisu
import tiramisu/audio
import tiramisu/audio_positional
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick.{type TickContext}
import tiramisu/transform
import vec/vec3
import vec/vec3f

// TYPES -----------------------------------------------------------------------

/// Application state
pub type Model {
  Model(
    /// Time accumulator for ball movement
    time: Float,
    /// Ball position (moves in a circle)
    ball_x: Float,
    ball_z: Float,
    /// Whether background music is playing
    music_playing: Bool,
    /// Whether positional audio is playing
    sound_playing: Bool,
  )
}

/// Messages for state updates
pub type Msg {
  /// Animation tick
  Tick(TickContext)
  /// Toggle background music
  ToggleMusic
  /// Toggle positional sound
  ToggleSound
}

// AUDIO URLs ------------------------------------------------------------------

// Using free audio samples from reliable CORS-friendly sources
// Background music - ambient electronic loop from soundhelix
const music_url = "ambient.mp3"

// Positional sound - a short beep/ping sound from soundjay
const beep_url = "beep.mp3"

// CONSTANTS -------------------------------------------------------------------

// Ball movement radius
const orbit_radius = 5.0

// Ball movement speed
const orbit_speed = 0.5

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
  let initial_model =
    Model(
      time: 0.0,
      ball_x: orbit_radius,
      ball_z: 0.0,
      music_playing: False,
      sound_playing: False,
    )

  // Subscribe to tick updates for animation
  #(initial_model, tick.subscribe("", Tick))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick(ctx) -> {
      let dt = duration.to_seconds(ctx.delta_time)
      let new_time = model.time +. dt *. orbit_speed

      // Move ball in a circle around the origin
      let ball_x = maths.cos(new_time) *. orbit_radius
      let ball_z = maths.sin(new_time) *. orbit_radius

      #(
        Model(..model, time: new_time, ball_x: ball_x, ball_z: ball_z),
        effect.none(),
      )
    }

    ToggleMusic -> {
      #(Model(..model, music_playing: !model.music_playing), effect.none())
    }

    ToggleSound -> {
      #(Model(..model, sound_playing: !model.sound_playing), effect.none())
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
        // Camera (the listener position)
        camera.camera(
          "main",
          [
            camera.fov(60.0),
            camera.transform(
              transform.at(vec3.Vec3(0.0, 8.0, 12.0))
              |> transform.with_look_at(vec3f.zero),
            ),
            camera.active(True),
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
        // Center marker (shows where origin is)
        mesh.mesh(
          "center",
          [
            mesh.geometry_cylinder_simple(0.3, 0.1),
            mesh.color(0x666666),
            mesh.transform(transform.at(vec3.Vec3(0.0, 0.05, 0.0))),
          ],
          [],
        ),
        // Orbit path visualization (ring on ground)
        mesh.mesh(
          "orbit_path",
          [
            mesh.geometry_torus_simple(orbit_radius, 0.05),
            mesh.color(0x444444),
            mesh.transform(
              transform.at(vec3.Vec3(0.0, 0.01, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Moving ball (sound source)
        audio_positional.audio_positional(
          "ball_sound",
          [
            audio_positional.src(beep_url),
            audio_positional.volume(1.0),
            audio_positional.loop(True),
            audio_positional.playing(model.sound_playing),
            audio_positional.audio_transform(
              transform.at(vec3.Vec3(model.ball_x, 0.5, model.ball_z)),
            ),
            audio_positional.ref_distance(2.0),
            audio_positional.max_distance(20.0),
            audio_positional.rolloff_factor(1.0),
          ],
          [
            mesh.mesh(
              "sound_ball",
              [
                mesh.geometry_sphere_simple(0.5),
                mesh.color(case model.sound_playing {
                  True -> 0xff6b6b
                  False -> 0x666666
                }),
                mesh.metalness(0.3),
                mesh.roughness(0.4),
              ],
              [],
            ),
          ],
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
        // Global audio (background music)
        audio.audio(
          "background_music",
          [
            audio.src(music_url),
            audio.volume(0.3),
            audio.loop(True),
            audio.playing(model.music_playing),
          ],
          [],
        ),
      ],
    ),
    // Info panel
    html.div([class("info")], [
      html.h3([], [html.text("Audio Demo")]),
      // Controls
      html.div([class("controls")], [
        html.button(
          [
            attribute.class(case model.music_playing {
              True -> "playing"
              False -> ""
            }),
            event.on_click(ToggleMusic),
          ],
          [
            html.text(case model.music_playing {
              True -> "⏹ Stop Music"
              False -> "▶ Play Music"
            }),
          ],
        ),
        html.button(
          [
            attribute.class(case model.sound_playing {
              True -> "playing"
              False -> ""
            }),
            event.on_click(ToggleSound),
          ],
          [
            html.text(case model.sound_playing {
              True -> "⏹ Stop Ball Sound"
              False -> "▶ Play Ball Sound"
            }),
          ],
        ),
      ]),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Sound Source"),
      ]),
      info_row("Ball X", float_to_string_1(model.ball_x)),
      info_row("Ball Z", float_to_string_1(model.ball_z)),
      info_row("Orbit Radius", float.to_string(orbit_radius) <> " units"),
      html.h3([attribute.style("margin-top", "15px")], [
        html.text("Audio Types"),
      ]),
      info_row("Music", "Global (constant volume)"),
      info_row("Ball", "Positional (3D panning)"),
      html.div([class("note")], [
        html.text(
          "The ball orbits the center. When positional audio is playing, "
          <> "notice how the sound pans left/right and changes volume "
          <> "as the ball moves closer or farther from the camera.",
        ),
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
