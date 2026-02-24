//// Audio Demo Example
////
//// Demonstrates tiramisu's audio components:
//// - Global audio: Background music that plays at constant volume
//// - Positional audio: A sound attached to a moving ball
////   - Volume and panning change based on distance from camera
//// - Detune (pitch shift): Adjust pitch in cents (100 = 1 semitone)
////
//// Click the buttons to toggle audio playback.
//// Use pitch controls to shift the audio pitch up or down.
//// Watch how the positional audio changes as the ball moves!

import gleam/float
import gleam/int
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
import tiramisu/camera
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/scene
import tiramisu/tick.{type TickContext}
import tiramisu/transform
import vec/vec2
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
    /// Music pitch shift in cents (100 = 1 semitone)
    music_detune: Float,
    /// Positional sound pitch shift in cents
    sound_detune: Float,
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
  /// Shift music pitch by given cents
  ShiftMusicPitch(Float)
  /// Shift positional sound pitch by given cents
  ShiftSoundPitch(Float)
}

// AUDIO URLs ------------------------------------------------------------------

// Using free audio samples from reliable CORS-friendly sources
// Background music - ambient electronic loop from soundhelix
const music_url = "ambient.mp3"

// Positional sound - a short beep/ping sound from soundjay
const beep_url = "beep.mp3"

// CONSTANTS -------------------------------------------------------------------

// Ball movement radius
const orbit_radius = 10.0

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
      music_detune: 0.0,
      sound_detune: 0.0,
    )

  // Subscribe to tick updates for animation
  #(initial_model, tick.subscribe("main", Tick))
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

    ShiftMusicPitch(cents) -> {
      #(
        Model(..model, music_detune: model.music_detune +. cents),
        effect.none(),
      )
    }

    ShiftSoundPitch(cents) -> {
      #(
        Model(..model, sound_detune: model.sound_detune +. cents),
        effect.none(),
      )
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) {
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
        // Camera (the listener position)
        tiramisu.camera(
          "main",
          [
            camera.fov(60.0),
            transform.transform(
              transform.at(vec3.Vec3(0.0, 8.0, 12.0))
              |> transform.with_look_at(vec3f.zero),
            ),
            camera.active(True),
          ],
          [],
        ),
        // Ground plane
        tiramisu.mesh(
          "ground",
          [
            mesh.plane(vec2.Vec2(20.0, 20.0)),
            mesh.color(0x2d3436),
            material.metalness(0.1),
            material.roughness(0.9),
            transform.transform(
              transform.at(vec3.Vec3(0.0, 0.0, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Center marker (shows where origin is)
        tiramisu.mesh(
          "center",
          [
            mesh.cylinder(
              radius_top: 0.3,
              radius_bottom: 0.3,
              height: 0.1,
              segments: 16,
            ),
            mesh.color(0x666666),
            transform.transform(transform.at(vec3.Vec3(0.0, 0.5, 0.0))),
          ],
          [],
        ),
        // Orbit path visualization (ring on ground)
        tiramisu.mesh(
          "orbit_path",
          [
            mesh.torus(
              radius: orbit_radius,
              tube: 0.05,
              radial_segments: 16,
              tubular_segments: 100,
            ),
            mesh.color(0x444444),
            transform.transform(
              transform.at(vec3.Vec3(0.0, 0.01, 0.0))
              |> transform.with_rotation(
                quaternion.from_euler(vec3.Vec3(-1.5708, 0.0, 0.0)),
              ),
            ),
          ],
          [],
        ),
        // Moving ball (sound source)
        tiramisu.audio_positional(
          "ball_sound",
          [
            attribute.src(beep_url),
            audio.volume(1.0),
            attribute.loop(True),
            audio.playing(model.sound_playing),
            audio.detune(model.sound_detune),
            transform.transform(
              transform.at(vec3.Vec3(model.ball_x, 0.5, model.ball_z)),
            ),
            audio.ref_distance(2.0),
            audio.max_distance(20.0),
            audio.rolloff_factor(1.0),
          ],
          [
            tiramisu.mesh(
              "sound_ball",
              [
                mesh.sphere(radius: 0.5, segments: vec2.Vec2(16, 16)),
                mesh.color(case model.sound_playing {
                  True -> 0xff6b6b
                  False -> 0x666666
                }),
                material.metalness(0.3),
                material.roughness(0.4),
              ],
              [],
            ),
          ],
        ),
        // Lights
        tiramisu.light(
          "ambient",
          [
            light.kind(light.Ambient),
            mesh.color(0xffffff),
            light.intensity(0.4),
          ],
          [],
        ),
        tiramisu.light(
          "sun",
          [
            light.kind(light.Directional),
            light.color(0xffffff),
            light.intensity(1.0),
            transform.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          ],
          [],
        ),
        // Global audio (background music)
        tiramisu.global_audio(
          "background_music",
          [
            attribute.src(music_url),
            audio.volume(0.3),
            attribute.loop(True),
            audio.playing(model.music_playing),
            audio.detune(model.music_detune),
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
        // Music pitch controls
        html.div([class("pitch-controls")], [
          html.span([class("pitch-label")], [html.text("Music Pitch")]),
          html.button(
            [class("pitch-btn"), event.on_click(ShiftMusicPitch(-100.0))],
            [html.text("-")],
          ),
          html.span([class("pitch-value")], [
            html.text(float_to_string_0(model.music_detune) <> "c"),
          ]),
          html.button(
            [class("pitch-btn"), event.on_click(ShiftMusicPitch(100.0))],
            [html.text("+")],
          ),
        ]),
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
        // Ball sound pitch controls
        html.div([class("pitch-controls")], [
          html.span([class("pitch-label")], [html.text("Ball Pitch")]),
          html.button(
            [class("pitch-btn"), event.on_click(ShiftSoundPitch(-100.0))],
            [html.text("-")],
          ),
          html.span([class("pitch-value")], [
            html.text(float_to_string_0(model.sound_detune) <> "c"),
          ]),
          html.button(
            [class("pitch-btn"), event.on_click(ShiftSoundPitch(100.0))],
            [html.text("+")],
          ),
        ]),
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
      info_row(
        "Music Detune",
        float_to_string_0(model.music_detune) <> " cents",
      ),
      info_row("Ball", "Positional (3D panning)"),
      info_row("Ball Detune", float_to_string_0(model.sound_detune) <> " cents"),
      html.div([class("note")], [
        html.text(
          "The ball orbits the center. When positional audio is playing, "
          <> "notice how the sound pans left/right and changes volume "
          <> "as the ball moves closer or farther from the camera. "
          <> "Use the pitch controls to shift audio up or down "
          <> "(each step = 100 cents = 1 semitone).",
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

fn float_to_string_0(f: Float) -> String {
  f
  |> float.round()
  |> int.to_string()
}
