/// Declarative Audio Demo
///
/// Demonstrates the declarative audio API where audio state is expressed
/// in the view function, not through imperative commands.
///
/// Key concepts:
/// - Audio state (Playing, Stopped, Paused) is part of the model
/// - Fade configuration is declarative
/// - Volume controls are per-audio-source in the model
/// - The view function expresses desired audio state
/// - The renderer automatically handles transitions
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option
import tiramisu
import tiramisu/asset
import tiramisu/audio
import tiramisu/background
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  Main
  MusicCube
  MusicBg
  SfxCube
  MusicGroup
  SfxGroup
  Ambient
  Sun
  SfxBeep(Int)
}

pub fn main() {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

// --- Model ---

pub type Model {
  Model(
    rotation: Float,
    // Declarative audio states - these control what's in the view
    music_state: audio.AudioState,
    // For overlapping SFX, we track multiple instances with unique IDs
    // Each instance has its own state (Playing or Stopped)
    sfx_instances: Dict(Int, audio.AudioState),
    next_sfx_id: Int,
    // Audio buffers loaded from assets
    music_buffer: option.Option(audio.AudioBuffer),
    sfx_buffer: option.Option(audio.AudioBuffer),
    loading: Bool,
    // Individual volume controls (0.0 to 1.0)
    music_volume: Float,
    sfx_volume: Float,
  )
}

pub type Msg {
  Tick
  MusicLoaded(audio.AudioBuffer)
  SfxLoaded(audio.AudioBuffer)
  LoadError(String)
}

// --- Init ---

fn init(
  _ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  io.println("🎵 Declarative Audio Demo")
  io.println("========================")
  io.println("")
  io.println("Loading audio assets...")
  io.println("")

  #(
    Model(
      rotation: 0.0,
      music_state: audio.Stopped,
      sfx_instances: dict.new(),
      next_sfx_id: 0,
      music_buffer: option.None,
      sfx_buffer: option.None,
      loading: True,
      music_volume: 0.7,
      sfx_volume: 1.0,
    ),
    effect.batch([
      effect.tick(Tick),
      // Load audio assets
      effect.from_promise(
        asset.load_audio("music.ogg")
        |> promise.map(fn(result) {
          case result {
            Ok(buffer) -> MusicLoaded(buffer)
            Error(asset.LoadError(msg)) -> LoadError("Music: " <> msg)
            Error(asset.InvalidUrl(msg)) -> LoadError("Music: " <> msg)
            Error(asset.ParseError(msg)) -> LoadError("Music: " <> msg)
          }
        }),
      ),
      effect.from_promise(
        asset.load_audio("sfx.wav")
        |> promise.map(fn(result) {
          case result {
            Ok(buffer) -> SfxLoaded(buffer)
            Error(asset.LoadError(msg)) -> LoadError("SFX: " <> msg)
            Error(asset.InvalidUrl(msg)) -> LoadError("SFX: " <> msg)
            Error(asset.ParseError(msg)) -> LoadError("SFX: " <> msg)
          }
        }),
      ),
    ]),
    option.None,
  )
}

// --- Update ---

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Id),
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.0005

      // Keep a maximum of 10 recent SFX instances to prevent unbounded growth
      // Since the audio is short (0.139s), keeping 10 instances is plenty
      let sfx_instances = case dict.size(model.sfx_instances) > 10 {
        True -> {
          // Remove the oldest instances (lowest IDs)
          let sorted =
            model.sfx_instances
            |> dict.to_list
            |> list.sort(fn(a, b) {
              let #(id_a, _) = a
              let #(id_b, _) = b
              int.compare(id_a, id_b)
            })

          // Keep only the newest 10
          sorted
          |> list.drop(list.length(sorted) - 10)
          |> dict.from_list
        }
        False -> model.sfx_instances
      }

      // Check for space key press to create a NEW SFX instance
      // Use is_key_just_pressed to only trigger on the first frame of the press
      let #(sfx_instances, next_sfx_id) = case
        input.is_key_just_pressed(ctx.input, input.Space)
      {
        True -> {
          // Create a new SFX instance with unique ID
          let new_instances =
            sfx_instances
            |> dict.insert(model.next_sfx_id, audio.Playing)
          #(new_instances, model.next_sfx_id + 1)
        }
        False -> #(sfx_instances, model.next_sfx_id)
      }

      // Check for music control keys (1, 2, 3)
      let music_state = case
        input.is_key_just_pressed(ctx.input, input.Digit1),
        input.is_key_just_pressed(ctx.input, input.Digit2),
        input.is_key_just_pressed(ctx.input, input.Digit3)
      {
        True, _, _ -> audio.Playing
        _, True, _ -> audio.Stopped
        _, _, True -> audio.Paused
        _, _, _ -> model.music_state
      }

      // Volume controls
      // Q/A - Decrease/Increase Music volume
      // W/S - Decrease/Increase SFX volume
      let volume_delta = 0.1

      // Check volume key presses
      let music_vol_dec = input.is_key_just_pressed(ctx.input, input.KeyQ)
      let music_vol_inc = input.is_key_just_pressed(ctx.input, input.KeyA)
      let sfx_vol_dec = input.is_key_just_pressed(ctx.input, input.KeyW)
      let sfx_vol_inc = input.is_key_just_pressed(ctx.input, input.KeyS)

      // Music volume control
      let music_volume = case music_vol_dec, music_vol_inc {
        True, _ -> {
          let new_vol = model.music_volume -. volume_delta
          let clamped = case new_vol <. 0.0 {
            True -> 0.0
            False -> new_vol
          }
          io.println("Music Volume: " <> float_to_percentage(clamped))
          clamped
        }
        _, True -> {
          let new_vol = model.music_volume +. volume_delta
          let clamped = case new_vol >. 1.0 {
            True -> 1.0
            False -> new_vol
          }
          io.println("Music Volume: " <> float_to_percentage(clamped))
          clamped
        }
        _, _ -> model.music_volume
      }

      // SFX volume control
      let sfx_volume = case sfx_vol_dec, sfx_vol_inc {
        True, _ -> {
          let new_vol = model.sfx_volume -. volume_delta
          let clamped = case new_vol <. 0.0 {
            True -> 0.0
            False -> new_vol
          }
          io.println("SFX Volume: " <> float_to_percentage(clamped))
          clamped
        }
        _, True -> {
          let new_vol = model.sfx_volume +. volume_delta
          let clamped = case new_vol >. 1.0 {
            True -> 1.0
            False -> new_vol
          }
          io.println("SFX Volume: " <> float_to_percentage(clamped))
          clamped
        }
        _, _ -> model.sfx_volume
      }

      #(
        Model(
          ..model,
          rotation: new_rotation,
          sfx_instances: sfx_instances,
          next_sfx_id: next_sfx_id,
          music_state: music_state,
          music_volume: music_volume,
          sfx_volume: sfx_volume,
        ),
        effect.tick(Tick),
        option.None,
      )
    }

    MusicLoaded(buffer) -> {
      io.println("✅ Music loaded successfully")
      print_instructions(model.sfx_buffer)
      #(
        Model(..model, music_buffer: option.Some(buffer), loading: False),
        effect.none(),
        option.None,
      )
    }

    SfxLoaded(buffer) -> {
      io.println("✅ SFX loaded successfully")
      print_instructions(model.music_buffer)
      #(
        Model(..model, sfx_buffer: option.Some(buffer), loading: False),
        effect.none(),
        option.None,
      )
    }

    LoadError(msg) -> {
      io.println("❌ Failed to load audio: " <> msg)
      #(Model(..model, loading: False), effect.none(), option.None)
    }
  }
}

fn print_instructions(other_buffer_loaded: option.Option(a)) -> Nil {
  case other_buffer_loaded {
    option.Some(_) -> {
      io.println("")
      io.println("Controls:")
      io.println("  [1] Start/Resume music (1000ms fade-in from stopped)")
      io.println("  [2] Stop music (with 800ms fade-out)")
      io.println("  [3] Pause music (press [1] to resume)")
      io.println("  [SPACE] Play SFX (overlapping sounds - press rapidly!)")
      io.println("")
      io.println("Volume Controls:")
      io.println("  [Q/A] Decrease/Increase Music volume")
      io.println("  [W/S] Decrease/Increase SFX volume")
      io.println("")
    }
    option.None -> Nil
  }
}

fn float_to_percentage(value: Float) -> String {
  let percentage = value *. 100.0
  let rounded = float.round(percentage)
  int.to_string(rounded) <> "%"
}

// --- View ---

fn view(model: Model, _ctx: tiramisu.Context(Id)) -> List(scene.Node(Id)) {
  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  // Music audio config - declarative!
  // Individual volume is multiplied by group volume for final output
  let music_config =
    audio.config()
    |> audio.with_volume(model.music_volume)
    |> audio.with_loop(True)
    |> audio.with_group(audio.Music)
    |> audio.with_state(model.music_state)
    |> audio.with_fade(1000)
  // Fade duration for transitions

  // Geometries and materials
  let assert Ok(cube_geo) = geometry.box(width: 2.0, height: 2.0, depth: 2.0)
  let assert Ok(music_mat) =
    material.new() |> material.with_color(0x4ecca3) |> material.build()
  let assert Ok(sfx_mat) =
    material.new() |> material.with_color(0xff6b9d) |> material.build()

  let assert Ok(ambient_light) = light.ambient(color: 0xffffff, intensity: 0.4)

  let assert Ok(directional_light) =
    light.directional(color: 0xffffff, intensity: 0.8)

  // Build scene nodes based on loaded assets
  let music_nodes = case model.music_buffer {
    option.Some(buffer) -> [
      scene.mesh(
        id: MusicCube,
        geometry: cube_geo,
        material: music_mat,
        transform: transform.identity,
        physics: option.None,
      ),
      // Audio node - state is declarative!
      scene.audio(id: MusicBg, audio: audio.global(buffer, music_config)),
    ]
    option.None -> [
      scene.mesh(
        id: MusicCube,
        geometry: cube_geo,
        material: music_mat,
        transform: transform.identity,
        physics: option.None,
      ),
    ]
  }

  // Create audio nodes for all active SFX instances
  let sfx_audio_nodes = case model.sfx_buffer {
    option.Some(buffer) -> {
      model.sfx_instances
      |> dict.to_list
      |> list.map(fn(instance) {
        let #(id, state) = instance
        let sfx_config =
          audio.config()
          |> audio.with_volume(model.sfx_volume)
          |> audio.with_loop(False)
          |> audio.with_group(audio.SFX)
          |> audio.with_state(state)
          |> audio.with_no_fade()

        scene.audio(id: SfxBeep(id), audio: audio.global(buffer, sfx_config))
      })
    }
    option.None -> []
  }

  let sfx_nodes =
    [
      scene.mesh(
        id: SfxCube,
        geometry: cube_geo,
        material: sfx_mat,
        transform: transform.identity,
        physics: option.None,
      ),
    ]
    |> list.append(sfx_audio_nodes)

  [
    scene.camera(
      id: Main,
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 0.0, 10.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    ),
    // Music cube (left) - state determines if it plays
    scene.group(
      id: MusicGroup,
      transform: transform.identity
        |> transform.translate(by: vec3.Vec3(-3.0, 0.0, 0.0))
        |> transform.rotate_y(model.rotation),
      children: music_nodes,
    ),
    // SFX cube (right) - one-shot sounds
    scene.group(
      id: SfxGroup,
      transform: transform.identity
        |> transform.translate(by: vec3.Vec3(3.0, 0.0, 0.0))
        |> transform.rotate_y(model.rotation *. -1.5),
      children: sfx_nodes,
    ),
    // Lights
    scene.light(
      id: Ambient,
      light: ambient_light,
      transform: transform.identity,
    ),
    scene.light(
      id: Sun,
      light: directional_light,
      transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
    ),
  ]
}
