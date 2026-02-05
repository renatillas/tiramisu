//// The tiramisu-audio-positional web component.
////
//// This component creates a positional (3D) audio source.
//// Audio volume and panning are affected by the listener's position
//// relative to the audio source. Use for sound effects attached to
//// objects in the 3D scene.
////
//// ## Usage
////
//// ```html
//// <tiramisu-audio-positional
////   id="engine_sound"
////   src="sounds/engine.mp3"
////   volume="0.8"
////   loop="true"
////   playing="true"
////   transform="pos:5,0,0"
////   ref-distance="1"
////   max-distance="100"
//// ></tiramisu-audio-positional>
//// ```
////
//// ## Attributes
////
//// - `id`: Unique identifier for the audio (required)
//// - `src`: URL to the audio file (required)
//// - `volume`: Volume level 0.0-1.0 (default: 1.0)
//// - `loop`: Whether to loop playback (default: "false")
//// - `playing`: Whether to play/pause (default: "false")
//// - `playback-rate`: Playback speed multiplier (default: 1.0)
//// - `transform`: Position in 3D space (default: "pos:0,0,0")
//// - `ref-distance`: Distance where attenuation starts (default: 1.0)
//// - `max-distance`: Maximum distance for attenuation (default: 10000.0)
//// - `rolloff-factor`: How quickly volume decreases (default: 1.0)

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/float
import gleam/javascript/promise
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import savoiardi
import tiramisu/context.{type SceneContext, SceneContext}
import tiramisu/internal/dom
import tiramisu/internal/runtime
import tiramisu/transform.{type Transform}
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// The model for the positional audio component.
pub type Model {
  Model(
    /// The audio ID
    id: String,
    /// Scene context from parent renderer
    scene_context: Option(SceneContext),
    /// Parent object ID for hierarchical transforms (None = scene root)
    parent_id: Option(String),
    /// The audio listener (shared across all audio in the scene)
    listener: Option(savoiardi.AudioListener),
    /// The positional audio object
    audio: Option(savoiardi.PositionalAudio),
    /// The loaded audio buffer
    buffer: Option(savoiardi.AudioBuffer),
    /// Source URL
    src: Option(String),
    /// Whether currently loading
    loading: Bool,
    /// Volume level (0.0-1.0)
    volume: Float,
    /// Whether to loop
    loop: Bool,
    /// Whether playing
    playing: Bool,
    /// Playback rate multiplier
    playback_rate: Float,
    /// Transform (position in 3D space)
    transform: Transform,
    /// Reference distance for attenuation
    ref_distance: Float,
    /// Maximum distance for attenuation
    max_distance: Float,
    /// Rolloff factor
    rolloff_factor: Float,
  )
}

/// Messages for the positional audio component.
pub type Msg {
  /// Scene ID and optional parent object ID found via DOM traversal
  SceneIdFound(scene_id: String, parent_id: Option(String))
  /// ID attribute changed
  IdChanged(String)
  /// Source URL attribute changed
  SrcChanged(String)
  /// Audio buffer loaded successfully
  BufferLoaded(savoiardi.AudioBuffer)
  /// Audio buffer failed to load
  BufferLoadError
  /// Volume attribute changed
  VolumeChanged(Float)
  /// Loop attribute changed
  LoopChanged(Bool)
  /// Playing attribute changed
  PlayingChanged(Bool)
  /// Playback rate attribute changed
  PlaybackRateChanged(Float)
  /// Transform attribute changed
  TransformChanged(Transform)
  /// Reference distance changed
  RefDistanceChanged(Float)
  /// Max distance changed
  MaxDistanceChanged(Float)
  /// Rolloff factor changed
  RolloffFactorChanged(Float)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the positional audio component.
pub const tag_name = "tiramisu-audio-positional"

/// Register the tiramisu-audio-positional component as a custom element.
pub fn register() -> Result(Nil, lustre.Error) {
  let app =
    lustre.component(init, update, view, [
      component.on_attribute_change("id", fn(v) { Ok(IdChanged(v)) }),
      component.on_attribute_change("src", fn(v) { Ok(SrcChanged(v)) }),
      component.on_attribute_change("volume", parse_float_attr(VolumeChanged)),
      component.on_attribute_change("loop", parse_bool_attr(LoopChanged)),
      component.on_attribute_change("playing", parse_bool_attr(PlayingChanged)),
      component.on_attribute_change(
        "playback-rate",
        parse_float_attr(PlaybackRateChanged),
      ),
      component.on_attribute_change("transform", fn(v) {
        Ok(TransformChanged(transform.parse(v)))
      }),
      component.on_attribute_change(
        "ref-distance",
        parse_float_attr(RefDistanceChanged),
      ),
      component.on_attribute_change(
        "max-distance",
        parse_float_attr(MaxDistanceChanged),
      ),
      component.on_attribute_change(
        "rolloff-factor",
        parse_float_attr(RolloffFactorChanged),
      ),
    ])

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-audio-positional element (3D audio).
///
/// Positional audio changes volume and panning based on distance and
/// direction relative to the listener. Use for 3D sound effects
/// attached to objects in the scene.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/audio_positional
///
/// audio_positional.audio_positional("car_engine", [
///   audio_positional.src("sounds/engine.mp3"),
///   audio_positional.volume(0.8),
///   audio_positional.loop(True),
///   audio_positional.playing(True),
///   audio_positional.transform(transform.at(vec3.Vec3(5.0, 0.0, 0.0))),
///   audio_positional.ref_distance(1.0),
///   audio_positional.max_distance(50.0),
/// ], [])
/// ```
///
pub fn audio_positional(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(tag_name, [attribute.id(id), ..attributes], children)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Set the source URL for the audio file.
///
pub fn src(url: String) -> Attribute(msg) {
  attribute.attribute("src", url)
}

/// Set the volume level (0.0-1.0).
///
pub fn volume(level: Float) -> Attribute(msg) {
  attribute.attribute("volume", float.to_string(level))
}

/// Set whether the audio should loop.
///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  attribute.attribute("loop", case should_loop {
    True -> "true"
    False -> "false"
  })
}

/// Set whether the audio is playing.
///
pub fn playing(is_playing: Bool) -> Attribute(msg) {
  attribute.attribute("playing", case is_playing {
    True -> "true"
    False -> "false"
  })
}

/// Set the playback rate (1.0 = normal speed).
///
pub fn playback_rate(rate: Float) -> Attribute(msg) {
  attribute.attribute("playback-rate", float.to_string(rate))
}

/// Set the full transform (position) of the audio source.
///
pub fn audio_transform(t: Transform) -> Attribute(msg) {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  attribute.attribute(
    "transform",
    "pos:"
      <> float.to_string(px)
      <> ","
      <> float.to_string(py)
      <> ","
      <> float.to_string(pz)
      <> " quat:"
      <> float.to_string(qx)
      <> ","
      <> float.to_string(qy)
      <> ","
      <> float.to_string(qz)
      <> ","
      <> float.to_string(qw)
      <> " scale:"
      <> float.to_string(sx)
      <> ","
      <> float.to_string(sy)
      <> ","
      <> float.to_string(sz),
  )
}

/// Set the reference distance for attenuation.
///
/// The distance at which the volume reduction starts.
///
pub fn ref_distance(distance: Float) -> Attribute(msg) {
  attribute.attribute("ref-distance", float.to_string(distance))
}

/// Set the maximum distance for attenuation.
///
/// Beyond this distance, audio is not attenuated further.
///
pub fn max_distance(distance: Float) -> Attribute(msg) {
  attribute.attribute("max-distance", float.to_string(distance))
}

/// Set the rolloff factor.
///
/// Higher values = faster volume dropoff with distance.
///
pub fn rolloff_factor(factor: Float) -> Attribute(msg) {
  attribute.attribute("rolloff-factor", float.to_string(factor))
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      id: "",
      scene_context: None,
      parent_id: None,
      listener: None,
      audio: None,
      buffer: None,
      src: None,
      loading: False,
      volume: 1.0,
      loop: False,
      playing: False,
      playback_rate: 1.0,
      transform: transform.identity,
      ref_distance: 1.0,
      max_distance: 10_000.0,
      rolloff_factor: 1.0,
    )

  #(model, effect.after_paint(discover_scene_and_parent))
}

fn discover_scene_and_parent(
  dispatch: fn(Msg) -> Nil,
  root: dynamic.Dynamic,
) -> Nil {
  // Find both the scene ID and the immediate parent object ID
  let parent_id = dom.find_parent_object_id(root)

  case dom.find_parent_scene_id(root) {
    Some(scene_id) -> dispatch(SceneIdFound(scene_id, parent_id))
    None ->
      dom.listen_for_scene_ready(root, fn(scene_id) {
        dispatch(SceneIdFound(scene_id, parent_id))
      })
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    SceneIdFound(scene_id, parent_id) -> {
      let ctx = SceneContext(scene_id:)
      // Create listener and positional audio object
      let listener = savoiardi.create_audio_listener()
      let audio_obj = savoiardi.create_positional_audio(listener)

      // Apply initial settings
      savoiardi.set_positional_audio_volume(audio_obj, model.volume)
      savoiardi.set_positional_audio_loop(audio_obj, model.loop)
      savoiardi.set_positional_audio_playback_rate(
        audio_obj,
        model.playback_rate,
      )
      savoiardi.set_ref_distance(audio_obj, model.ref_distance)
      savoiardi.set_max_distance(audio_obj, model.max_distance)
      savoiardi.set_rolloff_factor(audio_obj, model.rolloff_factor)

      // Apply initial transform
      apply_transform_to_audio(audio_obj, model.transform)

      // Add to scene with proper parent (use parent_id if set, otherwise scene root)
      let scene_ref = runtime.SceneRef(scene_id)
      let parent = case parent_id {
        Some(pid) -> pid
        None -> scene_id
      }
      let _obj_ref =
        runtime.add_object_to_scene(
          scene_ref,
          parent,
          model.id,
          savoiardi.positional_audio_to_object3d(audio_obj),
        )

      let new_model =
        Model(
          ..model,
          scene_context: Some(ctx),
          parent_id:,
          listener: Some(listener),
          audio: Some(audio_obj),
        )

      // If we already have a src, start loading
      case model.src {
        Some(url) -> #(
          Model(..new_model, loading: True),
          load_audio_effect(url),
        )
        None -> #(new_model, effect.none())
      }
    }

    IdChanged(id) -> {
      #(Model(..model, id:), effect.none())
    }

    SrcChanged(url) -> {
      let new_model = Model(..model, src: Some(url), loading: True)
      case model.scene_context {
        Some(_) -> #(new_model, load_audio_effect(url))
        None -> #(new_model, effect.none())
      }
    }

    BufferLoaded(buffer) -> {
      case model.audio {
        Some(audio_obj) -> {
          savoiardi.set_positional_audio_buffer(audio_obj, buffer)
          case model.playing {
            True -> savoiardi.play_positional_audio(audio_obj)
            False -> Nil
          }
          #(Model(..model, buffer: Some(buffer), loading: False), effect.none())
        }
        None -> #(Model(..model, loading: False), effect.none())
      }
    }

    BufferLoadError -> {
      #(Model(..model, loading: False), effect.none())
    }

    VolumeChanged(vol) -> {
      case model.audio {
        Some(audio_obj) -> savoiardi.set_positional_audio_volume(audio_obj, vol)
        None -> Nil
      }
      #(Model(..model, volume: vol), effect.none())
    }

    LoopChanged(should_loop) -> {
      case model.audio {
        Some(audio_obj) ->
          savoiardi.set_positional_audio_loop(audio_obj, should_loop)
        None -> Nil
      }
      #(Model(..model, loop: should_loop), effect.none())
    }

    PlayingChanged(is_playing) -> {
      case model.audio, model.buffer {
        Some(audio_obj), Some(_) -> {
          case is_playing {
            True -> savoiardi.play_positional_audio(audio_obj)
            False -> savoiardi.pause_positional_audio(audio_obj)
          }
        }
        _, _ -> Nil
      }
      #(Model(..model, playing: is_playing), effect.none())
    }

    PlaybackRateChanged(rate) -> {
      case model.audio {
        Some(audio_obj) ->
          savoiardi.set_positional_audio_playback_rate(audio_obj, rate)
        None -> Nil
      }
      #(Model(..model, playback_rate: rate), effect.none())
    }

    TransformChanged(new_transform) -> {
      case model.audio {
        Some(audio_obj) -> apply_transform_to_audio(audio_obj, new_transform)
        None -> Nil
      }
      #(Model(..model, transform: new_transform), effect.none())
    }

    RefDistanceChanged(distance) -> {
      case model.audio {
        Some(audio_obj) -> savoiardi.set_ref_distance(audio_obj, distance)
        None -> Nil
      }
      #(Model(..model, ref_distance: distance), effect.none())
    }

    MaxDistanceChanged(distance) -> {
      case model.audio {
        Some(audio_obj) -> savoiardi.set_max_distance(audio_obj, distance)
        None -> Nil
      }
      #(Model(..model, max_distance: distance), effect.none())
    }

    RolloffFactorChanged(factor) -> {
      case model.audio {
        Some(audio_obj) -> savoiardi.set_rolloff_factor(audio_obj, factor)
        None -> Nil
      }
      #(Model(..model, rolloff_factor: factor), effect.none())
    }
  }
}

fn load_audio_effect(url: String) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    savoiardi.load_audio(url)
    |> promise.map(fn(result) {
      case result {
        Ok(buffer) -> dispatch(BufferLoaded(buffer))
        Error(_) -> dispatch(BufferLoadError)
      }
    })
    Nil
  })
}

fn apply_transform_to_audio(
  audio: savoiardi.PositionalAudio,
  t: Transform,
) -> Nil {
  let pos = transform.position(t)
  let obj = savoiardi.positional_audio_to_object3d(audio)
  savoiardi.set_object_position(obj, pos)
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  // Audio component has no visual representation
  html.div([attribute.style("display", "none")], [])
}

// HELPERS ---------------------------------------------------------------------

fn parse_float_attr(to_msg: fn(Float) -> Msg) -> fn(String) -> Result(Msg, Nil) {
  fn(v) {
    case float.parse(v) {
      Ok(f) -> Ok(to_msg(f))
      Error(_) -> Error(Nil)
    }
  }
}

fn parse_bool_attr(to_msg: fn(Bool) -> Msg) -> fn(String) -> Result(Msg, Nil) {
  fn(v) {
    case v {
      "true" | "1" | "" -> Ok(to_msg(True))
      "false" | "0" -> Ok(to_msg(False))
      _ -> Error(Nil)
    }
  }
}
