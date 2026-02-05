//// The tiramisu-audio web component.
////
//// This component creates a global (non-positional) audio source.
//// Audio plays at constant volume regardless of camera/listener position.
//// Use for background music, UI sounds, and ambient audio.
////
//// ## Usage
////
//// ```html
//// <tiramisu-audio
////   id="music"
////   src="sounds/background.mp3"
////   volume="0.5"
////   loop="true"
////   playing="true"
//// ></tiramisu-audio>
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

// TYPES -----------------------------------------------------------------------

/// The model for the audio component.
pub type Model {
  Model(
    /// The audio ID
    id: String,
    /// Scene context from parent renderer
    scene_context: Option(SceneContext),
    /// The audio listener (shared across all audio in the scene)
    listener: Option(savoiardi.AudioListener),
    /// The audio object
    audio: Option(savoiardi.Audio),
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
  )
}

/// Messages for the audio component.
pub type Msg {
  /// Scene ID found via DOM traversal
  SceneIdFound(String)
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
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the audio component.
pub const tag_name = "tiramisu-audio"

/// Register the tiramisu-audio component as a custom element.
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
    ])

  lustre.register(app, tag_name)
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-audio element (global audio).
///
/// Global audio plays at constant volume regardless of camera position.
/// Use for background music, UI sounds, and ambient audio.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/audio
///
/// audio.audio("background_music", [
///   audio.src("sounds/music.mp3"),
///   audio.volume(0.5),
///   audio.loop(True),
///   audio.playing(model.music_playing),
/// ], [])
/// ```
///
pub fn audio(
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
/// Toggle this to play/pause the audio.
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

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      id: "",
      scene_context: None,
      listener: None,
      audio: None,
      buffer: None,
      src: None,
      loading: False,
      volume: 1.0,
      loop: False,
      playing: False,
      playback_rate: 1.0,
    )

  #(model, effect.after_paint(discover_scene_id))
}

fn discover_scene_id(dispatch: fn(Msg) -> Nil, root: dynamic.Dynamic) -> Nil {
  case dom.find_parent_scene_id(root) {
    Some(scene_id) -> dispatch(SceneIdFound(scene_id))
    None ->
      dom.listen_for_scene_ready(root, fn(scene_id) {
        dispatch(SceneIdFound(scene_id))
      })
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    SceneIdFound(scene_id) -> {
      let ctx = SceneContext(scene_id:)
      // Create listener and audio object
      let listener = savoiardi.create_audio_listener()
      let audio_obj = savoiardi.create_audio(listener)

      // Apply initial settings
      savoiardi.set_audio_volume(audio_obj, model.volume)
      savoiardi.set_audio_loop(audio_obj, model.loop)
      savoiardi.set_audio_playback_rate(audio_obj, model.playback_rate)

      let new_model =
        Model(
          ..model,
          scene_context: Some(ctx),
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
      // Start loading if we have context
      case model.scene_context {
        Some(_) -> #(new_model, load_audio_effect(url))
        None -> #(new_model, effect.none())
      }
    }

    BufferLoaded(buffer) -> {
      case model.audio {
        Some(audio_obj) -> {
          savoiardi.set_audio_buffer(audio_obj, buffer)
          // If playing was requested, start now
          case model.playing {
            True -> savoiardi.play_audio(audio_obj)
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
        Some(audio_obj) -> savoiardi.set_audio_volume(audio_obj, vol)
        None -> Nil
      }
      #(Model(..model, volume: vol), effect.none())
    }

    LoopChanged(should_loop) -> {
      case model.audio {
        Some(audio_obj) -> savoiardi.set_audio_loop(audio_obj, should_loop)
        None -> Nil
      }
      #(Model(..model, loop: should_loop), effect.none())
    }

    PlayingChanged(is_playing) -> {
      case model.audio, model.buffer {
        Some(audio_obj), Some(_) -> {
          case is_playing {
            True -> savoiardi.play_audio(audio_obj)
            False -> savoiardi.pause_audio(audio_obj)
          }
        }
        _, _ -> Nil
      }
      #(Model(..model, playing: is_playing), effect.none())
    }

    PlaybackRateChanged(rate) -> {
      case model.audio {
        Some(audio_obj) -> savoiardi.set_audio_playback_rate(audio_obj, rate)
        None -> Nil
      }
      #(Model(..model, playback_rate: rate), effect.none())
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
