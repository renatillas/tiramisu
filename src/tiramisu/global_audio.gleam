import gleam/dict
import gleam/float
import gleam/javascript/promise
import gleam/json
import gleam/option
import gleam/result
import lustre/attribute.{type Attribute}
import savoiardi
import tiramisu/internal/node_utils
import tiramisu/internal/registry

import tiramisu/extension

@internal
pub const tag = "tiramisu-global-audio"

pub fn volume(level: Float) -> Attribute(msg) {
  attribute.attribute("volume", float.to_string(level))
}

pub fn playing(is_playing: Bool) -> Attribute(msg) {
  case is_playing {
    True -> attribute.attribute("playing", "")
    False -> attribute.property("playing", json.bool(False))
  }
}

pub fn playback_rate(rate: Float) -> Attribute(msg) {
  attribute.attribute("playback-rate", float.to_string(rate))
}

pub fn detune(cents: Float) -> Attribute(msg) {
  attribute.attribute("detune", float.to_string(cents))
}

pub fn extension() -> extension.Extension {
  extension.NodeExtension(
    extension.Node(
      tag: "tiramisu-global-audio",
      observed_attributes: [
        "transform", "src", "volume", "loop", "playing", "playback-rate",
        "detune",
      ],
      create: fn(ctx, id, parent_id, attrs, _transform) {
        let #(reg, listener) = node_utils.get_or_create_listener(ctx.registry)
        let audio = savoiardi.create_audio(listener)
        savoiardi.set_audio_volume(
          audio,
          node_utils.get_float(attrs, "volume", 1.0),
        )
        savoiardi.set_audio_loop(
          audio,
          node_utils.get_bool(attrs, "loop", False),
        )
        savoiardi.set_audio_playback_rate(
          audio,
          node_utils.get_float(attrs, "playback-rate", 1.0),
        )
        savoiardi.set_audio_detune(
          audio,
          node_utils.get_float(attrs, "detune", 0.0),
        )
        load_and_play(
          audio,
          dict.get(attrs, "src") |> option.from_result,
          node_utils.get_bool(attrs, "playing", False),
        )
        let reg = registry.register_audio(reg, parent_id, id, audio)
        extension.Context(..ctx, registry: reg)
      },
      update: fn(ctx, id, _old_attrs, new_attrs, _transform) {
        let _ =
          registry.get_audio(ctx.registry, id)
          |> result.map(fn(audio) {
            savoiardi.set_audio_volume(
              audio,
              node_utils.get_float(new_attrs, "volume", 1.0),
            )
            savoiardi.set_audio_loop(
              audio,
              node_utils.get_bool(new_attrs, "loop", False),
            )
            savoiardi.set_audio_playback_rate(
              audio,
              node_utils.get_float(new_attrs, "playback-rate", 1.0),
            )
            savoiardi.set_audio_detune(
              audio,
              node_utils.get_float(new_attrs, "detune", 0.0),
            )
            update_audio_playback(
              audio,
              node_utils.get_bool(new_attrs, "playing", False),
            )
          })
        ctx
      },
      remove: fn(ctx, id) { node_utils.default_remove(ctx, id) },
    ),
  )
}

fn load_and_play(
  audio: savoiardi.Audio,
  src: option.Option(String),
  should_play: Bool,
) -> Nil {
  case src {
    option.None -> Nil
    option.Some(url) -> {
      let _ = {
        use result <- promise.map(savoiardi.load_audio(url))
        use buffer <- result.map(result)
        savoiardi.set_audio_buffer(audio, buffer)
        case should_play {
          True -> {
            savoiardi.resume_audio_context()
            savoiardi.play_audio(audio)
          }
          False -> Nil
        }
      }
      Nil
    }
  }
}

fn update_audio_playback(audio: savoiardi.Audio, playing: Bool) -> Nil {
  case playing, savoiardi.is_audio_playing(audio) {
    True, False -> {
      savoiardi.resume_audio_context()
      savoiardi.play_audio(audio)
    }
    False, True -> savoiardi.pause_audio(audio)
    _, _ -> Nil
  }
}
