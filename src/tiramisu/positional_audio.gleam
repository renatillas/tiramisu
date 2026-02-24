import gleam/dict
import gleam/float
import gleam/javascript/promise
import gleam/json
import gleam/option
import gleam/result
import lustre/attribute.{type Attribute}
import savoiardi
import tiramisu/extension
import tiramisu/internal/node_utils
import tiramisu/internal/registry

@internal
pub const tag = "tiramisu-positional-audio"

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

pub fn ref_distance(distance: Float) -> Attribute(msg) {
  attribute.attribute("ref-distance", float.to_string(distance))
}

pub fn max_distance(distance: Float) -> Attribute(msg) {
  attribute.attribute("max-distance", float.to_string(distance))
}

pub fn rolloff_factor(factor: Float) -> Attribute(msg) {
  attribute.attribute("rolloff-factor", float.to_string(factor))
}

pub fn extension() -> extension.Extension {
  extension.NodeExtension(
    extension.Node(
      tag: "tiramisu-positional-audio",
      observed_attributes: [
        "transform", "src", "volume", "loop", "playing", "playback-rate",
        "detune", "ref-distance", "max-distance", "rolloff-factor",
      ],
      create: fn(ctx, id, parent_id, attrs, transform) {
        let #(reg, listener) = node_utils.get_or_create_listener(ctx.registry)
        let pos_audio = savoiardi.create_positional_audio(listener)
        savoiardi.set_positional_audio_volume(
          pos_audio,
          node_utils.get_float(attrs, "volume", 1.0),
        )
        savoiardi.set_positional_audio_loop(
          pos_audio,
          node_utils.get_bool(attrs, "loop", False),
        )
        savoiardi.set_positional_audio_playback_rate(
          pos_audio,
          node_utils.get_float(attrs, "playback-rate", 1.0),
        )
        savoiardi.set_positional_audio_detune(
          pos_audio,
          node_utils.get_float(attrs, "detune", 0.0),
        )
        savoiardi.set_ref_distance(
          pos_audio,
          node_utils.get_float(attrs, "ref-distance", 1.0),
        )
        savoiardi.set_max_distance(
          pos_audio,
          node_utils.get_float(attrs, "max-distance", 100.0),
        )
        savoiardi.set_rolloff_factor(
          pos_audio,
          node_utils.get_float(attrs, "rolloff-factor", 1.0),
        )
        load_and_play_positional(
          pos_audio,
          dict.get(attrs, "src") |> option.from_result,
          node_utils.get_bool(attrs, "playing", False),
        )
        let reg =
          registry.register_positional_audio(reg, parent_id, id, pos_audio)
        registry.set_transform(reg, id, transform)
        extension.Context(..ctx, registry: reg)
      },
      update: fn(ctx, id, _old_attrs, new_attrs, transform) {
        registry.set_transform(ctx.registry, id, transform)
        let _ =
          registry.get_positional_audio(ctx.registry, id)
          |> result.map(fn(pos_audio) {
            savoiardi.set_positional_audio_volume(
              pos_audio,
              node_utils.get_float(new_attrs, "volume", 1.0),
            )
            savoiardi.set_positional_audio_loop(
              pos_audio,
              node_utils.get_bool(new_attrs, "loop", False),
            )
            savoiardi.set_positional_audio_playback_rate(
              pos_audio,
              node_utils.get_float(new_attrs, "playback-rate", 1.0),
            )
            savoiardi.set_positional_audio_detune(
              pos_audio,
              node_utils.get_float(new_attrs, "detune", 0.0),
            )
            savoiardi.set_ref_distance(
              pos_audio,
              node_utils.get_float(new_attrs, "ref-distance", 1.0),
            )
            savoiardi.set_max_distance(
              pos_audio,
              node_utils.get_float(new_attrs, "max-distance", 100.0),
            )
            savoiardi.set_rolloff_factor(
              pos_audio,
              node_utils.get_float(new_attrs, "rolloff-factor", 1.0),
            )
            update_positional_audio_playback(
              pos_audio,
              node_utils.get_bool(new_attrs, "playing", False),
            )
          })
        ctx
      },
      remove: fn(ctx, id) { node_utils.default_remove(ctx, id) },
    ),
  )
}

fn load_and_play_positional(
  audio: savoiardi.PositionalAudio,
  src: option.Option(String),
  should_play: Bool,
) -> Nil {
  case src {
    option.None -> Nil
    option.Some(url) -> {
      let _ = {
        use result <- promise.map(savoiardi.load_audio(url))
        use buffer <- result.map(result)
        savoiardi.set_positional_audio_buffer(audio, buffer)
        case should_play {
          True -> {
            savoiardi.resume_audio_context()
            savoiardi.play_positional_audio(audio)
          }
          False -> Nil
        }
      }
      Nil
    }
  }
}

fn update_positional_audio_playback(
  audio: savoiardi.PositionalAudio,
  playing: Bool,
) -> Nil {
  case playing, savoiardi.is_positional_audio_playing(audio) {
    True, False -> {
      savoiardi.resume_audio_context()
      savoiardi.play_positional_audio(audio)
    }
    False, True -> savoiardi.pause_positional_audio(audio)
    _, _ -> Nil
  }
}
