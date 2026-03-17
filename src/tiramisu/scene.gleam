//// Structural scene root for Tiramisu.
////
//// A scene lives inside a `tiramisu.renderer` and owns the scene-node subtree
//// that will be parsed and reconciled into Three.js objects.
////
//// Scene roots are structural rather than imperative: application code declares
//// the scene subtree, and Tiramisu reconciles it into the runtime.

import gleam/bool
import gleam/dict
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import lustre/attribute.{type Attribute}
import lustre/effect
import lustre/event
import lustre/server_component
import savoiardi.{type Object3D}
import tiramisu/dev/extension
import tiramisu/dev/runtime
import tiramisu/internal/async_policy
import tiramisu/internal/element

// EVENTS ----------------------------------------------------------------------

/// The custom element tag used for scene roots.
@internal
pub const tag = "tiramisu-scene"

@internal
pub fn observed_attributes() -> List(String) {
  ["background", "background-color-space", "fog"]
}

/// Context provided on each animation frame tick.
///
/// Contains timing and input information useful for smooth animations
/// and responsive game controls.
pub type Tick {
  Tick(
    /// Time elapsed since the last frame (typically ~16ms at 60fps).
    /// Use `duration.to_seconds()` to convert to a Float for animation math.
    delta_time: duration.Duration,
    /// The timestamp when this frame was rendered.
    /// Useful for time-based effects or synchronized animations.
    timestamp: timestamp.Timestamp,
  )
}

/// Set the background color for the scene (as hex int).
pub fn background_color(hex: Int) -> Attribute(msg) {
  attribute.attribute("background", int.to_base16(hex))
}

/// Clear any scene background.
pub fn clear_background() -> Attribute(msg) {
  attribute.attribute("background", "none")
}

/// Set the background to a flat 2D texture.
pub fn background_texture(url: String) -> Attribute(msg) {
  attribute.attribute("background", "texture:" <> url)
}

/// Set the background to an equirectangular panorama.
pub fn background_equirectangular(url: String) -> Attribute(msg) {
  attribute.attribute("background", "equirectangular:" <> url)
}

/// Set the background to a cubemap skybox.
pub fn background_cube(
  positive_x px: String,
  negative_x nx: String,
  positive_y py: String,
  negative_y ny: String,
  positive_z pz: String,
  negative_z nz: String,
) -> Attribute(msg) {
  attribute.attribute(
    "background",
    "cube:" <> string.join([px, nx, py, ny, pz, nz], with: "|"),
  )
}

/// Set the color space used for loaded scene background textures.
pub fn background_color_space_linear() -> Attribute(msg) {
  attribute.attribute("background-color-space", "linear-srgb")
}

/// Set the color space used for loaded scene background textures to sRGB.
pub fn background_color_space_srgb() -> Attribute(msg) {
  attribute.attribute("background-color-space", "srgb")
}

/// Set linear fog on the scene.
pub fn fog(color color: Int, near near: Float, far far: Float) -> Attribute(msg) {
  attribute.attribute(
    "fog",
    "linear:"
      <> int.to_base16(color)
      <> "|"
      <> float.to_string(near)
      <> "|"
      <> float.to_string(far),
  )
}

/// Set exponential fog on the scene.
pub fn fog_exp2(color color: Int, density density: Float) -> Attribute(msg) {
  attribute.attribute(
    "fog",
    "exp2:" <> int.to_base16(color) <> "|" <> float.to_string(density),
  )
}

/// Clear fog from the scene.
pub fn clear_fog() -> Attribute(msg) {
  attribute.attribute("fog", "none")
}

/// Listen for per-frame tick events emitted by the renderer.
///
/// This is the main entry point for animation and simulation work in a scene.
/// The callback receives a [`Tick`](#Tick) containing the frame delta and
/// timestamp.
pub fn on_tick(to_msg: fn(Tick) -> msg) -> Attribute(msg) {
  event.on("tiramisu:tick", {
    use tick_context <- decode.field("detail", tick_decoder())
    decode.success(to_msg(tick_context))
  })
  |> server_component.include(["detail.delta_ms", "detail.timestamp_ms"])
}

/// Attach a per-frame tick handler to the current scene element.
fn tick_decoder() -> decode.Decoder(Tick) {
  use delta_ms <- decode.field("delta_ms", decode.int)
  use timestamp_ms <- decode.field("timestamp_ms", decode.int)

  let seconds = timestamp_ms / 1000
  let milliseconds = timestamp_ms - seconds * 1000
  let nanoseconds = milliseconds * 1_000_000

  Tick(
    delta_time: duration.milliseconds(delta_ms),
    timestamp: timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds),
  )
  |> decode.success
}

@internal
pub fn dispatch_tick(delta_ms: Int, timestamp_ms: Int) -> effect.Effect(msg) {
  event.emit(
    "tiramisu:tick",
    json.object([
      #("delta_ms", json.int(delta_ms)),
      #("timestamp_ms", json.int(timestamp_ms)),
    ]),
  )
}

@internal
pub type Node {
  Node(
    key: String,
    tag: String,
    children: List(Node),
    attributes: dict.Dict(String, String),
  )
}

@internal
pub type Patch {
  CreateNode(
    id: String,
    parent_id: String,
    tag: String,
    attributes: dict.Dict(String, String),
  )
  UpdateNode(
    id: String,
    parent_id: String,
    tag: String,
    attributes: dict.Dict(String, String),
    changed_attributes: extension.AttributeChanges,
  )
  Remove(id: String)
  Reparent(id: String, new_parent_id: String)
}

@internal
pub fn parse(
  root: element.HtmlElement,
  extensions: extension.Extensions,
) -> List(Node) {
  parse_children(root, extensions)
}

@internal
pub fn diff(
  old_nodes: List(Node),
  new_nodes: List(Node),
  scene_id: String,
) -> List(Patch) {
  let old_map = flatten(dict.new(), old_nodes, scene_id)
  let #(patches, remaining) = walk(new_nodes, scene_id, old_map, [])

  dict.fold(remaining, patches, fn(acc, id, _entry) { [Remove(id:), ..acc] })
  |> list.reverse
}

@internal
pub fn apply(
  patches: List(Patch),
  runtime: runtime.Runtime,
  extensions: extension.Extensions,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  use #(runtime, effects), patch <- list.fold(patches, #(runtime, effect.none()))
  let #(runtime, effect) = apply_patch(runtime, patch, extensions)
  #(runtime, effect.batch([effects, effect]))
}

type Background {
  None
  Color(Int)
  Texture(String)
  Equirectangular(String)
  Cube(List(String))
}

type BackgroundColorSpace {
  SRGB
  LinearSRGB
}

type Fog {
  NoFog
  LinearFog(Int, Float, Float)
  Exp2Fog(Int, Float)
}

@internal
pub fn apply_scene_attributes(
  runtime: runtime.Runtime,
  root: element.HtmlElement,
  previous_background_signature: option.Option(String),
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  let background_color_space =
    parse_background_color_space(
      element.attribute(root, "background-color-space")
      |> result.unwrap("srgb"),
    )
  let background_signature = current_background_signature(root)
  let #(runtime, background_effect) =
    apply_background(
      runtime,
      element.attribute(root, "background") |> result.unwrap("000000"),
      background_color_space,
      previous_background_signature,
      background_signature,
    )
  #(
    runtime
      |> apply_fog(element.attribute(root, "fog") |> result.unwrap("none")),
    background_effect,
  )
}

fn apply_background(
  runtime: runtime.Runtime,
  background: String,
  background_color_space: BackgroundColorSpace,
  previous_background_signature: option.Option(String),
  background_signature: String,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  case parse_background(background) {
    None -> {
      let _ = savoiardi.clear_scene_background(runtime.scene(runtime))
      #(runtime, effect.none())
    }

    Color(color) -> {
      let _ =
        savoiardi.set_scene_background_color(runtime.scene(runtime), color)
      #(runtime, effect.none())
    }

    Texture(url) -> #(
      runtime,
      case
        async_policy.should_reload_background(
          previous_signature: previous_background_signature,
          next_signature: background_signature,
        )
      {
        True ->
          extension.request(
            extension.SceneOwner,
            extension.request_key("background"),
            load_background_texture(url, background_color_space),
          )
        False -> effect.none()
      },
    )

    Equirectangular(url) -> #(
      runtime,
      case
        async_policy.should_reload_background(
          previous_signature: previous_background_signature,
          next_signature: background_signature,
        )
      {
        True ->
          extension.request(
            extension.SceneOwner,
            extension.request_key("background"),
            load_equirectangular_background(url, background_color_space),
          )
        False -> effect.none()
      },
    )

    Cube(urls) -> #(
      runtime,
      case
        async_policy.should_reload_background(
          previous_signature: previous_background_signature,
          next_signature: background_signature,
        )
      {
        True ->
          extension.request(
            extension.SceneOwner,
            extension.request_key("background"),
            load_cube_background(urls),
          )
        False -> effect.none()
      },
    )
  }
}

@internal
pub fn current_background_signature(root: element.HtmlElement) -> String {
  let background =
    element.attribute(root, "background") |> result.unwrap("000000")
  let color_space =
    element.attribute(root, "background-color-space")
    |> result.unwrap("srgb")
  background <> "::" <> color_space
}

fn load_background_texture(
  url: String,
  background_color_space: BackgroundColorSpace,
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(savoiardi.load_texture(url))
  case result {
    Ok(texture) -> {
      savoiardi.set_texture_color_space(
        texture,
        to_savoiardi_color_space(background_color_space),
      )
      promise.resolve([extension.set_background_texture(texture)])
    }
    Error(Nil) -> promise.resolve([])
  }
}

fn load_equirectangular_background(
  url: String,
  background_color_space: BackgroundColorSpace,
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(savoiardi.load_equirectangular_texture(url))
  case result {
    Ok(texture) -> {
      savoiardi.set_texture_color_space(
        texture,
        to_savoiardi_color_space(background_color_space),
      )
      promise.resolve([extension.set_background_texture(texture)])
    }
    Error(Nil) -> promise.resolve([])
  }
}

fn load_cube_background(
  urls: List(String),
) -> promise.Promise(List(extension.RuntimeAction)) {
  use result <- promise.await(savoiardi.load_cube_texture(urls))
  case result {
    Ok(texture) ->
      promise.resolve([extension.set_background_cube_texture(texture)])
    Error(Nil) -> promise.resolve([])
  }
}

fn parse_background(background: String) -> Background {
  case background {
    "" | "none" -> None
    _ ->
      case string.starts_with(background, "texture:") {
        True -> Texture(string.drop_start(from: background, up_to: 8))
        False ->
          case string.starts_with(background, "equirectangular:") {
            True ->
              Equirectangular(string.drop_start(from: background, up_to: 16))

            False ->
              case string.starts_with(background, "cube:") {
                True ->
                  parse_cube_background(string.drop_start(
                    from: background,
                    up_to: 5,
                  ))

                False ->
                  case
                    background
                    |> int.base_parse(16)
                  {
                    Ok(color) -> Color(color)
                    Error(Nil) -> None
                  }
              }
          }
      }
  }
}

fn parse_cube_background(encoded: String) -> Background {
  let urls = string.split(encoded, "|")
  case list.length(urls) == 6 {
    True -> Cube(urls)
    False -> None
  }
}

fn parse_background_color_space(encoded: String) -> BackgroundColorSpace {
  case string.lowercase(encoded) {
    "linear-srgb" -> LinearSRGB
    _ -> SRGB
  }
}

fn to_savoiardi_color_space(space: BackgroundColorSpace) -> savoiardi.ColorSpace {
  case space {
    SRGB -> savoiardi.SRGBColorSpace
    LinearSRGB -> savoiardi.LinearSRGBColorSpace
  }
}

fn apply_fog(runtime: runtime.Runtime, fog: String) -> runtime.Runtime {
  case parse_fog(fog) {
    NoFog -> {
      let _ = savoiardi.clear_scene_fog(runtime.scene(runtime))
      runtime
    }

    LinearFog(color, near, far) -> {
      let _ = savoiardi.set_scene_fog(runtime.scene(runtime), color, near, far)
      runtime
    }

    Exp2Fog(color, density) -> {
      let _ =
        savoiardi.set_scene_fog_exp2(runtime.scene(runtime), color, density)
      runtime
    }
  }
}

fn parse_fog(fog: String) -> Fog {
  case fog {
    "" | "none" -> NoFog
    _ -> {
      let parts = string.split(fog, "|")
      case parts {
        ["linear:" <> color, near, far] -> {
          case int.base_parse(color, 16), float.parse(near), float.parse(far) {
            Ok(color), Ok(near), Ok(far) -> LinearFog(color, near, far)
            _, _, _ -> NoFog
          }
        }

        ["exp2:" <> color, density] -> {
          case int.base_parse(color, 16), float.parse(density) {
            Ok(color), Ok(density) -> Exp2Fog(color, density)
            _, _ -> NoFog
          }
        }

        _ -> NoFog
      }
    }
  }
}

@internal
pub fn find_root(host: element.HtmlElement) -> #(String, element.HtmlElement) {
  case find_scene_element(element.children(host)) {
    Ok(root) -> {
      let scene_id = element.attribute(root, "id") |> result.unwrap("")
      #(scene_id, root)
    }

    Error(Nil) -> #("", host)
  }
}

fn find_scene_element(
  children: List(element.HtmlElement),
) -> Result(element.HtmlElement, Nil) {
  list.fold(children, Error(Nil), fn(found, child) {
    case found {
      Ok(_) -> found

      Error(Nil) ->
        case string.lowercase(element.tag(child)) == tag {
          True -> Ok(child)
          False -> Error(Nil)
        }
    }
  })
}

fn parse_element(
  item: element.HtmlElement,
  extensions: extension.Extensions,
) -> Node {
  let tag = element.tag(item) |> string.lowercase
  let attributes = element.attributes(item)
  let key = dict.get(attributes, "id") |> result.unwrap("")
  let node_attributes = dict.delete(attributes, "id")

  case extension.get_node(extensions, tag) {
    Ok(_) ->
      Node(
        key:,
        tag:,
        children: parse_children(item, extensions),
        attributes: node_attributes,
      )

    Error(Nil) ->
      Node(
        key:,
        tag:,
        children: parse_children(item, extensions),
        attributes: dict.new(),
      )
  }
}

fn parse_children(
  root: element.HtmlElement,
  extensions: extension.Extensions,
) -> List(Node) {
  use child <- list.map(element.children(root))
  parse_element(child, extensions)
}

fn flatten(
  map: dict.Dict(String, #(Node, String)),
  nodes: List(Node),
  parent_id: String,
) -> dict.Dict(String, #(Node, String)) {
  list.fold(nodes, map, fn(acc, node) {
    case node.key {
      "" -> flatten(acc, node.children, parent_id)
      id -> {
        let acc = dict.insert(acc, id, #(node, parent_id))
        flatten(acc, node.children, id)
      }
    }
  })
}

fn walk(
  nodes: List(Node),
  parent_id: String,
  old_map: dict.Dict(String, #(Node, String)),
  patches: List(Patch),
) -> #(List(Patch), dict.Dict(String, #(Node, String))) {
  list.fold(nodes, #(patches, old_map), fn(acc, node) {
    let #(patches, old_map) = acc
    case node.key {
      "" -> walk(node.children, parent_id, old_map, patches)
      id -> {
        case dict.get(old_map, id) {
          Ok(#(old_node, old_parent_id)) -> {
            let old_map = dict.delete(old_map, id)
            let patches = case old_parent_id == parent_id {
              True -> patches
              False -> [Reparent(id:, new_parent_id: parent_id), ..patches]
            }
            let patches = diff_node(old_node, node, id, parent_id, patches)
            walk(node.children, id, old_map, patches)
          }

          Error(Nil) -> {
            let patches = [
              CreateNode(
                id:,
                parent_id:,
                tag: node.tag,
                attributes: node.attributes,
              ),
              ..patches
            ]
            walk(node.children, id, old_map, patches)
          }
        }
      }
    }
  })
}

fn diff_node(
  old: Node,
  new: Node,
  id: String,
  parent_id: String,
  patches: List(Patch),
) -> List(Patch) {
  use <- bool.guard(old.tag != new.tag, [Remove(id:), ..patches])
  use <- bool.guard(old.attributes == new.attributes, patches)

  [
    UpdateNode(
      id:,
      parent_id:,
      tag: new.tag,
      attributes: new.attributes,
      changed_attributes: difference_of_attributes(
        old.attributes,
        new.attributes,
      ),
    ),
    ..patches
  ]
}

fn difference_of_attributes(
  old_attributes: dict.Dict(String, String),
  new_attributes: dict.Dict(String, String),
) -> extension.AttributeChanges {
  let changes =
    dict.fold(old_attributes, dict.new(), fn(acc, key, old_value) {
      case dict.get(new_attributes, key) {
        Ok(new_value) if new_value == old_value -> acc
        Ok(new_value) -> dict.insert(acc, key, extension.Updated(new_value))
        Error(Nil) -> dict.insert(acc, key, extension.Removed)
      }
    })

  dict.fold(new_attributes, changes, fn(acc, key, new_value) {
    case dict.get(old_attributes, key) {
      Ok(_) -> acc
      Error(Nil) -> dict.insert(acc, key, extension.Added(new_value))
    }
  })
}

fn apply_patch(
  runtime: runtime.Runtime,
  patch: Patch,
  extensions: extension.Extensions,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  case patch {
    CreateNode(id:, parent_id:, tag:, attributes:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let #(new_runtime, node_effect) =
            handler.create(runtime, id, parent_id, attributes)
          let object = runtime.object(new_runtime, id) |> option.from_result
          let attribute_effect =
            notify_create(extensions, new_runtime, tag, id, object, attributes)
          #(new_runtime, effect.batch([node_effect, attribute_effect]))
        }

        Error(Nil) -> #(runtime, effect.none())
      }
    }

    UpdateNode(id:, parent_id:, tag:, attributes:, changed_attributes:) -> {
      case extension.get_node(extensions, tag) {
        Ok(handler) -> {
          let object = runtime.object(runtime, id) |> option.from_result
          let #(new_runtime, node_effect) =
            handler.update(
              runtime,
              id,
              parent_id,
              object,
              attributes,
              changed_attributes,
            )
          let attribute_effect =
            notify_update(
              extensions,
              new_runtime,
              tag,
              id,
              object,
              attributes,
              changed_attributes,
            )
          #(new_runtime, effect.batch([node_effect, attribute_effect]))
        }

        Error(Nil) -> #(runtime, effect.none())
      }
    }

    Remove(id:) -> {
      case dict.get(runtime.entries(runtime), id) {
        Ok(runtime.ObjectEntry(parent_id:, tag:, object:)) -> {
          let attribute_effect =
            notify_remove(extensions, runtime, id, parent_id, object)
          case extension.get_node(extensions, tag) {
            Ok(handler) -> {
              let #(runtime, node_effect) =
                handler.remove(runtime, id, parent_id, object)
              #(runtime, effect.batch([attribute_effect, node_effect]))
            }
            Error(Nil) -> #(runtime, attribute_effect)
          }
        }

        Error(Nil) -> #(runtime, effect.none())
      }
    }

    Reparent(id:, new_parent_id:) -> {
      let next_runtime = runtime.reparent_object(runtime, id, new_parent_id)
      #(next_runtime, effect.none())
    }
  }
}

fn notify_create(
  extensions: extension.Extensions,
  runtime,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
) -> effect.Effect(extension.Msg) {
  list.fold(extension.attribute_hooks(extensions), effect.none(), fn(acc, hook) {
    effect.batch([acc, hook.on_create(runtime, tag, id, object, attributes)])
  })
}

fn notify_update(
  extensions: extension.Extensions,
  runtime,
  tag: String,
  id: String,
  object: option.Option(Object3D),
  attributes: dict.Dict(String, String),
  changed_attributes: extension.AttributeChanges,
) -> effect.Effect(extension.Msg) {
  list.fold(extension.attribute_hooks(extensions), effect.none(), fn(acc, hook) {
    effect.batch([
      acc,
      hook.on_update(runtime, tag, id, object, attributes, changed_attributes),
    ])
  })
}

fn notify_remove(
  extensions: extension.Extensions,
  runtime: runtime.Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
) -> effect.Effect(extension.Msg) {
  list.fold(extension.attribute_hooks(extensions), effect.none(), fn(acc, hook) {
    effect.batch([acc, hook.on_remove(runtime, id, parent_id, object)])
  })
}

@internal
pub fn notify_resolved(
  extensions: extension.Extensions,
  runtime: runtime.Runtime,
  tag: String,
  id: String,
  object: Object3D,
  attributes: dict.Dict(String, String),
) -> effect.Effect(extension.Msg) {
  list.fold(extension.attribute_hooks(extensions), effect.none(), fn(acc, hook) {
    effect.batch([acc, hook.on_resolved(runtime, tag, id, object, attributes)])
  })
}
