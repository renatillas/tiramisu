//// Renderer host component for Tiramisu.
////
//// A renderer owns the WebGL renderer, canvas, frame loop, and one nested scene.
//// This mirrors the public API shape:
////
//// ```gleam
//// tiramisu.renderer("renderer", [], [
////   tiramisu.scene("scene", [], [...]),
//// ])
//// ```

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import savoiardi

import tiramisu/dev/extension
import tiramisu/internal/element as dom_element
import tiramisu/internal/renderer as renderer_runtime
import tiramisu/internal/scene
import tiramisu/internal/web_component

pub const height = attribute.height

pub const width = attribute.width

/// The model for the renderer component.
type PendingAsync {
  PendingAsync(
    scope: extension.AsyncScope,
    key: extension.AsyncKey,
    version: Int,
    effect: extension.AsyncEffect,
  )
}

type Model {
  Model(
    runtime: Option(scene.Runtime),
    async_versions: dict.Dict(String, Int),
    next_async_version: Int,
    pending_async: List(PendingAsync),
    runtime_generation: Int,
    in_flight_generation: Option(Int),
    reconcile_queued: Bool,
    previous_timestamp_ms: Option(Float),
    /// Canvas width
    width: Int,
    /// Canvas height
    height: Int,
    /// Antialiasing enabled
    antialias: Bool,
    /// Transparent background
    alpha: Bool,
    /// Compiled extensions (built-ins + user-provided)
    extensions: extension.Extensions,
  )
}

/// Messages for the renderer component.
type AsyncMsg {
  InitFinished(Int, scene.Runtime)
  ReconcileFinished(Int, scene.Runtime)
  RequestSpawned(
    extension.AsyncScope,
    extension.AsyncKey,
    promise.Promise(extension.AsyncEffect),
  )
  RequestResolved(
    extension.AsyncScope,
    extension.AsyncKey,
    Int,
    extension.AsyncEffect,
  )
}

type Msg {
  /// Light DOM children changed — re-parse, diff, and apply
  ParentDomMutated
  Async(AsyncMsg)
  WidthChanged(Int)
  HeightChanged(Int)
  AntialiasSet(Bool)
  AntialiasToggled
  AlphaSet(Bool)
  AlphaToggled
  Tick(Float)
}

// COMPONENT -------------------------------------------------------------------

/// The tag name for the renderer host component.
@internal
pub const tag = "tiramisu-renderer"

/// Register the renderer web component with Lustre.
///
/// Most applications should call `tiramisu.register/1` instead of calling this
/// function directly.
@internal
pub fn register(
  extensions: List(extension.Extension),
) -> Result(Nil, lustre.Error) {
  let extensions = extension.from_list(extensions)

  let app =
    lustre.component(init: init(extensions, _), update:, view:, options: [
      component.on_attribute_change("width", fn(v) {
        v |> int.parse |> result.map(WidthChanged)
      }),
      component.on_attribute_change("height", fn(v) {
        v |> int.parse |> result.map(HeightChanged)
      }),
      component.on_attribute_change("antialias", fn(value) {
        case value {
          "" -> Ok(AntialiasToggled)
          _ -> Ok(AntialiasSet(True))
        }
      }),
      component.on_attribute_change("alpha", fn(value) {
        case value {
          "" -> Ok(AlphaToggled)
          _ -> Ok(AlphaSet(True))
        }
      }),
    ])

  lustre.register(app, tag)
}

// ATTRIBUTES ------------------------------------------------------------------

/// Enable or disable antialiasing on the renderer.
pub fn antialias(enabled: Bool) -> Attribute(msg) {
  case enabled {
    True -> attribute.attribute("antialias", "")
    False -> attribute.property("antialias", json.bool(False))
  }
}

/// Enable or disable a transparent renderer background.
pub fn alpha(enabled: Bool) -> Attribute(msg) {
  case enabled {
    True -> attribute.attribute("alpha", "")
    False -> attribute.property("alpha", json.bool(False))
  }
}

// INIT ------------------------------------------------------------------------

fn init(exts: extension.Extensions, _flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(
      runtime: None,
      async_versions: dict.new(),
      next_async_version: 0,
      pending_async: [],
      runtime_generation: 0,
      in_flight_generation: Some(0),
      reconcile_queued: False,
      previous_timestamp_ms: None,
      width: 1920,
      height: 1080,
      antialias: True,
      alpha: False,
      extensions: exts,
    ),
    web_component.before_paint(do_init(exts)),
  )
}

fn do_init(
  extensions: extension.Extensions,
) -> fn(fn(Msg) -> Nil, Dynamic, dom_element.HtmlElement) -> Nil {
  fn(dispatch, shadow_root, host) {
    let generation = 0
    promise.map(
      scene.initialize(
        shadow_root,
        host,
        extensions,
        fn() { dispatch(ParentDomMutated) },
        fn(scope, key, async_effect) {
          on_async(scope, key, async_effect, dispatch)
        },
        fn(timestamp_ms) { dispatch(Tick(timestamp_ms)) },
      ),
      fn(runtime) { dispatch(Async(InitFinished(generation, runtime))) },
    )
    Nil
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    ParentDomMutated -> queue_reconcile(model)

    Async(async_msg) -> update_async(model, async_msg)

    WidthChanged(width) -> resize_width(model, width)

    HeightChanged(height) -> resize_height(model, height)

    AntialiasSet(aa) -> {
      #(Model(..model, antialias: aa), effect.none())
    }
    AntialiasToggled -> {
      #(Model(..model, antialias: !model.antialias), effect.none())
    }

    AlphaSet(a) -> {
      #(Model(..model, alpha: a), effect.none())
    }
    AlphaToggled -> {
      #(Model(..model, alpha: !model.alpha), effect.none())
    }

    Tick(timestamp_ms) -> tick(model, timestamp_ms)
  }
}

fn update_async(model: Model, msg: AsyncMsg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    InitFinished(generation, runtime) ->
      finalize_runtime(model, generation, runtime)
    ReconcileFinished(generation, runtime) ->
      finalize_runtime(model, generation, runtime)
    RequestSpawned(scope, key, task) ->
      register_async_request(model, scope, key, task)
    RequestResolved(scope, key, version, async_effect) ->
      apply_async_effect(model, scope, key, version, async_effect)
  }
}

fn register_async_request(
  model: Model,
  scope: extension.AsyncScope,
  key: extension.AsyncKey,
  task: promise.Promise(extension.AsyncEffect),
) -> #(Model, effect.Effect(Msg)) {
  let version = model.next_async_version + 1
  let registry_key = async_registry_key(scope, key)
  let model =
    Model(
      ..model,
      async_versions: dict.insert(model.async_versions, registry_key, version),
      next_async_version: version,
    )

  #(
    model,
    effect.from(fn(dispatch) {
      promise.map(task, fn(async_effect) {
        dispatch(Async(RequestResolved(scope, key, version, async_effect)))
      })
      Nil
    }),
  )
}

fn queue_reconcile(model: Model) -> #(Model, effect.Effect(Msg)) {
  case model.in_flight_generation {
    Some(_) -> #(Model(..model, reconcile_queued: True), effect.none())
    None ->
      case model.runtime {
        Some(_) -> reconcile_runtime(model)
        None -> #(Model(..model, reconcile_queued: True), effect.none())
      }
  }
}

fn reconcile_runtime(model: Model) -> #(Model, effect.Effect(Msg)) {
  case model.runtime {
    Some(runtime) -> {
      let generation = model.runtime_generation + 1
      #(
        Model(
          ..model,
          runtime_generation: generation,
          in_flight_generation: Some(generation),
          pending_async: [],
          reconcile_queued: False,
        ),
        effect.from(fn(dispatch) {
          promise.map(
            scene.reconcile(runtime, fn(scope, key, async_effect) {
              on_async(scope, key, async_effect, dispatch)
            }),
            fn(runtime) {
              dispatch(Async(ReconcileFinished(generation, runtime)))
            },
          )
          Nil
        }),
      )
    }

    None -> #(model, effect.none())
  }
}

fn finalize_runtime(
  model: Model,
  generation: Int,
  runtime: scene.Runtime,
) -> #(Model, effect.Effect(Msg)) {
  case model.in_flight_generation {
    Some(in_flight_generation) if in_flight_generation == generation -> {
      let runtime =
        list.fold(model.pending_async, runtime, fn(runtime, pending) {
          apply_pending_async(model, runtime, pending)
        })
      let model =
        Model(
          ..model,
          runtime: Some(runtime),
          pending_async: [],
          in_flight_generation: None,
        )
      case model.reconcile_queued {
        True -> reconcile_runtime(model)
        False -> #(model, effect.none())
      }
    }

    _ -> #(model, effect.none())
  }
}

fn apply_async_effect(
  model: Model,
  scope: extension.AsyncScope,
  key: extension.AsyncKey,
  version: Int,
  async_effect: extension.AsyncEffect,
) -> #(Model, effect.Effect(Msg)) {
  let is_current = case
    dict.get(model.async_versions, async_registry_key(scope, key))
  {
    Ok(current_version) -> current_version == version
    Error(Nil) -> False
  }
  case is_current {
    False -> #(model, effect.none())
    True ->
      case model.in_flight_generation {
        Some(_) -> #(
          Model(
            ..model,
            pending_async: list.append(model.pending_async, [
              PendingAsync(scope:, key:, version:, effect: async_effect),
            ]),
          ),
          effect.none(),
        )

        None ->
          case model.runtime {
            Some(runtime) -> {
              let runtime = case scene.has_scope(runtime, scope) {
                True -> scene.apply_async_effect(runtime, async_effect)
                False -> runtime
              }
              #(Model(..model, runtime: Some(runtime)), effect.none())
            }

            None -> #(
              Model(
                ..model,
                pending_async: list.append(model.pending_async, [
                  PendingAsync(scope:, key:, version:, effect: async_effect),
                ]),
              ),
              effect.none(),
            )
          }
      }
  }
}

fn async_registry_key(
  scope: extension.AsyncScope,
  key: extension.AsyncKey,
) -> String {
  scope_to_string(scope) <> "::" <> key_to_string(key)
}

fn scope_to_string(scope: extension.AsyncScope) -> String {
  case scope {
    extension.SceneScope -> "scene"
    extension.NodeScope(id) -> "node:" <> id
    extension.CustomScope(value) -> "custom:" <> value
  }
}

fn key_to_string(key: extension.AsyncKey) -> String {
  extension.async_key_to_string(key)
}

fn apply_pending_async(
  model: Model,
  runtime: scene.Runtime,
  pending: PendingAsync,
) -> scene.Runtime {
  let PendingAsync(scope:, key:, version:, effect:) = pending
  let is_current = case
    dict.get(model.async_versions, async_registry_key(scope, key))
  {
    Ok(current_version) -> current_version == version
    Error(Nil) -> False
  }

  case is_current, scene.has_scope(runtime, scope) {
    True, True -> scene.apply_async_effect(runtime, effect)
    _, _ -> runtime
  }
}

fn resize_width(model: Model, width: Int) -> #(Model, effect.Effect(Msg)) {
  let model = Model(..model, width:)
  let effect = case model.runtime {
    Some(runtime) -> resize_effect(scene.renderer(runtime), width, model.height)
    None -> effect.none()
  }
  #(model, effect)
}

fn resize_height(model: Model, height: Int) -> #(Model, effect.Effect(Msg)) {
  let model = Model(..model, height:)
  let effect = case model.runtime {
    Some(runtime) -> resize_effect(scene.renderer(runtime), model.width, height)
    None -> effect.none()
  }
  #(model, effect)
}

fn resize_effect(
  renderer: savoiardi.Renderer,
  width: Int,
  height: Int,
) -> effect.Effect(Msg) {
  effect.from(fn(_) { renderer_runtime.resize(renderer, width, height) })
}

fn tick(model: Model, timestamp_ms: Float) -> #(Model, effect.Effect(Msg)) {
  let delta_ms = case model.previous_timestamp_ms {
    Some(previous_timestamp_ms) -> timestamp_ms -. previous_timestamp_ms
    None -> 16.67
  }

  let model = Model(..model, previous_timestamp_ms: Some(timestamp_ms))
  let effect = case model.runtime {
    Some(runtime) ->
      effect.from(fn(_) {
        let delta_ms = float.round(delta_ms) |> int.to_float
        scene.dispatch_tick(runtime, delta_ms, timestamp_ms |> float.round)
        render_active_camera(runtime)
      })

    None -> effect.none()
  }

  #(model, effect)
}

fn render_active_camera(runtime: scene.Runtime) -> Nil {
  case scene.active_camera(runtime) {
    Ok(camera) ->
      savoiardi.render(
        scene.renderer(runtime),
        scene.root_scene(runtime),
        camera,
      )

    Error(Nil) -> Nil
  }
}

fn on_async(
  scope: extension.AsyncScope,
  key: extension.AsyncKey,
  async_effect: promise.Promise(extension.AsyncEffect),
  dispatch: fn(Msg) -> Nil,
) -> promise.Promise(Nil) {
  dispatch(Async(RequestSpawned(scope, key, async_effect)))
  promise.resolve(Nil)
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [])
}
