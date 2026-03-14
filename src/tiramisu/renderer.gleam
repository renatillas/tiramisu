//// Renderer host component and renderer-level attributes.
////
//// A renderer is the bridge between Lustre's declarative view tree and the
//// Three.js runtime managed by Tiramisu. Each renderer owns one canvas, one
//// render loop, and one nested `tiramisu.scene`.
////
//// Most applications use this module indirectly through `tiramisu.renderer`,
//// while using the attribute helpers here to configure the renderer itself.
////
//// ```gleam
//// tiramisu.renderer(
////   "main-renderer",
////   [
////     renderer.width(1280),
////     renderer.height(720),
////     renderer.antialias(True),
////   ],
////   [tiramisu.scene("scene", [], [])],
//// )
//// ```

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
import tiramisu/internal/async_state
import tiramisu/internal/element as dom_element
import tiramisu/internal/renderer as renderer_runtime
import tiramisu/internal/scene
import tiramisu/internal/web_component

/// Set the renderer canvas height in CSS pixels.
pub const height = attribute.height

/// Set the renderer canvas width in CSS pixels.
pub const width = attribute.width

type Model {
  Model(
    runtime: Option(scene.Runtime),
    async_state: async_state.State,
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
  InitFinished(
    generation: Int,
    runtime: scene.Runtime,
    effects: effect.Effect(extension.Msg),
  )
  ReconcileFinished(
    generation: Int,
    runtime: scene.Runtime,
    effects: effect.Effect(extension.Msg),
  )
  Extension(extension.Msg)
  RequestResolved(
    extension.RequestOwner,
    extension.RequestKey,
    Int,
    List(extension.RuntimeAction),
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
      async_state: async_state.new(),
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
    let #(runtime, effects) =
      scene.initialize(
        shadow_root,
        host,
        extensions,
        fn() { dispatch(ParentDomMutated) },
        fn(timestamp_ms) { dispatch(Tick(timestamp_ms)) },
      )
    dispatch(Async(InitFinished(generation:, runtime:, effects:)))
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
    InitFinished(generation, runtime, extension_effect) ->
      finalize_runtime(model, generation, runtime, extension_effect)
    ReconcileFinished(generation, runtime, extension_effect) ->
      finalize_runtime(model, generation, runtime, extension_effect)
    Extension(extension.NotifyResolved(tag, id, object)) ->
      notify_resolved(model, tag, id, object)
    Extension(extension.Spawn(owner:, key:, task:)) ->
      register_async_request(model, owner, key, task)
    RequestResolved(owner, key, version, actions) ->
      apply_async_effect(model, owner, key, version, actions)
  }
}

fn notify_resolved(
  model: Model,
  tag: String,
  id: String,
  object: savoiardi.Object3D,
) -> #(Model, effect.Effect(Msg)) {
  case model.runtime {
    Some(runtime) -> #(
      model,
      execute_extension_effects(scene.notify_resolved_for_runtime(
        runtime,
        tag,
        id,
        object,
      )),
    )

    None -> #(model, effect.none())
  }
}

fn register_async_request(
  model: Model,
  owner: extension.RequestOwner,
  key: extension.RequestKey,
  task: promise.Promise(List(extension.RuntimeAction)),
) -> #(Model, effect.Effect(Msg)) {
  let #(async_state, version) =
    async_state.register(model.async_state, owner, key)
  let model = Model(..model, async_state:)

  #(
    model,
    effect.from(fn(dispatch) {
      promise.map(task, fn(actions) {
        dispatch(Async(RequestResolved(owner, key, version, actions)))
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
          reconcile_queued: False,
        ),
        effect.from(fn(dispatch) {
          let #(runtime, extension_effect) = scene.reconcile(runtime)
          dispatch(
            Async(ReconcileFinished(generation, runtime, extension_effect)),
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
  extension_effect: effect.Effect(extension.Msg),
) -> #(Model, effect.Effect(Msg)) {
  case model.in_flight_generation {
    Some(in_flight_generation) if in_flight_generation == generation -> {
      let #(next_async_state, ready_actions) =
        async_state.drain_ready(model.async_state, fn(owner) {
          scene.has_owner(runtime, owner)
        })
      let #(runtime, pending_effect) =
        list.fold(ready_actions, #(runtime, effect.none()), fn(acc, actions) {
          let #(runtime, effects) = acc
          let #(next_runtime, next_effect) =
            scene.apply_runtime_actions(runtime, actions)
          #(next_runtime, effect.batch([effects, next_effect]))
        })
      let model =
        Model(
          ..model,
          runtime: Some(runtime),
          async_state: next_async_state,
          in_flight_generation: None,
        )
      let runtime_effect =
        execute_extension_effects(
          effect.batch([extension_effect, pending_effect]),
        )
      case model.reconcile_queued {
        True -> {
          let #(model, reconcile_effect) = reconcile_runtime(model)
          #(model, effect.batch([runtime_effect, reconcile_effect]))
        }
        False -> #(model, runtime_effect)
      }
    }

    _ -> #(model, effect.none())
  }
}

fn apply_async_effect(
  model: Model,
  owner: extension.RequestOwner,
  key: extension.RequestKey,
  version: Int,
  actions: List(extension.RuntimeAction),
) -> #(Model, effect.Effect(Msg)) {
  let owner_exists = does_owner_exist(model, owner)
  let #(next_async_state, decision) =
    async_state.resolve(
      model.async_state,
      owner,
      key,
      version,
      actions,
      model.in_flight_generation != None,
      model.runtime != None,
      owner_exists,
    )

  case decision {
    async_state.Drop -> #(
      Model(..model, async_state: next_async_state),
      effect.none(),
    )
    async_state.Queue -> #(
      Model(..model, async_state: next_async_state),
      effect.none(),
    )
    async_state.ApplyNow(actions) ->
      apply_runtime_actions(model, next_async_state, actions)
  }
}

fn does_owner_exist(model: Model, owner: extension.RequestOwner) -> Bool {
  case model.runtime {
    Some(runtime) -> scene.has_owner(runtime, owner)
    None -> True
  }
}

fn apply_runtime_actions(
  model: Model,
  next_async_state: async_state.State,
  actions: List(extension.RuntimeAction),
) -> #(Model, effect.Effect(Msg)) {
  case model.runtime {
    Some(runtime) -> {
      let #(next_runtime, next_effect) =
        scene.apply_runtime_actions(runtime, actions)
      #(
        Model(
          ..model,
          async_state: next_async_state,
          runtime: Some(next_runtime),
        ),
        execute_extension_effects(next_effect),
      )
    }

    None -> #(Model(..model, async_state: next_async_state), effect.none())
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

fn execute_extension_effects(
  extension_effect: effect.Effect(extension.Msg),
) -> effect.Effect(Msg) {
  extension_effect
  |> effect.map(fn(msg) { Async(Extension(msg)) })
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [])
}
