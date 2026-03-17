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
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/time/duration
import gleam/time/timestamp
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/server_component
import savoiardi

import tiramisu/dev/extension
import tiramisu/dev/runtime
import tiramisu/internal/async_state
import tiramisu/internal/element as html_element
import tiramisu/internal/web_component
import tiramisu/scene

/// Set the renderer canvas height in CSS pixels.
pub const height = attribute.height

/// Set the renderer canvas width in CSS pixels.
pub const width = attribute.width

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

type Model {
  Model(
    runtime: Option(Runtime),
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
    runtime: Runtime,
    effects: effect.Effect(extension.Msg),
  )
  ReconcileFinished(
    generation: Int,
    runtime: Runtime,
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
  RendererTicked(Float)
}

type Runtime {
  Runtime(
    runtime: runtime.Runtime,
    scene_id: String,
    root: html_element.HtmlElement,
    previous_nodes: List(scene.Node),
    background_signature: Option(String),
    extensions: extension.Extensions,
  )
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
) -> fn(fn(Msg) -> Nil, Dynamic, html_element.HtmlElement) -> Nil {
  fn(dispatch, shadow_root, host) {
    let generation = 0
    let #(runtime, effects) =
      initialize_scene_runtime(
        shadow_root,
        host,
        extensions,
        fn() { dispatch(ParentDomMutated) },
        fn(timestamp_ms) { dispatch(RendererTicked(timestamp_ms)) },
      )
    dispatch(Async(InitFinished(generation:, runtime:, effects:)))
    Nil
  }
}

fn initialize_scene_runtime(
  shadow_root: Dynamic,
  host: html_element.HtmlElement,
  extensions: extension.Extensions,
  on_dom_mutated: fn() -> Nil,
  on_tick: fn(Float) -> Nil,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let #(scene_id, root) = scene.find_root(host)
  let renderer_runtime = initialize(shadow_root, host, scene_id, on_tick)

  html_element.observe_mutations(
    host,
    list.append(scene.observed_attributes(), extensions.observed_attributes),
    on_dom_mutated,
  )

  let #(renderer_runtime, extension_effect) =
    scene.apply_scene_attributes(renderer_runtime, root, None)

  let initial_nodes = scene.parse(root, extensions)
  let #(runtime, reconcile_effect) =
    reconcile_nodes(renderer_runtime, [], initial_nodes, scene_id, extensions)

  #(
    Runtime(
      runtime:,
      scene_id:,
      root:,
      previous_nodes: initial_nodes,
      background_signature: None,
      extensions:,
    ),
    effect.batch([extension_effect, reconcile_effect]),
  )
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

    RendererTicked(timestamp_ms) -> tick(model, timestamp_ms)
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
    Some(runtime) ->
      case html_element.find(id) {
        Ok(node) -> #(
          model,
          execute_extension_effects(scene.notify_resolved(
            runtime.extensions,
            runtime.runtime,
            tag,
            id,
            object,
            html_element.attributes(node),
          )),
        )

        Error(Nil) -> #(model, effect.none())
      }

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
          let #(runtime, extension_effect) = reconcile(runtime)
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
  runtime: Runtime,
  extension_effect: effect.Effect(extension.Msg),
) -> #(Model, effect.Effect(Msg)) {
  case model.in_flight_generation {
    Some(in_flight_generation) if in_flight_generation == generation -> {
      let #(next_async_state, ready_actions) =
        async_state.drain_ready(model.async_state, fn(owner) {
          has_owner(runtime, owner)
        })
      let #(runtime, pending_effect) =
        list.fold(ready_actions, #(runtime, effect.none()), fn(acc, actions) {
          let #(runtime, effects) = acc
          let #(next_runtime, next_effect) =
            apply_runtime_actions_to_runtime(runtime, actions)
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
    Some(runtime) -> has_owner(runtime, owner)
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
        apply_runtime_actions_to_runtime(runtime, actions)
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
    Some(runtime) ->
      resize_effect(
        runtime.runtime |> runtime.threejs_renderer,
        width,
        model.height,
      )
    None -> effect.none()
  }
  #(model, effect)
}

fn resize_height(model: Model, height: Int) -> #(Model, effect.Effect(Msg)) {
  let model = Model(..model, height:)
  let effect = case model.runtime {
    Some(runtime) ->
      resize_effect(
        runtime.runtime |> runtime.threejs_renderer,
        model.width,
        height,
      )
    None -> effect.none()
  }
  #(model, effect)
}

fn resize_effect(
  renderer: savoiardi.Renderer,
  width: Int,
  height: Int,
) -> effect.Effect(Msg) {
  effect.from(fn(_) { resize(renderer, width, height) })
}

fn tick(model: Model, timestamp_ms: Float) -> #(Model, effect.Effect(Msg)) {
  let delta_ms = case model.previous_timestamp_ms {
    Some(previous_timestamp_ms) -> timestamp_ms -. previous_timestamp_ms
    None -> 16.67
  }

  let model = Model(..model, previous_timestamp_ms: Some(timestamp_ms))
  let effect = case model.runtime {
    Some(runtime) ->
      effect.batch([
        dispatch_tick(delta_ms |> float.round, timestamp_ms |> float.round),
        effect.from(fn(_) { render_active_camera(runtime) }),
      ])

    None -> effect.none()
  }

  #(model, effect)
}

fn render_active_camera(runtime: Runtime) -> Nil {
  case runtime.runtime |> runtime.active_camera {
    Ok(camera) ->
      savoiardi.render(
        runtime.runtime |> runtime.threejs_renderer,
        runtime.runtime |> runtime.scene,
        camera,
      )

    Error(Nil) -> Nil
  }
}

fn reconcile(rt: Runtime) -> #(Runtime, effect.Effect(extension.Msg)) {
  let new_nodes = scene.parse(rt.root, rt.extensions)
  let #(renderer_runtime, scene_effect) =
    scene.apply_scene_attributes(rt.runtime, rt.root, rt.background_signature)
  let #(runtime, reconcile_effect) =
    reconcile_nodes(
      renderer_runtime,
      rt.previous_nodes,
      new_nodes,
      rt.scene_id,
      rt.extensions,
    )

  #(
    Runtime(
      ..rt,
      runtime:,
      previous_nodes: new_nodes,
      background_signature: Some(scene.current_background_signature(rt.root)),
    ),
    effect.batch([scene_effect, reconcile_effect]),
  )
}

fn reconcile_nodes(
  runtime: runtime.Runtime,
  previous_nodes: List(scene.Node),
  next_nodes: List(scene.Node),
  scene_id: String,
  extensions: extension.Extensions,
) -> #(runtime.Runtime, effect.Effect(extension.Msg)) {
  let #(runtime, extension_effect) =
    scene.apply(
      scene.diff(previous_nodes, next_nodes, scene_id),
      runtime,
      extensions,
    )
  #(runtime, extension_effect)
}

fn apply_runtime_actions_to_runtime(
  rt: Runtime,
  actions: List(extension.RuntimeAction),
) -> #(Runtime, effect.Effect(extension.Msg)) {
  list.fold(actions, #(rt, effect.none()), fn(acc, action) {
    let #(runtime, effects) = acc
    let #(next_runtime, next_effect) = apply_runtime_action(runtime, action)
    #(next_runtime, effect.batch([effects, next_effect]))
  })
}

fn apply_runtime_action(
  rt: Runtime,
  action: extension.RuntimeAction,
) -> #(Runtime, effect.Effect(extension.Msg)) {
  let #(runtime, next_effect) = extension.run_action(action, rt.runtime)
  #(Runtime(..rt, runtime:), next_effect)
}

fn has_owner(rt: Runtime, owner: extension.RequestOwner) -> Bool {
  case owner {
    extension.SceneOwner -> True
    extension.NodeOwner(id) ->
      case runtime.find_entry(rt.runtime, id) {
        Ok(_) -> True
        Error(Nil) -> node_belongs_to_scene(rt, id)
      }
    extension.CustomOwner(_) -> True
  }
}

fn node_belongs_to_scene(rt: Runtime, id: String) -> Bool {
  case html_element.find(id) {
    Ok(node) ->
      case html_element.closest(node, "#" <> rt.scene_id) {
        Ok(_) -> True
        Error(Nil) -> False
      }
    Error(Nil) -> False
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

type Config {
  Config(width: Int, height: Int, antialias: Bool, alpha: Bool)
}

fn config(
  host: html_element.HtmlElement,
  width fallback_width: Int,
  height fallback_height: Int,
) -> Config {
  let width =
    html_element.attribute(host, "width")
    |> result.try(int.parse)
    |> option.from_result()
    |> option.unwrap(fallback_width)
  let height =
    html_element.attribute(host, "height")
    |> result.try(int.parse)
    |> option.from_result()
    |> option.unwrap(fallback_height)

  Config(
    width:,
    height:,
    antialias: case html_element.attribute(host, "antialias") {
      Ok("") -> True
      _ -> False
    },
    alpha: case html_element.attribute(host, "alpha") {
      Ok("") -> True
      _ -> False
    },
  )
}

fn initialize(
  shadow_root: Dynamic,
  host: html_element.HtmlElement,
  scene_id: String,
  on_tick: fn(Float) -> Nil,
) -> runtime.Runtime {
  let config = host |> config(width: 1920, height: 1080)
  let scene = savoiardi.create_scene()
  let renderer = create(config)
  let canvas = savoiardi.get_renderer_dom_element(renderer)

  html_element.append_canvas(shadow_root, canvas)
  savoiardi.set_animation_loop(renderer, on_tick)
  runtime.new(scene, scene_id, renderer)
}

fn resize(renderer: savoiardi.Renderer, width: Int, height: Int) -> Nil {
  savoiardi.set_renderer_size(renderer, width, height)
}

fn create(config: Config) -> savoiardi.Renderer {
  let renderer =
    savoiardi.create_renderer(antialias: config.antialias, alpha: config.alpha)
  savoiardi.set_renderer_size(renderer, config.width, config.height)
  savoiardi.enable_renderer_shadow_map(renderer, True)
  renderer
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
