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

import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/json
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
import tiramisu/dev/runtime.{type Runtime}
import tiramisu/internal/element as dom_element
import tiramisu/internal/renderer as renderer_runtime
import tiramisu/internal/scene
import tiramisu/internal/web_component

pub const height = attribute.height

pub const width = attribute.width

/// The model for the renderer component.
type Model {
  Model(
    runtime: Option(scene.Runtime),
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
type Msg {
  /// Scene and renderer have been initialized
  Initialized(scene.Runtime)
  RuntimeReconciled(scene.Runtime)
  /// Light DOM children changed — re-parse, diff, and apply
  ParentDomMutated
  /// Async runtime mutation (model loading, etc.) — always applied to latest runtime
  RuntimePatched(fn(Runtime) -> Runtime)
  WidthChanged(Int)
  HeightChanged(Int)
  AntialiasSet(Bool)
  AntialiasToggled
  AlphaSet(Bool)
  AlphaToggled
  Tick(Float, Int)
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
    scene.initialize(
      shadow_root,
      host,
      extensions,
      fn() { dispatch(ParentDomMutated) },
      on_async(_, dispatch),
      fn(delta_ms, timestamp_ms) { dispatch(Tick(delta_ms, timestamp_ms)) },
    )
    |> Initialized
    |> dispatch
  }
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Initialized(runtime) -> #(
      Model(..model, runtime: Some(runtime)),
      effect.none(),
    )

    ParentDomMutated -> reconcile_runtime(model)

    RuntimeReconciled(runtime) -> #(
      Model(..model, runtime: Some(runtime)),
      effect.none(),
    )

    RuntimePatched(transform) -> apply_runtime_patch(model, transform)

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

    Tick(delta_ms, timestamp_ms) -> tick(model, delta_ms, timestamp_ms)
  }
}

fn reconcile_runtime(model: Model) -> #(Model, effect.Effect(Msg)) {
  case model.runtime {
    Some(runtime) -> #(
      model,
      effect.from(fn(dispatch) {
        let runtime = scene.reconcile(runtime, on_async(_, dispatch))
        dispatch(RuntimeReconciled(runtime))
      }),
    )

    None -> #(model, effect.none())
  }
}

fn apply_runtime_patch(
  model: Model,
  transform: fn(Runtime) -> Runtime,
) -> #(Model, effect.Effect(Msg)) {
  case model.runtime {
    Some(runtime) -> {
      let runtime = scene.apply_runtime_transform(runtime, transform)
      #(Model(..model, runtime: Some(runtime)), effect.none())
    }

    None -> #(model, effect.none())
  }
}

fn resize_width(model: Model, width: Int) -> #(Model, effect.Effect(Msg)) {
  let next_model = Model(..model, width:)
  case model.runtime {
    Some(runtime) -> #(
      next_model,
      resize_effect(scene.renderer(runtime), width, model.height),
    )

    None -> #(next_model, effect.none())
  }
}

fn resize_height(model: Model, height: Int) -> #(Model, effect.Effect(Msg)) {
  let next_model = Model(..model, height:)
  case model.runtime {
    Some(runtime) -> #(
      next_model,
      resize_effect(scene.renderer(runtime), model.width, height),
    )

    None -> #(next_model, effect.none())
  }
}

fn resize_effect(
  renderer: savoiardi.Renderer,
  width: Int,
  height: Int,
) -> effect.Effect(Msg) {
  effect.from(fn(_) { renderer_runtime.resize(renderer, width, height) })
}

fn tick(
  model: Model,
  delta_ms: Float,
  timestamp_ms: Int,
) -> #(Model, effect.Effect(Msg)) {
  case model.runtime {
    Some(runtime) -> #(
      model,
      effect.from(fn(_) {
        let delta_ms = float.round(delta_ms) |> int.to_float
        scene.dispatch_tick(runtime, delta_ms, timestamp_ms)
        render_active_camera(runtime)
      }),
    )

    None -> #(model, effect.none())
  }
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
  transform: promise.Promise(fn(Runtime) -> Runtime),
  dispatch: fn(Msg) -> Nil,
) -> Nil {
  promise.map(transform, fn(transform) { dispatch(RuntimePatched(transform)) })
  Nil
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [])
}
