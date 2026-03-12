import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/javascript/array
import gleam/json
import gleam/option
import gleam/result
import savoiardi

pub type HtmlElement

pub type RendererConfig {
  RendererConfig(
    width: option.Option(Int),
    height: option.Option(Int),
    background: String,
    antialias: Bool,
    alpha: Bool,
  )
}

@external(javascript, "./element.ffi.mjs", "shadowRootHost")
fn shadow_root_host(shadow_root: Dynamic) -> HtmlElement

@external(javascript, "./element.ffi.mjs", "getAttribute")
fn get_attribute(element: HtmlElement, name: String) -> Result(String, Nil)

@external(javascript, "./element.ffi.mjs", "setAttribute")
fn set_dom_attribute(element: HtmlElement, name: String, value: String) -> Nil

@external(javascript, "./element.ffi.mjs", "closest")
fn closest_element(
  element: HtmlElement,
  selector: String,
) -> Result(HtmlElement, Nil)

@external(javascript, "./element.ffi.mjs", "tagName")
fn tag_name(element: HtmlElement) -> String

@external(javascript, "./element.ffi.mjs", "children")
fn child_elements(element: HtmlElement) -> List(HtmlElement)

@external(javascript, "./element.ffi.mjs", "parentElement")
fn parent_element(element: HtmlElement) -> Result(HtmlElement, Nil)

@external(javascript, "./element.ffi.mjs", "dispatchCustomEvent")
fn dispatch_custom_event(
  element: HtmlElement,
  event_name: String,
  detail: a,
) -> Nil

@external(javascript, "./element.ffi.mjs", "setProperty")
fn set_property(element: HtmlElement, name: String, value: a) -> Nil

@external(javascript, "./element.ffi.mjs", "deleteProperty")
fn delete_property(element: HtmlElement, name: String) -> Nil

@external(javascript, "./element.ffi.mjs", "getElementById")
fn get_element_by_id(id: String) -> Result(HtmlElement, Nil)

@external(javascript, "./element.ffi.mjs", "setupMutationObserver")
fn setup_mutation_observer(
  host: HtmlElement,
  observed_attrs: List(String),
  callback: fn() -> Nil,
) -> Nil

@external(javascript, "./element.ffi.mjs", "getAllAttributesList")
fn get_all_attributes_list(
  element: HtmlElement,
) -> array.Array(#(String, String))

@external(javascript, "./element.ffi.mjs", "appendCanvasToContainer")
fn append_canvas(container: a, canvas: b) -> Nil

pub fn attribute(element: HtmlElement, name: String) -> Result(String, Nil) {
  get_attribute(element, name)
}

pub fn children(element: HtmlElement) -> List(HtmlElement) {
  child_elements(element)
}

pub fn closest(
  element: HtmlElement,
  selector: String,
) -> Result(HtmlElement, Nil) {
  closest_element(element, selector)
}

pub fn find(id: String) -> Result(HtmlElement, Nil) {
  get_element_by_id(id)
}

pub fn parent(element: HtmlElement) -> Result(HtmlElement, Nil) {
  parent_element(element)
}

pub fn set_attribute(element: HtmlElement, key: String, value: String) -> Nil {
  set_dom_attribute(element, key, value)
}

pub fn tag(element: HtmlElement) -> String {
  tag_name(element)
}

pub fn attributes(element: HtmlElement) -> Dict(String, String) {
  get_all_attributes_list(element)
  |> array.fold(dict.new(), fn(acc, pair) { dict.insert(acc, pair.0, pair.1) })
}

pub fn store_object(id: String, object: savoiardi.Object3D) -> Nil {
  case find(id) {
    Ok(element) -> set_property(element, "_object3d", object)
    Error(Nil) -> Nil
  }
}

pub fn clear_object(id: String) -> Nil {
  case find(id) {
    Ok(element) -> delete_property(element, "_object3d")
    Error(Nil) -> Nil
  }
}

pub fn dispatch_event(id: String, event_name: String, detail) -> Nil {
  case find(id) {
    Ok(element) -> dispatch_custom_event(element, event_name, detail)
    Error(Nil) -> Nil
  }
}

pub fn host_from_shadow_root(shadow_root: Dynamic) -> HtmlElement {
  shadow_root_host(shadow_root)
}

pub fn observe_mutations(
  host: HtmlElement,
  observed_attributes: List(String),
  on_mutated: fn() -> Nil,
) -> Nil {
  setup_mutation_observer(host, observed_attributes, on_mutated)
}

pub fn append_canvas_to_container(container, canvas) -> Nil {
  append_canvas(container, canvas)
}

pub fn renderer_config(
  host: HtmlElement,
  width fallback_width: Int,
  height fallback_height: Int,
) -> RendererConfig {
  let parsed_width = case attribute(host, "width") {
    Ok(width) ->
      case int.parse(width) {
        Ok(value) -> option.Some(value)
        Error(Nil) -> option.None
      }
    Error(Nil) -> option.None
  }
  let parsed_height = case attribute(host, "height") {
    Ok(height) ->
      case int.parse(height) {
        Ok(value) -> option.Some(value)
        Error(Nil) -> option.None
      }
    Error(Nil) -> option.None
  }

  RendererConfig(
    width: case parsed_width {
      option.Some(_) -> parsed_width
      option.None -> option.Some(fallback_width)
    },
    height: case parsed_height {
      option.Some(_) -> parsed_height
      option.None -> option.Some(fallback_height)
    },
    background: attribute(host, "background") |> result.unwrap("#000000"),
    antialias: case attribute(host, "antialias") {
      Ok("false") -> False
      _ -> True
    },
    alpha: case attribute(host, "alpha") {
      Ok("true") -> True
      _ -> False
    },
  )
}

pub fn dispatch_tick(
  scene_id: String,
  delta_ms: Float,
  timestamp_ms: Int,
) -> Nil {
  dispatch_event(
    scene_id,
    "tiramisu:tick",
    json.object([
      #("delta_ms", json.float(delta_ms)),
      #("timestamp_ms", json.int(timestamp_ms)),
    ]),
  )
}
