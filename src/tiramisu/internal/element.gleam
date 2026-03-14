import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/javascript/array
import gleam/result
import savoiardi

pub type HtmlElement

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

@external(javascript, "./element.ffi.mjs", "getAllAttributesList")
fn get_all_attributes_list(
  element: HtmlElement,
) -> array.Array(#(String, String))

@external(javascript, "./element.ffi.mjs", "appendCanvasToContainer")
pub fn append_canvas(shadow_root: Dynamic, canvas: savoiardi.Canvas) -> Nil

@external(javascript, "./element.ffi.mjs", "getAttribute")
pub fn attribute(element: HtmlElement, name: String) -> Result(String, Nil)

@external(javascript, "./element.ffi.mjs", "children")
pub fn children(element: HtmlElement) -> List(HtmlElement)

@external(javascript, "./element.ffi.mjs", "closest")
pub fn closest(
  element: HtmlElement,
  selector: String,
) -> Result(HtmlElement, Nil)

@external(javascript, "./element.ffi.mjs", "getElementById")
pub fn find(id: String) -> Result(HtmlElement, Nil)

@external(javascript, "./element.ffi.mjs", "parentElement")
pub fn parent(element: HtmlElement) -> Result(HtmlElement, Nil)

@external(javascript, "./element.ffi.mjs", "setAttribute")
pub fn set_attribute(element: HtmlElement, key: String, value: String) -> Nil

@external(javascript, "./element.ffi.mjs", "tagName")
pub fn tag(element: HtmlElement) -> String

pub fn attributes(element: HtmlElement) -> Dict(String, String) {
  use acc, pair <- array.fold(get_all_attributes_list(element), dict.new())
  dict.insert(acc, pair.0, pair.1)
}

pub fn store_object(id: String, object: savoiardi.Object3D) -> Result(Nil, Nil) {
  use element <- result.map(find(id))
  set_property(element, "_object3d", object)
}

pub fn clear_object(id: String) -> Result(Nil, Nil) {
  use element <- result.map(find(id))
  delete_property(element, "_object3d")
}

pub fn dispatch_event(
  id: String,
  event_name: String,
  detail: _,
) -> Result(Nil, Nil) {
  use element <- result.map(find(id))
  dispatch_custom_event(element, event_name, detail)
}

@external(javascript, "./element.ffi.mjs", "shadowRootHost")
pub fn host_from_shadow_root(shadow_root: Dynamic) -> HtmlElement

@external(javascript, "./element.ffi.mjs", "setupMutationObserver")
pub fn observe_mutations(
  host: HtmlElement,
  observed_attributes: List(String),
  on_mutated: fn() -> Nil,
) -> Nil
