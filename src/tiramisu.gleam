import lustre
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

import tiramisu/audio
import tiramisu/camera
import tiramisu/debug
import tiramisu/empty
import tiramisu/instanced_mesh
import tiramisu/light
import tiramisu/mesh
import tiramisu/scene

pub fn register() -> Result(Nil, lustre.Error) {
  scene.register()
}

// ELEMENTS --------------------------------------------------------------------

/// Create a tiramisu-renderer element.
///
/// This is the root container for all 3D content. It creates a WebGL renderer
/// and scene, and parses light DOM children as the scene description.
///
/// ## Example
///
/// ```gleam
/// import tiramisu
/// import tiramisu/renderer
///
/// renderer.renderer([renderer.background("#1a1a2e")], [
///   tiramisu.camera("main", [...], []),
///   tiramisu.mesh("cube", [...], []),
/// ])
/// ```
///
pub fn scene(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(
    scene.tag,
    [attribute.attribute("scene-id", id), ..attributes],
    children,
  )
}

pub fn camera(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(camera.tag, [attribute.id(id), ..attributes], children)
}

pub fn light(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(light.tag, [attribute.id(id), ..attributes], children)
}

pub fn mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(mesh.tag, [attribute.id(id), ..attributes], children)
}

pub fn empty(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(empty.tag, [attribute.id(id), ..attributes], children)
}

pub fn instanced_mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(
    instanced_mesh.tag,
    [attribute.id(id), ..attributes],
    children,
  )
}

pub fn global_audio(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(audio.global_tag, [attribute.id(id), ..attributes], children)
}

pub fn audio_positional(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(
    audio.positional_tag,
    [attribute.id(id), ..attributes],
    children,
  )
}

pub fn debug(id: String, attributes: List(Attribute(msg))) -> Element(msg) {
  element.element(debug.tag, [attribute.id(id), ..attributes], [])
}
