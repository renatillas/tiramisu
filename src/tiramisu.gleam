//// Public entrypoint for Tiramisu.
////
//// Most applications only need this module plus a handful of node-specific
//// attribute modules such as `tiramisu/camera`, `tiramisu/primitive`, or
//// `tiramisu/material`.
////
//// A minimal application looks like this:
////
//// ```gleam
//// import lustre
//// import lustre/attribute
//// import tiramisu
//// import tiramisu/camera
//// import tiramisu/renderer
//// import tiramisu/transform
//// import vec/vec3
////
//// pub fn main() -> Nil {
////   let assert Ok(_) = tiramisu.register(tiramisu.builtin_extensions())
////   let assert Ok(_) = lustre.start(lustre.element(view()), "#app", Nil)
////   Nil
//// }
////
//// fn view() {
////   tiramisu.renderer(
////     "renderer",
////     [attribute.width(800), attribute.height(480)],
////     [
////       tiramisu.scene("scene", [], [
////         tiramisu.camera(
////           "camera",
////           [camera.active(True), transform.position(vec3.Vec3(0.0, 0.0, 6.0))],
////           [],
////         ),
////       ]),
////     ],
////   )
//// }
//// ```

import lustre
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/element/html
import tiramisu/transform

import tiramisu/camera
import tiramisu/dev/extension
import tiramisu/empty
import tiramisu/light
import tiramisu/material
import tiramisu/mesh
import tiramisu/primitive
import tiramisu/renderer
import tiramisu/scene
import tiramisu/tick

/// Register Tiramisu's custom elements with Lustre.
///
/// Most applications should call this once at startup with
/// `tiramisu.builtin_extensions()`.
///
/// If you are writing custom nodes or attribute extensions you can append them
/// to the built-ins before calling `register/1`.
pub fn register(
  extensions: List(extension.Extension),
) -> Result(Nil, lustre.Error) {
  renderer.register(extensions)
}

/// Create a renderer host.
///
/// A renderer owns the canvas, render loop, and runtime state for exactly one
/// nested `tiramisu.scene`.
///
/// In practice a renderer is the root of each independent 3D world on the page.
pub fn renderer(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(renderer.tag, [attribute.id(id), ..attributes], children)
}

/// Create a scene root inside a renderer.
///
/// Scene children are Tiramisu nodes such as cameras, primitives, meshes,
/// lights, and empties.
///
/// A renderer should contain exactly one scene root.
pub fn scene(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  scene.scene(id, attributes, children)
}

/// Create a camera node.
///
/// Cameras are part of the scene hierarchy, so they can be nested under other
/// nodes and transformed like any other scene object.
pub fn camera(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(camera.tag, [attribute.id(id), ..attributes], children)
}

/// Create a light node.
///
/// Lights affect the scene from their place in the scene hierarchy.
pub fn light(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(light.tag, [attribute.id(id), ..attributes], children)
}

/// Create a primitive mesh node.
///
/// Primitives create geometry directly from attributes such as
/// `primitive.box(...)` or `primitive.sphere(...)`.
pub fn primitive(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(primitive.tag, [attribute.id(id), ..attributes], children)
}

/// Create an asset-backed mesh node.
///
/// Meshes load external assets through attributes such as `attribute.src(...)`.
pub fn mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(mesh.tag, [attribute.id(id), ..attributes], children)
}

/// Create an empty group node for hierarchy and shared transforms.
///
/// Use empty nodes to build scene structure without introducing visible geometry.
pub fn empty(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(empty.tag, [attribute.id(id), ..attributes], children)
}

/// Attach a per-frame tick handler to the current scene.
///
/// This is the main way to drive animation from Lustre state.
pub fn on_tick(handler: fn(tick.TickContext) -> msg) -> Attribute(msg) {
  tick.on_tick(handler)
}

/// Get the built-in extensions shipped with Tiramisu.
///
/// Pass this to `tiramisu.register/1` unless you are assembling a custom
/// extension list yourself.
pub fn builtin_extensions() -> List(extension.Extension) {
  [
    transform.extension(),
    mesh.extension(),
    primitive.extension(),
    camera.extension(),
    light.extension(),
    empty.extension(),
    material.extension(),
  ]
}

pub fn script() {
  html.script(
    [attribute.type_("importmap")],
    "{
       \"imports\": {
         \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\",
         \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\" 
       } 
     }",
  )
}
