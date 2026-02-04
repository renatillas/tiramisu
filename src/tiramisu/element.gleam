//// Element constructors for Tiramisu scene nodes.
////
//// This module provides Lustre-style element constructors for building 3D scenes
//// declaratively. Each function creates an `Element(msg)` that will be rendered
//// by the Tiramisu platform as a Three.js object.
////
//// ## Usage
////
//// ```gleam
//// import tiramisu/element
//// import tiramisu/attribute
////
//// element.mesh("cube", [
////   attribute.position(vec3.Vec3(0.0, 1.0, 0.0)),
////   attribute.color(0xff0000),
//// ], [])
//// ```
////
//// ## Node Types
////
//// - **Core**: mesh, camera, light, empty
//// - **Special**: sprite, model, audio, lod
//// - **Instanced**: instanced_mesh, instanced_model
//// - **UI**: css2d, css3d, canvas
//// - **Animation**: animated_sprite
//// - **Debug**: debug_box, debug_sphere, debug_line, debug_axes, debug_grid, debug_point

// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

// CORE 3D ELEMENTS ------------------------------------------------------------

/// Create a mesh node (3D object with geometry and material).
///
/// Meshes are the basic building blocks for 3D objects. Use attributes to configure
/// geometry, material, transform, and optional physics.
///
/// ## Example
///
/// ```gleam
/// element.mesh("player", [
///   attribute.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
///   attribute.color(0xff0000),
///   attribute.position(vec3.Vec3(0.0, 1.0, 0.0)),
/// ], [])
/// ```
///
pub fn mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("mesh", [attribute.id(id), ..attributes], children)
}

/// Create a camera node (viewpoint with projection settings).
///
/// Cameras define the view into the scene. Use attributes to configure FOV, near/far
/// clipping, viewport, and post-processing effects.
///
/// ## Example
///
/// ```gleam
/// element.camera("main-camera", [
///   attribute.fov(75.0),
///   attribute.near(0.1),
///   attribute.far(1000.0),
///   attribute.position(vec3.Vec3(0.0, 5.0, 10.0)),
///   attribute.look_at(vec3.Vec3(0.0, 0.0, 0.0)),
/// ], [])
/// ```
///
pub fn camera(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("camera", [attribute.id(id), ..attributes], children)
}

/// Create a light node (ambient, directional, point, or spot light).
///
/// Lights illuminate the scene. Use attributes to configure type, intensity, color,
/// and shadow settings.
///
/// ## Example
///
/// ```gleam
/// element.light("sun", [
///   attribute.light_type("directional"),
///   attribute.intensity(1.0),
///   attribute.color(0xffffff),
///   attribute.position(vec3.Vec3(10.0, 10.0, 10.0)),
/// ], [])
/// ```
///
pub fn light(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("light", [attribute.id(id), ..attributes], children)
}

/// Create an empty node (invisible grouping for organization).
///
/// Empty nodes act as containers for other nodes, useful for grouping and hierarchy.
/// They have transforms but no visual representation.
///
/// ## Example
///
/// ```gleam
/// element.empty("enemies", [
///   attribute.position(vec3.Vec3(0.0, 0.0, 0.0)),
/// ], [
///   element.mesh("enemy-1", [...], []),
///   element.mesh("enemy-2", [...], []),
/// ])
/// ```
///
pub fn empty(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("empty", [attribute.id(id), ..attributes], children)
}

// SPECIAL ELEMENTS ------------------------------------------------------------

/// Create a sprite node (2D billboard that always faces camera).
///
/// Sprites are useful for particle effects, UI elements in 3D space, or 2D game objects.
///
pub fn sprite(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("sprite", [attribute.id(id), ..attributes], children)
}

/// Create a model node (loaded GLTF/GLB 3D model with animations).
///
/// Models are loaded from external files. Use attributes to configure animations,
/// material overrides, and physics.
///
pub fn model(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("model", [attribute.id(id), ..attributes], children)
}

/// Create an audio node (global or positional audio source).
///
/// Audio nodes play sounds in the scene. Use attributes to configure volume, loop,
/// and spatial audio settings.
///
pub fn audio(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("audio", [attribute.id(id), ..attributes], children)
}

/// Create an LOD node (Level of Detail with automatic switching).
///
/// LOD nodes display different meshes based on camera distance to improve performance.
///
pub fn lod(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("lod", [attribute.id(id), ..attributes], children)
}

// INSTANCED RENDERING ---------------------------------------------------------

/// Create an instanced mesh node (many copies with one draw call).
///
/// Instanced meshes efficiently render many identical objects at different transforms.
/// Use for crowds, forests, particles, etc.
///
pub fn instanced_mesh(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("instanced-mesh", [attribute.id(id), ..attributes], children)
}

/// Create an instanced model node (many model copies with one draw call).
///
/// Like instanced_mesh but for GLTF/GLB models.
///
pub fn instanced_model(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("instanced-model", [attribute.id(id), ..attributes], children)
}

// UI OVERLAYS -----------------------------------------------------------------

/// Create a CSS2D node (HTML overlay with perspective).
///
/// CSS2D elements are HTML that follow 3D positions with perspective transform.
/// Useful for labels, tooltips, and UI elements attached to 3D objects.
///
pub fn css2d(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("ui", "css2d", [attribute.id(id), ..attributes], children)
}

/// Create a CSS3D node (3D-transformed HTML).
///
/// CSS3D elements are HTML that can be rotated and positioned in 3D space.
/// Useful for complex UI elements that need full 3D transformation.
///
pub fn css3d(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("ui", "css3d", [attribute.id(id), ..attributes], children)
}

/// Create a canvas node (canvas drawing rendered to texture).
///
/// Canvas nodes render 2D canvas drawings as textures on sprites with depth testing.
///
pub fn canvas(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("ui", "canvas", [attribute.id(id), ..attributes], children)
}

// ANIMATION -------------------------------------------------------------------

/// Create an animated sprite node (spritesheet animation).
///
/// Animated sprites use spritesheets for frame-by-frame animation.
///
pub fn animated_sprite(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("animated-sprite", [attribute.id(id), ..attributes], children)
}

// DEBUG VISUALIZATION ---------------------------------------------------------

/// Create a debug box node (wireframe bounding box).
///
/// Debug boxes visualize bounding volumes for debugging collision detection.
///
pub fn debug_box(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("debug", "box", [attribute.id(id), ..attributes], children)
}

/// Create a debug sphere node (wireframe sphere).
///
/// Debug spheres visualize spherical volumes or points.
///
pub fn debug_sphere(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced(
    "debug",
    "sphere",
    [attribute.id(id), ..attributes],
    children,
  )
}

/// Create a debug line node (line between two points).
///
/// Debug lines visualize vectors, paths, or connections.
///
pub fn debug_line(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("debug", "line", [attribute.id(id), ..attributes], children)
}

/// Create a debug axes node (XYZ axes helper).
///
/// Debug axes visualize coordinate systems or object orientations.
///
pub fn debug_axes(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("debug", "axes", [attribute.id(id), ..attributes], children)
}

/// Create a debug grid node (ground plane grid).
///
/// Debug grids visualize the ground plane and help with depth perception.
///
pub fn debug_grid(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced("debug", "grid", [attribute.id(id), ..attributes], children)
}

/// Create a debug point node (colored sphere marker).
///
/// Debug points visualize specific 3D positions.
///
pub fn debug_point(
  id: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.namespaced(
    "debug",
    "point",
    [attribute.id(id), ..attributes],
    children,
  )
}
