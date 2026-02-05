//// Tiramisu - A 3D game engine using web components.
////
//// Tiramisu provides declarative 3D scene construction through custom HTML
//// elements. Use these components in any Lustre application or plain HTML.
////
//// ## Quick Start
////
//// ```gleam
//// import lustre
//// import lustre/element/html
//// import tiramisu
//// import tiramisu/renderer
//// import tiramisu/mesh
//// import tiramisu/camera
//// import tiramisu/light
////
//// pub fn main() {
////   // Register all Tiramisu web components
////   let assert Ok(_) = tiramisu.register()
////
////   // Start a simple Lustre app that uses them
////   let app = lustre.element(view())
////   let assert Ok(_) = lustre.start(app, "#app", Nil)
//// }
////
//// fn view() {
////   html.div([], [
////     renderer.renderer([renderer.background("#1a1a2e")], [
////       camera.camera("main", [camera.fov(75.0), camera.active(True)], []),
////       mesh.mesh("cube", [mesh.geometry_box(2.0, 2.0, 2.0), mesh.color(0xff6b6b)], []),
////       light.light("sun", [light.light_type("directional"), light.intensity(1.0)], []),
////     ]),
////   ])
//// }
//// ```
////
//// ## Pure HTML Usage
////
//// ```html
//// <script type="module">
////   import { register } from './tiramisu.mjs';
////   register();
//// </script>
////
//// <tiramisu-renderer background="#1a1a2e">
////   <tiramisu-camera id="main" position="0,5,10" active="true"></tiramisu-camera>
////   <tiramisu-mesh id="cube" geometry="box:2,2,2" color="#ff6b6b"></tiramisu-mesh>
////   <tiramisu-light id="sun" type="directional" intensity="1"></tiramisu-light>
//// </tiramisu-renderer>
//// ```
////
//// ## Components
////
//// - `<tiramisu-renderer>`: The main container that owns the WebGL renderer and scene
//// - `<tiramisu-mesh>`: A 3D mesh with geometry and material
//// - `<tiramisu-camera>`: A perspective or orthographic camera
//// - `<tiramisu-light>`: Ambient, directional, point, or spot lighting
//// - `<tiramisu-empty>`: An invisible group for hierarchical organization

// IMPORTS ---------------------------------------------------------------------

import gleam/result
import lustre
import tiramisu/audio
import tiramisu/audio_positional
import tiramisu/camera
import tiramisu/empty
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer

// REGISTRATION ----------------------------------------------------------------

/// Register all Tiramisu web components.
///
/// Call this once at application startup before using any Tiramisu elements.
/// Returns `Error` if any component registration fails.
///
/// ## Example
///
/// ```gleam
/// pub fn main() {
///   let assert Ok(_) = tiramisu.register()
///   // Now you can use <tiramisu-renderer>, <tiramisu-mesh>, etc.
/// }
/// ```
///
pub fn register() -> Result(Nil, lustre.Error) {
  use _ <- result.try(renderer.register())
  use _ <- result.try(mesh.register())
  use _ <- result.try(camera.register())
  use _ <- result.try(light.register())
  use _ <- result.try(empty.register())
  use _ <- result.try(audio.register())
  use _ <- result.try(audio_positional.register())
  Ok(Nil)
}

/// Register individual components if you don't need all of them.
///
/// This is useful if you want to minimize bundle size by only including
/// the components you actually use.
///
pub fn register_renderer() -> Result(Nil, lustre.Error) {
  renderer.register()
}

pub fn register_mesh() -> Result(Nil, lustre.Error) {
  mesh.register()
}

pub fn register_camera() -> Result(Nil, lustre.Error) {
  camera.register()
}

pub fn register_light() -> Result(Nil, lustre.Error) {
  light.register()
}

pub fn register_empty() -> Result(Nil, lustre.Error) {
  empty.register()
}

pub fn register_audio() -> Result(Nil, lustre.Error) {
  audio.register()
}

pub fn register_audio_positional() -> Result(Nil, lustre.Error) {
  audio_positional.register()
}

