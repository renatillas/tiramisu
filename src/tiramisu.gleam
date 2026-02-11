//// Tiramisu - A 3D game engine using web components.
////
//// Tiramisu provides declarative 3D scene construction through custom HTML
//// elements. Only the renderer is a web component — all other elements
//// (mesh, camera, light, empty, audio) are plain DOM elements parsed from
//// the light DOM.
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
////   // Register the tiramisu-renderer web component
////   let assert Ok(_) = tiramisu.register()
////
////   // Start a simple Lustre app that uses it
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
////   <tiramisu-camera id="main" transform="pos:0,5,10" active="true"></tiramisu-camera>
////   <tiramisu-mesh id="cube" geometry="box:2,2,2" color="#ff6b6b"></tiramisu-mesh>
////   <tiramisu-light id="sun" type="directional" intensity="1"></tiramisu-light>
//// </tiramisu-renderer>
//// ```
////
//// ## Elements
////
//// - `<tiramisu-renderer>`: The main container (only web component) that owns the WebGL renderer and scene
//// - `<tiramisu-mesh>`: A 3D mesh with geometry and material
//// - `<tiramisu-camera>`: A perspective or orthographic camera
//// - `<tiramisu-light>`: Ambient, directional, point, or spot lighting
//// - `<tiramisu-empty>`: An invisible group for hierarchical organization
//// - `<tiramisu-audio>`: Global (non-positional) audio
//// - `<tiramisu-audio-positional>`: 3D positional audio

// IMPORTS ---------------------------------------------------------------------

import lustre
import tiramisu/renderer

// REGISTRATION ----------------------------------------------------------------

/// Register the tiramisu-renderer web component.
///
/// Call this once at application startup before using any Tiramisu elements.
/// Only the renderer needs registration — all other elements are plain DOM
/// elements that the renderer parses from its light DOM.
///
/// ## Example
///
/// ```gleam
/// pub fn main() {
///   let assert Ok(_) = tiramisu.register()
///   // Now you can use <tiramisu-renderer> with child elements
/// }
/// ```
///
pub fn register() -> Result(Nil, lustre.Error) {
  renderer.register()
}
