pub type DomElement

pub type WebGLRenderer

/// Internal Three.js Scene type (not exposed to users)
@internal
pub type Scene

/// Internal Three.js Camera type (not exposed to users)
@internal
pub type ThreeCamera

/// Configuration options for the renderer
pub type RendererOptions {
  RendererOptions(antialias: Bool, alpha: Bool, width: Int, height: Int)
}

/// Create a new WebGL renderer
@external(javascript, "./ffi/renderer.mjs", "createRenderer")
pub fn create(options: RendererOptions) -> WebGLRenderer

/// Render a scene with a camera (internal use only)
@external(javascript, "./ffi/renderer.mjs", "render")
@internal
pub fn render(renderer: WebGLRenderer, scene: Scene, camera: ThreeCamera) -> Nil

/// Set the size of the renderer
@external(javascript, "./ffi/renderer.mjs", "setSize")
pub fn set_size(
  renderer: WebGLRenderer,
  width: Int,
  height: Int,
) -> WebGLRenderer

/// Get the DOM element (canvas) from the renderer
@external(javascript, "./ffi/renderer.mjs", "getDomElement")
pub fn get_dom_element(renderer: WebGLRenderer) -> DomElement

/// Set the clear color of the renderer
@external(javascript, "./ffi/renderer.mjs", "setClearColor")
pub fn set_clear_color(
  renderer: WebGLRenderer,
  color: Int,
  alpha: Float,
) -> WebGLRenderer
