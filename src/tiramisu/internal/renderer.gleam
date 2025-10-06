pub type DomElement

pub type WebGLRenderer

/// Internal Three.js Scene type (not exposed to users)
pub type Scene

/// Internal Three.js Camera type (not exposed to users)
pub type ThreeCamera

/// Configuration options for the renderer
pub type RendererOptions {
  RendererOptions(antialias: Bool, alpha: Bool, width: Int, height: Int)
}

/// Create a new WebGL renderer
@external(javascript, "../ffi/renderer.mjs", "createRenderer")
pub fn create(options: RendererOptions) -> WebGLRenderer

/// Render a scene with a camera (internal use only)
@external(javascript, "../ffi/renderer.mjs", "render")
pub fn render(renderer: WebGLRenderer, scene: Scene, camera: ThreeCamera) -> Nil

@external(javascript, "../ffi/renderer.mjs", "getDomElement")
pub fn get_dom_element(renderer: WebGLRenderer) -> DomElement
