//// Post-processing effects for WebGL rendering using Three.js EffectComposer.
////
//// Post-processing allows you to apply visual effects to your rendered scene,
//// such as bloom, pixelation, film grain, and more. Effects are applied in order
//// and composed using Three.js's EffectComposer system.
////
//// ## Camera-Based Architecture
////
//// Postprocessing in Tiramisu is **camera-based**, following industry standards
//// from Unity and Unreal Engine. Each camera can have its own postprocessing
//// pipeline, enabling split-screen with different effects, mini-maps without
//// effects, or picture-in-picture with unique visual styles.
////
//// ## Dynamic Updates
////
//// Postprocessing configurations are automatically diffed each frame. When you
//// change a camera's postprocessing setup in your view function, the engine
//// detects the change and recreates the effect composer. This allows for:
////
//// - Toggling effects on/off based on game state
//// - Changing effect parameters dynamically (e.g., bloom intensity)
//// - Swapping entire effect pipelines at runtime
////
//// The diffing algorithm compares pass types and parameters, only recreating
//// when actual changes are detected, minimizing performance impact.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/postprocessing as pp
//// import tiramisu/scene
//// import gleam/option.{None, Some}
////
//// // In your view function:
//// fn view(model: Model, _ctx) -> scene.Node(String) {
////   let camera = scene.camera(
////     id: "main",
////     camera: cam,
////     transform: transform.identity,
////     look_at: option.None,
////     active: True,
////     viewport: option.None,
////     postprocessing: option.Some(
////       pp.new()
////       |> pp.add_pass(pp.clear_pass(option.None))
////       |> pp.add_pass(pp.render_pass())
////       |> pp.add_pass(pp.bloom(strength: 1.5, threshold: 0.85, radius: 0.4))
////       |> pp.add_pass(pp.pixelate(pixel_size: 4))
////       |> pp.add_pass(pp.vignette(darkness: 1.0, offset: 1.0))
////       |> pp.add_pass(pp.fxaa())
////       |> pp.add_pass(pp.output_pass())
////     ),
////   )
////
////   scene.empty(id: "root", transform: transform.identity, children: [
////     camera,
////     // ... your scene nodes
////   ])
//// }
//// ```
////
//// ## Multi-Camera Example
////
//// ```gleam
//// // Player 1: Retro pixel effect
//// let camera1 = scene.camera(
////   id: "player1",
////   viewport: option.Some(scene.Viewport(x: 0, y: 0, width: 400, height: 600)),
////   postprocessing: option.Some(
////     pp.new()
////     |> pp.add_pass(pp.clear_pass(option.None))
////     |> pp.add_pass(pp.render_pass())
////     |> pp.add_pass(pp.pixelate(4))
////     |> pp.add_pass(pp.output_pass())
////   ),
////   // ...
//// )
////
//// // Player 2: Cinematic bloom
//// let camera2 = scene.camera(
////   id: "player2",
////   viewport: option.Some(scene.Viewport(x: 400, y: 0, width: 400, height: 600)),
////   postprocessing: option.Some(
////     pp.new()
////     |> pp.add_pass(pp.clear_pass(option.None))
////     |> pp.add_pass(pp.render_pass())
////     |> pp.add_pass(pp.bloom(strength: 1.5, threshold: 0.7, radius: 0.5))
////     |> pp.add_pass(pp.vignette(darkness: 0.8, offset: 1.0))
////     |> pp.add_pass(pp.output_pass())
////   ),
////   // ...
//// )
//// ```
////
//// ## Effect Order
////
//// Effects are applied in the order you add them. A typical pipeline looks like:
////
//// 1. Bloom (brighten glowing areas)
//// 2. Pixelate (retro pixel effect)
//// 3. Film grain (add texture)
//// 4. Vignette (darken edges)
//// 5. FXAA (anti-aliasing for smooth final output)
////
//// ## Performance
////
//// Post-processing has a performance cost. Each effect adds a render pass.
//// Each camera with postprocessing creates a separate EffectComposer.
//// For mobile devices, consider using fewer effects or lower quality settings.

import gleam/list
import gleam/option.{type Option}

/// Opaque post-processing composer type.
///
/// Contains a list of passes that will be applied in order to the rendered scene.
pub opaque type PostProcessing {
  PostProcessing(passes: List(Pass))
}

/// Post-processing pass types.
///
/// Each pass represents a visual effect that will be applied to the scene.
/// Passes are executed in the order they are added to the pipeline.
///
/// ## Pipeline Passes
///
/// Three.js postprocessing requires specific passes for proper rendering:
/// - **RenderPass**: Renders the scene to the render target (usually first)
/// - **ClearPass**: Clears render target with a color (for backgrounds)
/// - **OutputPass**: Final tone mapping and output (usually last)
///
/// You must explicitly add these passes in the correct order.
pub type Pass {
  /// Render pass - renders the scene to the render target.
  ///
  /// This pass actually draws your 3D scene. It should typically be one of the
  /// first passes in your pipeline. If you need the scene background to work,
  /// add a ClearPass before this.
  RenderPass
  /// Clear pass - clears the render target with a color.
  ///
  /// Use this before RenderPass to make scene backgrounds work correctly.
  /// The color parameter overrides the scene background if provided.
  ///
  /// - `None`: Uses the scene's background color
  /// - `Some(color)`: Uses the specified hex color
  ClearPass(color: Option(Int))
  /// Output pass - applies tone mapping and outputs to screen.
  ///
  /// This should typically be the last pass in your pipeline. It applies
  /// final color corrections and tone mapping before displaying the result.
  OutputPass
  /// Pixelation effect with optional edge detection.
  ///
  /// Creates a retro pixel-art aesthetic by reducing the resolution of the image.
  /// Edge detection can add outlines based on surface normals and depth.
  PixelatePass(
    pixel_size: Int,
    normal_edge_strength: Float,
    depth_edge_strength: Float,
  )
  /// Bloom effect (glow for bright areas).
  ///
  /// Makes bright areas of the scene glow and bleed into surrounding pixels.
  /// Great for emissive materials, lights, and sci-fi aesthetics.
  BloomPass(strength: Float, threshold: Float, radius: Float)
  /// Film grain effect.
  ///
  /// Adds analog film texture with grain noise and optional scanlines.
  /// Can create a retro or cinematic look.
  FilmPass(
    noise_intensity: Float,
    scanline_intensity: Float,
    scanline_count: Int,
    grayscale: Bool,
  )
  /// Vignette effect (darkened edges).
  ///
  /// Darkens the edges of the screen, focusing attention on the center.
  VignettePass(darkness: Float, offset: Float)
  /// FXAA anti-aliasing.
  ///
  /// Fast approximate anti-aliasing that smooths jagged edges.
  /// Usually added as the last pass for a polished final output.
  FXAAPass
  /// Glitch effect.
  ///
  /// Creates digital corruption artifacts with RGB channel offsets.
  /// Great for cyberpunk or error state aesthetics.
  GlitchPass(dt_size: Int)
  /// Color correction.
  ///
  /// Adjust brightness, contrast, and saturation of the final image.
  ColorCorrectionPass(brightness: Float, contrast: Float, saturation: Float)
  /// Custom shader pass.
  ///
  /// Apply a custom GLSL shader for advanced effects.
  CustomShaderPass(
    vertex_shader: String,
    fragment_shader: String,
    uniforms: List(#(String, UniformValue)),
  )
}

/// Uniform values for custom shaders.
///
/// When creating custom shader passes, you can pass uniform values of different types.
pub type UniformValue {
  FloatUniform(Float)
  IntUniform(Int)
  Vec2Uniform(Float, Float)
  Vec3Uniform(Float, Float, Float)
  ColorUniform(Int)
}

/// Create a new empty post-processing pipeline.
///
/// Start with this and add passes using `add_pass`.
///
/// ## Example
///
/// ```gleam
/// let pp = postprocessing.new()
///   |> postprocessing.add_pass(postprocessing.clear_pass(option.None))
///   |> postprocessing.add_pass(postprocessing.render_pass())
///   |> postprocessing.add_pass(postprocessing.bloom(...))
///   |> postprocessing.add_pass(postprocessing.fxaa())
///   |> postprocessing.add_pass(postprocessing.output_pass())
/// ```
pub fn new() -> PostProcessing {
  PostProcessing(passes: [])
}

/// Add a pass to the pipeline.
///
/// Passes are executed in the order they are added.
///
/// ## Example
///
/// ```gleam
/// postprocessing.new()
/// |> postprocessing.add_pass(postprocessing.bloom(
///   strength: 1.5,
///   threshold: 0.85,
///   radius: 0.4,
/// ))
/// |> postprocessing.add_pass(postprocessing.fxaa())
/// ```
pub fn add_pass(pp: PostProcessing, pass: Pass) -> PostProcessing {
  let PostProcessing(passes) = pp
  PostProcessing(passes: list.append(passes, [pass]))
}

/// Get the list of passes (internal use).
@internal
pub fn get_passes(pp: PostProcessing) -> List(Pass) {
  let PostProcessing(passes) = pp
  passes
}

// ============================================================================
// CONVENIENCE CONSTRUCTORS
// ============================================================================

/// Create a render pass.
///
/// This pass renders your 3D scene to the render target. It should typically
/// be one of the first passes in your pipeline (after ClearPass if you need
/// background rendering).
///
/// ## Example
///
/// ```gleam
/// pp.new()
/// |> pp.add_pass(pp.render_pass())
/// |> pp.add_pass(pp.bloom(...))
/// ```
pub fn render_pass() -> Pass {
  RenderPass
}

/// Create a clear pass.
///
/// Clears the render target with a color. Use this before RenderPass to make
/// scene backgrounds work correctly with postprocessing.
///
/// ## Parameters
///
/// - `color`: Optional hex color to clear with
///   - `None`: Uses the scene's background color
///   - `Some(0xff0000)`: Clears with the specified color
///
/// ## Example
///
/// ```gleam
/// // Use scene background
/// pp.clear_pass(option.None)
///
/// // Use custom color
/// pp.clear_pass(option.Some(0x000000))  // Black
/// ```
pub fn clear_pass(color color: Option(Int)) -> Pass {
  ClearPass(color: color)
}

/// Create an output pass.
///
/// Applies final tone mapping and outputs to the screen. This should typically
/// be the last pass in your pipeline.
///
/// ## Example
///
/// ```gleam
/// pp.new()
/// |> pp.add_pass(pp.render_pass())
/// |> pp.add_pass(pp.bloom(...))
/// |> pp.add_pass(pp.output_pass())  // Last pass
/// ```
pub fn output_pass() -> Pass {
  OutputPass
}

/// Create a bloom effect pass.
///
/// Bloom makes bright areas glow and bleed into surrounding pixels.
///
/// ## Parameters
///
/// - `strength`: How much bloom to apply (0.0 - 3.0, typically 0.5 - 2.0)
/// - `threshold`: Brightness threshold for bloom (0.0 - 1.0, typically 0.5 - 0.9)
/// - `radius`: How far the bloom spreads (0.0 - 1.0, typically 0.4 - 0.8)
///
/// ## Example
///
/// ```gleam
/// // Subtle bloom for realistic glow
/// pp.bloom(strength: 0.8, threshold: 0.85, radius: 0.4)
///
/// // Intense bloom for sci-fi effect
/// pp.bloom(strength: 2.0, threshold: 0.5, radius: 0.8)
/// ```
pub fn bloom(
  strength strength: Float,
  threshold threshold: Float,
  radius radius: Float,
) -> Pass {
  BloomPass(strength: strength, threshold: threshold, radius: radius)
}

/// Create a simple pixelation effect without edge detection.
///
/// ## Parameters
///
/// - `pixel_size`: Size of pixels (1 - 16, where 1 is no effect)
///
/// ## Example
///
/// ```gleam
/// // Subtle pixelation
/// pp.pixelate(pixel_size: 2)
///
/// // Strong retro effect
/// pp.pixelate(pixel_size: 8)
/// ```
pub fn pixelate(pixel_size pixel_size: Int) -> Pass {
  PixelatePass(
    pixel_size: pixel_size,
    normal_edge_strength: 0.0,
    depth_edge_strength: 0.0,
  )
}

/// Create a pixelation effect with edge detection.
///
/// Edge detection adds outlines based on surface normals and depth changes.
///
/// ## Parameters
///
/// - `pixel_size`: Size of pixels (1 - 16)
/// - `normal_edge_strength`: Strength of normal-based edges (0.0 - 2.0)
/// - `depth_edge_strength`: Strength of depth-based edges (0.0 - 1.0)
///
/// ## Example
///
/// ```gleam
/// pp.pixelate_with_edges(
///   pixel_size: 4,
///   normal_edge_strength: 1.0,
///   depth_edge_strength: 0.5,
/// )
/// ```
pub fn pixelate_with_edges(
  pixel_size pixel_size: Int,
  normal_edge_strength normal_edge_strength: Float,
  depth_edge_strength depth_edge_strength: Float,
) -> Pass {
  PixelatePass(
    pixel_size: pixel_size,
    normal_edge_strength: normal_edge_strength,
    depth_edge_strength: depth_edge_strength,
  )
}

/// Create a film grain effect.
///
/// Adds analog film texture with grain noise and optional scanlines.
///
/// ## Parameters
///
/// - `noise_intensity`: Amount of grain (0.0 - 1.0, typically 0.2 - 0.5)
/// - `scanline_intensity`: Strength of scanlines (0.0 - 1.0, typically 0.0 - 0.3)
/// - `scanline_count`: Number of scanlines (e.g., 512, 1024)
/// - `grayscale`: Convert to black and white
///
/// ## Example
///
/// ```gleam
/// // Subtle film grain
/// pp.film_grain(
///   noise_intensity: 0.2,
///   scanline_intensity: 0.0,
///   scanline_count: 0,
///   grayscale: False,
/// )
///
/// // Retro CRT monitor effect
/// pp.film_grain(
///   noise_intensity: 0.4,
///   scanline_intensity: 0.3,
///   scanline_count: 512,
///   grayscale: True,
/// )
/// ```
pub fn film_grain(
  noise_intensity noise_intensity: Float,
  scanline_intensity scanline_intensity: Float,
  scanline_count scanline_count: Int,
  grayscale grayscale: Bool,
) -> Pass {
  FilmPass(
    noise_intensity: noise_intensity,
    scanline_intensity: scanline_intensity,
    scanline_count: scanline_count,
    grayscale: grayscale,
  )
}

/// Create a vignette effect.
///
/// Darkens the edges of the screen, focusing attention on the center.
///
/// ## Parameters
///
/// - `darkness`: How dark the edges are (0.0 - 2.0, typically 0.5 - 1.5)
/// - `offset`: How far from center the vignette starts (0.0 - 2.0, typically 0.8 - 1.2)
///
/// ## Example
///
/// ```gleam
/// // Subtle vignette
/// pp.vignette(darkness: 0.5, offset: 1.0)
///
/// // Dramatic vignette
/// pp.vignette(darkness: 1.5, offset: 0.8)
/// ```
pub fn vignette(darkness darkness: Float, offset offset: Float) -> Pass {
  VignettePass(darkness: darkness, offset: offset)
}

/// Create an FXAA anti-aliasing pass.
///
/// Fast approximate anti-aliasing that smooths jagged edges.
/// Usually added as the last pass in the pipeline.
///
/// ## Example
///
/// ```gleam
/// postprocessing.new()
/// |> postprocessing.add_pass(postprocessing.bloom(...))
/// |> postprocessing.add_pass(postprocessing.fxaa())  // Last pass
/// ```
pub fn fxaa() -> Pass {
  FXAAPass
}

/// Create a glitch effect pass.
///
/// Creates digital corruption artifacts with RGB channel offsets.
///
/// ## Parameters
///
/// - `dt_size`: Size of distortion blocks (typically 32 - 128)
///
/// ## Example
///
/// ```gleam
/// pp.glitch(dt_size: 64)
/// ```
pub fn glitch(dt_size dt_size: Int) -> Pass {
  GlitchPass(dt_size: dt_size)
}

/// Create a color correction pass.
///
/// Adjust brightness, contrast, and saturation of the final image.
///
/// ## Parameters
///
/// - `brightness`: Brightness adjustment (-1.0 to 1.0, 0.0 is no change)
/// - `contrast`: Contrast adjustment (-1.0 to 1.0, 0.0 is no change)
/// - `saturation`: Saturation adjustment (-1.0 to 1.0, 0.0 is no change, -1.0 is grayscale)
///
/// ## Example
///
/// ```gleam
/// // Brighten and increase saturation
/// pp.color_correction(
///   brightness: 0.2,
///   contrast: 0.1,
///   saturation: 0.3,
/// )
///
/// // Desaturated look
/// pp.color_correction(
///   brightness: 0.0,
///   contrast: 0.2,
///   saturation: -0.5,
/// )
/// ```
pub fn color_correction(
  brightness brightness: Float,
  contrast contrast: Float,
  saturation saturation: Float,
) -> Pass {
  ColorCorrectionPass(
    brightness: brightness,
    contrast: contrast,
    saturation: saturation,
  )
}

/// Create a custom shader pass.
///
/// Apply custom GLSL shaders for advanced effects.
///
/// ## Parameters
///
/// - `vertex_shader`: GLSL vertex shader code
/// - `fragment_shader`: GLSL fragment shader code
/// - `uniforms`: List of uniform name/value pairs
///
/// ## Example
///
/// ```gleam
/// pp.custom_shader(
///   vertex_shader: "
///     varying vec2 vUv;
///     void main() {
///       vUv = uv;
///       gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
///     }
///   ",
///   fragment_shader: "
///     uniform sampler2D tDiffuse;
///     uniform float intensity;
///     varying vec2 vUv;
///     void main() {
///       vec4 color = texture2D(tDiffuse, vUv);
///       gl_FragColor = color * intensity;
///     }
///   ",
///   uniforms: [
///     #("intensity", pp.FloatUniform(1.5)),
///   ],
/// )
/// ```
pub fn custom_shader(
  vertex_shader vertex_shader: String,
  fragment_shader fragment_shader: String,
  uniforms uniforms: List(#(String, UniformValue)),
) -> Pass {
  CustomShaderPass(
    vertex_shader: vertex_shader,
    fragment_shader: fragment_shader,
    uniforms: uniforms,
  )
}
