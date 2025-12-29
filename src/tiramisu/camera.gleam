//// Camera configuration and post-processing effects.
////
//// Cameras define how your 3D scene is projected onto the 2D screen. This module provides
//// both perspective (3D) and orthographic (2D) camera types, plus a full post-processing
//// pipeline for visual effects.
////
//// ## Camera Types
////
//// ```gleam
//// // Perspective camera for 3D games
//// let assert Ok(cam) = camera.perspective(
////   field_of_view: 75.0,
////   near: 0.1,
////   far: 1000.0,
//// )
////
//// // Orthographic camera for 2D games
//// let cam = camera.camera_2d(size: vec2.Vec2(800, 600))
//// ```
////
//// ## Adding to Scene
////
//// ```gleam
//// scene.camera(
////   id: "main-camera",
////   camera: cam,
////   transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
////   look_at: Some(vec3.Vec3(0.0, 0.0, 0.0)),
////   active: True,
////   viewport: None,
////   postprocessing: None,
//// )
//// ```
////
//// ## Post-Processing
////
//// Add visual effects like bloom, vignette, and film grain:
////
//// ```gleam
//// let pp = camera.new_postprocessing()
////   |> camera.add_pass(camera.clear_pass(None))
////   |> camera.add_pass(camera.render_pass())
////   |> camera.add_pass(camera.bloom(strength: 1.0, threshold: 0.8, radius: 0.4))
////   |> camera.add_pass(camera.fxaa())
////   |> camera.add_pass(camera.output_pass())
////
//// scene.camera(
////   id: "main-camera",
////   camera: cam,
////   // ...
////   postprocessing: Some(pp),
//// )
//// ```
////
//// ## Viewports
////
//// Create split-screen or picture-in-picture views:
////
//// ```gleam
//// let minimap = camera.ViewPort(
////   position: vec2.Vec2(650, 10),
////   size: vec2.Vec2(150, 100),
//// )
//// ```
////

import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option}
import vec/vec2.{type Vec2}

/// Camera configuration (perspective or orthographic projection).
///
/// Use with `scene.Camera` nodes to define viewpoints in your scene.
/// Position and orientation are set via the scene.Camera node's transform and look_at fields.
pub opaque type Camera {
  Camera(projection: CameraProjection)
}

@internal
pub type CameraProjection {
  Perspective(fov: Float, aspect: Float, near: Float, far: Float)
  Orthographic(
    left: Float,
    right: Float,
    top: Float,
    bottom: Float,
    near: Float,
    far: Float,
  )
}

/// Validation errors for camera creation.
pub type CameraError {
  /// Field of view must be between 0 and 180 degrees
  InvalidFieldOfView(Float)
  /// Aspect ratio must be positive
  InvalidAspectRatio(Float)
  /// Near plane must be positive
  InvalidNearPlane(Float)
  /// Far plane must be positive
  InvalidFarPlane(Float)
  /// Near plane must be less than far plane
  NearFarConflict(near: Float, far: Float)
}

/// Create a perspective camera (for 3D games).
///
/// Objects further away appear smaller, like in real life.
/// The aspect ratio is automatically calculated from the viewport or renderer dimensions at render time.
///
/// ## Parameters
/// - `field_of_view`: Vertical FOV in degrees (typically 60-90)
/// - `near`: Near clipping plane (objects closer are not rendered)
/// - `far`: Far clipping plane (objects further are not rendered)
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cam) = camera.perspective(
///   field_of_view: 75.0,
///   near: 0.1,
///   far: 1000.0,
/// )
/// ```
pub fn perspective(
  field_of_view fov: Float,
  near near: Float,
  far far: Float,
) -> Result(Camera, CameraError) {
  use <- bool.guard(
    fov <=. 0.0 || fov >=. 180.0,
    Error(InvalidFieldOfView(fov)),
  )
  use <- bool.guard(near <=. 0.0, Error(InvalidNearPlane(near)))
  use <- bool.guard(far <=. 0.0, Error(InvalidFarPlane(far)))
  use <- bool.guard(near >=. far, Error(NearFarConflict(near, far)))

  // Aspect ratio will be calculated at render time based on viewport/renderer dimensions
  // Use 1.0 as placeholder - the FFI layer will calculate the correct aspect
  Ok(
    Camera(projection: Perspective(fov: fov, aspect: 1.0, near: near, far: far)),
  )
}

/// Create an orthographic camera (for 2D games or isometric views).
///
/// No perspective distortion - objects are the same size regardless of distance.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.orthographic(
///   left: -400.0, right: 400.0,
///   top: 300.0, bottom: -300.0,
///   near: 0.1, far: 1000.0,
/// )
/// ```
pub fn orthographic(
  left left: Float,
  right right: Float,
  top top: Float,
  bottom bottom: Float,
  near near: Float,
  far far: Float,
) -> Camera {
  Camera(projection: Orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: near,
    far: far,
  ))
}

/// Create a 2D camera centered at origin with world coordinates.
///
/// Useful for 2D games where (0,0) is the center of the screen.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d(size: vec2.Vec2(800, 600))
/// scene.Camera(
///   id: "main_camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.None,
/// )
/// // (0, 0) is screen center, positive Y is up
/// ```
pub fn camera_2d(size size: Vec2(Int)) -> Camera {
  let w = int.to_float(size.x)
  let h = int.to_float(size.y)
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  orthographic(
    left: 0.0 -. half_w,
    right: half_w,
    top: half_h,
    bottom: 0.0 -. half_h,
    near: 0.1,
    far: 1000.0,
  )
}

/// Create a 2D camera with screen-space coordinates (top-left origin).
///
/// Useful for UI or pixel-perfect 2D games where (0,0) is top-left corner.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d_screen_space(size: vec2.Vec2(800, 600))
/// scene.Camera(
///   id: "ui_camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.None,
/// )
/// // (0, 0) is top-left, positive Y is down (like CSS)
/// ```
pub fn camera_2d_screen_space(size size: Vec2(Int)) -> Camera {
  let w = int.to_float(size.x)
  let h = int.to_float(size.y)

  orthographic(
    left: 0.0,
    right: w,
    top: 0.0,
    bottom: 0.0 -. h,
    near: 0.1,
    far: 1000.0,
  )
}

/// Create a 2D camera with custom bounds.
///
/// ## Example
///
/// ```gleam
/// let cam = camera.camera_2d_with_bounds(
///   left: -100.0, right: 100.0,
///   top: 75.0, bottom: -75.0,
/// )
/// scene.Camera(
///   id: "game_camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.None,
/// )
/// ```
pub fn camera_2d_with_bounds(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
) -> Camera {
  orthographic(
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    near: 0.1,
    far: 1000.0,
  )
}

/// Viewport configuration for split-screen or picture-in-picture rendering.
///
/// Coordinates are in pixels from the top-left of the canvas.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/camera
/// import tiramisu/scene
/// import vec/vec2
///
/// // Create a viewport in the top-right corner
/// let minimap_viewport = camera.ViewPort(
///   position: vec2.Vec2(650, 10),
///   size: vec2.Vec2(150, 100),
/// )
///
/// scene.camera(
///   id: "minimap",
///   camera: minimap_cam,
///   transform: transform.identity,
///   look_at: option.None,
///   active: True,
///   viewport: option.Some(minimap_viewport),
///   postprocessing: option.None,
/// )
/// ```
pub type ViewPort {
  ViewPort(
    /// Position from top-left corner in pixels (x, y)
    position: Vec2(Int),
    /// Size in pixels (width, height)
    size: Vec2(Int),
  )
}

/// Internal function to get the camera projection
///
/// Used by the internal renderer to create Three.js cameras
@internal
pub fn get_projection(camera: Camera) -> CameraProjection {
  camera.projection
}

// ============================================================================
// POST-PROCESSING
// ============================================================================

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
/// Start with this and add passes using `postprocessing_add_pass`.
///
/// ## Example
///
/// ```gleam
/// let pp = camera.new_postprocessing()
///   |> camera.add_pass(camera.clear_pass(option.None))
///   |> camera.add_pass(camera.render_pass())
///   |> camera.add_pass(camera.bloom(...))
///   |> camera.add_pass(camera.fxaa())
///   |> camera.add_pass(camera.output_pass())
/// ```
pub fn new_postprocessing() -> PostProcessing {
  PostProcessing(passes: [])
}

/// Add a pass to the pipeline.
///
/// Passes are executed in the order they are added.
///
/// ## Example
///
/// ```gleam
/// camera.postprocessing_new()
/// |> camera.add_pass(camera.bloom(
///   strength: 1.5,
///   threshold: 0.85,
///   radius: 0.4,
/// ))
/// |> camera.postprocessing_add_pass(camera.fxaa())
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
// PASS CONSTRUCTORS
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
/// camera.postprocessing_new()
/// |> camera.postprocessing_add_pass(camera.render_pass())
/// |> camera.postprocessing_add_pass(camera.bloom(...))
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
/// camera.clear_pass(option.None)
///
/// // Use custom color
/// camera.clear_pass(option.Some(0x000000))  // Black
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
/// camera.postprocessing_new()
/// |> camera.postprocessing_add_pass(camera.render_pass())
/// |> camera.postprocessing_add_pass(camera.bloom(...))
/// |> camera.postprocessing_add_pass(camera.output_pass())  // Last pass
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
/// camera.bloom(strength: 0.8, threshold: 0.85, radius: 0.4)
///
/// // Intense bloom for sci-fi effect
/// camera.bloom(strength: 2.0, threshold: 0.5, radius: 0.8)
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
/// camera.pixelate(pixel_size: 2)
///
/// // Strong retro effect
/// camera.pixelate(pixel_size: 8)
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
/// camera.pixelate_with_edges(
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
/// camera.film_grain(
///   noise_intensity: 0.2,
///   scanline_intensity: 0.0,
///   scanline_count: 0,
///   grayscale: False,
/// )
///
/// // Retro CRT monitor effect
/// camera.film_grain(
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
/// camera.vignette(darkness: 0.5, offset: 1.0)
///
/// // Dramatic vignette
/// camera.vignette(darkness: 1.5, offset: 0.8)
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
/// camera.postprocessing_new()
/// |> camera.postprocessing_add_pass(camera.bloom(...))
/// |> camera.postprocessing_add_pass(camera.fxaa())  // Last pass
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
/// camera.glitch(dt_size: 64)
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
/// camera.color_correction(
///   brightness: 0.2,
///   contrast: 0.1,
///   saturation: 0.3,
/// )
///
/// // Desaturated look
/// camera.color_correction(
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
/// camera.custom_shader(
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
///     #("intensity", camera.FloatUniform(1.5)),
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
