import gleam/javascript/promise
import savoiardi

pub type Texture =
  savoiardi.Texture

pub type CubeTexture =
  savoiardi.CubeTexture

/// Texture wrapping mode
pub type WrapMode {
  /// Repeat the texture (requires power-of-two dimensions)
  RepeatWrapping
  /// Clamp to edge (default, works with any dimensions)
  ClampToEdgeWrapping
  /// Mirror the texture when repeating
  MirroredRepeatWrapping
}

/// Texture filtering mode
pub type FilterMode {
  /// Nearest neighbor filtering (best for pixel art)
  NearestFilter
  /// Linear interpolation filtering (smooth, default)
  LinearFilter
}

/// Clone a texture for independent manipulation.
///
/// This is essential for spritesheet animation when you want multiple sprites
/// to show different frames from the same source texture.
///
/// ## Example
///
/// ```gleam
/// let base_texture = asset.get_texture(cache, "spritesheet.png")
/// let player1_texture = texture.clone(base_texture)
/// let player2_texture = texture.clone(base_texture)
///
/// // Now player1 and player2 can show different frames
/// texture.set_offset(player1_texture, 0.0, 0.0)  // Frame 0
/// texture.set_offset(player2_texture, 0.25, 0.0) // Frame 1
/// ```
pub fn clone(texture: savoiardi.Texture) -> savoiardi.Texture {
  savoiardi.clone_texture(texture)
}

/// Set the texture UV offset.
///
/// Controls which portion of the texture starts being displayed.
/// Values range from 0.0 to 1.0.
///
/// ## Parameters
///
/// - `x`: Horizontal offset (0.0 = left edge, 1.0 = right edge)
/// - `y`: Vertical offset (0.0 = bottom edge, 1.0 = top edge)
///
/// ## Example
///
/// ```gleam
/// // Show the right half of the texture
/// texture.set_offset(my_texture, 0.5, 0.0)
/// ```
pub fn set_offset(
  texture: savoiardi.Texture,
  x x: Float,
  y y: Float,
) -> savoiardi.Texture {
  savoiardi.set_texture_offset(texture, x, y)
  texture
}

/// Set the texture UV repeat (scaling).
///
/// Controls how much of the texture is displayed.
/// Values range from 0.0 to 1.0 (or higher for tiling).
///
/// ## Parameters
///
/// - `x`: Horizontal repeat (0.5 = show half width, 2.0 = tile twice)
/// - `y`: Vertical repeat (0.5 = show half height, 2.0 = tile twice)
///
/// ## Example
///
/// ```gleam
/// // Show only 1/4 of texture width (for 4-frame horizontal sprite)
/// texture.set_repeat(my_texture, 0.25, 1.0)
/// ```
pub fn set_repeat(
  texture: savoiardi.Texture,
  x x: Float,
  y y: Float,
) -> savoiardi.Texture {
  savoiardi.set_texture_repeat(texture, x, y)
  texture
}

/// Set the texture wrapping mode.
///
/// Controls how the texture behaves at edges when UV coordinates exceed 0-1 range.
///
/// **Important**: RepeatWrapping only works with power-of-two texture dimensions
/// (2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, etc.)
///
/// ## Parameters
///
/// - `wrap_s`: Horizontal wrapping mode
/// - `wrap_t`: Vertical wrapping mode
///
/// ## Example
///
/// ```gleam
/// // Required for spritesheet animation
/// texture.set_wrap_mode(
///   my_texture,
///   texture.RepeatWrapping,
///   texture.RepeatWrapping,
/// )
/// ```
pub fn set_wrap_mode(
  texture: savoiardi.Texture,
  wrap_s wrap_s: WrapMode,
  wrap_t wrap_t: WrapMode,
) -> savoiardi.Texture {
  let s = wrap_mode_to_constant(wrap_s)
  let t = wrap_mode_to_constant(wrap_t)
  savoiardi.set_texture_wrap_mode(texture, s, t)
  texture
}

/// Set the texture filtering mode.
///
/// Controls how the texture is sampled when scaled.
///
/// ## Parameters
///
/// - `min_filter`: Minification filter (when texture appears smaller)
/// - `mag_filter`: Magnification filter (when texture appears larger)
///
/// ## Example
///
/// ```gleam
/// // Use nearest filtering for crisp pixel art
/// texture.set_filter_mode(
///   my_texture,
///   texture.NearestFilter,
///   texture.NearestFilter,
/// )
/// ```
pub fn set_filter_mode(
  texture: savoiardi.Texture,
  min_filter min_filter: FilterMode,
  mag_filter mag_filter: FilterMode,
) -> savoiardi.Texture {
  let min = filter_mode_to_constant(min_filter)
  let mag = filter_mode_to_constant(mag_filter)
  savoiardi.set_texture_filter_mode(texture, min, mag)
  texture
}

// ============================================================================
// Internal Helpers
// ============================================================================

fn wrap_mode_to_constant(mode: WrapMode) -> Int {
  case mode {
    RepeatWrapping -> savoiardi.get_repeat_wrapping()
    ClampToEdgeWrapping -> savoiardi.get_clamp_to_edge_wrapping()
    MirroredRepeatWrapping -> savoiardi.get_mirrored_repeat_wrapping()
  }
}

fn filter_mode_to_constant(mode: FilterMode) -> Int {
  case mode {
    NearestFilter -> savoiardi.get_nearest_filter()
    LinearFilter -> savoiardi.get_linear_filter()
  }
}

/// Load a texture from URL
pub fn load(from_url url: String) -> promise.Promise(Result(Texture, Nil)) {
  savoiardi.load_texture(url)
}

/// Load an equirectangular (360°) texture from URL
pub fn load_equirectangular(
  url: String,
) -> promise.Promise(Result(Texture, Nil)) {
  savoiardi.load_equirectangular_texture(url)
}

/// Load a cube texture (skybox) from 6 URLs
pub fn load_cube(
  urls: List(String),
) -> promise.Promise(Result(CubeTexture, Nil)) {
  savoiardi.load_cube_texture(urls)
}
