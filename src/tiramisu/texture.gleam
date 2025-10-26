//// Texture manipulation utilities for Three.js textures.
////
//// This module provides type-safe access to Three.js texture properties,
//// particularly useful for spritesheet animation where you need to control
//// UV offset and repeat to display different frames.
////
//// ## UV Coordinates
////
//// UV coordinates in Three.js range from 0.0 to 1.0:
//// - (0, 0) is the bottom-left corner
//// - (1, 1) is the top-right corner
////
//// ## Offset and Repeat
////
//// - **Offset**: Shifts which portion of the texture is displayed
//// - **Repeat**: Controls how much of the texture is shown (0.5 = show half)
////
//// ## Example: Spritesheet Animation
////
//// ```gleam
//// // For a 4-frame horizontal spritesheet:
//// // Frame 0: offset (0.0, 0.0), repeat (0.25, 1.0)
//// // Frame 1: offset (0.25, 0.0), repeat (0.25, 1.0)
//// // Frame 2: offset (0.5, 0.0), repeat (0.25, 1.0)
//// // Frame 3: offset (0.75, 0.0), repeat (0.25, 1.0)
////
//// let texture = asset.get_texture(cache, "spritesheet.png")
////
//// // Setup for animation
//// texture
//// |> texture.set_repeat(0.25, 1.0)  // Show 1/4 of texture width
//// |> texture.set_wrap_mode(texture.RepeatWrapping, texture.RepeatWrapping)
////
//// // Change to frame 2
//// texture.set_offset(texture, 0.5, 0.0)
//// ```

import tiramisu/asset

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
pub fn clone(texture: asset.Texture) -> asset.Texture {
  clone_ffi(texture)
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
  texture: asset.Texture,
  x x: Float,
  y y: Float,
) -> asset.Texture {
  set_offset_ffi(texture, x, y)
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
  texture: asset.Texture,
  x x: Float,
  y y: Float,
) -> asset.Texture {
  set_repeat_ffi(texture, x, y)
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
  texture: asset.Texture,
  wrap_s wrap_s: WrapMode,
  wrap_t wrap_t: WrapMode,
) -> asset.Texture {
  let s = wrap_mode_to_constant(wrap_s)
  let t = wrap_mode_to_constant(wrap_t)
  set_wrap_mode_ffi(texture, s, t)
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
  texture: asset.Texture,
  min_filter min_filter: FilterMode,
  mag_filter mag_filter: FilterMode,
) -> asset.Texture {
  let min = filter_mode_to_constant(min_filter)
  let mag = filter_mode_to_constant(mag_filter)
  set_filter_mode_ffi(texture, min, mag)
  texture
}

// ============================================================================
// Internal Helpers
// ============================================================================

fn wrap_mode_to_constant(mode: WrapMode) -> Int {
  case mode {
    RepeatWrapping -> get_repeat_wrapping_ffi()
    ClampToEdgeWrapping -> get_clamp_to_edge_wrapping_ffi()
    MirroredRepeatWrapping -> get_mirrored_repeat_wrapping_ffi()
  }
}

fn filter_mode_to_constant(mode: FilterMode) -> Int {
  case mode {
    NearestFilter -> get_nearest_filter_ffi()
    LinearFilter -> get_linear_filter_ffi()
  }
}

// ============================================================================
// FFI Declarations
// ============================================================================

@external(javascript, "../threejs.ffi.mjs", "cloneTexture")
fn clone_ffi(texture: asset.Texture) -> asset.Texture

@external(javascript, "../threejs.ffi.mjs", "setTextureOffset")
fn set_offset_ffi(texture: asset.Texture, x: Float, y: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setTextureRepeat")
fn set_repeat_ffi(texture: asset.Texture, x: Float, y: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setTextureWrapMode")
fn set_wrap_mode_ffi(texture: asset.Texture, wrap_s: Int, wrap_t: Int) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setTextureFilterMode")
fn set_filter_mode_ffi(
  texture: asset.Texture,
  min_filter: Int,
  mag_filter: Int,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getRepeatWrapping")
fn get_repeat_wrapping_ffi() -> Int

@external(javascript, "../threejs.ffi.mjs", "getClampToEdgeWrapping")
fn get_clamp_to_edge_wrapping_ffi() -> Int

@external(javascript, "../threejs.ffi.mjs", "getMirroredRepeatWrapping")
fn get_mirrored_repeat_wrapping_ffi() -> Int

@external(javascript, "../threejs.ffi.mjs", "getNearestFilter")
fn get_nearest_filter_ffi() -> Int

@external(javascript, "../threejs.ffi.mjs", "getLinearFilter")
fn get_linear_filter_ffi() -> Int
