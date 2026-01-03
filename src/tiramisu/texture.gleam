//// Texture loading and manipulation.
////
//// Textures are images applied to materials or used as sprites. This module
//// provides loading, cloning, and UV manipulation for texture coordinates.
////
//// ## Loading Textures
////
//// ```gleam
//// fn init(ctx: Context) {
////   #(
////     Model(texture: option.None),
////     texture.load(
////       from_url: "/textures/player.png",
////       on_success: TextureLoaded,
////       on_error: TextureFailed,
////     ),
////     None,
////   )
//// }
//// ```
////
//// ## Spritesheet UV Manipulation
////
//// For animated sprites, use offset and repeat to show specific frames:
////
//// ```gleam
//// // 4x4 spritesheet: show frame at row 1, column 2
//// texture.set_repeat(tex, vec2.Vec2(0.25, 0.25))  // Each frame is 1/4 of texture
//// texture.set_offset(tex, vec2.Vec2(0.5, 0.25))   // Column 2, row 1
//// ```
////
//// ## Texture Filtering
////
//// ```gleam
//// // Pixel art: use nearest neighbor for crisp pixels
//// texture.set_filter_mode(tex, texture.NearestFilter, texture.NearestFilter)
////
//// // Smooth textures: use linear (default)
//// texture.set_filter_mode(tex, texture.LinearFilter, texture.LinearFilter)
//// ```
////

import gleam/javascript/promise
import savoiardi
import tiramisu/effect
import vec/vec2.{type Vec2}

pub type Texture =
  savoiardi.Texture

/// Texture wrapping mode.
///
/// Controls how textures behave when UV coordinates exceed the 0-1 range.
///
/// ## Variants
///
/// - `RepeatWrapping` - Texture repeats infinitely (requires power-of-two dimensions)
/// - `ClampToEdgeWrapping` - Edge pixels stretch infinitely (works with any dimensions)
/// - `MirroredRepeatWrapping` - Texture repeats with alternating mirrored copies
pub type WrapMode {
  RepeatWrapping
  ClampToEdgeWrapping
  MirroredRepeatWrapping
}

/// Texture filtering mode.
///
/// Controls how textures are sampled when scaled.
///
/// ## Variants
///
/// - `NearestFilter` - No interpolation, uses nearest pixel (best for pixel art)
/// - `LinearFilter` - Bilinear interpolation for smooth texture sampling (default)
pub type FilterMode {
  NearestFilter
  LinearFilter
}

// Convert tiramisu WrapMode to savoiardi WrapMode
fn to_savoiardi_wrap_mode(mode: WrapMode) -> savoiardi.WrapMode {
  case mode {
    RepeatWrapping -> savoiardi.RepeatWrapping
    ClampToEdgeWrapping -> savoiardi.ClampToEdgeWrapping
    MirroredRepeatWrapping -> savoiardi.MirroredRepeatWrapping
  }
}

// Convert tiramisu FilterMode to savoiardi FilterMode
fn to_savoiardi_filter_mode(mode: FilterMode) -> savoiardi.FilterMode {
  case mode {
    NearestFilter -> savoiardi.NearestFilter
    LinearFilter -> savoiardi.LinearFilter
  }
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

/// Sets the texture UV offset.
///
/// Controls which portion of the texture starts being displayed.
/// Values range from 0.0 to 1.0.
///
/// ## Example
///
/// ```gleam
/// // Show the right half of the texture
/// texture.set_offset(my_texture, vec2.Vec2(0.5, 0.0))
/// ```
pub fn set_offset(
  texture: savoiardi.Texture,
  offset offset: Vec2(Float),
) -> savoiardi.Texture {
  savoiardi.set_texture_offset(texture, offset.x, offset.y)
  texture
}

/// Sets the texture UV repeat (scaling).
///
/// Controls how much of the texture is displayed.
/// Values range from 0.0 to 1.0 (or higher for tiling).
///
/// ## Example
///
/// ```gleam
/// // Show only 1/4 of texture width (for 4-frame horizontal sprite)
/// texture.set_repeat(my_texture, vec2.Vec2(0.25, 1.0))
/// ```
pub fn set_repeat(
  texture: savoiardi.Texture,
  repeat repeat: Vec2(Float),
) -> savoiardi.Texture {
  savoiardi.set_texture_repeat(texture, repeat.x, repeat.y)
  texture
}

/// Sets the texture wrapping mode.
///
/// Controls how the texture behaves at edges when UV coordinates exceed 0-1 range.
///
/// **Important**: RepeatWrapping only works with power-of-two texture dimensions.
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
  savoiardi.set_texture_wrap_mode(
    texture,
    to_savoiardi_wrap_mode(wrap_s),
    to_savoiardi_wrap_mode(wrap_t),
  )
  texture
}

/// Sets the texture filtering mode.
///
/// Controls how the texture is sampled when scaled.
/// Use NearestFilter for crisp pixel art, LinearFilter for smooth textures.
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
  savoiardi.set_texture_filter_mode(
    texture,
    to_savoiardi_filter_mode(min_filter),
    to_savoiardi_filter_mode(mag_filter),
  )
  texture
}

/// Load a texture from URL
pub fn load(
  from_url url: String,
  on_success on_success: fn(Texture) -> msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  let promise =
    savoiardi.load_texture(url)
    |> promise.map(fn(result) {
      case result {
        Ok(data) -> on_success(data)
        Error(Nil) -> on_error
      }
    })

  effect.from_promise(promise)
}
