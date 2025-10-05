import gleam/javascript/promise.{type Promise}

/// Opaque type wrapping THREE.Texture
pub type Texture

/// Texture loading error
pub type TextureError {
  LoadError(String)
  InvalidUrl(String)
}

/// Texture wrapping modes
pub type TextureWrap {
  ClampToEdge
  Repeat
  MirroredRepeat
}

/// Texture filtering modes
pub type TextureFilter {
  Nearest
  Linear
}

/// Texture configuration options
pub type TextureOptions {
  TextureOptions(
    wrap_s: TextureWrap,
    wrap_t: TextureWrap,
    min_filter: TextureFilter,
    mag_filter: TextureFilter,
  )
}

/// Default texture options (repeat with linear filtering)
pub fn default_options() -> TextureOptions {
  TextureOptions(
    wrap_s: Repeat,
    wrap_t: Repeat,
    min_filter: Linear,
    mag_filter: Linear,
  )
}

/// Load a texture from a URL (callback-based, legacy)
@external(javascript, "./ffi/texture.mjs", "loadTexture")
pub fn load(
  url: String,
  on_success: fn(Texture) -> Nil,
  on_error: fn(String) -> Nil,
) -> Nil

/// Load a texture from a URL using Promises
@external(javascript, "./ffi/texture.mjs", "loadTextureAsync")
pub fn load_async(url: String) -> Promise(Result(Texture, TextureError))

/// Load a texture with custom options
@external(javascript, "./ffi/texture.mjs", "loadTextureWithOptions")
pub fn load_with_options(
  url: String,
  options: TextureOptions,
) -> Promise(Result(Texture, TextureError))

/// Create a texture from an image
@external(javascript, "./ffi/texture.mjs", "createTexture")
pub fn create(image: Image) -> Texture

/// Set texture wrap mode for S coordinate (horizontal)
@external(javascript, "./ffi/texture.mjs", "setTextureWrapS")
pub fn set_wrap_s(texture: Texture, wrap: Int) -> Texture

/// Set texture wrap mode for T coordinate (vertical)
@external(javascript, "./ffi/texture.mjs", "setTextureWrapT")
pub fn set_wrap_t(texture: Texture, wrap: Int) -> Texture

/// Set texture filtering
@external(javascript, "./ffi/texture.mjs", "setTextureFilter")
pub fn set_filter(
  texture: Texture,
  min_filter: Int,
  mag_filter: Int,
) -> Texture

/// Update texture (marks it for re-upload to GPU)
@external(javascript, "./ffi/texture.mjs", "updateTexture")
pub fn update(texture: Texture) -> Texture

// Texture wrap constants
@external(javascript, "./ffi/texture.mjs", "ClampToEdgeWrapping")
pub fn clamp_to_edge_wrapping() -> Int

@external(javascript, "./ffi/texture.mjs", "RepeatWrapping")
pub fn repeat_wrapping() -> Int

@external(javascript, "./ffi/texture.mjs", "MirroredRepeatWrapping")
pub fn mirrored_repeat_wrapping() -> Int

// Texture filter constants
@external(javascript, "./ffi/texture.mjs", "NearestFilter")
pub fn nearest_filter() -> Int

@external(javascript, "./ffi/texture.mjs", "LinearFilter")
pub fn linear_filter() -> Int

/// Helper type for images (used when creating textures manually)
pub type Image
