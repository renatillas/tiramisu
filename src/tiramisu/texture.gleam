import gleam/javascript/promise.{type Promise}
import tiramisu/scene.{type Texture}

/// Texture loading error
pub type TextureError {
  LoadError(String)
  InvalidUrl(String)
}

/// Load a texture from a URL using Promises
@external(javascript, "./ffi/texture.mjs", "loadTextureAsync")
pub fn load(url: String) -> Promise(Result(Texture, TextureError))
