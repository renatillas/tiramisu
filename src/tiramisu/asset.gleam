import gleam/javascript/promise.{type Promise}
import gleam/list
import tiramisu/three/texture.{type Texture, type TextureError}

/// Asset types that can be loaded
pub type Asset {
  TextureAsset(Texture)
  // Future: ModelAsset, AudioAsset, etc.
}

/// Asset loading error
pub type AssetError {
  TextureLoadError(TextureError)
  AssetNotFound(String)
}

/// Asset metadata
pub type AssetInfo {
  AssetInfo(id: String, url: String, asset_type: AssetType)
}

/// Types of assets
pub type AssetType {
  TextureType
  // Future: ModelType, AudioType, etc.
}

/// Load a single texture asset
pub fn load_texture(url: String) -> Promise(Result(Asset, AssetError)) {
  promise.map(texture.load_async(url), fn(result) {
    case result {
      Ok(tex) -> Ok(TextureAsset(tex))
      Error(err) -> Error(TextureLoadError(err))
    }
  })
}

/// Load a texture with custom options
pub fn load_texture_with_options(
  url: String,
  options: texture.TextureOptions,
) -> Promise(Result(Asset, AssetError)) {
  promise.map(texture.load_with_options(url, options), fn(result) {
    case result {
      Ok(tex) -> Ok(TextureAsset(tex))
      Error(err) -> Error(TextureLoadError(err))
    }
  })
}

/// Load multiple assets in parallel
pub fn load_many(
  loaders: List(Promise(Result(Asset, AssetError))),
) -> Promise(Result(List(Asset), AssetError)) {
  load_many_helper(loaders, [])
}

// Helper to load assets sequentially and collect results
fn load_many_helper(
  loaders: List(Promise(Result(Asset, AssetError))),
  acc: List(Asset),
) -> Promise(Result(List(Asset), AssetError)) {
  case loaders {
    [] -> promise.resolve(Ok(list.reverse(acc)))
    [first, ..rest] ->
      promise.await(first, fn(result) {
        case result {
          Ok(asset) -> load_many_helper(rest, [asset, ..acc])
          Error(err) -> promise.resolve(Error(err))
        }
      })
  }
}

/// Extract texture from asset
pub fn get_texture(asset: Asset) -> Result(Texture, String) {
  case asset {
    TextureAsset(tex) -> Ok(tex)
  }
}
