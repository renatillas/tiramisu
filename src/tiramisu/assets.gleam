/// Asset Management System
///
/// Provides declarative asset loading with progress tracking, caching,
/// and batch loading capabilities.
import gleam/dict.{type Dict}
import gleam/javascript/array
import gleam/javascript/promise.{type Promise}
import gleam/option.{type Option}
import tiramisu/audio.{type AudioBuffer}
import tiramisu/gltf
import tiramisu/object3d.{type Object3D}
import tiramisu/scene.{type BufferGeometry, type Texture}
import tiramisu/stl
import tiramisu/texture

// --- Public Types ---

/// Types of assets that can be loaded
pub type AssetType {
  /// GLTF/GLB 3D model
  ModelAsset(url: String)
  /// Texture image (PNG, JPG, etc.)
  TextureAsset(url: String)
  /// Audio file (MP3, WAV, OGG)
  AudioAsset(url: String)
  /// STL 3D model
  STLAsset(url: String)
}

/// A loaded asset (opaque to enforce type safety)
pub opaque type LoadedAsset {
  LoadedModel(data: gltf.GLTFData)
  LoadedTexture(texture: Texture)
  LoadedAudio(audio: AudioBuffer)
  LoadedSTL(geometry: BufferGeometry)
}

// Internal constructors for FFI use
@internal
pub fn loaded_model(data: gltf.GLTFData) -> LoadedAsset {
  LoadedModel(data)
}

@internal
pub fn loaded_texture(texture: Texture) -> LoadedAsset {
  LoadedTexture(texture)
}

@internal
pub fn loaded_audio(audio: AudioBuffer) -> LoadedAsset {
  LoadedAudio(audio)
}

@internal
pub fn loaded_stl(geometry: BufferGeometry) -> LoadedAsset {
  LoadedSTL(geometry)
}

/// Asset loading error
pub type AssetError {
  AssetLoadError(url: String, reason: String)
  AssetNotFound(url: String)
  InvalidAssetType(url: String)
}

/// Progress information for batch loading
pub type LoadProgress {
  LoadProgress(loaded: Int, total: Int, current_url: String)
}

/// Asset cache/registry
pub opaque type AssetCache {
  AssetCache(assets: Dict(String, LoadedAsset))
}

// --- Cache Management ---

/// Create a new empty asset cache
pub fn new_cache() -> AssetCache {
  AssetCache(assets: dict.new())
}

/// Get the number of cached assets
pub fn cache_size(cache: AssetCache) -> Int {
  dict.size(cache.assets)
}

/// Check if an asset is cached
pub fn is_cached(cache: AssetCache, url: String) -> Bool {
  dict.has_key(cache.assets, url)
}

/// Clear all cached assets
pub fn clear_cache(_cache: AssetCache) -> AssetCache {
  AssetCache(assets: dict.new())
}

// --- Single Asset Loading ---

/// Load a single asset
pub fn load_asset(asset: AssetType) -> Promise(Result(LoadedAsset, AssetError)) {
  case asset {
    ModelAsset(url) -> {
      promise.map(gltf.load(url), fn(result) {
        case result {
          Ok(data) -> Ok(LoadedModel(data))
          Error(gltf.LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(gltf.InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(gltf.ParseError(msg)) -> Error(AssetLoadError(url, msg))
        }
      })
    }

    TextureAsset(url) -> {
      promise.map(texture.load(url), fn(result) {
        case result {
          Ok(tex) -> Ok(LoadedTexture(tex))
          Error(texture.LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(texture.InvalidUrl(_)) ->
            Error(AssetLoadError(url, "Invalid URL"))
        }
      })
    }

    AudioAsset(url) -> {
      promise.map(load_audio_ffi(url), fn(result) {
        case result {
          Ok(audio) -> Ok(LoadedAudio(audio))
          Error(msg) -> Error(AssetLoadError(url, msg))
        }
      })
    }

    STLAsset(url) -> {
      promise.map(stl.load(url), fn(result) {
        case result {
          Ok(geom) -> Ok(LoadedSTL(geom))
          Error(stl.LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(stl.InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(stl.ParseError(msg)) -> Error(AssetLoadError(url, msg))
        }
      })
    }
  }
}

// --- Batch Loading with Progress ---

/// Result of batch loading
pub type BatchLoadResult {
  BatchLoadResult(cache: AssetCache, errors: List(AssetError))
}

/// Load multiple assets with progress tracking
/// Returns a promise that resolves with the loaded assets and any errors
pub fn load_batch(
  assets: List(AssetType),
  on_progress: fn(LoadProgress) -> Nil,
) -> Promise(BatchLoadResult) {
  // Convert list to array for FFI
  let assets_array = array.from_list(assets)
  load_batch_ffi(assets_array, on_progress)
}

/// Load multiple assets without progress tracking
pub fn load_batch_simple(assets: List(AssetType)) -> Promise(BatchLoadResult) {
  let assets_array = array.from_list(assets)
  load_batch_ffi(assets_array, fn(_) { Nil })
}

// --- Asset Retrieval ---

/// Get a GLTF model from the cache
pub fn get_model(
  cache: AssetCache,
  url: String,
) -> Result(gltf.GLTFData, AssetError) {
  case dict.get(cache.assets, url) {
    Ok(LoadedModel(data)) -> Ok(data)
    Ok(_) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get the scene object from a cached GLTF model
pub fn get_model_scene(
  cache: AssetCache,
  url: String,
) -> Result(Object3D, AssetError) {
  case get_model(cache, url) {
    Ok(data) -> Ok(data.scene)
    Error(err) -> Error(err)
  }
}

/// Get a texture from the cache
pub fn get_texture(
  cache: AssetCache,
  url: String,
) -> Result(Texture, AssetError) {
  case dict.get(cache.assets, url) {
    Ok(LoadedTexture(tex)) -> Ok(tex)
    Ok(_) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get an audio buffer from the cache
pub fn get_audio(
  cache: AssetCache,
  url: String,
) -> Result(AudioBuffer, AssetError) {
  case dict.get(cache.assets, url) {
    Ok(LoadedAudio(audio)) -> Ok(audio)
    Ok(_) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get an STL geometry from the cache
pub fn get_stl(
  cache: AssetCache,
  url: String,
) -> Result(BufferGeometry, AssetError) {
  case dict.get(cache.assets, url) {
    Ok(LoadedSTL(geom)) -> Ok(geom)
    Ok(_) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Try to get any asset (returns Option)
pub fn try_get(cache: AssetCache, url: String) -> Option(LoadedAsset) {
  dict.get(cache.assets, url)
  |> option.from_result
}

/// Get all cached URLs
pub fn cached_urls(cache: AssetCache) -> List(String) {
  dict.keys(cache.assets)
}

/// Insert a loaded asset into the cache manually
pub fn insert_asset(
  cache: AssetCache,
  url: String,
  asset: LoadedAsset,
) -> AssetCache {
  AssetCache(assets: dict.insert(cache.assets, url, asset))
}

// --- FFI Functions ---

@external(javascript, "./ffi/assets.mjs", "loadAudio")
fn load_audio_ffi(url: String) -> Promise(Result(AudioBuffer, String))

@external(javascript, "./ffi/assets.mjs", "loadBatch")
fn load_batch_ffi(
  assets: array.Array(AssetType),
  on_progress: fn(LoadProgress) -> Nil,
) -> Promise(BatchLoadResult)
