//// Asset Management System - loading and caching for textures, models, and audio.
////
//// Provides declarative asset loading with progress tracking, caching,
//// and batch loading capabilities with LRU eviction.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/asset
////
//// // Load a texture
//// let load_effect = asset.load_texture("player.png")
////   |> promise.map(fn(result) {
////     case result {
////       Ok(texture) -> TextureLoaded(texture)
////       Error(err) -> LoadFailed(err)
////     }
////   })
////   |> effect.from_promise
////
//// // Use with cache
//// let cache = asset.new_cache()
//// let cache = asset.insert_texture(cache, "player.png", texture)
//// let texture = asset.get_texture(cache, "player.png")
//// ```

import gleam/dict.{type Dict}
import gleam/javascript/array
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/option.{type Option}
import gleam/order
import tiramisu/audio.{type AudioBuffer}
import tiramisu/object3d.{type AnimationClip, type Object3D}

/// Opaque type for Three.js textures.
///
/// Created via `asset.load_texture()` and used in materials.
pub type Texture

/// Opaque type for Three.js BufferGeometry.
///
/// Created by loading 3D models with `asset.load_stl()` or `asset.load_model()`.
pub type BufferGeometry

// --- Public Types ---
/// STL loading error
pub type LoadError {
  LoadError(String)
  InvalidUrl(String)
  ParseError(String)
}

pub type GLTFData {
  GLTFData(scene: Object3D, animations: List(AnimationClip))
}

/// Types of asset that can be loaded
pub type AssetType {
  /// GLTF/GLB 3D model
  ModelAsset(url: String)
  /// Texture image (PNG, JPG, etc.)
  TextureAsset(url: String)
  /// Audio file (MP3, WAV, OGG)
  AudioAsset(url: String)
  /// STL 3D model
  STLAsset(url: String)
  /// OBJ 3D model with optional MTL material file
  ///
  /// Supports loading Wavefront OBJ models with their associated MTL material
  /// files. The loader automatically:
  /// - Loads diffuse color maps (map_Kd)
  /// - Loads normal maps (map_bump) for surface detail
  /// - Loads ambient occlusion maps (map_Ka) for realistic shadows
  /// - Centers the model at origin
  /// - Computes vertex normals if missing
  ///
  /// MTL files can include texture paths with options (e.g., "map_bump -bm 1 texture.jpg"),
  /// which are properly parsed. Textures are assumed to be in the same directory as
  /// the MTL file.
  OBJAsset(obj_url: String, mtl_url: option.Option(String))
}

/// A loaded asset (opaque to enforce type safety)
pub opaque type LoadedAsset {
  LoadedModel(data: GLTFData)
  LoadedTexture(texture: Texture)
  LoadedAudio(audio: AudioBuffer)
  LoadedSTL(geometry: BufferGeometry)
  LoadedOBJ(object: Object3D)
}

// Internal constructors for FFI use
@internal
pub fn loaded_model(data: GLTFData) -> LoadedAsset {
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

@internal
pub fn loaded_obj(object: Object3D) -> LoadedAsset {
  LoadedOBJ(object)
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

/// LRU (Least Recently Used) entry with access timestamp
type CacheEntry {
  CacheEntry(asset: LoadedAsset, last_accessed: Int)
}

/// Asset cache configuration
pub type CacheConfig {
  CacheConfig(max_size: Int, current_time: Int)
}

/// Asset cache/registry with LRU eviction
pub opaque type AssetCache {
  AssetCache(asset: Dict(String, CacheEntry), config: CacheConfig)
}

// --- Cache Management ---

/// Create a new empty asset cache with default max size (100 asset)
pub fn new_cache() -> AssetCache {
  AssetCache(
    asset: dict.new(),
    config: CacheConfig(max_size: 100, current_time: 0),
  )
}

/// Create a new empty asset cache with custom max size
pub fn new_cache_with_size(max_size: Int) -> AssetCache {
  AssetCache(
    asset: dict.new(),
    config: CacheConfig(max_size: max_size, current_time: 0),
  )
}

/// Get the number of cached asset
pub fn cache_size(cache: AssetCache) -> Int {
  dict.size(cache.asset)
}

/// Check if an asset is cached
pub fn is_cached(cache: AssetCache, url: String) -> Bool {
  dict.has_key(cache.asset, url)
}

/// Clear all cached asset
pub fn clear_cache(cache: AssetCache) -> AssetCache {
  AssetCache(asset: dict.new(), config: cache.config)
}

// --- Single Asset Loading ---

/// Load a single asset
pub fn load_asset(asset: AssetType) -> Promise(Result(LoadedAsset, AssetError)) {
  case asset {
    ModelAsset(url) -> {
      promise.map(load_gltf(url), fn(result) {
        case result {
          Ok(data) -> Ok(LoadedModel(data))
          Error(LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(ParseError(msg)) -> Error(AssetLoadError(url, msg))
        }
      })
    }

    TextureAsset(url) -> {
      promise.map(load_texture(url), fn(result) {
        case result {
          Ok(tex) -> Ok(LoadedTexture(tex))
          Error(LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(ParseError(_)) ->
            Error(AssetLoadError(url, "Failed to parse texture"))
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
      promise.map(load_stl(url), fn(result) {
        case result {
          Ok(geom) -> Ok(LoadedSTL(geom))
          Error(LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(ParseError(msg)) -> Error(AssetLoadError(url, msg))
        }
      })
    }

    OBJAsset(obj_url, mtl_url) -> {
      let mtl_url_str = case mtl_url {
        option.Some(url) -> url
        option.None -> ""
      }
      promise.map(load_obj(obj_url, mtl_url_str), fn(result) {
        case result {
          Ok(object) -> Ok(LoadedOBJ(object))
          Error(LoadError(msg)) -> Error(AssetLoadError(obj_url, msg))
          Error(InvalidUrl(_)) -> Error(AssetLoadError(obj_url, "Invalid URL"))
          Error(ParseError(msg)) -> Error(AssetLoadError(obj_url, msg))
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

/// Load multiple asset with progress tracking
/// Returns a promise that resolves with the loaded asset and any errors
pub fn load_batch(
  asset: List(AssetType),
  on_progress: fn(LoadProgress) -> Nil,
) -> Promise(BatchLoadResult) {
  // Convert list to array for FFI
  let asset_array = array.from_list(asset)
  load_batch_ffi(asset_array, on_progress)
}

/// Load multiple asset without progress tracking
pub fn load_batch_simple(asset: List(AssetType)) -> Promise(BatchLoadResult) {
  let asset_array = array.from_list(asset)
  load_batch_ffi(asset_array, fn(_) { Nil })
}

// --- Asset Retrieval ---

/// Get a GLTF model from the cache (updates LRU timestamp)
pub fn get_model(cache: AssetCache, url: String) -> Result(GLTFData, AssetError) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(LoadedModel(data), _)) -> Ok(data)
    Ok(CacheEntry(_, _)) -> Error(InvalidAssetType(url))
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
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(LoadedTexture(tex), _)) -> Ok(tex)
    Ok(CacheEntry(_, _)) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get an audio buffer from the cache
pub fn get_audio(
  cache: AssetCache,
  url: String,
) -> Result(AudioBuffer, AssetError) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(LoadedAudio(audio), _)) -> Ok(audio)
    Ok(CacheEntry(_, _)) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get an STL geometry from the cache
pub fn get_stl(
  cache: AssetCache,
  url: String,
) -> Result(BufferGeometry, AssetError) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(LoadedSTL(geom), _)) -> Ok(geom)
    Ok(CacheEntry(_, _)) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get an OBJ model from the cache
///
/// Returns the loaded OBJ model as an Object3D. The model will have:
/// - Materials with textures applied (if MTL file was loaded)
/// - Vertex normals computed
/// - Center position at origin
///
/// ## Example
///
/// ```gleam
/// let assert Ok(bread_model) = asset.get_obj(cache, "models/bread.obj")
///
/// scene.Model3D(
///   id: "bread",
///   object: bread_model,
///   transform: transform.identity,
///   animation: option.None,
///   physics: option.None,
/// )
/// ```
pub fn get_obj(cache: AssetCache, url: String) -> Result(Object3D, AssetError) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(LoadedOBJ(object), _)) -> Ok(object)
    Ok(CacheEntry(_, _)) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Try to get any asset (returns Option)
pub fn try_get(cache: AssetCache, url: String) -> Option(LoadedAsset) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(asset, _)) -> option.Some(asset)
    Error(_) -> option.None
  }
}

/// Get all cached URLs
pub fn cached_urls(cache: AssetCache) -> List(String) {
  dict.keys(cache.asset)
}

/// Insert a loaded asset into the cache manually
/// If cache exceeds max_size, evicts least recently used asset
pub fn insert_asset(
  cache: AssetCache,
  url: String,
  asset: LoadedAsset,
) -> AssetCache {
  let new_time = cache.config.current_time + 1
  let entry = CacheEntry(asset, new_time)
  let new_asset = dict.insert(cache.asset, url, entry)

  // Check if we need to evict
  let new_cache =
    AssetCache(
      asset: new_asset,
      config: CacheConfig(..cache.config, current_time: new_time),
    )

  case dict.size(new_asset) > cache.config.max_size {
    True -> evict_lru(new_cache)
    False -> new_cache
  }
}

/// Evict the least recently used asset from the cache
fn evict_lru(cache: AssetCache) -> AssetCache {
  // Find the URL with the oldest timestamp
  case
    dict.to_list(cache.asset)
    |> list.sort(fn(a, b) {
      let #(_, CacheEntry(_, time_a)) = a
      let #(_, CacheEntry(_, time_b)) = b
      case time_a < time_b {
        True -> order.Lt
        False ->
          case time_a > time_b {
            True -> order.Gt
            False -> order.Eq
          }
      }
    })
    |> list.first
  {
    Ok(#(url_to_evict, _)) -> {
      let new_asset = dict.delete(cache.asset, url_to_evict)
      AssetCache(asset: new_asset, config: cache.config)
    }
    Error(_) -> cache
  }
}

// --- Resource Disposal ---

/// Dispose of a texture and free GPU memory
pub fn dispose_texture(texture: Texture) -> Nil {
  dispose_texture_ffi(texture)
}

/// Dispose of a geometry and free GPU memory
pub fn dispose_geometry(geometry: BufferGeometry) -> Nil {
  dispose_geometry_ffi(geometry)
}

/// Dispose of an Object3D and all its resources (geometry, materials, textures, children)
pub fn dispose_object3d(object: Object3D) -> Nil {
  dispose_object3d_ffi(object)
}

// --- FFI Functions ---

@external(javascript, "./ffi/asset.mjs", "loadAudio")
fn load_audio_ffi(url: String) -> Promise(Result(AudioBuffer, String))

@external(javascript, "./ffi/asset.mjs", "loadBatch")
fn load_batch_ffi(
  asset: array.Array(AssetType),
  on_progress: fn(LoadProgress) -> Nil,
) -> Promise(BatchLoadResult)

@external(javascript, "./ffi/asset.mjs", "disposeTexture")
fn dispose_texture_ffi(texture: Texture) -> Nil

@external(javascript, "./ffi/asset.mjs", "disposeGeometry")
fn dispose_geometry_ffi(geometry: BufferGeometry) -> Nil

@external(javascript, "./ffi/asset.mjs", "disposeObject3D")
fn dispose_object3d_ffi(object: Object3D) -> Nil

/// Load an STL file from a URL using Promises
@external(javascript, "./ffi/stl.mjs", "loadSTLAsync")
pub fn load_stl(url: String) -> Promise(Result(BufferGeometry, LoadError))

/// Load a texture from a URL using Promises
@external(javascript, "./ffi/texture.mjs", "loadTextureAsync")
pub fn load_texture(url: String) -> Promise(Result(Texture, LoadError))

/// Load a GLTF/GLB file from a URL using Promises
@external(javascript, "./ffi/gltf.mjs", "loadGLTFAsync")
pub fn load_gltf(url: String) -> Promise(Result(GLTFData, LoadError))

/// Load an audio file from a URL using Promises
///
/// Supports common audio formats including MP3, WAV, and OGG.
/// Returns an AudioBuffer that can be used with the audio system.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/asset
/// import gleam/javascript/promise
///
/// let load_effect = asset.load_audio("sounds/jump.mp3")
///   |> promise.map(fn(result) {
///     case result {
///       Ok(audio_buffer) -> AudioLoaded(audio_buffer)
///       Error(err) -> LoadFailed(err)
///     }
///   })
/// ```
pub fn load_audio(url: String) -> Promise(Result(AudioBuffer, LoadError)) {
  promise.map(load_audio_ffi(url), fn(result) {
    case result {
      Ok(audio) -> Ok(audio)
      Error(msg) -> Error(LoadError(msg))
    }
  })
}

/// Load a Wavefront OBJ file with optional MTL materials
///
/// Loads a 3D model in OBJ format with full material and texture support.
/// The loader handles:
/// - **Diffuse/Color Maps** (map_Kd): Base color textures
/// - **Normal Maps** (map_bump): Surface detail and lighting
/// - **Ambient Occlusion Maps** (map_Ka): Contact shadows and depth
/// - **Vertex Normals**: Computed automatically if missing
/// - **Model Centering**: Centers the model at origin
///
/// Textures are loaded from the same directory as the MTL file. The loader
/// properly parses MTL texture paths with options like `map_bump -bm 1 normal.jpg`.
///
/// ## Parameters
///
/// - `obj_url`: Path to the OBJ file
/// - `mtl_url`: Path to the MTL file, or empty string `""` for no materials
///
/// ## Returns
///
/// A Promise that resolves to:
/// - `Ok(Object3D)`: Loaded model with materials and textures
/// - `Error(LoadError)`: File not found
/// - `Error(InvalidUrl)`: Invalid URL provided
/// - `Error(ParseError)`: Failed to parse OBJ/MTL file
@external(javascript, "./ffi/obj.mjs", "loadOBJAsync")
pub fn load_obj(
  obj_url obj_url: String,
  mtl_url mtl_url: String,
) -> Promise(Result(Object3D, LoadError))
