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
import gleam/option
import gleam/order
import tiramisu/animation.{type AnimationClip}
import tiramisu/audio.{type AudioBuffer}

/// Opaque type for Three.js textures.
///
/// Created via `asset.load_texture()` and used in materials.
pub type Texture

/// Opaque type for Three.js Object3D.
///
/// Represents a 3D object loaded from GLTF, FBX, or OBJ files.
/// Can be used in scene nodes or cloned for multiple instances.
pub type Object3D

/// Texture filtering mode.
///
/// Controls how textures are sampled when they're displayed at different sizes.
pub type TextureFilter {
  /// Linear filtering - smooth, blurred appearance (default in Three.js)
  /// Good for realistic graphics and smooth gradients
  LinearFilter
  /// Nearest filtering - crisp, pixelated appearance
  /// Perfect for pixel art, low-poly aesthetics, and retro games
  NearestFilter
}

/// Opaque type for Three.js BufferGeometry.
///
/// Created by loading 3D models with `asset.load_stl()`.
pub type BufferGeometry

/// Opaque type for Three.js Font (for TextGeometry).
///
/// Created via `asset.load_font()` and used in text geometries.
/// Fonts must be in typeface.json format.
pub type Font

// --- Public Types ---

/// Errors that can occur when loading individual assets.
pub type LoadError {
  /// General loading error with a message
  LoadError(String)
  /// The provided URL was invalid or empty
  InvalidUrl(String)
  /// The asset file could not be parsed
  ParseError(String)
}

/// Data loaded from a GLTF/GLB file.
///
/// Contains both the 3D scene and any embedded animations.
pub type GLTFData {
  GLTFData(
    /// The root scene object containing all meshes and materials
    scene: Object3D,
    /// List of animation clips found in the GLTF file
    animations: List(AnimationClip),
  )
}

/// Data loaded from an FBX file.
///
/// Contains both the 3D scene and any embedded animations.
pub type FBXData {
  FBXData(
    /// The root scene object containing all meshes and materials
    scene: Object3D,
    /// List of animation clips found in the FBX file
    animations: List(AnimationClip),
  )
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
  /// FBX 3D model with animations
  ///
  /// Supports loading Autodesk FBX models with embedded animations and materials.
  /// The FBX loader automatically:
  /// - Loads embedded textures and materials
  /// - Loads skeletal animations
  /// - Loads morph target animations
  /// - Computes vertex normals if missing
  /// - Handles both ASCII and binary FBX formats
  ///
  /// FBX files can contain multiple animations, which are returned as AnimationClips
  /// that can be used with the animation system.
  ///
  /// **Texture Path**: If textures are in a different directory than the FBX file,
  /// provide the `texture_path` to tell the loader where to find them. For example:
  /// - FBX: "assets/PSX_Dungeon/Models/Door.fbx"
  /// - Textures: "assets/PSX_Dungeon/Textures/"
  /// - Use: `FBXAsset(url: "...", texture_path: Some("assets/PSX_Dungeon/Textures/"))`
  FBXAsset(url: String, texture_path: option.Option(String))
  /// Font for TextGeometry (typeface.json format)
  ///
  /// Fonts must be in Three.js typeface.json format. You can convert TTF/OTF fonts
  /// using the facetype.js converter or download pre-converted fonts from the
  /// Three.js repository (examples/fonts/).
  FontAsset(url: String)
}

/// A loaded asset (opaque to enforce type safety)
pub opaque type LoadedAsset {
  LoadedModel(data: GLTFData)
  LoadedTexture(texture: Texture)
  LoadedAudio(audio: AudioBuffer)
  LoadedSTL(geometry: BufferGeometry)
  LoadedOBJ(object: Object3D)
  LoadedFBX(data: FBXData)
  LoadedFont(font: Font)
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

@internal
pub fn loaded_fbx(data: FBXData) -> LoadedAsset {
  LoadedFBX(data)
}

@internal
pub fn loaded_font(font: Font) -> LoadedAsset {
  LoadedFont(font)
}

/// Errors that can occur when working with the asset cache.
pub type AssetError {
  /// Failed to load an asset with a specific reason
  AssetLoadError(url: String, reason: String)
  /// The requested asset was not found in the cache
  AssetNotFound(url: String)
  /// The cached asset exists but is the wrong type (e.g., requesting a texture when a model is cached)
  InvalidAssetType(url: String)
}

/// Progress information for batch loading.
///
/// Provided to the progress callback during `load_batch()`.
pub type LoadProgress {
  LoadProgress(
    /// Number of assets successfully loaded so far
    loaded: Int,
    /// Total number of assets to load
    total: Int,
    /// URL of the asset currently being loaded
    current_url: String,
  )
}

/// LRU (Least Recently Used) entry with access timestamp
type CacheEntry {
  CacheEntry(asset: LoadedAsset, last_accessed: Int)
}

/// Asset cache configuration.
///
/// Controls the maximum number of assets that can be cached before
/// the least recently used asset is evicted.
pub type CacheConfig {
  /// Maximum number of assets before LRU eviction occurs
  CacheConfig(max_size: Int, current_time: Int)
}

/// Asset cache/registry with LRU eviction
pub opaque type AssetCache {
  AssetCache(asset: Dict(String, CacheEntry), config: CacheConfig)
}

// --- Cache Management ---

/// Create a new empty asset cache with default max size.
///
/// The default maximum size is 100 assets. When this limit is exceeded,
/// the least recently used asset is automatically evicted.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/asset
///
/// let cache = asset.new_cache()
/// // Load and cache assets...
/// ```
pub fn new_cache() -> AssetCache {
  AssetCache(
    asset: dict.new(),
    config: CacheConfig(max_size: 100, current_time: 0),
  )
}

/// Create a new empty asset cache with a custom maximum size.
///
/// The `max_size` is the number of assets to cache before LRU eviction occurs.
///
/// ## Example
///
/// ```gleam
/// // Create a larger cache for games with many assets
/// let cache = asset.new_cache_with_size(500)
///
/// // Or a smaller cache for memory-constrained environments
/// let cache = asset.new_cache_with_size(20)
/// ```
pub fn new_cache_with_size(max_size: Int) -> AssetCache {
  AssetCache(
    asset: dict.new(),
    config: CacheConfig(max_size: max_size, current_time: 0),
  )
}

/// Get the number of assets currently in the cache.
///
/// ## Example
///
/// ```gleam
/// let cache = asset.new_cache()
/// // ... load some assets
/// let count = asset.cache_size(cache)  // => 5
/// ```
pub fn cache_size(cache: AssetCache) -> Int {
  dict.size(cache.asset)
}

/// Check if a specific asset is cached.
///
/// ## Example
///
/// ```gleam
/// case asset.is_cached(cache, "textures/player.png") {
///   True -> {
///     // Asset is cached, get it immediately
///     let assert Ok(texture) = asset.get_texture(cache, "textures/player.png")
///     use_texture(texture)
///   }
///   False -> {
///     // Need to load the asset first
///     load_and_cache_texture("textures/player.png")
///   }
/// }
/// ```
pub fn is_cached(cache: AssetCache, url: String) -> Bool {
  dict.has_key(cache.asset, url)
}

/// Clear all cached assets.
///
/// This removes all assets from the cache but preserves the cache configuration.
/// Useful for level transitions or when you need to free memory.
///
/// ## Example
///
/// ```gleam
/// // When transitioning to a new level
/// fn load_new_level(model: Model) {
///   let cleared_cache = asset.clear_cache(model.assets)
///   Model(..model, assets: cleared_cache)
/// }
/// ```
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

    FBXAsset(url, texture_path) -> {
      let texture_path_str = case texture_path {
        option.Some(path) -> path
        option.None -> ""
      }
      promise.map(load_fbx(url, texture_path_str), fn(result) {
        case result {
          Ok(data) -> Ok(LoadedFBX(data))
          Error(LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(ParseError(msg)) -> Error(AssetLoadError(url, msg))
        }
      })
    }

    FontAsset(url) -> {
      promise.map(load_font(url), fn(result) {
        case result {
          Ok(font) -> Ok(LoadedFont(font))
          Error(LoadError(msg)) -> Error(AssetLoadError(url, msg))
          Error(InvalidUrl(_)) -> Error(AssetLoadError(url, "Invalid URL"))
          Error(ParseError(msg)) -> Error(AssetLoadError(url, msg))
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

/// Get an FBX model from the cache
///
/// Returns the loaded FBX model data including the scene object and animations.
/// FBX files can contain:
/// - Skeletal animations for character animation
/// - Morph target animations
/// - Embedded textures and materials
/// - Multiple mesh objects in a scene hierarchy
///
/// ## Example
///
/// ```gleam
/// let assert Ok(character_data) = asset.get_fbx(cache, "models/character.fbx")
///
/// scene.Model3D(
///   id: "character",
///   object: character_data.scene,
///   transform: transform.identity,
///   animation: option.Some(
///     animation.state_machine(character_data.animations)
///   ),
///   physics: option.None,
/// )
/// ```
pub fn get_fbx(cache: AssetCache, url: String) -> Result(FBXData, AssetError) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(LoadedFBX(data), _)) -> Ok(data)
    Ok(CacheEntry(_, _)) -> Error(InvalidAssetType(url))
    Error(_) -> Error(AssetNotFound(url))
  }
}

/// Get the scene object from a cached FBX model
pub fn get_fbx_scene(
  cache: AssetCache,
  url: String,
) -> Result(Object3D, AssetError) {
  case get_fbx(cache, url) {
    Ok(data) -> Ok(data.scene)
    Error(err) -> Error(err)
  }
}

/// Try to get any asset 
pub fn get(cache: AssetCache, url: String) -> Result(LoadedAsset, AssetError) {
  case dict.get(cache.asset, url) {
    Ok(CacheEntry(asset, _)) -> Ok(asset)
    Error(_) -> Error(AssetNotFound(url))
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

// --- FFI Functions ---

@external(javascript, "../tiramisu.ffi.mjs", "loadAudio")
fn load_audio_ffi(url: String) -> Promise(Result(AudioBuffer, String))

@external(javascript, "../tiramisu.ffi.mjs", "loadTextureSafe")
fn load_texture_ffi(url: String) -> Promise(Result(Texture, String))

@external(javascript, "../tiramisu.ffi.mjs", "loadBatch")
fn load_batch_ffi(
  asset: array.Array(AssetType),
  on_progress: fn(LoadProgress) -> Nil,
) -> Promise(BatchLoadResult)

/// Load an STL file from a URL using Promises
pub fn load_stl(url: String) -> Promise(Result(BufferGeometry, LoadError)) {
  // Validate URL
  case url == "" {
    True -> promise.resolve(Error(InvalidUrl(url)))
    False -> {
      // Call safe wrapper that returns Result
      load_stl_ffi(url)
      |> promise.map(fn(result) {
        case result {
          Ok(geometry) -> Ok(geometry)
          Error(msg) -> Error(LoadError(msg))
        }
      })
    }
  }
}

@external(javascript, "../tiramisu.ffi.mjs", "loadSTLSafe")
fn load_stl_ffi(url: String) -> Promise(Result(BufferGeometry, String))

/// Load a texture from a URL using Promises
pub fn load_texture(url: String) -> Promise(Result(Texture, LoadError)) {
  // Validate URL
  case url == "" {
    True -> promise.resolve(Error(InvalidUrl(url)))
    False -> {
      // Call safe wrapper that returns Result
      load_texture_ffi(url)
      |> promise.map(fn(result) {
        case result {
          Ok(texture) -> Ok(texture)
          Error(msg) -> Error(LoadError(msg))
        }
      })
    }
  }
}

/// Load a GLTF/GLB file from a URL using Promises
pub fn load_gltf(url: String) -> Promise(Result(GLTFData, LoadError)) {
  // Validate URL
  case url == "" {
    True -> promise.resolve(Error(InvalidUrl(url)))
    False -> {
      // Call safe wrapper that returns Result
      load_gltf_ffi(url)
      |> promise.map(fn(result) {
        case result {
          Ok(data) -> Ok(data)
          Error(msg) -> Error(LoadError(msg))
        }
      })
    }
  }
}

@external(javascript, "../tiramisu.ffi.mjs", "loadGLTFSafe")
fn load_gltf_ffi(url: String) -> Promise(Result(GLTFData, String))

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
pub fn load_obj(
  obj_url obj_url: String,
  mtl_url mtl_url: String,
) -> Promise(Result(Object3D, LoadError)) {
  // Validate URL
  case obj_url == "" {
    True -> promise.resolve(Error(InvalidUrl(obj_url)))
    False -> {
      // Call safe wrapper that returns Result
      load_obj_ffi(obj_url, mtl_url)
      |> promise.map(fn(result) {
        case result {
          Ok(object) -> Ok(object)
          Error(msg) -> Error(LoadError(msg))
        }
      })
    }
  }
}

@external(javascript, "../tiramisu.ffi.mjs", "loadOBJSafe")
fn load_obj_ffi(
  obj_url: String,
  mtl_url: String,
) -> Promise(Result(Object3D, String))

/// Load an FBX file with animations
///
/// Loads a 3D model in FBX format with full animation and material support.
/// The loader handles:
/// - **Skeletal Animations**: Bone-based character animations
/// - **Morph Target Animations**: Vertex-based shape animations
/// - **Embedded Textures**: Textures embedded in the FBX file
/// - **Materials**: PBR and standard materials with properties
/// - **Scene Hierarchy**: Complex object hierarchies preserved
/// - **Both Formats**: ASCII and binary FBX files
///
/// ## Parameters
///
/// - `url`: Path to the FBX file
/// - `texture_path`: Optional path to texture directory (if textures are separate from FBX)
///
/// ## Returns
///
/// A Promise that resolves to:
/// - `Ok(FBXData)`: Loaded model with scene object and animation clips
/// - `Error(LoadError)`: File not found
/// - `Error(InvalidUrl)`: Invalid URL provided
/// - `Error(ParseError)`: Failed to parse FBX file
///
/// ## Example
///
/// ```gleam
/// import tiramisu/asset
/// import gleam/javascript/promise
/// import gleam/option
///
/// // FBX with textures in same directory
/// let load_effect = asset.load_fbx("models/character.fbx", "")
///   |> promise.map(fn(result) {
///     case result {
///       Ok(fbx_data) -> ModelLoaded(fbx_data)
///       Error(err) -> LoadFailed(err)
///     }
///   })
///
/// // FBX with textures in different directory
/// let load_effect = asset.load_fbx(
///   "assets/PSX_Dungeon/Models/Door.fbx",
///   "assets/PSX_Dungeon/Textures/"
/// )
/// ```
pub fn load_fbx(
  url: String,
  texture_path: String,
) -> Promise(Result(FBXData, LoadError)) {
  // Validate URL
  case url == "" {
    True -> promise.resolve(Error(InvalidUrl(url)))
    False -> {
      // Call safe wrapper that returns Result
      load_fbx_ffi(url, texture_path)
      |> promise.map(fn(result) {
        case result {
          Ok(data) -> Ok(data)
          Error(msg) -> Error(LoadError(msg))
        }
      })
    }
  }
}

@external(javascript, "../tiramisu.ffi.mjs", "loadFBXSafe")
fn load_fbx_ffi(
  url: String,
  texture_path: String,
) -> Promise(Result(FBXData, String))

/// Load a font file (typeface.json format) for use with TextGeometry.
///
/// Three.js fonts must be in typeface.json format. You can:
/// - Download pre-converted fonts from Three.js repository (examples/fonts/)
/// - Convert TTF/OTF fonts using facetype.js converter
///
/// ## Example
///
/// ```gleam
/// import gleam/javascript/promise
/// import tiramisu/asset
///
/// let load_effect = asset.load_font("fonts/helvetiker_regular.typeface.json")
///   |> promise.map(fn(result) {
///     case result {
///       Ok(font) -> FontLoaded(font)
///       Error(err) -> LoadFailed(err)
///     }
///   })
/// ```
///
/// ## Returns
///
/// A Promise that resolves to:
/// - `Ok(Font)`: Loaded font ready for TextGeometry
/// - `Error(LoadError)`: File not found
/// - `Error(InvalidUrl)`: Invalid URL provided
/// - `Error(ParseError)`: Failed to parse font file
pub fn load_font(url: String) -> Promise(Result(Font, LoadError)) {
  // Validate URL
  case url == "" {
    True -> promise.resolve(Error(InvalidUrl(url)))
    False -> {
      // Call safe wrapper that returns Result
      load_font_ffi(url)
      |> promise.map(fn(result) {
        case result {
          Ok(font) -> Ok(font)
          Error(msg) -> Error(LoadError(msg))
        }
      })
    }
  }
}

@external(javascript, "../tiramisu.ffi.mjs", "loadFontSafe")
fn load_font_ffi(url: String) -> Promise(Result(Font, String))

/// Clone an Object3D for reuse in multiple scene locations
///
/// When you need to place the same model in multiple locations, you must clone it.
/// Three.js doesn't allow the same Object3D to exist in multiple places in the scene graph.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/asset
///
/// let assert Ok(fbx_data) = asset.get_fbx(cache, "models/tree.fbx")
///
/// // Create multiple instances of the tree
/// let tree1 = fbx_data.scene
/// let tree2 = asset.clone_object3d(fbx_data.scene)
/// let tree3 = asset.clone_object3d(fbx_data.scene)
/// ```
@external(javascript, "../threejs.ffi.mjs", "cloneObject3D")
pub fn clone_object3d(object: Object3D) -> Object3D

/// Set the filtering mode for a texture
///
/// Controls how the texture is sampled when displayed at different sizes.
/// This should be called before using the texture in materials.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/asset
///
/// let assert Ok(texture) = asset.get_texture(cache, "textures/pixel_art.png")
///
/// // Make the texture crisp for pixel art
/// asset.set_texture_filter(texture, asset.NearestFilter)
/// ```
@external(javascript, "../threejs.ffi.mjs", "setTextureFilter")
fn set_texture_filter_ffi(texture: Texture, filter_mode: String) -> Nil

/// Set the filtering mode for a texture
pub fn set_texture_filter(texture: Texture, filter: TextureFilter) -> Nil {
  let filter_str = case filter {
    LinearFilter -> "LinearFilter"
    NearestFilter -> "NearestFilter"
  }
  set_texture_filter_ffi(texture, filter_str)
}

/// Apply a texture to all materials in a loaded 3D model
///
/// This is useful when FBX files have missing or broken texture references.
/// It will traverse the entire object hierarchy and apply the given texture
/// to all mesh materials with the specified filtering mode.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/asset
///
/// // Load the FBX model
/// let assert Ok(fbx_data) = asset.get_fbx(cache, "models/door.fbx")
///
/// // Load the texture separately
/// let assert Ok(texture) = asset.get_texture(cache, "textures/door_texture.png")
///
/// // Apply texture with nearest filtering for crisp look
/// asset.apply_texture_to_object(fbx_data.scene, texture, asset.NearestFilter)
/// ```
@external(javascript, "../threejs.ffi.mjs", "applyTextureToObject")
fn apply_texture_to_object_ffi(
  object: Object3D,
  texture: Texture,
  filter_mode: String,
) -> Nil

/// Apply a texture to all materials in a loaded 3D model
pub fn apply_texture_to_object(
  object: Object3D,
  texture: Texture,
  filter: TextureFilter,
) -> Nil {
  let filter_str = case filter {
    LinearFilter -> "LinearFilter"
    NearestFilter -> "NearestFilter"
  }
  apply_texture_to_object_ffi(object, texture, filter_str)
}
