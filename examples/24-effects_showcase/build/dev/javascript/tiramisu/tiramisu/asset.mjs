import * as $array from "../../gleam_javascript/gleam/javascript/array.mjs";
import * as $promise from "../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Ok, Error, CustomType as $CustomType } from "../gleam.mjs";
import {
  loadAudio as load_audio_ffi,
  loadTextureSafe as load_texture_ffi,
  loadBatch as load_batch_ffi,
  disposeTexture as dispose_texture_ffi,
  disposeGeometry as dispose_geometry_ffi,
  disposeObject3D as dispose_object3d_ffi,
  loadSTLSafe as load_stl_ffi,
  loadGLTFSafe as load_gltf_ffi,
  loadOBJSafe as load_obj_ffi,
} from "../tiramisu.ffi.mjs";
import * as $audio from "../tiramisu/audio.mjs";
import * as $object3d from "../tiramisu/object3d.mjs";

export class LoadError extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InvalidUrl extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ParseError extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class GLTFData extends $CustomType {
  constructor(scene, animations) {
    super();
    this.scene = scene;
    this.animations = animations;
  }
}

/**
 * GLTF/GLB 3D model
 */
export class ModelAsset extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

/**
 * Texture image (PNG, JPG, etc.)
 */
export class TextureAsset extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

/**
 * Audio file (MP3, WAV, OGG)
 */
export class AudioAsset extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

/**
 * STL 3D model
 */
export class STLAsset extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

/**
 * OBJ 3D model with optional MTL material file
 *
 * Supports loading Wavefront OBJ models with their associated MTL material
 * files. The loader automatically:
 * - Loads diffuse color maps (map_Kd)
 * - Loads normal maps (map_bump) for surface detail
 * - Loads ambient occlusion maps (map_Ka) for realistic shadows
 * - Centers the model at origin
 * - Computes vertex normals if missing
 *
 * MTL files can include texture paths with options (e.g., "map_bump -bm 1 texture.jpg"),
 * which are properly parsed. Textures are assumed to be in the same directory as
 * the MTL file.
 */
export class OBJAsset extends $CustomType {
  constructor(obj_url, mtl_url) {
    super();
    this.obj_url = obj_url;
    this.mtl_url = mtl_url;
  }
}

class LoadedModel extends $CustomType {
  constructor(data) {
    super();
    this.data = data;
  }
}

class LoadedTexture extends $CustomType {
  constructor(texture) {
    super();
    this.texture = texture;
  }
}

class LoadedAudio extends $CustomType {
  constructor(audio) {
    super();
    this.audio = audio;
  }
}

class LoadedSTL extends $CustomType {
  constructor(geometry) {
    super();
    this.geometry = geometry;
  }
}

class LoadedOBJ extends $CustomType {
  constructor(object) {
    super();
    this.object = object;
  }
}

export class AssetLoadError extends $CustomType {
  constructor(url, reason) {
    super();
    this.url = url;
    this.reason = reason;
  }
}

export class AssetNotFound extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

export class InvalidAssetType extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

export class LoadProgress extends $CustomType {
  constructor(loaded, total, current_url) {
    super();
    this.loaded = loaded;
    this.total = total;
    this.current_url = current_url;
  }
}

class CacheEntry extends $CustomType {
  constructor(asset, last_accessed) {
    super();
    this.asset = asset;
    this.last_accessed = last_accessed;
  }
}

export class CacheConfig extends $CustomType {
  constructor(max_size, current_time) {
    super();
    this.max_size = max_size;
    this.current_time = current_time;
  }
}

class AssetCache extends $CustomType {
  constructor(asset, config) {
    super();
    this.asset = asset;
    this.config = config;
  }
}

export class BatchLoadResult extends $CustomType {
  constructor(cache, errors) {
    super();
    this.cache = cache;
    this.errors = errors;
  }
}

export function loaded_model(data) {
  return new LoadedModel(data);
}

export function loaded_texture(texture) {
  return new LoadedTexture(texture);
}

export function loaded_audio(audio) {
  return new LoadedAudio(audio);
}

export function loaded_stl(geometry) {
  return new LoadedSTL(geometry);
}

export function loaded_obj(object) {
  return new LoadedOBJ(object);
}

/**
 * Create a new empty asset cache with default max size (100 asset)
 */
export function new_cache() {
  return new AssetCache($dict.new$(), new CacheConfig(100, 0));
}

/**
 * Create a new empty asset cache with custom max size
 */
export function new_cache_with_size(max_size) {
  return new AssetCache($dict.new$(), new CacheConfig(max_size, 0));
}

/**
 * Get the number of cached asset
 */
export function cache_size(cache) {
  return $dict.size(cache.asset);
}

/**
 * Check if an asset is cached
 */
export function is_cached(cache, url) {
  return $dict.has_key(cache.asset, url);
}

/**
 * Clear all cached asset
 */
export function clear_cache(cache) {
  return new AssetCache($dict.new$(), cache.config);
}

/**
 * Get a GLTF model from the cache (updates LRU timestamp)
 */
export function get_model(cache, url) {
  let $ = $dict.get(cache.asset, url);
  if ($ instanceof Ok) {
    let $1 = $[0].asset;
    if ($1 instanceof LoadedModel) {
      let data = $1.data;
      return new Ok(data);
    } else {
      return new Error(new InvalidAssetType(url));
    }
  } else {
    return new Error(new AssetNotFound(url));
  }
}

/**
 * Get the scene object from a cached GLTF model
 */
export function get_model_scene(cache, url) {
  let $ = get_model(cache, url);
  if ($ instanceof Ok) {
    let data = $[0];
    return new Ok(data.scene);
  } else {
    return $;
  }
}

/**
 * Get a texture from the cache
 */
export function get_texture(cache, url) {
  let $ = $dict.get(cache.asset, url);
  if ($ instanceof Ok) {
    let $1 = $[0].asset;
    if ($1 instanceof LoadedTexture) {
      let tex = $1.texture;
      return new Ok(tex);
    } else {
      return new Error(new InvalidAssetType(url));
    }
  } else {
    return new Error(new AssetNotFound(url));
  }
}

/**
 * Get an audio buffer from the cache
 */
export function get_audio(cache, url) {
  let $ = $dict.get(cache.asset, url);
  if ($ instanceof Ok) {
    let $1 = $[0].asset;
    if ($1 instanceof LoadedAudio) {
      let audio = $1.audio;
      return new Ok(audio);
    } else {
      return new Error(new InvalidAssetType(url));
    }
  } else {
    return new Error(new AssetNotFound(url));
  }
}

/**
 * Get an STL geometry from the cache
 */
export function get_stl(cache, url) {
  let $ = $dict.get(cache.asset, url);
  if ($ instanceof Ok) {
    let $1 = $[0].asset;
    if ($1 instanceof LoadedSTL) {
      let geom = $1.geometry;
      return new Ok(geom);
    } else {
      return new Error(new InvalidAssetType(url));
    }
  } else {
    return new Error(new AssetNotFound(url));
  }
}

/**
 * Get an OBJ model from the cache
 *
 * Returns the loaded OBJ model as an Object3D. The model will have:
 * - Materials with textures applied (if MTL file was loaded)
 * - Vertex normals computed
 * - Center position at origin
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(bread_model) = asset.get_obj(cache, "models/bread.obj")
 *
 * scene.Model3D(
 *   id: "bread",
 *   object: bread_model,
 *   transform: transform.identity,
 *   animation: option.None,
 *   physics: option.None,
 * )
 * ```
 */
export function get_obj(cache, url) {
  let $ = $dict.get(cache.asset, url);
  if ($ instanceof Ok) {
    let $1 = $[0].asset;
    if ($1 instanceof LoadedOBJ) {
      let object = $1.object;
      return new Ok(object);
    } else {
      return new Error(new InvalidAssetType(url));
    }
  } else {
    return new Error(new AssetNotFound(url));
  }
}

/**
 * Try to get any asset (returns Option)
 */
export function try_get(cache, url) {
  let $ = $dict.get(cache.asset, url);
  if ($ instanceof Ok) {
    let asset = $[0].asset;
    return new $option.Some(asset);
  } else {
    return new $option.None();
  }
}

/**
 * Get all cached URLs
 */
export function cached_urls(cache) {
  return $dict.keys(cache.asset);
}

/**
 * Evict the least recently used asset from the cache
 * 
 * @ignore
 */
function evict_lru(cache) {
  let $ = (() => {
    let _pipe = $dict.to_list(cache.asset);
    let _pipe$1 = $list.sort(
      _pipe,
      (a, b) => {
        let time_a;
        time_a = a[1].last_accessed;
        let time_b;
        time_b = b[1].last_accessed;
        let $1 = time_a < time_b;
        if ($1) {
          return new $order.Lt();
        } else {
          let $2 = time_a > time_b;
          if ($2) {
            return new $order.Gt();
          } else {
            return new $order.Eq();
          }
        }
      },
    );
    return $list.first(_pipe$1);
  })();
  if ($ instanceof Ok) {
    let url_to_evict = $[0][0];
    let new_asset = $dict.delete$(cache.asset, url_to_evict);
    return new AssetCache(new_asset, cache.config);
  } else {
    return cache;
  }
}

/**
 * Insert a loaded asset into the cache manually
 * If cache exceeds max_size, evicts least recently used asset
 */
export function insert_asset(cache, url, asset) {
  let new_time = cache.config.current_time + 1;
  let entry = new CacheEntry(asset, new_time);
  let new_asset = $dict.insert(cache.asset, url, entry);
  let new_cache$1 = new AssetCache(
    new_asset,
    (() => {
      let _record = cache.config;
      return new CacheConfig(_record.max_size, new_time);
    })(),
  );
  let $ = $dict.size(new_asset) > cache.config.max_size;
  if ($) {
    return evict_lru(new_cache$1);
  } else {
    return new_cache$1;
  }
}

/**
 * Load multiple asset with progress tracking
 * Returns a promise that resolves with the loaded asset and any errors
 */
export function load_batch(asset, on_progress) {
  let asset_array = $array.from_list(asset);
  return load_batch_ffi(asset_array, on_progress);
}

/**
 * Load multiple asset without progress tracking
 */
export function load_batch_simple(asset) {
  let asset_array = $array.from_list(asset);
  return load_batch_ffi(asset_array, (_) => { return undefined; });
}

/**
 * Dispose of a texture and free GPU memory
 */
export function dispose_texture(texture) {
  return dispose_texture_ffi(texture);
}

/**
 * Dispose of a geometry and free GPU memory
 */
export function dispose_geometry(geometry) {
  return dispose_geometry_ffi(geometry);
}

/**
 * Dispose of an Object3D and all its resources (geometry, materials, textures, children)
 */
export function dispose_object3d(object) {
  return dispose_object3d_ffi(object);
}

/**
 * Load an STL file from a URL using Promises
 */
export function load_stl(url) {
  let $ = url === "";
  if ($) {
    return $promise.resolve(new Error(new InvalidUrl(url)));
  } else {
    let _pipe = load_stl_ffi(url);
    return $promise.map(
      _pipe,
      (result) => {
        if (result instanceof Ok) {
          return result;
        } else {
          let msg = result[0];
          return new Error(new LoadError(msg));
        }
      },
    );
  }
}

/**
 * Load a texture from a URL using Promises
 */
export function load_texture(url) {
  let $ = url === "";
  if ($) {
    return $promise.resolve(new Error(new InvalidUrl(url)));
  } else {
    let _pipe = load_texture_ffi(url);
    return $promise.map(
      _pipe,
      (result) => {
        if (result instanceof Ok) {
          return result;
        } else {
          let msg = result[0];
          return new Error(new LoadError(msg));
        }
      },
    );
  }
}

/**
 * Load a GLTF/GLB file from a URL using Promises
 */
export function load_gltf(url) {
  let $ = url === "";
  if ($) {
    return $promise.resolve(new Error(new InvalidUrl(url)));
  } else {
    let _pipe = load_gltf_ffi(url);
    return $promise.map(
      _pipe,
      (result) => {
        if (result instanceof Ok) {
          return result;
        } else {
          let msg = result[0];
          return new Error(new LoadError(msg));
        }
      },
    );
  }
}

/**
 * Load an audio file from a URL using Promises
 *
 * Supports common audio formats including MP3, WAV, and OGG.
 * Returns an AudioBuffer that can be used with the audio system.
 *
 * ## Example
 *
 * ```gleam
 * import tiramisu/asset
 * import gleam/javascript/promise
 *
 * let load_effect = asset.load_audio("sounds/jump.mp3")
 *   |> promise.map(fn(result) {
 *     case result {
 *       Ok(audio_buffer) -> AudioLoaded(audio_buffer)
 *       Error(err) -> LoadFailed(err)
 *     }
 *   })
 * ```
 */
export function load_audio(url) {
  return $promise.map(
    load_audio_ffi(url),
    (result) => {
      if (result instanceof Ok) {
        return result;
      } else {
        let msg = result[0];
        return new Error(new LoadError(msg));
      }
    },
  );
}

/**
 * Load a Wavefront OBJ file with optional MTL materials
 *
 * Loads a 3D model in OBJ format with full material and texture support.
 * The loader handles:
 * - **Diffuse/Color Maps** (map_Kd): Base color textures
 * - **Normal Maps** (map_bump): Surface detail and lighting
 * - **Ambient Occlusion Maps** (map_Ka): Contact shadows and depth
 * - **Vertex Normals**: Computed automatically if missing
 * - **Model Centering**: Centers the model at origin
 *
 * Textures are loaded from the same directory as the MTL file. The loader
 * properly parses MTL texture paths with options like `map_bump -bm 1 normal.jpg`.
 *
 * ## Parameters
 *
 * - `obj_url`: Path to the OBJ file
 * - `mtl_url`: Path to the MTL file, or empty string `""` for no materials
 *
 * ## Returns
 *
 * A Promise that resolves to:
 * - `Ok(Object3D)`: Loaded model with materials and textures
 * - `Error(LoadError)`: File not found
 * - `Error(InvalidUrl)`: Invalid URL provided
 * - `Error(ParseError)`: Failed to parse OBJ/MTL file
 */
export function load_obj(obj_url, mtl_url) {
  let $ = obj_url === "";
  if ($) {
    return $promise.resolve(new Error(new InvalidUrl(obj_url)));
  } else {
    let _pipe = load_obj_ffi(obj_url, mtl_url);
    return $promise.map(
      _pipe,
      (result) => {
        if (result instanceof Ok) {
          return result;
        } else {
          let msg = result[0];
          return new Error(new LoadError(msg));
        }
      },
    );
  }
}

/**
 * Load a single asset
 */
export function load_asset(asset) {
  if (asset instanceof ModelAsset) {
    let url = asset.url;
    return $promise.map(
      load_gltf(url),
      (result) => {
        if (result instanceof Ok) {
          let data = result[0];
          return new Ok(new LoadedModel(data));
        } else {
          let $ = result[0];
          if ($ instanceof LoadError) {
            let msg = $[0];
            return new Error(new AssetLoadError(url, msg));
          } else if ($ instanceof InvalidUrl) {
            return new Error(new AssetLoadError(url, "Invalid URL"));
          } else {
            let msg = $[0];
            return new Error(new AssetLoadError(url, msg));
          }
        }
      },
    );
  } else if (asset instanceof TextureAsset) {
    let url = asset.url;
    return $promise.map(
      load_texture(url),
      (result) => {
        if (result instanceof Ok) {
          let tex = result[0];
          return new Ok(new LoadedTexture(tex));
        } else {
          let $ = result[0];
          if ($ instanceof LoadError) {
            let msg = $[0];
            return new Error(new AssetLoadError(url, msg));
          } else if ($ instanceof InvalidUrl) {
            return new Error(new AssetLoadError(url, "Invalid URL"));
          } else {
            return new Error(new AssetLoadError(url, "Failed to parse texture"));
          }
        }
      },
    );
  } else if (asset instanceof AudioAsset) {
    let url = asset.url;
    return $promise.map(
      load_audio_ffi(url),
      (result) => {
        if (result instanceof Ok) {
          let audio = result[0];
          return new Ok(new LoadedAudio(audio));
        } else {
          let msg = result[0];
          return new Error(new AssetLoadError(url, msg));
        }
      },
    );
  } else if (asset instanceof STLAsset) {
    let url = asset.url;
    return $promise.map(
      load_stl(url),
      (result) => {
        if (result instanceof Ok) {
          let geom = result[0];
          return new Ok(new LoadedSTL(geom));
        } else {
          let $ = result[0];
          if ($ instanceof LoadError) {
            let msg = $[0];
            return new Error(new AssetLoadError(url, msg));
          } else if ($ instanceof InvalidUrl) {
            return new Error(new AssetLoadError(url, "Invalid URL"));
          } else {
            let msg = $[0];
            return new Error(new AssetLoadError(url, msg));
          }
        }
      },
    );
  } else {
    let obj_url = asset.obj_url;
    let mtl_url = asset.mtl_url;
    let _block;
    if (mtl_url instanceof $option.Some) {
      let url = mtl_url[0];
      _block = url;
    } else {
      _block = "";
    }
    let mtl_url_str = _block;
    return $promise.map(
      load_obj(obj_url, mtl_url_str),
      (result) => {
        if (result instanceof Ok) {
          let object = result[0];
          return new Ok(new LoadedOBJ(object));
        } else {
          let $ = result[0];
          if ($ instanceof LoadError) {
            let msg = $[0];
            return new Error(new AssetLoadError(obj_url, msg));
          } else if ($ instanceof InvalidUrl) {
            return new Error(new AssetLoadError(obj_url, "Invalid URL"));
          } else {
            let msg = $[0];
            return new Error(new AssetLoadError(obj_url, msg));
          }
        }
      },
    );
  }
}
