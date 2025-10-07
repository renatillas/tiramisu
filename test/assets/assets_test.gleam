import gleam/list
import gleam/option
import tiramisu/asset
import tiramisu/audio
import tiramisu/scene

// --- Cache Management Tests ---

// Test: Create new cache
pub fn new_cache_test() {
  let cache = asset.new_cache()
  assert asset.cache_size(cache) == 0
}

// Test: Cache size after operations
pub fn cache_size_test() {
  let cache = asset.new_cache()
  assert asset.cache_size(cache) == 0

  // Can't actually load asset in tests (FFI), so we'll test insert
  let texture = unsafe_mock_texture()
  let loaded_asset = asset.loaded_texture(texture)
  let cache = asset.insert_asset(cache, "test.png", loaded_asset)

  assert asset.cache_size(cache) == 1
}

// Test: Check if asset is cached
pub fn is_cached_test() {
  let cache = asset.new_cache()
  assert !asset.is_cached(cache, "test.png")

  let texture = unsafe_mock_texture()
  let loaded_asset = asset.loaded_texture(texture)
  let cache = asset.insert_asset(cache, "test.png", loaded_asset)

  assert asset.is_cached(cache, "test.png")
}

// Test: Check non-existent asset
pub fn is_cached_not_found_test() {
  let cache = asset.new_cache()
  let texture = unsafe_mock_texture()
  let loaded_asset = asset.loaded_texture(texture)
  let cache = asset.insert_asset(cache, "exists.png", loaded_asset)

  assert !asset.is_cached(cache, "missing.png")
}

// Test: Clear cache
pub fn clear_cache_test() {
  let cache = asset.new_cache()
  let texture = unsafe_mock_texture()
  let loaded_asset = asset.loaded_texture(texture)
  let cache = asset.insert_asset(cache, "test.png", loaded_asset)

  assert asset.cache_size(cache) == 1

  let cache = asset.clear_cache(cache)
  assert asset.cache_size(cache) == 0
}

// Test: Insert multiple asset
pub fn insert_multiple_asset_test() {
  let cache = asset.new_cache()

  let texture1 = unsafe_mock_texture()
  let texture2 = unsafe_mock_texture()
  let audio = unsafe_mock_audio()

  let cache =
    cache
    |> asset.insert_asset("texture1.png", asset.loaded_texture(texture1))
    |> asset.insert_asset("texture2.png", asset.loaded_texture(texture2))
    |> asset.insert_asset("audio.mp3", asset.loaded_audio(audio))

  assert asset.cache_size(cache) == 3
  assert asset.is_cached(cache, "texture1.png")
  assert asset.is_cached(cache, "texture2.png")
  assert asset.is_cached(cache, "audio.mp3")
}

// --- Asset Retrieval Tests ---

// Test: Get texture from cache (success)
pub fn get_texture_success_test() {
  let cache = asset.new_cache()
  let texture = unsafe_mock_texture()
  let cache =
    asset.insert_asset(cache, "test.png", asset.loaded_texture(texture))

  let result = asset.get_texture(cache, "test.png")
  assert case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// Test: Get texture not found
pub fn get_texture_not_found_test() {
  let cache = asset.new_cache()
  let result = asset.get_texture(cache, "missing.png")

  assert case result {
    Error(asset.AssetNotFound("missing.png")) -> True
    _ -> False
  }
}

// Test: Get texture with wrong type
pub fn get_texture_wrong_type_test() {
  let cache = asset.new_cache()
  let audio = unsafe_mock_audio()
  let cache = asset.insert_asset(cache, "audio.mp3", asset.loaded_audio(audio))

  // Try to get as texture (wrong type)
  let result = asset.get_texture(cache, "audio.mp3")

  assert case result {
    Error(asset.InvalidAssetType("audio.mp3")) -> True
    _ -> False
  }
}

// Test: Get audio from cache (success)
pub fn get_audio_success_test() {
  let cache = asset.new_cache()
  let audio = unsafe_mock_audio()
  let cache = asset.insert_asset(cache, "sound.mp3", asset.loaded_audio(audio))

  let result = asset.get_audio(cache, "sound.mp3")
  assert case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// Test: Get audio not found
pub fn get_audio_not_found_test() {
  let cache = asset.new_cache()
  let result = asset.get_audio(cache, "missing.mp3")

  assert case result {
    Error(asset.AssetNotFound("missing.mp3")) -> True
    _ -> False
  }
}

// Test: Get audio with wrong type
pub fn get_audio_wrong_type_test() {
  let cache = asset.new_cache()
  let texture = unsafe_mock_texture()
  let cache =
    asset.insert_asset(cache, "image.png", asset.loaded_texture(texture))

  // Try to get as audio (wrong type)
  let result = asset.get_audio(cache, "image.png")

  assert case result {
    Error(asset.InvalidAssetType("image.png")) -> True
    _ -> False
  }
}

// Test: Get STL from cache (success)
pub fn get_stl_success_test() {
  let cache = asset.new_cache()
  let geometry = unsafe_mock_geometry()
  let cache = asset.insert_asset(cache, "model.stl", asset.loaded_stl(geometry))

  let result = asset.get_stl(cache, "model.stl")
  assert case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// Test: Get STL not found
pub fn get_stl_not_found_test() {
  let cache = asset.new_cache()
  let result = asset.get_stl(cache, "missing.stl")

  assert case result {
    Error(asset.AssetNotFound("missing.stl")) -> True
    _ -> False
  }
}

// Test: Get STL with wrong type
pub fn get_stl_wrong_type_test() {
  let cache = asset.new_cache()
  let audio = unsafe_mock_audio()
  let cache = asset.insert_asset(cache, "sound.mp3", asset.loaded_audio(audio))

  // Try to get as STL (wrong type)
  let result = asset.get_stl(cache, "sound.mp3")

  assert case result {
    Error(asset.InvalidAssetType("sound.mp3")) -> True
    _ -> False
  }
}

// Test: Try get (returns Option)
pub fn try_get_some_test() {
  let cache = asset.new_cache()
  let texture = unsafe_mock_texture()
  let cache =
    asset.insert_asset(cache, "test.png", asset.loaded_texture(texture))

  let result = asset.try_get(cache, "test.png")
  assert case result {
    option.Some(_) -> True
    option.None -> False
  }
}

// Test: Try get None
pub fn try_get_none_test() {
  let cache = asset.new_cache()
  let result = asset.try_get(cache, "missing.png")

  assert case result {
    option.None -> True
    option.Some(_) -> False
  }
}

// Test: Get cached URLs (empty)
pub fn cached_urls_empty_test() {
  let cache = asset.new_cache()
  let urls = asset.cached_urls(cache)

  assert urls == []
}

// Test: Get cached URLs (multiple)
pub fn cached_urls_multiple_test() {
  let cache = asset.new_cache()
  let texture = unsafe_mock_texture()
  let audio = unsafe_mock_audio()

  let cache =
    cache
    |> asset.insert_asset("texture.png", asset.loaded_texture(texture))
    |> asset.insert_asset("sound.mp3", asset.loaded_audio(audio))

  let urls = asset.cached_urls(cache)

  assert list.length(urls) == 2
  assert list.contains(urls, "texture.png")
  assert list.contains(urls, "sound.mp3")
}

// Test: Asset types are distinct
pub fn asset_types_distinct_test() {
  let model_asset = asset.ModelAsset("model.glb")
  let texture_asset = asset.TextureAsset("texture.png")
  let audio_asset = asset.AudioAsset("audio.mp3")
  let stl_asset = asset.STLAsset("model.stl")

  // These are different types
  assert case model_asset {
    asset.ModelAsset(_) -> True
  }

  assert case texture_asset {
    asset.TextureAsset(_) -> True
  }

  assert case audio_asset {
    asset.AudioAsset(_) -> True
  }

  assert case stl_asset {
    asset.STLAsset(_) -> True
  }
}

// Test: Error types
pub fn error_types_test() {
  let load_error = asset.AssetLoadError("file.png", "Network error")
  let not_found_error = asset.AssetNotFound("missing.png")
  let invalid_type_error = asset.InvalidAssetType("wrong.png")

  assert case load_error {
    asset.AssetLoadError(url, reason) ->
      url == "file.png" && reason == "Network error"
  }

  assert case not_found_error {
    asset.AssetNotFound(url) -> url == "missing.png"
  }

  assert case invalid_type_error {
    asset.InvalidAssetType(url) -> url == "wrong.png"
  }
}

// Test: Overwriting cached asset
pub fn overwrite_cached_asset_test() {
  let cache = asset.new_cache()
  let texture1 = unsafe_mock_texture()
  let texture2 = unsafe_mock_texture()

  // Insert first texture
  let cache =
    asset.insert_asset(cache, "texture.png", asset.loaded_texture(texture1))
  assert asset.cache_size(cache) == 1

  // Overwrite with second texture
  let cache =
    asset.insert_asset(cache, "texture.png", asset.loaded_texture(texture2))
  assert asset.cache_size(cache) == 1
  // Still only one entry

  // Should be able to retrieve it
  let result = asset.get_texture(cache, "texture.png")
  assert case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// Test: BatchLoadResult structure
pub fn batch_load_result_test() {
  let cache = asset.new_cache()
  let errors = [
    asset.AssetLoadError("file1.png", "Network error"),
    asset.AssetNotFound("file2.png"),
  ]

  let result = asset.BatchLoadResult(cache: cache, errors: errors)

  assert case result {
    asset.BatchLoadResult(c, e) ->
      asset.cache_size(c) == 0 && list.length(e) == 2
  }
}

// Test: LoadProgress structure
pub fn load_progress_test() {
  let progress =
    asset.LoadProgress(loaded: 5, total: 10, current_url: "test.png")

  assert case progress {
    asset.LoadProgress(loaded, total, url) ->
      loaded == 5 && total == 10 && url == "test.png"
  }
}

// --- Helper Functions (unsafe mocks for testing) ---

@external(javascript, "../tiramisu/ffi/test_helpers.mjs", "mockTexture")
fn unsafe_mock_texture() -> scene.Texture

@external(javascript, "../tiramisu/ffi/test_helpers.mjs", "mockAudio")
fn unsafe_mock_audio() -> audio.AudioBuffer

@external(javascript, "../tiramisu/ffi/test_helpers.mjs", "mockGeometry")
fn unsafe_mock_geometry() -> scene.BufferGeometry
