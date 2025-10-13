import gleeunit
import tiramisu/asset

pub fn main() {
  gleeunit.main()
}

// Cache Creation Tests

pub fn new_cache_test() {
  let cache = asset.new_cache()

  assert 0 == asset.cache_size(cache)
}

pub fn new_cache_with_size_test() {
  let cache = asset.new_cache_with_size(50)

  assert 0 == asset.cache_size(cache)
}

// Is Cached Tests

pub fn is_cached_empty_test() {
  let cache = asset.new_cache()

  let assert False = asset.is_cached(cache, "test.jpg")
}

// Cached URLs Tests

pub fn cached_urls_empty_test() {
  let cache = asset.new_cache()

  assert [] == asset.cached_urls(cache)
}

// Clear Cache Tests

pub fn clear_cache_test() {
  let cache = asset.new_cache()

  assert 0 == asset.clear_cache(cache) |> asset.cache_size()
}
