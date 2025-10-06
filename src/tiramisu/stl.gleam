import gleam/javascript/promise.{type Promise}
import tiramisu/scene.{type BufferGeometry}

/// STL loading error
pub type STLError {
  LoadError(String)
  InvalidUrl(String)
  ParseError(String)
}

/// Load an STL file from a URL using Promises
@external(javascript, "./ffi/stl.mjs", "loadSTLAsync")
pub fn load(url: String) -> Promise(Result(BufferGeometry, STLError))
