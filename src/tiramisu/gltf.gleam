import gleam/javascript/promise.{type Promise}
import gleam/list
import tiramisu/object3d.{type AnimationClip, type Object3D}

/// GLTF/GLB loading error
pub type GLTFError {
  LoadError(String)
  InvalidUrl(String)
  ParseError(String)
}

/// GLTF loaded data containing the scene and animations
pub type GLTFData {
  GLTFData(scene: Object3D, animations: List(AnimationClip))
}

/// Load a GLTF/GLB file from a URL using Promises
@external(javascript, "./ffi/gltf.mjs", "loadGLTFAsync")
pub fn load(url: String) -> Promise(Result(GLTFData, GLTFError))

/// Get the number of animations in the GLTF data
pub fn animation_count(data: GLTFData) -> Int {
  data.animations
  |> list.length
}

/// Get an animation clip by index
pub fn get_animation(data: GLTFData, index: Int) -> Result(AnimationClip, Nil) {
  data.animations
  |> list.drop(index)
  |> list.first
}
