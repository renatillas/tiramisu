import gleam/javascript/promise.{type Promise}
import savoiardi

/// Load a GLTF/GLB model from URL
pub fn load_gltf(url url: String) -> Promise(Result(savoiardi.GLTFData, Nil)) {
  savoiardi.load_gltf(url)
}

/// Load an OBJ model from URL
pub fn load_obj(
  obj_url obj_url: String,
) -> Promise(Result(savoiardi.Object3D, Nil)) {
  savoiardi.load_obj(obj_url)
}

/// Load an FBX model from URL
pub fn load_fbx(url url: String) -> Promise(Result(savoiardi.FBXData, Nil)) {
  savoiardi.load_fbx(url)
}
