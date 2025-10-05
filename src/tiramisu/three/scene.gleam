import tiramisu/three/mesh

/// Opaque type wrapping THREE.Scene
pub type Scene

/// Create a new Three.js scene
@external(javascript, "./ffi/scene.mjs", "createScene")
pub fn create() -> Scene

/// Add a mesh to the scene
@external(javascript, "./ffi/scene.mjs", "addToScene")
pub fn add(scene: Scene, object: mesh.Mesh) -> Scene

/// Remove a mesh from the scene
@external(javascript, "./ffi/scene.mjs", "removeFromScene")
pub fn remove(scene: Scene, object: mesh.Mesh) -> Scene

/// Set the background color of the scene
@external(javascript, "./ffi/scene.mjs", "setBackground")
pub fn set_background(scene: Scene, color: Int) -> Scene
