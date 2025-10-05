import tiramisu/three/geometry
import tiramisu/three/material

/// Opaque type wrapping THREE.Mesh
pub type Mesh

/// Create a new mesh from geometry and material
@external(javascript, "./ffi/mesh.mjs", "createMesh")
pub fn create(geometry: geometry.Geometry, material: material.Material) -> Mesh

/// Set the position of the mesh
@external(javascript, "./ffi/mesh.mjs", "setPosition")
pub fn set_position(mesh: Mesh, x x: Float, y y: Float, z z: Float) -> Mesh

/// Set the rotation of the mesh (in radians)
@external(javascript, "./ffi/mesh.mjs", "setRotation")
pub fn set_rotation(mesh: Mesh, x x: Float, y y: Float, z z: Float) -> Mesh

/// Set the scale of the mesh
@external(javascript, "./ffi/mesh.mjs", "setScale")
pub fn set_scale(mesh: Mesh, x: Float, y: Float, z: Float) -> Mesh

/// Rotate the mesh around the X axis
@external(javascript, "./ffi/mesh.mjs", "rotateX")
pub fn rotate_x(mesh: Mesh, angle: Float) -> Mesh

/// Rotate the mesh around the Y axis
@external(javascript, "./ffi/mesh.mjs", "rotateY")
pub fn rotate_y(mesh: Mesh, angle: Float) -> Mesh

/// Rotate the mesh around the Z axis
@external(javascript, "./ffi/mesh.mjs", "rotateZ")
pub fn rotate_z(mesh: Mesh, angle: Float) -> Mesh
