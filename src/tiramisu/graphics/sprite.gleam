import tiramisu/three/geometry
import tiramisu/three/material
import tiramisu/three/mesh
import tiramisu/three/texture

/// Create a 2D sprite from a texture using a plane geometry
pub fn create(tex: texture.Texture, width: Float, height: Float) -> mesh.Mesh {
  let geometry = geometry.plane(width, height)
  let material = material.basic_texture(texture: tex, transparent: True, opacity: 1.0)
  mesh.create(geometry, material)
}

/// Create a sprite with explicit size (1:1 pixel mapping)
pub fn create_sized(tex: texture.Texture, size: Float) -> mesh.Mesh {
  create(tex, size, size)
}

/// Create a sprite with default size of 1.0
pub fn from_texture(tex: texture.Texture) -> mesh.Mesh {
  create(tex, 1.0, 1.0)
}
