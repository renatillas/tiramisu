import gleam/option
import tiramisu/scene.{type Texture}
import tiramisu/transform

/// Create a 2D sprite mesh node from a texture
pub fn mesh(
  id id: String,
  texture texture: Texture,
  width width: Float,
  height height: Float,
) -> scene.SceneNode {
  let geometry = scene.PlaneGeometry(width, height)
  let material =
    scene.BasicMaterial(
      color: 0xffffff,
      transparent: True,
      opacity: 1.0,
      map: option.Some(texture),
    )

  scene.Mesh(id, geometry, material, transform.identity(), option.None)
}

/// Create a square sprite mesh node with equal width and height
pub fn mesh_sized(
  id id: String,
  texture texture: Texture,
  size size: Float,
) -> scene.SceneNode {
  mesh(id, texture, size, size)
}

/// Create a sprite mesh node with default size of 1.0
pub fn from_texture(id id: String, texture texture: Texture) -> scene.SceneNode {
  mesh(id, texture, 1.0, 1.0)
}
