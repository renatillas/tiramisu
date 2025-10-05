import tiramisu/three/texture

/// Opaque type wrapping THREE.Material variants
pub type Material

/// Create a basic material (unaffected by lights)
@external(javascript, "./ffi/material.mjs", "createBasicMaterial")
pub fn basic(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
) -> Material

/// Create a basic material with a texture
@external(javascript, "./ffi/material.mjs", "createBasicMaterialWithTexture")
pub fn basic_texture(
  texture texture: texture.Texture,
  transparent transparent: Bool,
  opacity opacity: Float,
) -> Material

/// Create a standard physically-based material
@external(javascript, "./ffi/material.mjs", "createStandardMaterial")
pub fn standard(
  color color: Int,
  metalness metalness: Float,
  roughness roughness: Float,
) -> Material

/// Create a phong material (for shiny surfaces)
@external(javascript, "./ffi/material.mjs", "createPhongMaterial")
pub fn phong(color color: Int, shininess shininess: Float) -> Material

/// Create a sprite material (for 2D sprites)
@external(javascript, "./ffi/material.mjs", "createSpriteMaterial")
pub fn sprite(texture: texture.Texture) -> Material
