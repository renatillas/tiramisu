//// Background type for scene rendering.
////
//// Defines what is rendered behind all scene objects. Can be either a solid color,
//// a texture image, or a cube texture (skybox).

/// Background type for the scene.
///
/// Defines what is rendered behind all scene objects. Can be either a solid color,
/// a texture image, an equirectangular (360°) texture, or a cube texture (skybox).
///
/// ## Variants
///
/// - `Color(Int)`: Solid color background using hex color (e.g., 0x111111)
/// - `Texture(String)`: 2D texture image loaded from a URL or path
/// - `EquirectangularTexture(String)`: 360° spherical texture (equirectangular projection)
/// - `CubeTexture(List(String))`: Cube texture (skybox) with 6 faces [px, nx, py, ny, pz, nz]
///
/// ## Examples
///
/// ```gleam
/// import tiramisu
/// import tiramisu/background
///
/// // Solid color background
/// tiramisu.run(
///   dimensions: None,
///   background: background.Color(0x1a1a2e),
///   // ...
/// )
///
/// // Flat texture background
/// tiramisu.run(
///   dimensions: None,
///   background: background.Texture("assets/sky.jpg"),
///   // ...
/// )
///
/// // Spherical 360° background (equirectangular)
/// tiramisu.run(
///   dimensions: None,
///   background: background.EquirectangularTexture("assets/hdri/sunset.hdr"),
///   // ...
/// )
///
/// // Skybox with 6 faces
/// tiramisu.run(
///   dimensions: None,
///   background: background.CubeTexture([
///     "assets/skybox/px.jpg",  // positive x
///     "assets/skybox/nx.jpg",  // negative x
///     "assets/skybox/py.jpg",  // positive y
///     "assets/skybox/ny.jpg",  // negative y
///     "assets/skybox/pz.jpg",  // positive z
///     "assets/skybox/nz.jpg",  // negative z
///   ]),
///   // ...
/// )
/// ```
pub type Background {
  /// Solid color background (hex color, e.g., 0x111111)
  Color(Int)
  /// 2D texture background loaded from URL or path
  Texture(String)
  /// Equirectangular (360° spherical) texture background
  EquirectangularTexture(String)
  /// Cube texture (skybox) with 6 face images [px, nx, py, ny, pz, nz]
  CubeTexture(List(String))
}
