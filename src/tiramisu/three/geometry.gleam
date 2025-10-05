pub type Geometry

/// Create a box geometry
@external(javascript, "./ffi/geometry.mjs", "createBoxGeometry")
pub fn box(width: Float, height: Float, depth: Float) -> Geometry

/// Create a plane geometry
@external(javascript, "./ffi/geometry.mjs", "createPlaneGeometry")
pub fn plane(width: Float, height: Float) -> Geometry

/// Create a sphere geometry
@external(javascript, "./ffi/geometry.mjs", "createSphereGeometry")
pub fn sphere(
  radius: Float,
  width_segments: Int,
  height_segments: Int,
) -> Geometry

/// Create a circle geometry
@external(javascript, "./ffi/geometry.mjs", "createCircleGeometry")
pub fn circle(radius radius: Float, segments segments: Int) -> Geometry

/// Create a cone geometry
@external(javascript, "./ffi/geometry.mjs", "createConeGeometry")
pub fn cone(
  radius radius: Float,
  height height: Float,
  segments segments: Int,
) -> Geometry
