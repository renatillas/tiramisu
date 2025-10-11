import gleam/bool
import tiramisu/asset

/// 3D geometry types supported by the engine.
///
/// Each variant represents a different primitive shape or custom geometry.
/// Use the validated constructor functions like `box()`, `sphere()`, etc.
pub opaque type Geometry {
  BoxGeometry(width: Float, height: Float, depth: Float)
  SphereGeometry(radius: Float, width_segments: Int, height_segments: Int)
  ConeGeometry(radius: Float, height: Float, segments: Int)
  PlaneGeometry(width: Float, height: Float)
  CircleGeometry(radius: Float, segments: Int)
  CylinderGeometry(
    radius_top: Float,
    radius_bottom: Float,
    height: Float,
    radial_segments: Int,
  )
  TorusGeometry(
    radius: Float,
    tube: Float,
    radial_segments: Int,
    tubular_segments: Int,
  )
  TetrahedronGeometry(radius: Float, detail: Int)
  IcosahedronGeometry(radius: Float, detail: Int)
  CustomGeometry(asset.BufferGeometry)
}

pub type GeometryError {
  NegativeWidth(width: Float)
  NegativeHeight(height: Float)
  InvalidGeometryDepth(depth: Float)
  NegativeRadius(radius: Float)
  InvalidGeometryTube(tube: Float)
  LessThanThreeSegmentCountWidth(count: Int)
  LessThanTwoSegmentCountHeight(count: Int)
  NegativeSegmentCount(count: Int)
}

// --- Validated Geometry Constructors ---

/// Create a validated box geometry.
///
/// All dimensions must be positive (> 0).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cube) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
/// let assert Ok(wall) = scene.box(width: 10.0, height: 3.0, depth: 0.1)
/// ```
pub fn box(
  width width: Float,
  height height: Float,
  depth depth: Float,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(width <=. 0.0, Error(NegativeWidth(width)))
  use <- bool.guard(height <=. 0.0, Error(NegativeHeight(height)))
  use <- bool.guard(depth <=. 0.0, Error(InvalidGeometryDepth(depth)))

  Ok(BoxGeometry(width, height, depth))
}

/// Create a validated sphere geometry.
///
/// Radius must be positive. Width segments >= 3, height segments >= 2.
/// More segments = smoother sphere but more triangles.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(ball) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 16)
/// let assert Ok(low_poly) = scene.sphere(radius: 1.0, width_segments: 8, height_segments: 6)
/// ```
pub fn sphere(
  radius radius: Float,
  width_segments width_segments: Int,
  height_segments height_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NegativeRadius(radius)))
  use <- bool.guard(
    width_segments < 3,
    Error(LessThanThreeSegmentCountWidth(width_segments)),
  )
  use <- bool.guard(
    height_segments < 2,
    Error(LessThanTwoSegmentCountHeight(height_segments)),
  )

  Ok(SphereGeometry(radius, width_segments, height_segments))
}

pub fn cone(
  radius radius: Float,
  height height: Float,
  segments segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NegativeRadius(radius)))
  use <- bool.guard(height <=. 0.0, Error(NegativeHeight(height)))
  use <- bool.guard(segments < 3, Error(NegativeSegmentCount(segments)))

  Ok(ConeGeometry(radius, height, segments))
}

pub fn plane(
  width width: Float,
  height height: Float,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(width <=. 0.0, Error(NegativeWidth(width)))
  use <- bool.guard(height <=. 0.0, Error(NegativeHeight(height)))

  Ok(PlaneGeometry(width, height))
}

pub fn circle(
  radius radius: Float,
  segments segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NegativeRadius(radius)))
  use <- bool.guard(segments < 3, Error(NegativeSegmentCount(segments)))

  Ok(CircleGeometry(radius, segments))
}

pub fn custom_geometry(geometry geometry: asset.BufferGeometry) -> Geometry {
  CustomGeometry(geometry)
}

/// Create a validated cylinder geometry.
///
/// Both radii must be non-negative, height positive, radial segments >= 3.
/// Set one radius to 0 to create a cone shape.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cylinder) = scene.cylinder(radius_top: 1.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)
/// let assert Ok(cone) = scene.cylinder(radius_top: 0.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)
/// ```
pub fn cylinder(
  radius_top radius_top: Float,
  radius_bottom radius_bottom: Float,
  height height: Float,
  radial_segments radial_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius_top <. 0.0, Error(NegativeRadius(radius_top)))
  use <- bool.guard(radius_bottom <. 0.0, Error(NegativeRadius(radius_bottom)))
  use <- bool.guard(height <=. 0.0, Error(NegativeHeight(height)))
  use <- bool.guard(
    radial_segments < 3,
    Error(NegativeSegmentCount(radial_segments)),
  )

  Ok(CylinderGeometry(radius_top, radius_bottom, height, radial_segments))
}

/// Create a validated torus (donut) geometry.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(donut) = scene.torus(radius: 2.0, tube: 0.5, radial_segments: 16, tubular_segments: 100)
/// ```
pub fn torus(
  radius radius: Float,
  tube tube: Float,
  radial_segments radial_segments: Int,
  tubular_segments tubular_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NegativeRadius(radius)))
  use <- bool.guard(tube <=. 0.0, Error(InvalidGeometryTube(tube)))
  use <- bool.guard(
    radial_segments < 3,
    Error(NegativeSegmentCount(radial_segments)),
  )
  use <- bool.guard(
    tubular_segments < 3,
    Error(NegativeSegmentCount(tubular_segments)),
  )

  Ok(TorusGeometry(radius, tube, radial_segments, tubular_segments))
}

/// Create a validated tetrahedron (4-sided polyhedron) geometry.
///
/// Detail level controls subdivision (0 = no subdivision, higher = more triangles).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(shape) = scene.tetrahedron(radius: 1.0, detail: 0)
/// ```
pub fn tetrahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NegativeRadius(radius)))
  use <- bool.guard(detail < 0, Error(NegativeSegmentCount(detail)))

  Ok(TetrahedronGeometry(radius, detail))
}

/// Create a validated icosahedron (20-sided polyhedron) geometry.
///
/// Detail level controls subdivision. Good for creating spheres with flat faces.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(shape) = scene.icosahedron(radius: 1.0, detail: 2)
/// ```
pub fn icosahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NegativeRadius(radius)))
  use <- bool.guard(detail < 0, Error(NegativeSegmentCount(detail)))

  Ok(IcosahedronGeometry(radius, detail))
}

@internal
pub fn create_geometry(geometry: Geometry) -> Nil {
  case geometry {
    BoxGeometry(width:, height:, depth:) ->
      create_box_geometry(width, height, depth)
    CircleGeometry(radius:, segments:) ->
      create_circle_geometry(radius, segments)
    ConeGeometry(radius:, height:, segments:) ->
      create_cone_geometry(radius, height, segments)
    CustomGeometry(buffer) -> create_custom_geometry(buffer)
    CylinderGeometry(radius_top:, radius_bottom:, height:, radial_segments:) ->
      create_cylinder_geometry(
        radius_top,
        radius_bottom,
        height,
        radial_segments,
      )
    IcosahedronGeometry(radius:, detail:) ->
      create_icosahedron_geometry(radius, detail)
    PlaneGeometry(width:, height:) -> create_plane_geometry(width, height)
    SphereGeometry(radius:, width_segments:, height_segments:) ->
      create_sphere_geometry(radius, width_segments, height_segments)
    TetrahedronGeometry(radius:, detail:) ->
      create_tetrahedron_geometry(radius, detail)
    TorusGeometry(radius:, tube:, radial_segments:, tubular_segments:) ->
      create_torus_geometry(radius, tube, radial_segments, tubular_segments)
  }
}

// Note: Custom geometry is just a passthrough since the buffer is already created
fn create_custom_geometry(_buffer: asset.BufferGeometry) -> Nil {
  Nil
}

@external(javascript, "../threejs.ffi.mjs", "createConeGeometry")
fn create_cone_geometry(radius: Float, height: Float, segments: Int) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createCircleGeometry")
fn create_circle_geometry(radius: Float, segments: Int) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createBoxGeometry")
fn create_box_geometry(width: Float, height: Float, depth: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createCylinderGeometry")
fn create_cylinder_geometry(
  radius_top: Float,
  radius_bottom: Float,
  height: Float,
  radial_segments: Int,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createIcosahedronGeometry")
fn create_icosahedron_geometry(radius: Float, detail: Int) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createPlaneGeometry")
fn create_plane_geometry(width: Float, height: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createSphereGeometry")
fn create_sphere_geometry(
  radius: Float,
  width_segments: Int,
  height_segments: Int,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createTetrahedronGeometry")
fn create_tetrahedron_geometry(radius: Float, detail: Int) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createTorusGeometry")
fn create_torus_geometry(
  radius: Float,
  tube: Float,
  radial_segments: Int,
  tubular_segments: Int,
) -> Nil
