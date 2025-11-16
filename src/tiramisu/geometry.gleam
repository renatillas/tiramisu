//// Geometry module - validated 3D shapes and primitives.
////
//// Provides validated constructors for geometric primitives used in 3D rendering.
//// All geometry functions return `Result` to catch invalid parameters at construction time.
////
//// ## Core Concepts
////
//// - **Validation**: Geometry constructors validate dimensions (positive sizes, valid segment counts)
//// - **Primitives**: Built-in shapes like boxes, spheres, planes, cylinders, toruses
//// - **Custom Geometry**: Support for loading external geometry via asset system
//// - **Text Geometry**: 3D text rendering with loaded fonts
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/geometry
//// import tiramisu/scene
//// import tiramisu/material
////
//// // Create validated geometries
//// let assert Ok(cube) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
//// let assert Ok(ball) = geometry.sphere(radius: 0.5, width_segments: 32, height_segments: 16)
//// let assert Ok(ground) = geometry.plane(width: 100.0, height: 100.0)
////
//// // Use in scene
//// scene.Mesh(
////   id: "player",
////   geometry: cube,
////   material: my_material,
////   transform: transform.identity,
////   physics: option.None,
//// )
//// ```
////
//// ## Available Primitives
////
//// - **box**: Rectangular cuboid (e.g., walls, buildings, crates)
//// - **sphere**: Perfect sphere (e.g., balls, planets, bubbles)
//// - **plane**: Flat 2D rectangle (e.g., ground, walls, billboards)
//// - **circle**: Flat circle (e.g., coins, targets, platforms)
//// - **cylinder**: Cylinder with configurable top/bottom radius (e.g., pillars, trees, pipes)
//// - **cone**: Cone shape (e.g., traffic cones, party hats)
//// - **capsule**: Cylinder with rounded caps (e.g., character colliders)
//// - **torus**: Donut shape (e.g., rings, hoops)
//// - **tetrahedron**: 4-sided polyhedron (e.g., low-poly crystals)
//// - **icosahedron**: 20-sided polyhedron (e.g., smooth low-poly spheres, dice)
//// - **text**: 3D extruded text with beveling (e.g., logos, titles, signs)
////
//// ## Validation
////
//// All geometry constructors validate their parameters:
////
//// ```gleam
//// // Valid
//// let assert Ok(cube) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
////
//// // Invalid - negative dimensions
//// let assert Error(geometry.NonPositiveWidth(-1.0)) =
////   geometry.box(width: -1.0, height: 1.0, depth: 1.0)
////
//// // Invalid - too few segments
//// let assert Error(geometry.LessThanThreeSegmentCountWidth(2)) =
////   geometry.sphere(radius: 1.0, width_segments: 2, height_segments: 16)
//// ```

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
  PlaneGeometry(
    width: Float,
    height: Float,
    width_segments: Int,
    height_segments: Int,
  )
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
  TextGeometry(
    text: String,
    font: asset.Font,
    size: Float,
    depth: Float,
    curve_segments: Int,
    bevel_enabled: Bool,
    bevel_thickness: Float,
    bevel_size: Float,
    bevel_offset: Float,
    bevel_segments: Int,
  )
}

pub type GeometryError {
  NonPositiveWidth(width: Float)
  NonPositiveHeight(height: Float)
  NonPositiveDepth(depth: Float)
  NonPositiveRadius(radius: Float)
  InvalidGeometryTube(tube: Float)
  LessThanThreeSegmentCountWidth(count: Int)
  LessThanTwoSegmentCountHeight(count: Int)
  NegativeSegmentCount(count: Int)
  EmptyText
  NonPositiveSize(size: Float)
  NegativeBevelThickness(thickness: Float)
  NegativeBevelSize(size: Float)
}

@internal
pub type ThreeGeometry

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
  use <- bool.guard(width <=. 0.0, Error(NonPositiveWidth(width)))
  use <- bool.guard(height <=. 0.0, Error(NonPositiveHeight(height)))
  use <- bool.guard(depth <=. 0.0, Error(NonPositiveDepth(depth)))

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
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
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
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
  use <- bool.guard(height <=. 0.0, Error(NonPositiveHeight(height)))
  use <- bool.guard(segments < 3, Error(NegativeSegmentCount(segments)))

  Ok(ConeGeometry(radius, height, segments))
}

pub fn plane(
  width width: Float,
  height height: Float,
) -> Result(Geometry, GeometryError) {
  sheet(width:, height:, width_segments: 1, height_segments: 1)
}

pub fn sheet(
  width width: Float,
  height height: Float,
  width_segments width_segments: Int,
  height_segments height_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(width <=. 0.0, Error(NonPositiveWidth(width)))
  use <- bool.guard(height <=. 0.0, Error(NonPositiveHeight(height)))
  use <- bool.guard(
    width_segments < 1,
    Error(NegativeSegmentCount(width_segments)),
  )
  use <- bool.guard(
    height_segments < 1,
    Error(NegativeSegmentCount(height_segments)),
  )

  Ok(PlaneGeometry(width:, height:, width_segments:, height_segments:))
}

pub fn circle(
  radius radius: Float,
  segments segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
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
  use <- bool.guard(radius_top <. 0.0, Error(NonPositiveRadius(radius_top)))
  use <- bool.guard(
    radius_bottom <. 0.0,
    Error(NonPositiveRadius(radius_bottom)),
  )
  use <- bool.guard(height <=. 0.0, Error(NonPositiveHeight(height)))
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
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
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
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
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
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
  use <- bool.guard(detail < 0, Error(NegativeSegmentCount(detail)))

  Ok(IcosahedronGeometry(radius, detail))
}

/// Create validated 3D text geometry from a loaded font.
///
/// Renders text as 3D geometry with optional beveling (rounded edges).
/// Requires a font loaded via `asset.load_font()`.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/geometry
/// import tiramisu/asset
///
/// // After loading font...
/// let assert Ok(text) = geometry.text(
///   text: "Hello!",
///   font: my_font,
///   size: 1.0,
///   depth: 0.2,
///   curve_segments: 12,
///   bevel_enabled: True,
///   bevel_thickness: 0.05,
///   bevel_size: 0.02,
///   bevel_offset: 0.0,
///   bevel_segments: 5,
/// )
/// ```
///
/// ## Parameters
///
/// - `text`: String to render (must not be empty)
/// - `font`: Loaded font from `asset.load_font()`
/// - `size`: Text size (must be positive)
/// - `depth`: Extrusion depth for 3D effect (0 for flat, >0 for 3D)
/// - `curve_segments`: Quality of curves (more = smoother but more triangles)
/// - `bevel_enabled`: Whether to add rounded edges
/// - `bevel_thickness`: How deep bevels cut into text
/// - `bevel_size`: How far bevels extend outward
/// - `bevel_offset`: Starting distance of bevel from outline
/// - `bevel_segments`: Smoothness of bevel (more = rounder)
pub fn text(
  text text: String,
  font font: asset.Font,
  size size: Float,
  depth depth: Float,
  curve_segments curve_segments: Int,
  bevel_enabled bevel_enabled: Bool,
  bevel_thickness bevel_thickness: Float,
  bevel_size bevel_size: Float,
  bevel_offset bevel_offset: Float,
  bevel_segments bevel_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(text == "", Error(EmptyText))
  use <- bool.guard(size <=. 0.0, Error(NonPositiveSize(size)))
  use <- bool.guard(depth <. 0.0, Error(NonPositiveDepth(depth)))
  use <- bool.guard(
    curve_segments < 1,
    Error(NegativeSegmentCount(curve_segments)),
  )
  use <- bool.guard(
    bevel_thickness <. 0.0,
    Error(NegativeBevelThickness(bevel_thickness)),
  )
  use <- bool.guard(bevel_size <. 0.0, Error(NegativeBevelSize(bevel_size)))
  use <- bool.guard(
    bevel_segments < 1,
    Error(NegativeSegmentCount(bevel_segments)),
  )

  Ok(TextGeometry(
    text,
    font,
    size,
    depth,
    curve_segments,
    bevel_enabled,
    bevel_thickness,
    bevel_size,
    bevel_offset,
    bevel_segments,
  ))
}

@internal
pub fn create_geometry(geometry: Geometry) -> ThreeGeometry {
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
    PlaneGeometry(width:, height:, width_segments:, height_segments:) ->
      create_plane_geometry(width, height, width_segments, height_segments)
    SphereGeometry(radius:, width_segments:, height_segments:) ->
      create_sphere_geometry(radius, width_segments, height_segments)
    TetrahedronGeometry(radius:, detail:) ->
      create_tetrahedron_geometry(radius, detail)
    TextGeometry(
      text:,
      font:,
      size:,
      depth:,
      curve_segments:,
      bevel_enabled:,
      bevel_thickness:,
      bevel_size:,
      bevel_offset:,
      bevel_segments:,
    ) ->
      create_text_geometry(
        text,
        font,
        size,
        depth,
        curve_segments,
        bevel_enabled,
        bevel_thickness,
        bevel_size,
        bevel_offset,
        bevel_segments,
      )
    TorusGeometry(radius:, tube:, radial_segments:, tubular_segments:) ->
      create_torus_geometry(radius, tube, radial_segments, tubular_segments)
  }
}

// Note: Custom geometry is just a passthrough since the buffer is already created
// Use identity function to return the buffer as-is
@external(javascript, "../threejs.ffi.mjs", "identity")
fn create_custom_geometry(buffer: asset.BufferGeometry) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createConeGeometry")
fn create_cone_geometry(
  radius: Float,
  height: Float,
  segments: Int,
) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createCircleGeometry")
fn create_circle_geometry(radius: Float, segments: Int) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createBoxGeometry")
fn create_box_geometry(
  width: Float,
  height: Float,
  depth: Float,
) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createCylinderGeometry")
fn create_cylinder_geometry(
  radius_top: Float,
  radius_bottom: Float,
  height: Float,
  radial_segments: Int,
) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createIcosahedronGeometry")
fn create_icosahedron_geometry(radius: Float, detail: Int) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createPlaneGeometry")
fn create_plane_geometry(
  width: Float,
  height: Float,
  width_segments: Int,
  height_segments: Int,
) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createSphereGeometry")
fn create_sphere_geometry(
  radius: Float,
  width_segments: Int,
  height_segments: Int,
) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createTetrahedronGeometry")
fn create_tetrahedron_geometry(radius: Float, detail: Int) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createTorusGeometry")
fn create_torus_geometry(
  radius: Float,
  tube: Float,
  radial_segments: Int,
  tubular_segments: Int,
) -> ThreeGeometry

@external(javascript, "../threejs.ffi.mjs", "createTextGeometry")
fn create_text_geometry(
  text: String,
  font: asset.Font,
  size: Float,
  depth: Float,
  curve_segments: Int,
  bevel_enabled: Bool,
  bevel_thickness: Float,
  bevel_size: Float,
  bevel_offset: Float,
  bevel_segments: Int,
) -> ThreeGeometry
