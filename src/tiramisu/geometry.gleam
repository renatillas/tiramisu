//// 3D geometry primitives and custom geometry loading.
////
//// This module provides validated constructors for common 3D shapes used in game development.
//// All constructors return `Result` types to ensure valid geometry parameters at compile time.
////
//// ## Primitive Shapes
////
//// ```gleam
//// // Box with Vec3 size (width, height, depth)
//// let assert Ok(cube) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
////
//// // Sphere with radius and segment counts
//// let assert Ok(ball) = geometry.sphere(radius: 1.0, segments: vec2.Vec2(32, 16))
////
//// // Plane with Vec2 size (width, height)
//// let assert Ok(floor) = geometry.plane(size: vec2.Vec2(10.0, 10.0))
//// ```
////
//// ## Loading External Geometry
////
//// Load STL files for custom 3D models:
////
//// ```gleam
//// let load_model = geometry.load_stl(
////   from: "models/part.stl",
////   on_success: ModelLoaded,
////   on_error: ModelFailed,
//// )
//// ```
////
//// ## 3D Text
////
//// Create extruded 3D text with loaded fonts:
////
//// ```gleam
//// // First load a font
//// let load_font = geometry.load_font(
////   from: "fonts/helvetica.json",
////   on_success: FontLoaded,
////   on_error: FontFailed,
//// )
////
//// // Then create text geometry
//// let assert Ok(text) = geometry.text(
////   text: "Hello!",
////   font: my_font,
////   size: 1.0,
////   depth: 0.2,
////   // ...
//// )
//// ```
////

import gleam/bool
import gleam/javascript/promise
import savoiardi
import tiramisu/effect
import vec/vec2.{type Vec2}
import vec/vec3.{type Vec3}

pub type CustomGeometry =
  savoiardi.Geometry

pub type Font =
  savoiardi.Font

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
  CustomGeometry(CustomGeometry)
  TextGeometry(
    text: String,
    font: savoiardi.Font,
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

// --- Validated Geometry Constructors ---

/// Create a validated box geometry.
///
/// All dimensions must be positive (> 0).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cube) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
/// let assert Ok(wall) = geometry.box(size: vec3.Vec3(10.0, 3.0, 0.1))
/// ```
pub fn box(size size: Vec3(Float)) -> Result(Geometry, GeometryError) {
  use <- bool.guard(size.x <=. 0.0, Error(NonPositiveWidth(size.x)))
  use <- bool.guard(size.y <=. 0.0, Error(NonPositiveHeight(size.y)))
  use <- bool.guard(size.z <=. 0.0, Error(NonPositiveDepth(size.z)))

  Ok(BoxGeometry(size.x, size.y, size.z))
}

/// Create a validated sphere geometry.
///
/// Radius must be positive. Width segments >= 3, height segments >= 2.
/// More segments = smoother sphere but more triangles.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(ball) = geometry.sphere(radius: 1.0, segments: vec2.Vec2(32, 16))
/// let assert Ok(low_poly) = geometry.sphere(radius: 1.0, segments: vec2.Vec2(8, 6))
/// ```
pub fn sphere(
  radius radius: Float,
  segments segments: Vec2(Int),
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
  use <- bool.guard(
    segments.x < 3,
    Error(LessThanThreeSegmentCountWidth(segments.x)),
  )
  use <- bool.guard(
    segments.y < 2,
    Error(LessThanTwoSegmentCountHeight(segments.y)),
  )

  Ok(SphereGeometry(radius, segments.x, segments.y))
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

pub fn plane(size size: Vec2(Float)) -> Result(Geometry, GeometryError) {
  sheet(size:, segments: vec2.Vec2(1, 1))
}

pub fn sheet(
  size size: Vec2(Float),
  segments segments: Vec2(Int),
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(size.x <=. 0.0, Error(NonPositiveWidth(size.x)))
  use <- bool.guard(size.y <=. 0.0, Error(NonPositiveHeight(size.y)))
  use <- bool.guard(segments.x < 1, Error(NegativeSegmentCount(segments.x)))
  use <- bool.guard(segments.y < 1, Error(NegativeSegmentCount(segments.y)))

  Ok(PlaneGeometry(
    width: size.x,
    height: size.y,
    width_segments: segments.x,
    height_segments: segments.y,
  ))
}

pub fn circle(
  radius radius: Float,
  segments segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(NonPositiveRadius(radius)))
  use <- bool.guard(segments < 3, Error(NegativeSegmentCount(segments)))

  Ok(CircleGeometry(radius, segments))
}

pub fn custom_geometry(geometry geometry: savoiardi.Geometry) -> Geometry {
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
/// Requires a font loaded via `geometry.load_font()`.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/geometry
///
/// // After loading font with geometry.load_font()...
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
/// - `font`: Loaded font from `geometry.load_font()`
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
  font font: savoiardi.Font,
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
pub fn create_geometry(geometry: Geometry) -> savoiardi.Geometry {
  case geometry {
    BoxGeometry(width:, height:, depth:) ->
      savoiardi.create_box_geometry(width, height, depth)
    CircleGeometry(radius:, segments:) ->
      savoiardi.create_circle_geometry(radius, segments)
    ConeGeometry(radius:, height:, segments:) ->
      savoiardi.create_cone_geometry(radius, height, segments)
    CustomGeometry(buffer) -> buffer
    CylinderGeometry(radius_top:, radius_bottom:, height:, radial_segments:) ->
      savoiardi.create_cylinder_geometry(
        radius_top,
        radius_bottom,
        height,
        radial_segments,
      )
    IcosahedronGeometry(radius:, detail:) ->
      savoiardi.create_icosahedron_geometry(radius, detail)
    PlaneGeometry(width:, height:, width_segments:, height_segments:) ->
      savoiardi.create_plane_geometry(
        width,
        height,
        width_segments,
        height_segments,
      )
    SphereGeometry(radius:, width_segments:, height_segments:) ->
      savoiardi.create_sphere_geometry(radius, width_segments, height_segments)
    TetrahedronGeometry(radius:, detail:) ->
      savoiardi.create_tetrahedron_geometry(radius, detail)
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
      savoiardi.create_text_geometry(
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
      savoiardi.create_torus_geometry(
        radius,
        tube,
        radial_segments,
        tubular_segments,
      )
  }
}

pub fn load_font(
  from url: String,
  on_success on_success: fn(Font) -> msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  let promise =
    savoiardi.load_font(url)
    |> promise.map(fn(result) {
      case result {
        Ok(data) -> on_success(data)
        Error(Nil) -> on_error
      }
    })

  effect.from_promise(promise)
}

pub fn load_stl(
  from url: String,
  on_success on_success: fn(CustomGeometry) -> msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  let promise =
    savoiardi.load_stl(url)
    |> promise.map(fn(result) {
      case result {
        Ok(data) -> on_success(data)
        Error(Nil) -> on_error
      }
    })

  effect.from_promise(promise)
}

/// Centers a geometry around its bounding box center.
///
/// This is useful for STL or OBJ models that need to rotate around their
/// geometric center rather than their original origin point.
///
/// Many CAD-exported models have their origin at an arbitrary location.
/// Call this function after loading to ensure the model rotates around
/// its visual center.
///
/// ## Example
///
/// ```gleam
/// use result <- promise.await(geometry.load_stl("/models/part.stl"))
/// case result {
///   Ok(geom) -> {
///     // Center the geometry so it rotates around its middle
///     let centered = geometry.center(geom)
///     // Use centered geometry in your mesh...
///   }
///   Error(Nil) -> // handle error
/// }
/// ```
pub fn center(geometry geometry: CustomGeometry) -> CustomGeometry {
  savoiardi.center_geometry(geometry)
}
