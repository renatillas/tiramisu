import tiramisu/geometry
import vec/vec2
import vec/vec3

pub fn box_valid_test() {
  let assert Ok(_) = geometry.box(size: vec3.Vec3(1.0, 2.0, 3.0))
}

pub fn box_invalid_dimensions_test() {
  let assert Error(geometry.NonPositiveWidth(0.0)) =
    geometry.box(size: vec3.Vec3(0.0, 1.0, 1.0))
}

pub fn box_invalid_height_test() {
  let assert Error(geometry.NonPositiveHeight(-1.0)) =
    geometry.box(size: vec3.Vec3(1.0, -1.0, 1.0))
}

pub fn box_invalid_depth_test() {
  let assert Error(geometry.NonPositiveDepth(0.0)) =
    geometry.box(size: vec3.Vec3(1.0, 1.0, 0.0))
}

pub fn sphere_valid_test() {
  let assert Ok(_) = geometry.sphere(radius: 5.0, segments: vec2.Vec2(32, 16))
}

pub fn sphere_invalid_radius_test() {
  let assert Error(geometry.NonPositiveRadius(0.0)) =
    geometry.sphere(radius: 0.0, segments: vec2.Vec2(32, 16))
}

pub fn sphere_invalid_width_segments_test() {
  let assert Error(geometry.LessThanThreeSegmentCountWidth(2)) =
    geometry.sphere(radius: 5.0, segments: vec2.Vec2(2, 16))
}

pub fn sphere_invalid_height_segments_test() {
  let assert Error(geometry.LessThanTwoSegmentCountHeight(1)) =
    geometry.sphere(radius: 5.0, segments: vec2.Vec2(32, 1))
}

pub fn plane_valid_test() {
  let assert Ok(_) = geometry.plane(size: vec2.Vec2(10.0, 5.0))
}

pub fn plane_invalid_width_test() {
  let assert Error(geometry.NonPositiveWidth(0.0)) =
    geometry.plane(size: vec2.Vec2(0.0, 5.0))
}

pub fn plane_invalid_height_test() {
  let assert Error(geometry.NonPositiveHeight(-1.0)) =
    geometry.plane(size: vec2.Vec2(10.0, -1.0))
}

pub fn sheet_valid_test() {
  let assert Ok(_) =
    geometry.sheet(size: vec2.Vec2(10.0, 5.0), segments: vec2.Vec2(1, 1))
}

pub fn sheet_invalid_width_test() {
  let assert Error(geometry.NonPositiveWidth(0.0)) =
    geometry.sheet(size: vec2.Vec2(0.0, 5.0), segments: vec2.Vec2(1, 1))
}

pub fn sheet_invalid_height_test() {
  let assert Error(geometry.NonPositiveHeight(0.0)) =
    geometry.sheet(size: vec2.Vec2(10.0, 0.0), segments: vec2.Vec2(1, 1))
}

pub fn sheet_invalid_width_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(0)) =
    geometry.sheet(size: vec2.Vec2(10.0, 5.0), segments: vec2.Vec2(0, 1))
}

pub fn sheet_invalid_height_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(0)) =
    geometry.sheet(size: vec2.Vec2(10.0, 5.0), segments: vec2.Vec2(1, 0))
}

pub fn circle_valid_test() {
  let assert Ok(_) = geometry.circle(radius: 3.0, segments: 64)
}

pub fn circle_invalid_radius_test() {
  let assert Error(geometry.NonPositiveRadius(0.0)) =
    geometry.circle(radius: 0.0, segments: 64)
}

pub fn circle_invalid_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(2)) =
    geometry.circle(radius: 3.0, segments: 2)
}

pub fn cylinder_valid_test() {
  let assert Ok(_) =
    geometry.cylinder(
      radius_top: 1.0,
      radius_bottom: 2.0,
      height: 5.0,
      radial_segments: 32,
    )
}

pub fn cylinder_invalid_radius_top_test() {
  let assert Error(geometry.NonPositiveRadius(-1.0)) =
    geometry.cylinder(
      radius_top: -1.0,
      radius_bottom: 2.0,
      height: 5.0,
      radial_segments: 32,
    )
}

pub fn cylinder_invalid_radius_bottom_test() {
  let assert Error(geometry.NonPositiveRadius(-1.0)) =
    geometry.cylinder(
      radius_top: 1.0,
      radius_bottom: -1.0,
      height: 5.0,
      radial_segments: 32,
    )
}

pub fn cylinder_invalid_height_test() {
  let assert Error(geometry.NonPositiveHeight(0.0)) =
    geometry.cylinder(
      radius_top: 1.0,
      radius_bottom: 2.0,
      height: 0.0,
      radial_segments: 32,
    )
}

pub fn cylinder_invalid_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(2)) =
    geometry.cylinder(
      radius_top: 1.0,
      radius_bottom: 2.0,
      height: 5.0,
      radial_segments: 2,
    )
}

pub fn cone_valid_test() {
  let assert Ok(_) = geometry.cone(radius: 2.0, height: 4.0, segments: 16)
}

pub fn cone_invalid_radius_test() {
  let assert Error(geometry.NonPositiveRadius(0.0)) =
    geometry.cone(radius: 0.0, height: 4.0, segments: 16)
}

pub fn cone_invalid_height_test() {
  let assert Error(geometry.NonPositiveHeight(-1.0)) =
    geometry.cone(radius: 2.0, height: -1.0, segments: 16)
}

pub fn cone_invalid_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(2)) =
    geometry.cone(radius: 2.0, height: 4.0, segments: 2)
}

pub fn torus_valid_test() {
  let assert Ok(_) =
    geometry.torus(
      radius: 5.0,
      tube: 1.0,
      radial_segments: 16,
      tubular_segments: 100,
    )
}

pub fn torus_invalid_radius_test() {
  let assert Error(geometry.NonPositiveRadius(0.0)) =
    geometry.torus(
      radius: 0.0,
      tube: 1.0,
      radial_segments: 16,
      tubular_segments: 100,
    )
}

pub fn torus_invalid_tube_test() {
  let assert Error(geometry.InvalidGeometryTube(-1.0)) =
    geometry.torus(
      radius: 5.0,
      tube: -1.0,
      radial_segments: 16,
      tubular_segments: 100,
    )
}

pub fn torus_invalid_radial_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(2)) =
    geometry.torus(
      radius: 5.0,
      tube: 1.0,
      radial_segments: 2,
      tubular_segments: 100,
    )
}

pub fn torus_invalid_tubular_segments_test() {
  let assert Error(geometry.NegativeSegmentCount(2)) =
    geometry.torus(
      radius: 5.0,
      tube: 1.0,
      radial_segments: 16,
      tubular_segments: 2,
    )
}
