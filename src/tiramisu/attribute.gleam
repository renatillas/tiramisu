//// Attribute constructors for Tiramisu scene nodes.
////
//// This module provides attribute constructors for configuring 3D scene objects.
//// Attributes are applied to elements created with `tiramisu/element` and control
//// their appearance, behavior, and placement in the scene.
////
//// ## Categories
////
//// - **Transform**: position, rotation, scale, look_at
//// - **Geometry**: geometry type and parameters
//// - **Material**: color, metalness, roughness, textures
//// - **Camera**: FOV, near/far, projection, viewport
//// - **Light**: type, intensity, shadows
//// - **Physics**: rigid body, colliders, mass
//// - **Model**: animations, material overrides
////
//// ## Example
////
//// ```gleam
//// import tiramisu/element
//// import tiramisu/attribute as attr
//// import vec/vec3
////
//// element.mesh("cube", [
////   attr.position(vec3.Vec3(0.0, 1.0, 0.0)),
////   attr.rotation_xyz(0.0, 0.785, 0.0),
////   attr.color(0xff0000),
////   attr.metalness(0.8),
////   attr.roughness(0.2),
//// ], [])
//// ```

// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import lustre/attribute.{type Attribute}
import vec/vec3.{type Vec3}

// TRANSFORM ATTRIBUTES --------------------------------------------------------

/// Set the position of an object in 3D space.
///
/// ## Example
///
/// ```gleam
/// attr.position(vec3.Vec3(0.0, 5.0, 0.0))
/// ```
///
pub fn position(pos: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = pos
  attribute.attribute(
    "data-position",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the position using individual X, Y, Z components.
///
pub fn position_xyz(x: Float, y: Float, z: Float) -> Attribute(msg) {
  position(vec3.Vec3(x, y, z))
}

/// Set the rotation of an object using Euler angles (in radians).
///
/// ## Example
///
/// ```gleam
/// attr.rotation(vec3.Vec3(0.0, 1.57, 0.0))  // 90 degrees around Y axis
/// ```
///
pub fn rotation(euler: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = euler
  attribute.attribute(
    "data-rotation",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the rotation using individual X, Y, Z Euler angles.
///
pub fn rotation_xyz(x: Float, y: Float, z: Float) -> Attribute(msg) {
  rotation(vec3.Vec3(x, y, z))
}

/// Set the scale of an object.
///
/// ## Example
///
/// ```gleam
/// attr.scale(vec3.Vec3(2.0, 1.0, 2.0))  // Stretch on X and Z
/// ```
///
pub fn scale(s: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = s
  attribute.attribute(
    "data-scale",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set uniform scale (same on all axes).
///
pub fn scale_uniform(s: Float) -> Attribute(msg) {
  scale(vec3.Vec3(s, s, s))
}

/// Make an object look at a specific point.
///
/// Useful for cameras or directional lights that need to point at something.
///
pub fn look_at(target: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = target
  attribute.attribute(
    "data-look-at",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

// GEOMETRY ATTRIBUTES ---------------------------------------------------------

/// Set a box geometry with width, height, and depth.
///
/// ## Example
///
/// ```gleam
/// attr.geometry_box(1.0, 2.0, 1.0)  // Tall thin box
/// ```
///
pub fn geometry_box(width: Float, height: Float, depth: Float) -> Attribute(msg) {
  attribute.attribute(
    "data-geometry",
    "box:"
      <> float.to_string(width)
      <> ","
      <> float.to_string(height)
      <> ","
      <> float.to_string(depth),
  )
}

/// Set a sphere geometry with radius and segments.
///
pub fn geometry_sphere(
  radius: Float,
  width_segments: Int,
  height_segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "data-geometry",
    "sphere:"
      <> float.to_string(radius)
      <> ","
      <> int.to_string(width_segments)
      <> ","
      <> int.to_string(height_segments),
  )
}

/// Set a plane geometry with width and height.
///
pub fn geometry_plane(width: Float, height: Float) -> Attribute(msg) {
  attribute.attribute(
    "data-geometry",
    "plane:" <> float.to_string(width) <> "," <> float.to_string(height),
  )
}

/// Set a cylinder geometry.
///
pub fn geometry_cylinder(
  radius_top: Float,
  radius_bottom: Float,
  height: Float,
  segments: Int,
) -> Attribute(msg) {
  attribute.attribute(
    "data-geometry",
    "cylinder:"
      <> float.to_string(radius_top)
      <> ","
      <> float.to_string(radius_bottom)
      <> ","
      <> float.to_string(height)
      <> ","
      <> int.to_string(segments),
  )
}

// MATERIAL ATTRIBUTES ---------------------------------------------------------

/// Set the base color of a material (as a hex integer).
///
/// ## Example
///
/// ```gleam
/// attr.color(0xff0000)  // Red
/// attr.color(0x00ff00)  // Green
/// attr.color(0x0000ff)  // Blue
/// ```
///
pub fn color(hex: Int) -> Attribute(msg) {
  attribute.attribute("data-color", "#" <> int.to_base16(hex))
}

/// Set the metalness of a material (0.0 = dielectric, 1.0 = metal).
///
pub fn metalness(m: Float) -> Attribute(msg) {
  attribute.attribute("data-metalness", float.to_string(m))
}

/// Set the roughness of a material (0.0 = smooth, 1.0 = rough).
///
pub fn roughness(r: Float) -> Attribute(msg) {
  attribute.attribute("data-roughness", float.to_string(r))
}

/// Set the opacity of a material (0.0 = transparent, 1.0 = opaque).
///
pub fn opacity(o: Float) -> Attribute(msg) {
  attribute.attribute("data-opacity", float.to_string(o))
}

/// Enable or disable wireframe rendering.
///
pub fn wireframe(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("data-wireframe", case enabled {
    True -> "true"
    False -> "false"
  })
}

// CAMERA ATTRIBUTES -----------------------------------------------------------

/// Set the field of view for a perspective camera (in degrees).
///
pub fn fov(degrees: Float) -> Attribute(msg) {
  attribute.attribute("data-fov", float.to_string(degrees))
}

/// Set the near clipping plane distance.
///
pub fn near(distance: Float) -> Attribute(msg) {
  attribute.attribute("data-near", float.to_string(distance))
}

/// Set the far clipping plane distance.
///
pub fn far(distance: Float) -> Attribute(msg) {
  attribute.attribute("data-far", float.to_string(distance))
}

/// Set the aspect ratio for a camera.
///
pub fn aspect(ratio: Float) -> Attribute(msg) {
  attribute.attribute("data-aspect", float.to_string(ratio))
}

/// Set a viewport for a camera (x, y, width, height in pixels).
///
pub fn viewport(x: Int, y: Int, width: Int, height: Int) -> Attribute(msg) {
  attribute.attribute(
    "data-viewport",
    int.to_string(x)
      <> ","
      <> int.to_string(y)
      <> ","
      <> int.to_string(width)
      <> ","
      <> int.to_string(height),
  )
}

/// Mark a camera as active (only active cameras render).
///
pub fn active(is_active: Bool) -> Attribute(msg) {
  attribute.attribute("data-active", case is_active {
    True -> "true"
    False -> "false"
  })
}

// LIGHT ATTRIBUTES ------------------------------------------------------------

/// Set the light type (ambient, directional, point, spot).
///
pub fn light_type(type_: String) -> Attribute(msg) {
  attribute.attribute("data-light-type", type_)
}

/// Set the intensity of a light.
///
pub fn intensity(value: Float) -> Attribute(msg) {
  attribute.attribute("data-intensity", float.to_string(value))
}

/// Set whether a light casts shadows.
///
pub fn cast_shadow(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("data-cast-shadow", case enabled {
    True -> "true"
    False -> "false"
  })
}

// PHYSICS ATTRIBUTES ----------------------------------------------------------

/// Set the physics body type (static, dynamic, kinematic).
///
pub fn physics_type(type_: String) -> Attribute(msg) {
  attribute.attribute("data-physics-type", type_)
}

/// Set the mass of a dynamic physics body.
///
pub fn mass(value: Float) -> Attribute(msg) {
  attribute.attribute("data-mass", float.to_string(value))
}

/// Set friction coefficient for physics.
///
pub fn friction(value: Float) -> Attribute(msg) {
  attribute.attribute("data-friction", float.to_string(value))
}

/// Set restitution (bounciness) for physics.
///
pub fn restitution(value: Float) -> Attribute(msg) {
  attribute.attribute("data-restitution", float.to_string(value))
}

// MODEL ATTRIBUTES ------------------------------------------------------------

/// Set the model file path (GLTF/GLB).
///
pub fn model_src(path: String) -> Attribute(msg) {
  attribute.attribute("data-model-src", path)
}

/// Set the animation name to play.
///
pub fn animation_name(name: String) -> Attribute(msg) {
  attribute.attribute("data-animation-name", name)
}

/// Set whether animation loops.
///
pub fn animation_loop(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("data-animation-loop", case enabled {
    True -> "true"
    False -> "false"
  })
}

/// Set animation playback speed.
///
pub fn animation_speed(speed: Float) -> Attribute(msg) {
  attribute.attribute("data-animation-speed", float.to_string(speed))
}

// AUDIO ATTRIBUTES ------------------------------------------------------------

/// Set the audio source file path.
///
pub fn audio_src(path: String) -> Attribute(msg) {
  attribute.attribute("data-audio-src", path)
}

/// Set audio volume (0.0 to 1.0).
///
pub fn volume(value: Float) -> Attribute(msg) {
  attribute.attribute("data-volume", float.to_string(value))
}

/// Set whether audio loops.
///
pub fn loop(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("data-loop", case enabled {
    True -> "true"
    False -> "false"
  })
}

/// Enable positional audio (3D spatial audio).
///
pub fn positional(enabled: Bool) -> Attribute(msg) {
  attribute.attribute("data-positional", case enabled {
    True -> "true"
    False -> "false"
  })
}

// DEBUG ATTRIBUTES ------------------------------------------------------------

/// Set the minimum corner of a debug box.
///
pub fn debug_min(min: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = min
  attribute.attribute(
    "data-debug-min",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the maximum corner of a debug box.
///
pub fn debug_max(max: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = max
  attribute.attribute(
    "data-debug-max",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the radius for a debug sphere.
///
pub fn debug_radius(radius: Float) -> Attribute(msg) {
  attribute.attribute("data-debug-radius", float.to_string(radius))
}

/// Set the center point for debug shapes.
///
pub fn debug_center(center: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = center
  attribute.attribute(
    "data-debug-center",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the start point for a debug line.
///
pub fn debug_from(from: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = from
  attribute.attribute(
    "data-debug-from",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the end point for a debug line.
///
pub fn debug_to(to: Vec3(Float)) -> Attribute(msg) {
  let vec3.Vec3(x, y, z) = to
  attribute.attribute(
    "data-debug-to",
    float.to_string(x)
      <> ","
      <> float.to_string(y)
      <> ","
      <> float.to_string(z),
  )
}

/// Set the size for debug helpers.
///
pub fn debug_size(size: Float) -> Attribute(msg) {
  attribute.attribute("data-debug-size", float.to_string(size))
}

/// Set grid divisions for debug grid.
///
pub fn debug_divisions(divisions: Int) -> Attribute(msg) {
  attribute.attribute("data-debug-divisions", int.to_string(divisions))
}
