import gleam/bool
import gleam/option.{type Option}
import tiramisu/math/vec3
import tiramisu/three/texture

/// Validation errors for scene node creation
pub type ValidationError {
  InvalidDimension(String, Float)
  InvalidSegmentCount(String, Int)
  InvalidOpacity(Float)
  InvalidMetalness(Float)
  InvalidRoughness(Float)
  InvalidIntensity(Float)
  InvalidLinewidth(Float)
}

/// Transform represents position, rotation, and scale
pub type Transform {
  Transform(position: vec3.Vec3, rotation: vec3.Vec3, scale: vec3.Vec3)
}

/// Geometry types
pub type GeometryType {
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
  TorusGeometry(radius: Float, tube: Float, radial_segments: Int, tubular_segments: Int)
  TetrahedronGeometry(radius: Float, detail: Int)
  IcosahedronGeometry(radius: Float, detail: Int)
}

/// Material types
pub type MaterialType {
  BasicMaterial(
    color: Int,
    transparent: Bool,
    opacity: Float,
    map: Option(texture.Texture),
  )
  StandardMaterial(
    color: Int,
    metalness: Float,
    roughness: Float,
    map: Option(texture.Texture),
  )
  PhongMaterial(color: Int, shininess: Float, map: Option(texture.Texture))
  LambertMaterial(color: Int, map: Option(texture.Texture))
  ToonMaterial(color: Int, map: Option(texture.Texture))
  LineMaterial(color: Int, linewidth: Float)
  SpriteMaterial(
    color: Int,
    transparent: Bool,
    opacity: Float,
    map: Option(texture.Texture),
  )
}

/// Light types
pub type LightType {
  AmbientLight(color: Int, intensity: Float)
  DirectionalLight(color: Int, intensity: Float)
  PointLight(color: Int, intensity: Float, distance: Float)
  SpotLight(
    color: Int,
    intensity: Float,
    distance: Float,
    angle: Float,
    penumbra: Float,
  )
  HemisphereLight(sky_color: Int, ground_color: Int, intensity: Float)
}

/// Scene node - declarative description of 3D objects
pub type SceneNode {
  Mesh(
    id: String,
    geometry: GeometryType,
    material: MaterialType,
    transform: Transform,
  )
  Group(id: String, transform: Transform, children: List(SceneNode))
  Light(id: String, light_type: LightType, transform: Transform)
}

/// Helper to create a default transform
pub fn identity_transform() -> Transform {
  Transform(
    position: vec3.Vec3(0.0, 0.0, 0.0),
    rotation: vec3.Vec3(0.0, 0.0, 0.0),
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  )
}

/// Helper to create a transform with just position
pub fn transform_at(x: Float, y: Float, z: Float) -> Transform {
  Transform(
    position: vec3.Vec3(x, y, z),
    rotation: vec3.Vec3(0.0, 0.0, 0.0),
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  )
}

/// Helper to update position in a transform
pub fn set_position(transform: Transform, position: vec3.Vec3) -> Transform {
  Transform(..transform, position:)
}

/// Helper to update rotation in a transform
pub fn set_rotation(transform: Transform, rotation: vec3.Vec3) -> Transform {
  Transform(..transform, rotation:)
}

/// Helper to update scale in a transform
pub fn set_scale(transform: Transform, scale: vec3.Vec3) -> Transform {
  Transform(..transform, scale:)
}

/// Linearly interpolate between two transforms
pub fn lerp_transform(from: Transform, to: Transform, t: Float) -> Transform {
  Transform(
    position: vec3.lerp(from.position, to.position, t),
    rotation: vec3.lerp(from.rotation, to.rotation, t),
    scale: vec3.lerp(from.scale, to.scale, t),
  )
}

/// Compose two transforms (apply second transform after first)
pub fn compose_transform(first: Transform, second: Transform) -> Transform {
  Transform(
    position: vec3.add(first.position, second.position),
    rotation: vec3.add(first.rotation, second.rotation),
    scale: vec3.Vec3(
      first.scale.x *. second.scale.x,
      first.scale.y *. second.scale.y,
      first.scale.z *. second.scale.z,
    ),
  )
}

/// Create a transform that looks at a target position from a source position
pub fn look_at_transform(
  from: vec3.Vec3,
  to: vec3.Vec3,
  up: vec3.Vec3,
) -> Transform {
  // Calculate forward direction
  let forward = vec3.normalize(vec3.subtract(to, from))

  // Calculate right direction
  let right = vec3.normalize(vec3.cross(up, forward))

  // Calculate actual up direction
  let _actual_up = vec3.cross(forward, right)

  // Convert to Euler angles (simplified - works for most cases)
  // This is a simplified version; for production you'd want a full matrix-to-euler conversion
  let yaw = case forward.x == 0.0 && forward.z == 0.0 {
    True -> 0.0
    False -> {
      let assert Ok(result) = vec3.angle(vec3.Vec3(0.0, 0.0, 1.0), vec3.Vec3(forward.x, 0.0, forward.z))
      case forward.x <. 0.0 {
        True -> 0.0 -. result
        False -> result
      }
    }
  }

  let pitch = {
    let assert Ok(result) = vec3.angle(vec3.Vec3(forward.x, 0.0, forward.z), forward)
    case forward.y <. 0.0 {
      True -> 0.0 -. result
      False -> result
    }
  }

  // Roll is typically 0 for look-at transforms
  let roll = 0.0

  Transform(position: from, rotation: vec3.Vec3(pitch, yaw, roll), scale: vec3.one())
}

// --- Validated Geometry Constructors ---

/// Create a validated box geometry
pub fn validated_box(
  width width: Float,
  height height: Float,
  depth depth: Float,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(width <=. 0.0, Error(InvalidDimension("width", width)))
  use <- bool.guard(height <=. 0.0, Error(InvalidDimension("height", height)))
  use <- bool.guard(depth <=. 0.0, Error(InvalidDimension("depth", depth)))

  Ok(BoxGeometry(width, height, depth))
}

/// Create a validated sphere geometry
pub fn validated_sphere(
  radius radius: Float,
  width_segments width_segments: Int,
  height_segments height_segments: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(
    width_segments < 3,
    Error(InvalidSegmentCount("width_segments", width_segments)),
  )
  use <- bool.guard(
    height_segments < 2,
    Error(InvalidSegmentCount("height_segments", height_segments)),
  )

  Ok(SphereGeometry(radius, width_segments, height_segments))
}

/// Create a validated cylinder geometry
pub fn validated_cylinder(
  radius_top radius_top: Float,
  radius_bottom radius_bottom: Float,
  height height: Float,
  radial_segments radial_segments: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(
    radius_top <. 0.0,
    Error(InvalidDimension("radius_top", radius_top)),
  )
  use <- bool.guard(
    radius_bottom <. 0.0,
    Error(InvalidDimension("radius_bottom", radius_bottom)),
  )
  use <- bool.guard(height <=. 0.0, Error(InvalidDimension("height", height)))
  use <- bool.guard(
    radial_segments < 3,
    Error(InvalidSegmentCount("radial_segments", radial_segments)),
  )

  Ok(CylinderGeometry(radius_top, radius_bottom, height, radial_segments))
}

/// Create a validated torus geometry
pub fn validated_torus(
  radius radius: Float,
  tube tube: Float,
  radial_segments radial_segments: Int,
  tubular_segments tubular_segments: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(tube <=. 0.0, Error(InvalidDimension("tube", tube)))
  use <- bool.guard(
    radial_segments < 3,
    Error(InvalidSegmentCount("radial_segments", radial_segments)),
  )
  use <- bool.guard(
    tubular_segments < 3,
    Error(InvalidSegmentCount("tubular_segments", tubular_segments)),
  )

  Ok(TorusGeometry(radius, tube, radial_segments, tubular_segments))
}

/// Create a validated tetrahedron geometry
pub fn validated_tetrahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(detail < 0, Error(InvalidSegmentCount("detail", detail)))

  Ok(TetrahedronGeometry(radius, detail))
}

/// Create a validated icosahedron geometry
pub fn validated_icosahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(detail < 0, Error(InvalidSegmentCount("detail", detail)))

  Ok(IcosahedronGeometry(radius, detail))
}

// --- Validated Material Constructors ---

/// Create a validated basic material
pub fn validated_basic_material(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
) -> Result(MaterialType, ValidationError) {
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(InvalidOpacity(opacity)),
  )

  Ok(BasicMaterial(color, transparent, opacity, option.None))
}

/// Create a validated standard material
pub fn validated_standard_material(
  color color: Int,
  metalness metalness: Float,
  roughness roughness: Float,
) -> Result(MaterialType, ValidationError) {
  use <- bool.guard(
    metalness <. 0.0 || metalness >. 1.0,
    Error(InvalidMetalness(metalness)),
  )
  use <- bool.guard(
    roughness <. 0.0 || roughness >. 1.0,
    Error(InvalidRoughness(roughness)),
  )

  Ok(StandardMaterial(color, metalness, roughness, option.None))
}

/// Create a validated line material
pub fn validated_line_material(
  color color: Int,
  linewidth linewidth: Float,
) -> Result(MaterialType, ValidationError) {
  use <- bool.guard(linewidth <=. 0.0, Error(InvalidLinewidth(linewidth)))

  Ok(LineMaterial(color, linewidth))
}

/// Create a validated sprite material
pub fn validated_sprite_material(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
) -> Result(MaterialType, ValidationError) {
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(InvalidOpacity(opacity)),
  )

  Ok(SpriteMaterial(color, transparent, opacity, option.None))
}
