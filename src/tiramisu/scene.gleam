import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import tiramisu/math/vec3

/// Opaque type for Three.js textures
pub type Texture

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
  TorusGeometry(
    radius: Float,
    tube: Float,
    radial_segments: Int,
    tubular_segments: Int,
  )
  TetrahedronGeometry(radius: Float, detail: Int)
  IcosahedronGeometry(radius: Float, detail: Int)
}

/// Material types
pub type MaterialType {
  BasicMaterial(
    color: Int,
    transparent: Bool,
    opacity: Float,
    map: Option(Texture),
  )
  StandardMaterial(
    color: Int,
    metalness: Float,
    roughness: Float,
    map: Option(Texture),
  )
  PhongMaterial(color: Int, shininess: Float, map: Option(Texture))
  LambertMaterial(color: Int, map: Option(Texture))
  ToonMaterial(color: Int, map: Option(Texture))
  LineMaterial(color: Int, linewidth: Float)
  SpriteMaterial(
    color: Int,
    transparent: Bool,
    opacity: Float,
    map: Option(Texture),
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
      let assert Ok(result) =
        vec3.angle(
          vec3.Vec3(0.0, 0.0, 1.0),
          vec3.Vec3(forward.x, 0.0, forward.z),
        )
      case forward.x <. 0.0 {
        True -> 0.0 -. result
        False -> result
      }
    }
  }

  let pitch = {
    let assert Ok(result) =
      vec3.angle(vec3.Vec3(forward.x, 0.0, forward.z), forward)
    case forward.y <. 0.0 {
      True -> 0.0 -. result
      False -> result
    }
  }

  // Roll is typically 0 for look-at transforms
  let roll = 0.0

  Transform(
    position: from,
    rotation: vec3.Vec3(pitch, yaw, roll),
    scale: vec3.one(),
  )
}

// --- Validated Geometry Constructors ---

/// Create a validated box geometry
pub fn box(
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
pub fn sphere(
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
pub fn cylinder(
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
pub fn torus(
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
pub fn tetrahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(detail < 0, Error(InvalidSegmentCount("detail", detail)))

  Ok(TetrahedronGeometry(radius, detail))
}

/// Create a validated icosahedron geometry
pub fn icosahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(detail < 0, Error(InvalidSegmentCount("detail", detail)))

  Ok(IcosahedronGeometry(radius, detail))
}

pub fn basic_material(
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

pub fn standard_material(
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

pub fn line_material(
  color color: Int,
  linewidth linewidth: Float,
) -> Result(MaterialType, ValidationError) {
  use <- bool.guard(linewidth <=. 0.0, Error(InvalidLinewidth(linewidth)))

  Ok(LineMaterial(color, linewidth))
}

pub fn sprite_material(
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

@internal
pub type Patch {
  AddNode(id: String, node: SceneNode, parent_id: option.Option(String))
  RemoveNode(id: String)
  UpdateTransform(id: String, transform: Transform)
  UpdateMaterial(id: String, material: MaterialType)
  UpdateGeometry(id: String, geometry: GeometryType)
  UpdateLight(id: String, light_type: LightType)
}

type NodeWithParent {
  NodeWithParent(node: SceneNode, parent_id: option.Option(String))
}

fn flatten_scene(nodes: List(SceneNode)) -> dict.Dict(String, NodeWithParent) {
  flatten_scene_helper(nodes, option.None, dict.new())
}

fn flatten_scene_helper(
  nodes: List(SceneNode),
  parent_id: option.Option(String),
  acc: dict.Dict(String, NodeWithParent),
) -> dict.Dict(String, NodeWithParent) {
  list.fold(nodes, acc, fn(acc, node) {
    let node_id = get_node_id(node)
    let acc = dict.insert(acc, node_id, NodeWithParent(node, parent_id))
    case node {
      Group(_, _, children) ->
        flatten_scene_helper(children, option.Some(node_id), acc)
      _ -> acc
    }
  })
}

/// Get the ID from a scene node
fn get_node_id(node: SceneNode) -> String {
  case node {
    Mesh(id, ..) -> id
    Group(id, ..) -> id
    Light(id, ..) -> id
  }
}

@internal
pub fn diff(previous: List(SceneNode), current: List(SceneNode)) -> List(Patch) {
  let prev_dict = flatten_scene(previous)
  let curr_dict = flatten_scene(current)

  let prev_ids = dict.keys(prev_dict)
  let curr_ids = dict.keys(curr_dict)

  // Find removals: IDs in previous but not in current
  let removals =
    list.filter(prev_ids, fn(id) { !list.contains(curr_ids, id) })
    |> list.map(fn(id) { RemoveNode(id) })

  // Find additions: IDs in current but not in previous
  let additions =
    list.filter(curr_ids, fn(id) { !list.contains(prev_ids, id) })
    |> list.filter_map(fn(id) {
      case dict.get(curr_dict, id) {
        Ok(NodeWithParent(node, parent_id)) -> Ok(AddNode(id, node, parent_id))
        Error(_) -> Error(Nil)
      }
    })

  // Find updates: IDs in both, compare node properties
  let updates =
    list.filter(curr_ids, fn(id) { list.contains(prev_ids, id) })
    |> list.flat_map(fn(id) {
      case dict.get(prev_dict, id), dict.get(curr_dict, id) {
        Ok(NodeWithParent(prev_node, _)), Ok(NodeWithParent(curr_node, _)) ->
          compare_nodes(id, prev_node, curr_node)
        _, _ -> []
      }
    })

  list.flatten([removals, additions, updates])
}

/// Compare two nodes and generate update patches
fn compare_nodes(id: String, prev: SceneNode, curr: SceneNode) -> List(Patch) {
  case prev, curr {
    Mesh(_, prev_geom, prev_mat, prev_trans),
      Mesh(_, curr_geom, curr_mat, curr_trans)
    -> {
      []
      |> list.append(case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      })
      |> list.append(case prev_mat != curr_mat {
        True -> [UpdateMaterial(id, curr_mat)]
        False -> []
      })
      |> list.append(case prev_geom != curr_geom {
        True -> [UpdateGeometry(id, curr_geom)]
        False -> []
      })
    }

    Light(_, prev_light, prev_trans), Light(_, curr_light, curr_trans) -> {
      []
      |> list.append(case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      })
      |> list.append(case prev_light != curr_light {
        True -> [UpdateLight(id, curr_light)]
        False -> []
      })
    }

    Group(_, prev_trans, _), Group(_, curr_trans, _) -> {
      case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      }
    }

    _, _ -> []
  }
}
