import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/result
import tiramisu/math/vec3.{type Vec3}
import tiramisu/object3d.{type AnimationPlayback, type Object3D}
import tiramisu/physics.{type RigidBody}
import tiramisu/transform

/// Opaque type for Three.js textures
pub type Texture

/// Opaque type for Three.js BufferGeometry (for loaded geometries)
pub type BufferGeometry

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
  CustomGeometry(BufferGeometry)
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
    transform: transform.Transform,
    physics: option.Option(RigidBody),
  )
  Group(id: String, transform: transform.Transform, children: List(SceneNode))
  Light(id: String, light_type: LightType, transform: transform.Transform)
  Model3D(
    id: String,
    object: Object3D,
    transform: transform.Transform,
    animation: option.Option(AnimationPlayback),
    physics: option.Option(RigidBody),
  )
  // Debug visualization nodes
  DebugBox(id: String, min: Vec3, max: Vec3, color: Int)
  DebugSphere(id: String, center: Vec3, radius: Float, color: Int)
  DebugLine(id: String, from: Vec3, to: Vec3, color: Int)
  DebugAxes(id: String, origin: Vec3, size: Float)
  DebugGrid(id: String, size: Float, divisions: Int, color: Int)
  DebugPoint(id: String, position: Vec3, size: Float, color: Int)
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
  UpdateTransform(id: String, transform: transform.Transform)
  UpdateMaterial(id: String, material: MaterialType)
  UpdateGeometry(id: String, geometry: GeometryType)
  UpdateLight(id: String, light_type: LightType)
  UpdateAnimation(id: String, animation: option.Option(AnimationPlayback))
  UpdatePhysics(id: String, physics: option.Option(RigidBody))
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
    let acc = dict.insert(acc, node.id, NodeWithParent(node, parent_id))
    case node {
      Group(_, _, children) ->
        flatten_scene_helper(children, option.Some(node.id), acc)
      _ -> acc
    }
  })
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

  // Find nodes that exist in both but have changed parents (need remove + add)
  let #(parent_changed_ids, same_parent_ids) =
    list.filter(curr_ids, fn(id) { list.contains(prev_ids, id) })
    |> list.partition(fn(id) {
      case dict.get(prev_dict, id), dict.get(curr_dict, id) {
        Ok(NodeWithParent(_, prev_parent)), Ok(NodeWithParent(_, curr_parent)) ->
          prev_parent != curr_parent
        _, _ -> False
      }
    })

  // For nodes with changed parents, treat as remove + add
  let parent_change_removals =
    list.map(parent_changed_ids, fn(id) { RemoveNode(id) })

  let parent_change_additions =
    list.filter_map(parent_changed_ids, fn(id) {
      case dict.get(curr_dict, id) {
        Ok(NodeWithParent(node, parent_id)) -> Ok(AddNode(id, node, parent_id))
        Error(_) -> Error(Nil)
      }
    })

  // Find additions: IDs in current but not in previous
  // Sort additions so parents are added before children
  let additions =
    list.filter(curr_ids, fn(id) { !list.contains(prev_ids, id) })
    |> list.filter_map(fn(id) {
      case dict.get(curr_dict, id) {
        Ok(NodeWithParent(node, parent_id)) -> Ok(AddNode(id, node, parent_id))
        Error(_) -> Error(Nil)
      }
    })
    |> list.append(parent_change_additions)
    |> sort_patches_by_hierarchy(curr_dict)

  // Find updates: IDs in both with same parent, compare node properties
  let updates =
    list.flat_map(same_parent_ids, fn(id) {
      case dict.get(prev_dict, id), dict.get(curr_dict, id) {
        Ok(NodeWithParent(prev_node, _)), Ok(NodeWithParent(curr_node, _)) ->
          compare_nodes(id, prev_node, curr_node)
        _, _ -> []
      }
    })

  list.flatten([removals, parent_change_removals, additions, updates])
}

/// Sort AddNode patches so that parents are added before their children
fn sort_patches_by_hierarchy(
  patches: List(Patch),
  node_dict: dict.Dict(String, NodeWithParent),
) -> List(Patch) {
  // Build a depth map for each node
  let depth_map =
    list.fold(patches, dict.new(), fn(acc, patch) {
      case patch {
        AddNode(id, _, parent_id) -> {
          let depth = calculate_depth(parent_id, node_dict, 0)
          dict.insert(acc, id, depth)
        }
        _ -> acc
      }
    })

  // Sort patches by depth (lower depth = closer to root = added first)
  list.sort(patches, fn(a, b) {
    case a, b {
      AddNode(id_a, _, _), AddNode(id_b, _, _) -> {
        let depth_a = dict.get(depth_map, id_a) |> result.unwrap(0)
        let depth_b = dict.get(depth_map, id_b) |> result.unwrap(0)
        case depth_a < depth_b {
          True -> order.Lt
          False ->
            case depth_a > depth_b {
              True -> order.Gt
              False -> order.Eq
            }
        }
      }
      _, _ -> order.Eq
    }
  })
}

/// Calculate the depth of a node in the hierarchy (0 = root)
fn calculate_depth(
  parent_id: option.Option(String),
  node_dict: dict.Dict(String, NodeWithParent),
  current_depth: Int,
) -> Int {
  case parent_id {
    option.None -> current_depth
    option.Some(id) ->
      case dict.get(node_dict, id) {
        Ok(NodeWithParent(_, parent_parent_id)) ->
          calculate_depth(parent_parent_id, node_dict, current_depth + 1)
        Error(_) -> current_depth + 1
      }
  }
}

/// Compare two nodes and generate update patches
fn compare_nodes(id: String, prev: SceneNode, curr: SceneNode) -> List(Patch) {
  case prev, curr {
    Mesh(_, prev_geom, prev_mat, prev_trans, prev_phys),
      Mesh(_, curr_geom, curr_mat, curr_trans, curr_phys)
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
      |> list.append(case prev_phys != curr_phys {
        True -> [UpdatePhysics(id, curr_phys)]
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

    Model3D(_, _, prev_trans, prev_anim, prev_phys),
      Model3D(_, _, curr_trans, curr_anim, curr_phys)
    -> {
      []
      |> list.append(case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      })
      |> list.append(case prev_anim != curr_anim {
        True -> [UpdateAnimation(id, curr_anim)]
        False -> []
      })
      |> list.append(case prev_phys != curr_phys {
        True -> [UpdatePhysics(id, curr_phys)]
        False -> []
      })
    }

    _, _ -> []
  }
}
