import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set
import tiramisu/audio.{type AudioBuffer, type AudioConfig, type AudioType}
import tiramisu/object3d.{type AnimationPlayback, type Object3D}
import tiramisu/physics.{type RigidBody}
import tiramisu/transform
import tiramisu/vec3.{type Vec3}

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
    normal_map: Option(Texture),
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

/// Transform data for a single instance in an InstancedMesh
pub type InstanceTransform {
  InstanceTransform(position: vec3.Vec3, rotation: vec3.Vec3, scale: vec3.Vec3)
}

/// Create an instance transform at a position with default rotation and scale
pub fn instance_at(position: vec3.Vec3) -> InstanceTransform {
  InstanceTransform(
    position: position,
    rotation: vec3.zero(),
    scale: vec3.one(),
  )
}

/// Create an instance transform with custom position, rotation, and scale
pub fn instance(
  position position: vec3.Vec3,
  rotation rotation: vec3.Vec3,
  scale scale: vec3.Vec3,
) -> InstanceTransform {
  InstanceTransform(position: position, rotation: rotation, scale: scale)
}

/// Level of Detail configuration - a mesh to show at a specific distance from camera
pub type LODLevel {
  LODLevel(distance: Float, node: SceneNode)
}

/// Create an LOD level with a distance threshold and node
pub fn lod_level(distance distance: Float, node node: SceneNode) -> LODLevel {
  LODLevel(distance: distance, node: node)
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
  /// Instanced mesh - renders many copies of the same geometry/material with 1 draw call
  /// Much more efficient than creating individual Mesh nodes for identical objects
  InstancedMesh(
    id: String,
    geometry: GeometryType,
    material: MaterialType,
    instances: List(InstanceTransform),
  )
  Group(id: String, transform: transform.Transform, children: List(SceneNode))
  Light(id: String, light_type: LightType, transform: transform.Transform)
  /// Level of Detail - automatically switches between different meshes based on camera distance
  /// Levels should be ordered from closest (distance: 0.0) to farthest
  LOD(id: String, levels: List(LODLevel), transform: transform.Transform)
  Model3D(
    id: String,
    object: Object3D,
    transform: transform.Transform,
    animation: option.Option(AnimationPlayback),
    physics: option.Option(RigidBody),
  )
  Audio(
    id: String,
    buffer: AudioBuffer,
    config: AudioConfig,
    audio_type: AudioType,
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

  Ok(StandardMaterial(color, metalness, roughness, option.None, option.None))
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
  UpdateAudio(id: String, config: AudioConfig)
  UpdateInstances(id: String, instances: List(InstanceTransform))
  UpdateLODLevels(id: String, levels: List(LODLevel))
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

  // Early exit: if both scenes are empty, no patches needed
  let prev_size = dict.size(prev_dict)
  let curr_size = dict.size(curr_dict)
  case prev_size == 0 && curr_size == 0 {
    True -> []
    False -> {
      // Convert to sets for O(log n) lookups instead of O(n) list.contains
      let prev_ids = dict.keys(prev_dict)
      let curr_ids = dict.keys(curr_dict)
      let prev_id_set = set.from_list(prev_ids)
      let curr_id_set = set.from_list(curr_ids)

      // Find removals: IDs in previous but not in current
      let removals =
        list.filter(prev_ids, fn(id) { !set.contains(curr_id_set, id) })
        |> list.map(fn(id) { RemoveNode(id) })

      // Find nodes that exist in both but have changed parents (need remove + add)
      let #(parent_changed_ids, same_parent_ids) =
        list.filter(curr_ids, fn(id) { set.contains(prev_id_set, id) })
        |> list.partition(fn(id) {
          case dict.get(prev_dict, id), dict.get(curr_dict, id) {
            Ok(NodeWithParent(_, prev_parent)),
              Ok(NodeWithParent(_, curr_parent))
            -> prev_parent != curr_parent
            _, _ -> False
          }
        })

      // For nodes with changed parents, treat as remove + add
      let parent_change_removals =
        list.map(parent_changed_ids, fn(id) { RemoveNode(id) })

      let parent_change_additions =
        list.filter_map(parent_changed_ids, fn(id) {
          case dict.get(curr_dict, id) {
            Ok(NodeWithParent(node, parent_id)) ->
              Ok(AddNode(id, node, parent_id))
            Error(_) -> Error(Nil)
          }
        })

      // Find additions: IDs in current but not in previous
      // Sort additions so parents are added before children
      let additions =
        list.filter(curr_ids, fn(id) { !set.contains(prev_id_set, id) })
        |> list.filter_map(fn(id) {
          case dict.get(curr_dict, id) {
            Ok(NodeWithParent(node, parent_id)) ->
              Ok(AddNode(id, node, parent_id))
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

      // Batch patches by type for optimal renderer processing:
      // 1. Removals first (free resources)
      // 2. Updates (modify existing)
      // 3. Additions last (create new, already sorted by hierarchy)
      batch_patches(removals, parent_change_removals, updates, additions)
    }
  }
}

/// Batch patches by type for optimal rendering order
/// Optimized: Single-pass partitioning + manual concatenation (no list.flatten)
fn batch_patches(
  removals: List(Patch),
  parent_change_removals: List(Patch),
  updates: List(Patch),
  additions: List(Patch),
) -> List(Patch) {
  // Single-pass partitioning with fold (O(n) instead of O(3n))
  let #(transform_updates, material_updates, geometry_updates, misc_updates) =
    list.fold(updates, #([], [], [], []), fn(acc, patch) {
      let #(transforms, materials, geometries, misc) = acc
      case patch {
        UpdateTransform(_, _) -> #(
          [patch, ..transforms],
          materials,
          geometries,
          misc,
        )
        UpdateMaterial(_, _) -> #(
          transforms,
          [patch, ..materials],
          geometries,
          misc,
        )
        UpdateGeometry(_, _) -> #(
          transforms,
          materials,
          [patch, ..geometries],
          misc,
        )
        _ -> #(transforms, materials, geometries, [patch, ..misc])
      }
    })

  // Efficient concatenation: prepend in reverse order, then single reverse
  // This avoids list.flatten's multiple traversals
  concat_patches([
    removals,
    parent_change_removals,
    list.reverse(transform_updates),
    list.reverse(material_updates),
    list.reverse(geometry_updates),
    list.reverse(misc_updates),
    additions,
  ])
}

/// Efficiently concatenate multiple lists using fold + prepend
/// O(n) total instead of list.flatten's O(n * m)
fn concat_patches(lists: List(List(Patch))) -> List(Patch) {
  list.fold(lists, [], fn(acc, patches) {
    list.fold(patches, acc, fn(acc2, patch) { [patch, ..acc2] })
  })
  |> list.reverse
}

/// Sort AddNode patches so that parents are added before their children
/// Optimized: pre-compute depths as tuples to avoid dict lookups in comparator
fn sort_patches_by_hierarchy(
  patches: List(Patch),
  node_dict: dict.Dict(String, NodeWithParent),
) -> List(Patch) {
  // Pre-compute (depth, patch) tuples for efficient sorting
  let patches_with_depth =
    list.map(patches, fn(patch) {
      case patch {
        AddNode(_, _, parent_id) -> {
          let depth = calculate_depth(parent_id, node_dict, 0)
          #(depth, patch)
        }
        _ -> #(0, patch)
      }
    })

  // Sort tuples by depth (O(n log n) without dict lookups)
  list.sort(patches_with_depth, fn(a, b) {
    let #(depth_a, _) = a
    let #(depth_b, _) = b
    case depth_a < depth_b {
      True -> order.Lt
      False ->
        case depth_a > depth_b {
          True -> order.Gt
          False -> order.Eq
        }
    }
  })
  // Extract patches from tuples
  |> list.map(fn(tuple) {
    let #(_, patch) = tuple
    patch
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
  // Fast path: if nodes are structurally equal, skip all field comparisons
  case prev == curr {
    True -> []
    False -> compare_nodes_detailed(id, prev, curr)
  }
}

/// Detailed comparison of node properties (called only when nodes differ)
/// Uses accumulator pattern to avoid empty list allocations
fn compare_nodes_detailed(
  id: String,
  prev: SceneNode,
  curr: SceneNode,
) -> List(Patch) {
  case prev, curr {
    Mesh(_, prev_geom, prev_mat, prev_trans, prev_phys),
      Mesh(_, curr_geom, curr_mat, curr_trans, curr_phys)
    ->
      compare_mesh_fields(
        id,
        prev_geom,
        prev_mat,
        prev_trans,
        prev_phys,
        curr_geom,
        curr_mat,
        curr_trans,
        curr_phys,
      )

    InstancedMesh(_, prev_geom, prev_mat, prev_instances),
      InstancedMesh(_, curr_geom, curr_mat, curr_instances)
    ->
      compare_instanced_mesh_fields(
        id,
        prev_geom,
        prev_mat,
        prev_instances,
        curr_geom,
        curr_mat,
        curr_instances,
      )

    Light(_, prev_light, prev_trans), Light(_, curr_light, curr_trans) ->
      compare_light_fields(id, prev_light, prev_trans, curr_light, curr_trans)

    Group(_, prev_trans, _), Group(_, curr_trans, _) ->
      case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      }

    LOD(_, prev_levels, prev_trans), LOD(_, curr_levels, curr_trans) ->
      compare_lod_fields(id, prev_levels, prev_trans, curr_levels, curr_trans)

    Model3D(_, _, prev_trans, prev_anim, prev_phys),
      Model3D(_, _, curr_trans, curr_anim, curr_phys)
    ->
      compare_model3d_fields(
        id,
        prev_trans,
        prev_anim,
        prev_phys,
        curr_trans,
        curr_anim,
        curr_phys,
      )

    Audio(_, _, prev_config, _), Audio(_, _, curr_config, _) ->
      case prev_config != curr_config {
        True -> [UpdateAudio(id, curr_config)]
        False -> []
      }

    _, _ -> []
  }
}

/// Compare Mesh fields using accumulator pattern (no empty list allocations)
fn compare_mesh_fields(
  id: String,
  prev_geom: GeometryType,
  prev_mat: MaterialType,
  prev_trans: transform.Transform,
  prev_phys: option.Option(RigidBody),
  curr_geom: GeometryType,
  curr_mat: MaterialType,
  curr_trans: transform.Transform,
  curr_phys: option.Option(RigidBody),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, curr_mat), ..patches]
    False -> patches
  }

  let patches = case prev_geom != curr_geom {
    True -> [UpdateGeometry(id, curr_geom), ..patches]
    False -> patches
  }

  let patches = case prev_phys != curr_phys {
    True -> [UpdatePhysics(id, curr_phys), ..patches]
    False -> patches
  }

  patches
}

/// Compare InstancedMesh fields using accumulator pattern
fn compare_instanced_mesh_fields(
  id: String,
  prev_geom: GeometryType,
  prev_mat: MaterialType,
  prev_instances: List(InstanceTransform),
  curr_geom: GeometryType,
  curr_mat: MaterialType,
  curr_instances: List(InstanceTransform),
) -> List(Patch) {
  let patches = []

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, curr_mat), ..patches]
    False -> patches
  }

  let patches = case prev_geom != curr_geom {
    True -> [UpdateGeometry(id, curr_geom), ..patches]
    False -> patches
  }

  let patches = case prev_instances != curr_instances {
    True -> [UpdateInstances(id, curr_instances), ..patches]
    False -> patches
  }

  patches
}

/// Compare Light fields using accumulator pattern
fn compare_light_fields(
  id: String,
  prev_light: LightType,
  prev_trans: transform.Transform,
  curr_light: LightType,
  curr_trans: transform.Transform,
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_light != curr_light {
    True -> [UpdateLight(id, curr_light), ..patches]
    False -> patches
  }

  patches
}

/// Compare LOD fields using accumulator pattern
fn compare_lod_fields(
  id: String,
  prev_levels: List(LODLevel),
  prev_trans: transform.Transform,
  curr_levels: List(LODLevel),
  curr_trans: transform.Transform,
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_levels != curr_levels {
    True -> [UpdateLODLevels(id, curr_levels), ..patches]
    False -> patches
  }

  patches
}

/// Compare Model3D fields using accumulator pattern
fn compare_model3d_fields(
  id: String,
  prev_trans: transform.Transform,
  prev_anim: option.Option(AnimationPlayback),
  prev_phys: option.Option(RigidBody),
  curr_trans: transform.Transform,
  curr_anim: option.Option(AnimationPlayback),
  curr_phys: option.Option(RigidBody),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_anim != curr_anim {
    True -> [UpdateAnimation(id, curr_anim), ..patches]
    False -> patches
  }

  let patches = case prev_phys != curr_phys {
    True -> [UpdatePhysics(id, curr_phys), ..patches]
    False -> patches
  }

  patches
}
