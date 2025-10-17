//// Scene graph module - declarative 3D scene construction.
////
//// This module provides types and functions for building 3D scenes declaratively.
//// Scenes are composed of `SceneNode` values that describe meshes, lights, cameras, and groups.
////
//// ## Core Concepts
////
//// - **Immutability**: Scene nodes are immutable values. Updates create new nodes.
//// - **Hierarchy**: Use `Group` nodes to create parent-child relationships.
//// - **Validation**: Geometry and material constructors return `Result` to catch invalid parameters.
//// - **Performance**: Use `InstancedMesh` for many identical objects (1 draw call instead of thousands).
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/scene
//// import tiramisu/transform
//// import gleam/option
//// import vec/vec3
////
//// pub fn view(model: Model) {
////   let assert Ok(geometry) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
////   let assert Ok(material) = scene.basic_material(color: 0xff0000, transparent: False, opacity: 1.0)
////
////   [
////     scene.Mesh(
////       id: "player",
////       geometry: geometry,
////       material: material,
////       transform: transform.at(vec3.Vec3(0.0, 1.0, 0.0)),
////       physics: option.None,
////     ),
////     scene.Light(
////       id: "sun",
////       light: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),
////       transform: transform.identity,
////     ),
////   ]
//// }
//// ```

import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set
import tiramisu/audio
import tiramisu/camera
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/object3d
import tiramisu/particle_emitter
import tiramisu/physics
import tiramisu/transform
import vec/vec3.{type Vec3}

/// Level of Detail (LOD) configuration.
///
/// Defines which mesh to display based on camera distance. Use with `LOD` scene node
/// for automatic detail switching to improve performance.
///
/// ## Example
///
/// ```gleam
/// scene.LOD(
///   id: "tree",
///   levels: [
///     scene.lod_level(distance: 0.0, node: high_detail_mesh),   // 0-50 units
///     scene.lod_level(distance: 50.0, node: medium_detail_mesh), // 50-100 units
///     scene.lod_level(distance: 100.0, node: low_detail_mesh),   // 100+ units
///   ],
///   transform: transform.identity,
/// )
/// ```
pub type LODLevel(id) {
  LODLevel(distance: Float, node: Node(id))
}

/// Create an LOD level with a distance threshold and scene node.
///
/// Levels should be ordered from closest (distance: 0.0) to farthest.
///
/// ## Example
///
/// ```gleam
/// let high_detail = scene.lod_level(distance: 0.0, node: detailed_mesh)
/// let low_detail = scene.lod_level(distance: 100.0, node: simple_mesh)
/// ```
pub fn lod_level(distance distance: Float, node node: Node(id)) -> LODLevel(id) {
  LODLevel(distance: distance, node: node)
}

/// Scene node - the core building block of your 3D scene.
///
/// Scene nodes are immutable, declarative descriptions of objects in your game.
/// Each frame, your `view()` function returns a list of scene nodes, and the engine
/// efficiently updates only what changed.
///
/// ## Node Types
///
/// - **Mesh**: Standard 3D object (1 draw call per mesh)
/// - **InstancedMesh**: Many identical objects (1 draw call total!)
/// - **Group**: Container for organizing child nodes in a hierarchy
/// - **Light**: Illuminates the scene
/// - **Camera**: Defines viewpoint (must have at least one with `active: True`)
/// - **LOD**: Switches detail levels based on distance
/// - **Model3D**: Loaded 3D model with animations
/// - **Audio**: Background or positional audio
/// - **Debug***: Visualization helpers for development
///
/// ## Example
///
/// ```gleam
/// pub fn view(model: Model) {
///   [
///     scene.Group(
///       id: "player",
///       transform: transform.at(model.position),
///       children: [
///         scene.Mesh(
///           id: "player-body",
///           geometry: scene.BoxGeometry(1.0, 2.0, 1.0),
///           material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
///           transform: transform.identity,
///           physics: option.Some(model.physics_body),
///         ),
///       ],
///     ),
///   ]
/// }
/// ```
pub type Node(id) {
  Mesh(
    id: id,
    geometry: geometry.Geometry,
    material: material.Material,
    transform: transform.Transform,
    physics: Option(physics.RigidBody),
  )
  /// Instanced mesh - renders many copies of the same geometry/material with 1 draw call
  /// Much more efficient than creating individual Mesh nodes for identical objects
  InstancedMesh(
    id: id,
    geometry: geometry.Geometry,
    material: material.Material,
    instances: List(transform.Transform),
  )
  Group(id: id, transform: transform.Transform, children: List(Node(id)))
  Light(id: id, light: light.Light, transform: transform.Transform)
  /// Camera - defines a viewpoint in the scene
  /// Only one camera can be active at a time for rendering (when viewport is None)
  /// Set viewport to render in a specific area (for picture-in-picture effects)
  Camera(
    id: id,
    camera: camera.Camera,
    transform: transform.Transform,
    /// Optional look-at target in world space. If None, camera uses transform rotation.
    /// If Some, camera will orient itself to look at the target point.
    look_at: Option(vec3.Vec3(Float)),
    active: Bool,
    /// Optional viewport: (x, y, width, height) in pixels
    /// If None, camera fills entire canvas (only when active=True)
    /// If Some, camera renders in specified rectangle (regardless of active state)
    viewport: Option(#(Int, Int, Int, Int)),
  )
  /// Level of Detail - automatically switches between different meshes based on camera distance
  /// Levels should be ordered from closest (distance: 0.0) to farthest
  LOD(id: id, levels: List(LODLevel(id)), transform: transform.Transform)
  Model3D(
    id: id,
    object: object3d.Object3D,
    transform: transform.Transform,
    animation: Option(object3d.AnimationPlayback),
    physics: Option(physics.RigidBody),
  )
  Audio(id: id, audio: audio.Audio)
  /// Particle system - spawn and animate many small particles for visual effects
  /// Particles are simulated in the FFI layer and rendered efficiently using Three.js Points
  Particles(
    id: id,
    emitter: particle_emitter.ParticleEmitter,
    transform: transform.Transform,
    active: Bool,
  )
  // Debug visualization nodes
  DebugBox(id: id, min: Vec3(Float), max: Vec3(Float), color: Int)
  DebugSphere(id: id, center: Vec3(Float), radius: Float, color: Int)
  DebugLine(id: id, from: Vec3(Float), to: Vec3(Float), color: Int)
  DebugAxes(id: id, origin: Vec3(Float), size: Float)
  DebugGrid(id: id, size: Float, divisions: Int, color: Int)
  DebugPoint(id: id, position: Vec3(Float), size: Float, color: Int)
}

@internal
pub type Patch(id) {
  AddNode(id: id, node: Node(id), parent_id: Option(id))
  RemoveNode(id: id)
  UpdateTransform(id: id, transform: transform.Transform)
  UpdateMaterial(id: id, material: material.Material)
  UpdateGeometry(id: id, geometry: geometry.Geometry)
  UpdateLight(id: id, light: light.Light)
  UpdateAnimation(id: id, animation: Option(object3d.AnimationPlayback))
  UpdatePhysics(id: id, physics: Option(physics.RigidBody))
  UpdateAudio(id: id, audio: audio.Audio)
  UpdateInstances(id: id, instances: List(transform.Transform))
  UpdateLODLevels(id: id, levels: List(LODLevel(id)))
  UpdateCamera(
    id: id,
    camera_type: camera.Camera,
    look_at: Option(vec3.Vec3(Float)),
  )
  SetActiveCamera(id: id)
  UpdateParticleEmitter(id: id, emitter: particle_emitter.ParticleEmitter)
  UpdateParticleActive(id: id, active: Bool)
}

type NodeWithParent(id) {
  NodeWithParent(node: Node(id), parent_id: Option(id))
}

fn flatten_scene(nodes: List(Node(id))) -> dict.Dict(id, NodeWithParent(id)) {
  flatten_scene_helper(nodes, option.None, dict.new())
}

fn flatten_scene_helper(
  nodes: List(Node(id)),
  parent_id: Option(id),
  acc: dict.Dict(id, NodeWithParent(id)),
) -> dict.Dict(id, NodeWithParent(id)) {
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
pub fn diff(
  previous: List(Node(id)),
  current: List(Node(id)),
) -> List(Patch(id)) {
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
  removals: List(Patch(id)),
  parent_change_removals: List(Patch(id)),
  updates: List(Patch(id)),
  additions: List(Patch(id)),
) -> List(Patch(id)) {
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
fn concat_patches(lists: List(List(Patch(id)))) -> List(Patch(id)) {
  list.fold(lists, [], fn(acc, patches) {
    list.fold(patches, acc, fn(acc2, patch) { [patch, ..acc2] })
  })
  |> list.reverse
}

/// Sort AddNode patches so that parents are added before their children
/// Optimized: pre-compute depths as tuples to avoid dict lookups in comparator
fn sort_patches_by_hierarchy(
  patches: List(Patch(id)),
  node_dict: dict.Dict(id, NodeWithParent(id)),
) -> List(Patch(id)) {
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
  parent_id: Option(id),
  node_dict: dict.Dict(id, NodeWithParent(id)),
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

fn compare_nodes(id: id, prev: Node(id), curr: Node(id)) -> List(Patch(id)) {
  case prev == curr {
    True -> []
    False -> compare_nodes_detailed(id, prev, curr)
  }
}

/// Detailed comparison of node properties (called only when nodes differ)
/// Uses accumulator pattern to avoid empty list allocations
fn compare_nodes_detailed(
  id: id,
  prev: Node(id),
  curr: Node(id),
) -> List(Patch(id)) {
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

    Camera(_, prev_cam, prev_trans, prev_look_at, prev_active, prev_viewport),
      Camera(_, curr_cam, curr_trans, curr_look_at, curr_active, curr_viewport)
    ->
      compare_camera_fields(
        id,
        prev_cam,
        prev_trans,
        prev_look_at,
        prev_active,
        prev_viewport,
        curr_cam,
        curr_trans,
        curr_look_at,
        curr_active,
        curr_viewport,
      )

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

    Audio(_, prev_audio), Audio(_, curr_audio) ->
      case prev_audio != curr_audio {
        True -> [UpdateAudio(id, curr_audio)]
        False -> []
      }

    Particles(_, prev_emitter, prev_trans, prev_active),
      Particles(_, curr_emitter, curr_trans, curr_active)
    ->
      compare_particle_fields(
        id,
        prev_emitter,
        prev_trans,
        prev_active,
        curr_emitter,
        curr_trans,
        curr_active,
      )

    _, _ -> []
  }
}

/// Compare Mesh fields using accumulator pattern (no empty list allocations)
fn compare_mesh_fields(
  id: id,
  prev_geom: geometry.Geometry,
  prev_mat: material.Material,
  prev_trans: transform.Transform,
  prev_phys: Option(physics.RigidBody),
  curr_geom: geometry.Geometry,
  curr_mat: material.Material,
  curr_trans: transform.Transform,
  curr_phys: Option(physics.RigidBody),
) -> List(Patch(id)) {
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
  id: id,
  prev_geom: geometry.Geometry,
  prev_mat: material.Material,
  prev_instances: List(transform.Transform),
  curr_geom: geometry.Geometry,
  curr_mat: material.Material,
  curr_instances: List(transform.Transform),
) -> List(Patch(id)) {
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
  id: id,
  prev_light: light.Light,
  prev_trans: transform.Transform,
  curr_light: light.Light,
  curr_trans: transform.Transform,
) -> List(Patch(id)) {
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
  id: id,
  prev_levels: List(LODLevel(id)),
  prev_trans: transform.Transform,
  curr_levels: List(LODLevel(id)),
  curr_trans: transform.Transform,
) -> List(Patch(id)) {
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

/// Compare Camera fields using accumulator pattern
fn compare_camera_fields(
  id: id,
  prev_cam: camera.Camera,
  prev_trans: transform.Transform,
  prev_look_at: Option(vec3.Vec3(Float)),
  prev_active: Bool,
  prev_viewport: Option(#(Int, Int, Int, Int)),
  curr_cam: camera.Camera,
  curr_trans: transform.Transform,
  curr_look_at: Option(vec3.Vec3(Float)),
  curr_active: Bool,
  curr_viewport: Option(#(Int, Int, Int, Int)),
) -> List(Patch(id)) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  // If camera config, look_at, or viewport changed, emit UpdateCamera patch
  let patches = case
    prev_cam != curr_cam
    || prev_look_at != curr_look_at
    || prev_viewport != curr_viewport
  {
    True -> [UpdateCamera(id, curr_cam, curr_look_at), ..patches]
    False -> patches
  }

  // If active state changed, emit SetActiveCamera patch
  let patches = case prev_active, curr_active {
    False, True -> [SetActiveCamera(id), ..patches]
    _, _ -> patches
  }

  patches
}

/// Compare Model3D fields using accumulator pattern
fn compare_model3d_fields(
  id: id,
  prev_trans: transform.Transform,
  prev_anim: Option(object3d.AnimationPlayback),
  prev_phys: Option(physics.RigidBody),
  curr_trans: transform.Transform,
  curr_anim: Option(object3d.AnimationPlayback),
  curr_phys: Option(physics.RigidBody),
) -> List(Patch(id)) {
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

/// Compare Particles fields using accumulator pattern
fn compare_particle_fields(
  id: id,
  prev_emitter: particle_emitter.ParticleEmitter,
  prev_trans: transform.Transform,
  prev_active: Bool,
  curr_emitter: particle_emitter.ParticleEmitter,
  curr_trans: transform.Transform,
  curr_active: Bool,
) -> List(Patch(id)) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_emitter != curr_emitter {
    True -> [UpdateParticleEmitter(id, curr_emitter), ..patches]
    False -> patches
  }

  let patches = case prev_active != curr_active {
    True -> [UpdateParticleActive(id, curr_active), ..patches]
    False -> patches
  }

  patches
}
