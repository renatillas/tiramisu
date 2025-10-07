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
////       light_type: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),
////       transform: transform.identity,
////     ),
////   ]
//// }
//// ```

import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set
import tiramisu/audio.{type AudioBuffer, type AudioConfig, type AudioType}
import tiramisu/camera
import tiramisu/object3d.{type AnimationPlayback, type Object3D}
import tiramisu/physics.{type RigidBody}
import tiramisu/transform
import vec/vec3.{type Vec3}

/// Opaque type for Three.js textures.
///
/// Created via `asset.load_texture()` and used in materials.
pub type Texture

/// Opaque type for Three.js BufferGeometry.
///
/// Created by loading 3D models with `asset.load_stl()` or `asset.load_model()`.
pub type BufferGeometry

/// Validation errors returned by geometry and material constructors.
///
/// These errors help catch invalid parameters at creation time instead of runtime.
pub type ValidationError {
  /// Dimension parameter (width, height, radius, etc.) is invalid (must be > 0)
  InvalidDimension(String, Float)
  /// Segment count parameter is invalid (typically must be >= 3)
  InvalidSegmentCount(String, Int)
  /// Opacity must be between 0.0 and 1.0
  InvalidOpacity(Float)
  /// Metalness must be between 0.0 and 1.0
  InvalidMetalness(Float)
  /// Roughness must be between 0.0 and 1.0
  InvalidRoughness(Float)
  /// Light intensity must be positive
  InvalidIntensity(Float)
  /// Line width must be positive
  InvalidLinewidth(Float)
}

/// 3D geometry types supported by the engine.
///
/// Each variant represents a different primitive shape or custom geometry.
/// Use the validated constructor functions like `box()`, `sphere()`, etc.
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

/// Material types for rendering objects.
///
/// Materials define how surfaces appear when rendered. Different materials
/// have different performance characteristics and visual properties.
///
/// ## Performance
///
/// - `BasicMaterial`: Fastest, no lighting calculations
/// - `LambertMaterial`, `ToonMaterial`: Fast, simple lighting
/// - `PhongMaterial`: Medium, specular highlights
/// - `StandardMaterial`: Physically-based, most realistic but slower
pub type MaterialType {
  /// Unlit material (no lighting calculations). Fast and useful for flat-shaded objects.
  BasicMaterial(
    color: Int,
    transparent: Bool,
    opacity: Float,
    map: Option(Texture),
  )
  /// Physically-based material with metalness/roughness workflow. Most realistic.
  StandardMaterial(
    color: Int,
    metalness: Float,
    roughness: Float,
    map: Option(Texture),
    normal_map: Option(Texture),
  )
  /// Shiny material with specular highlights (like plastic or ceramic).
  PhongMaterial(color: Int, shininess: Float, map: Option(Texture))
  /// Matte material (like cloth or wood). Non-shiny diffuse lighting.
  LambertMaterial(color: Int, map: Option(Texture))
  /// Cartoon-style material with banded shading.
  ToonMaterial(color: Int, map: Option(Texture))
  /// Material for rendering lines.
  LineMaterial(color: Int, linewidth: Float)
  /// Material for 2D sprites that always face the camera.
  SpriteMaterial(
    color: Int,
    transparent: Bool,
    opacity: Float,
    map: Option(Texture),
  )
}

/// Light types for illuminating the scene.
///
/// Different lights have different performance impacts and visual characteristics.
/// Most games use a combination of ambient + directional for outdoor scenes,
/// or ambient + point/spot for indoor scenes.
pub type LightType {
  /// Global ambient light (affects all objects equally, no direction).
  AmbientLight(color: Int, intensity: Float)
  /// Directional light like the sun (parallel rays, infinite distance).
  DirectionalLight(color: Int, intensity: Float)
  /// Point light that radiates in all directions (like a light bulb).
  PointLight(color: Int, intensity: Float, distance: Float)
  /// Cone-shaped spotlight (like a flashlight or stage light).
  SpotLight(
    color: Int,
    intensity: Float,
    distance: Float,
    angle: Float,
    penumbra: Float,
  )
  /// Hemisphere light with different colors for sky and ground (outdoor ambient).
  HemisphereLight(sky_color: Int, ground_color: Int, intensity: Float)
}

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
pub type LODLevel {
  LODLevel(distance: Float, node: SceneNode)
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
pub fn lod_level(distance distance: Float, node node: SceneNode) -> LODLevel {
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
    instances: List(transform.Transform),
  )
  Group(id: String, transform: transform.Transform, children: List(SceneNode))
  Light(id: String, light_type: LightType, transform: transform.Transform)
  /// Camera - defines a viewpoint in the scene
  /// Only one camera can be active at a time for rendering (when viewport is None)
  /// Use effect.set_active_camera(id) to switch between cameras
  /// Set viewport to render in a specific area (for picture-in-picture effects)
  Camera(
    id: String,
    camera: camera.Camera,
    transform: transform.Transform,
    active: Bool,
    /// Optional viewport: (x, y, width, height) in pixels
    /// If None, camera fills entire canvas (only when active=True)
    /// If Some, camera renders in specified rectangle (regardless of active state)
    viewport: option.Option(#(Int, Int, Int, Int)),
  )
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
  DebugBox(id: String, min: Vec3(Float), max: Vec3(Float), color: Int)
  DebugSphere(id: String, center: Vec3(Float), radius: Float, color: Int)
  DebugLine(id: String, from: Vec3(Float), to: Vec3(Float), color: Int)
  DebugAxes(id: String, origin: Vec3(Float), size: Float)
  DebugGrid(id: String, size: Float, divisions: Int, color: Int)
  DebugPoint(id: String, position: Vec3(Float), size: Float, color: Int)
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
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(width <=. 0.0, Error(InvalidDimension("width", width)))
  use <- bool.guard(height <=. 0.0, Error(InvalidDimension("height", height)))
  use <- bool.guard(depth <=. 0.0, Error(InvalidDimension("depth", depth)))

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
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(detail < 0, Error(InvalidSegmentCount("detail", detail)))

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
) -> Result(GeometryType, ValidationError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidDimension("radius", radius)))
  use <- bool.guard(detail < 0, Error(InvalidSegmentCount("detail", detail)))

  Ok(IcosahedronGeometry(radius, detail))
}

/// Create a validated basic (unlit) material.
///
/// Basic materials don't react to lights, making them very fast to render.
/// Opacity must be between 0.0 (fully transparent) and 1.0 (fully opaque).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(red) = scene.basic_material(color: 0xff0000, transparent: False, opacity: 1.0)
/// let assert Ok(glass) = scene.basic_material(color: 0x88ccff, transparent: True, opacity: 0.5)
/// ```
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

/// Create a validated physically-based (PBR) standard material.
///
/// Standard materials use metalness/roughness workflow for realistic rendering.
/// - Metalness: 0.0 = dielectric (plastic, wood), 1.0 = metal
/// - Roughness: 0.0 = mirror-smooth, 1.0 = completely rough
///
/// ## Example
///
/// ```gleam
/// let assert Ok(gold) = scene.standard_material(color: 0xffd700, metalness: 1.0, roughness: 0.3)
/// let assert Ok(plastic) = scene.standard_material(color: 0xff0000, metalness: 0.0, roughness: 0.5)
/// ```
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

/// Create a validated line material for rendering lines.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(line_mat) = scene.line_material(color: 0xff0000, linewidth: 2.0)
/// ```
pub fn line_material(
  color color: Int,
  linewidth linewidth: Float,
) -> Result(MaterialType, ValidationError) {
  use <- bool.guard(linewidth <=. 0.0, Error(InvalidLinewidth(linewidth)))

  Ok(LineMaterial(color, linewidth))
}

/// Create a validated sprite material for 2D billboards.
///
/// Sprites always face the camera and are useful for particles, UI elements, etc.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(sprite_mat) = scene.sprite_material(color: 0xffffff, transparent: True, opacity: 0.8)
/// ```
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
  UpdateInstances(id: String, instances: List(transform.Transform))
  UpdateLODLevels(id: String, levels: List(LODLevel))
  UpdateCamera(id: String, camera_type: camera.Camera)
  SetActiveCamera(id: String)
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

    Camera(_, prev_cam, prev_trans, prev_active, prev_viewport),
      Camera(_, curr_cam, curr_trans, curr_active, curr_viewport)
    ->
      compare_camera_fields(
        id,
        prev_cam,
        prev_trans,
        prev_active,
        prev_viewport,
        curr_cam,
        curr_trans,
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
  prev_instances: List(transform.Transform),
  curr_geom: GeometryType,
  curr_mat: MaterialType,
  curr_instances: List(transform.Transform),
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

/// Compare Camera fields using accumulator pattern
fn compare_camera_fields(
  id: String,
  prev_cam: camera.Camera,
  prev_trans: transform.Transform,
  prev_active: Bool,
  prev_viewport: option.Option(#(Int, Int, Int, Int)),
  curr_cam: camera.Camera,
  curr_trans: transform.Transform,
  curr_active: Bool,
  curr_viewport: option.Option(#(Int, Int, Int, Int)),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  // If camera config or viewport changed, emit UpdateCamera patch
  let patches = case prev_cam != curr_cam || prev_viewport != curr_viewport {
    True -> [UpdateCamera(id, curr_cam), ..patches]
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
