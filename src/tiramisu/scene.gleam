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
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set
import tiramisu/animation.{type AnimationPlayback}
import tiramisu/asset
import tiramisu/audio
import tiramisu/camera
import tiramisu/geometry
import tiramisu/internal/audio_manager
import tiramisu/internal/object_cache.{
  type AnimationAction, type AnimationActions, type AnimationMixer,
  type CacheState, type ThreeObject,
}
import tiramisu/internal/particle_manager
import tiramisu/light
import tiramisu/material
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

pub opaque type Node(id) {
  Mesh(
    id: id,
    geometry: geometry.Geometry,
    material: material.Material,
    transform: transform.Transform,
    physics: Option(physics.RigidBody),
  )
  InstancedMesh(
    id: id,
    geometry: geometry.Geometry,
    material: material.Material,
    instances: List(transform.Transform),
  )
  Group(id: id, transform: transform.Transform, children: List(Node(id)))
  Light(id: id, light: light.Light, transform: transform.Transform)
  Camera(
    id: id,
    camera: camera.Camera,
    transform: transform.Transform,
    look_at: Option(vec3.Vec3(Float)),
    active: Bool,
    viewport: Option(#(Int, Int, Int, Int)),
  )
  LOD(id: id, levels: List(LODLevel(id)), transform: transform.Transform)
  Model3D(
    id: id,
    object: asset.Object3D,
    transform: transform.Transform,
    animation: Option(AnimationPlayback),
    physics: Option(physics.RigidBody),
  )
  InstancedModel(
    id: id,
    object: asset.Object3D,
    instances: List(transform.Transform),
    physics: Option(physics.RigidBody),
  )
  Audio(id: id, audio: audio.Audio)
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

/// Create a mesh scene node.
///
/// Meshes are the basic building blocks for 3D objects. They combine geometry (shape),
/// material (appearance), and transform (position/rotation/scale).
///
/// **Physics**: Optional rigid body for physics simulation.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/geometry
/// import tiramisu/material
/// import tiramisu/transform
/// import gleam/option
/// import vec/vec3
///
/// // Create a red cube
/// let assert Ok(cube_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
/// let assert Ok(red_mat) = material.new()
///   |> material.with_color(0xff0000)
///   |> material.build()
///
/// scene.Mesh(
///   id: "player",
///   geometry: cube_geo,
///   material: red_mat,
///   transform: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
///   physics: option.None,
/// )
/// ```
pub fn mesh(
  id id: id,
  geometry geometry: geometry.Geometry,
  material material: material.Material,
  transform transform: transform.Transform,
  physics physics: Option(physics.RigidBody),
) -> Node(id) {
  Mesh(id:, geometry:, material:, transform:, physics:)
}

/// Create an instanced mesh for rendering many identical objects efficiently.
///
/// Instead of creating N separate meshes (N draw calls), instanced meshes render all
/// instances in a single draw call. Perfect for forests, crowds, particles, or any
/// scene with many repeated objects.
///
/// **Performance**: Use this when you have 10+ identical objects for significant speedup.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/geometry
/// import tiramisu/material
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/list
///
/// // Create 100 trees efficiently
/// let assert Ok(tree_geo) = geometry.cylinder(radius: 0.2, height: 3.0)
/// let assert Ok(tree_mat) = material.lambert(
///   color: 0x8b4513,
///   map: option.None,
///   normal_map: option.None,
///   ambient_oclusion_map: option.None,
/// )
///
/// let tree_positions = list.range(0, 99)
///   |> list.map(fn(i) {
///     let x = int.to_float(i % 10) *. 5.0
///     let z = int.to_float(i / 10) *. 5.0
///     transform.at(position: vec3.Vec3(x, 0.0, z))
///   })
///
/// scene.InstancedMesh(
///   id: "forest",
///   geometry: tree_geo,
///   material: tree_mat,
///   instances: tree_positions,  // All rendered in 1 draw call!
/// )
/// ```
pub fn instanced_mesh(
  id id: id,
  geometry geometry: geometry.Geometry,
  material material: material.Material,
  instances instances: List(transform.Transform),
) {
  InstancedMesh(id:, geometry:, material:, instances:)
}

/// Create a group node for scene hierarchy.
///
/// Groups allow you to organize nodes in a parent-child hierarchy. The group's transform
/// is applied to all children, making it easy to move/rotate/scale multiple objects together.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec3
///
/// // Solar system: sun with orbiting planets
/// scene.Group(
///   id: "solar-system",
///   transform: transform.identity,
///   children: [
///     scene.Mesh(...),  // Sun at center
///     scene.Group(
///       id: "earth-orbit",
///       transform: transform.at(position: vec3.Vec3(10.0, 0.0, 0.0))
///         |> transform.rotate_y(model.earth_angle),
///       children: [
///         scene.Mesh(...),  // Earth
///         scene.Group(
///           id: "moon-orbit",
///           transform: transform.at(position: vec3.Vec3(2.0, 0.0, 0.0))
///             |> transform.rotate_y(model.moon_angle),
///           children: [scene.Mesh(...)],  // Moon
///         ),
///       ],
///     ),
///   ],
/// )
/// ```
pub fn group(
  id id: id,
  transform transform: transform.Transform,
  children children: List(Node(id)),
) {
  Group(id:, transform:, children:)
}

/// Create a light scene node.
///
/// Lights illuminate the scene. See the `light` module for different light types
/// (ambient, directional, point, spot, hemisphere).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/light
/// import tiramisu/transform
/// import vec/vec3
///
/// // Directional sun light
/// let assert Ok(sun) = light.directional(intensity: 1.2, color: 0xffffff)
///   |> light.with_shadows(True)
///
/// scene.Light(
///   id: "sun",
///   light: sun,
///   transform: transform.identity
///     |> transform.with_euler_rotation(vec3.Vec3(-0.8, 0.3, 0.0)),
/// )
/// ```
pub fn light(
  id id: id,
  light light: light.Light,
  transform transform: transform.Transform,
) {
  Light(id: id, light:, transform:)
}

/// Viewport configuration for split-screen or picture-in-picture rendering.
///
/// Coordinates are in pixels from the top-left of the canvas.
pub type ViewPort {
  ViewPort(
    /// X position from left edge in pixels
    x: Int,
    /// Y position from top edge in pixels
    y: Int,
    /// Width in pixels
    width: Int,
    /// Height in pixels
    height: Int,
  )
}

/// Create a camera scene node.
///
/// Cameras define the viewpoint for rendering. At least one active camera is required.
/// Multiple cameras can be used for split-screen, minimaps, or picture-in-picture.
///
/// **Active**: Only active cameras render. Set to `True` for at least one camera.
/// **Look At**: Optional target point the camera faces (camera auto-rotates to face it).
/// **Viewport**: Optional screen region for this camera (for split-screen).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/camera
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/option
///
/// // Main perspective camera
/// let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
///
/// scene.Camera(
///   id: "main-camera",
///   camera: cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
///   look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),  // Look at origin
///   active: True,
///   viewport: option.None,  // Fullscreen
/// )
///
/// // Minimap camera (top-down view in corner)
/// let assert Ok(minimap_cam) = camera.orthographic(
///   left: -20.0, right: 20.0, top: 20.0, bottom: -20.0, near: 0.1, far: 100.0
/// )
///
/// scene.Camera(
///   id: "minimap",
///   camera: minimap_cam,
///   transform: transform.at(position: vec3.Vec3(0.0, 50.0, 0.0))
///     |> transform.with_euler_rotation(vec3.Vec3(-1.57, 0.0, 0.0)),
///   look_at: option.None,
///   active: True,
///   viewport: option.Some(scene.ViewPort(x: 10, y: 10, width: 200, height: 200)),
/// )
/// ```
pub fn camera(
  id id: id,
  camera camera: camera.Camera,
  transform transform: transform.Transform,
  look_at look_at: Option(vec3.Vec3(Float)),
  active active: Bool,
  viewport viewport: Option(ViewPort),
) {
  Camera(
    id: id,
    camera:,
    transform:,
    look_at:,
    active:,
    viewport: option.map(viewport, fn(viewport) {
      #(viewport.x, viewport.y, viewport.width, viewport.height)
    }),
  )
}

/// Create a Level of Detail (LOD) node.
///
/// LOD nodes automatically switch between different detail levels based on camera distance,
/// improving performance by showing simpler models when far away.
///
/// **Levels**: Ordered list from closest (distance: 0.0) to farthest. Use `lod_level()` to create.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/geometry
/// import tiramisu/material
/// import tiramisu/transform
/// import gleam/option
///
/// // High detail mesh (shown up close)
/// let assert Ok(high_geo) = geometry.sphere(radius: 1.0, width_segments: 32, height_segments: 32)
/// let assert Ok(mat) = material.new() |> material.with_color(0x00ff00) |> material.build()
/// let high_detail = scene.Mesh(
///   id: "tree-high",
///   geometry: high_geo,
///   material: mat,
///   transform: transform.identity,
///   physics: option.None,
/// )
///
/// // Low detail mesh (shown far away)
/// let assert Ok(low_geo) = geometry.sphere(radius: 1.0, width_segments: 8, height_segments: 8)
/// let low_detail = scene.Mesh(
///   id: "tree-low",
///   geometry: low_geo,
///   material: mat,
///   transform: transform.identity,
///   physics: option.None,
/// )
///
/// scene.LOD(
///   id: "optimized-tree",
///   levels: [
///     scene.lod_level(distance: 0.0, node: high_detail),   // 0-50 units away
///     scene.lod_level(distance: 50.0, node: low_detail),   // 50+ units away
///   ],
///   transform: transform.identity,
/// )
/// ```
pub fn lod(
  id id: id,
  levels levels: List(LODLevel(id)),
  transform transform: transform.Transform,
) {
  LOD(id:, levels:, transform:)
}

/// Create a 3D model node from a loaded asset (GLTF, FBX, OBJ).
///
/// Use this for models loaded via the `asset` module. Supports animations and physics.
///
/// **Animation**: Optional animation playback (single or blended). See `animation` module.
/// **Physics**: Optional rigid body for physics simulation.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/asset
/// import tiramisu/animation
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/option
/// import gleam/list
///
/// // Load model from cache
/// let assert Ok(gltf_data) = asset.get_model(cache, "character.glb")
///
/// // Find walk animation
/// let walk_clip = list.find(gltf_data.animations, fn(clip) {
///   animation.clip_name(clip) == "Walk"
/// })
///
/// let walk_anim = animation.new_animation(walk_clip)
///   |> animation.set_speed(1.2)
///   |> animation.set_loop(animation.LoopRepeat)
///
/// scene.Model3D(
///   id: "player",
///   object: gltf_data.scene,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
///   animation: option.Some(animation.SingleAnimation(walk_anim)),
///   physics: option.None,
/// )
/// ```
pub fn model_3d(
  id id: id,
  object object: asset.Object3D,
  transform transform: transform.Transform,
  animation animation: Option(AnimationPlayback),
  physics physics: Option(physics.RigidBody),
) {
  Model3D(id:, object:, transform:, animation:, physics:)
}

/// Create instanced 3D models for rendering many copies of a loaded asset.
///
/// Like `InstancedMesh`, but for loaded models (GLTF/FBX/OBJ). Renders all instances
/// in one draw call for maximum performance.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/asset
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/option
/// import gleam/list
///
/// // Load rock model
/// let assert Ok(rock_data) = asset.get_model(cache, "rock.glb")
///
/// // Place 50 rocks around the scene
/// let rock_positions = list.range(0, 49)
///   |> list.map(fn(i) {
///     let angle = int.to_float(i) *. 0.125
///     let radius = 20.0
///     let x = radius *. gleam_community.maths.cos(angle)
///     let z = radius *. gleam_community.maths.sin(angle)
///     transform.at(position: vec3.Vec3(x, 0.0, z))
///   })
///
/// scene.InstancedModel(
///   id: "rock-field",
///   object: rock_data.scene,
///   instances: rock_positions,
///   physics: option.None,
/// )
/// ```
pub fn instanced_model(
  id id: id,
  object object: asset.Object3D,
  instances instances: List(transform.Transform),
  physics physics: Option(physics.RigidBody),
) {
  InstancedModel(id:, object:, instances:, physics:)
}

/// Create an audio scene node.
///
/// Audio nodes play sounds in the scene. See the `audio` module for creating audio sources.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/audio
/// import gleam/option
///
/// // Background music
/// let background_music = audio.new_audio("background")
///   |> audio.with_source(audio.Stream("music/theme.mp3"))
///   |> audio.with_loop(True)
///   |> audio.with_volume(0.5)
///   |> audio.with_autoplay(True)
///
/// scene.Audio(id: "bgm", audio: background_music)
///
/// // Sound effect (from pre-loaded buffer)
/// let assert Ok(jump_buffer) = asset.get_audio(cache, "sounds/jump.mp3")
/// let jump_sound = audio.new_audio("jump")
///   |> audio.with_source(audio.Buffer(jump_buffer))
///   |> audio.with_volume(0.8)
///
/// scene.Audio(id: "jump-sfx", audio: jump_sound)
/// ```
pub fn audio(id id: id, audio audio: audio.Audio) {
  Audio(id:, audio:)
}

/// Create a particle emitter scene node.
///
/// Particle systems create effects like fire, smoke, sparks, or magic spells.
/// See the `particle_emitter` module for configuring emitters.
///
/// **Active**: Set to `True` to emit particles, `False` to pause emission.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/particle_emitter as particles
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/option
///
/// // Fire effect
/// let fire_emitter = particles.new()
///   |> particles.with_rate(50.0)  // 50 particles per second
///   |> particles.with_lifetime(1.0, 2.0)  // Live 1-2 seconds
///   |> particles.with_initial_velocity(vec3.Vec3(0.0, 3.0, 0.0))
///   |> particles.with_velocity_randomness(vec3.Vec3(0.5, 0.5, 0.5))
///   |> particles.with_size(0.5, 0.1)  // Start large, end small
///   |> particles.with_color(0xff4500, 0xff0000)  // Orange to red
///   |> particles.with_opacity(1.0, 0.0)  // Fade out
///
/// scene.Particles(
///   id: "campfire",
///   emitter: fire_emitter,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
///   active: True,
/// )
/// ```
pub fn particles(
  id id: id,
  emitter emitter: particle_emitter.ParticleEmitter,
  transform transform: transform.Transform,
  active active: Bool,
) {
  Particles(id:, emitter:, transform:, active:)
}

/// Create a debug wireframe box visualization.
///
/// Useful for visualizing collision bounds, trigger zones, or spatial regions.
///
/// **Min/Max**: Define the axis-aligned bounding box corners in world space.
/// **Color**: Hex color for the wireframe lines.
///
/// ## Example
///
/// ```gleam
/// // Visualize a collision box
/// scene.debug_box(
///   id: "trigger_zone",
///   min: vec3.Vec3(-5.0, 0.0, -5.0),
///   max: vec3.Vec3(5.0, 3.0, 5.0),
///   color: 0x00ff00,  // Green wireframe
/// )
/// ```
pub fn debug_box(
  id id: id,
  min min: Vec3(Float),
  max max: Vec3(Float),
  color color: Int,
) {
  DebugBox(id:, min:, max:, color:)
}

/// Create a debug wireframe sphere visualization.
///
/// Useful for visualizing sphere colliders, range indicators, or explosion radii.
///
/// **Center**: Center position in world space.
/// **Radius**: Sphere radius (should match your collider if visualizing physics).
/// **Color**: Hex color for the wireframe lines.
///
/// ## Example
///
/// ```gleam
/// // Visualize attack range
/// scene.debug_sphere(
///   id: "attack_range",
///   center: player_position,
///   radius: 5.0,  // 5 unit attack radius
///   color: 0xff0000,  // Red wireframe
/// )
/// ```
pub fn debug_sphere(
  id id: id,
  center center: Vec3(Float),
  radius radius: Float,
  color color: Int,
) {
  DebugSphere(id:, center:, radius:, color:)
}

/// Create a debug line segment visualization.
///
/// Useful for visualizing raycasts, trajectories, connections, or directions.
///
/// **From/To**: Start and end points in world space.
/// **Color**: Hex color for the line.
///
/// ## Example
///
/// ```gleam
/// // Visualize raycast from player to target
/// scene.debug_line(
///   id: "raycast",
///   from: player_position,
///   to: target_position,
///   color: 0xffff00,  // Yellow line
/// )
/// ```
pub fn debug_line(
  id id: id,
  from from: Vec3(Float),
  to to: Vec3(Float),
  color color: Int,
) {
  DebugLine(id:, from:, to:, color:)
}

/// Create a debug coordinate axes visualization.
///
/// Displays X (red), Y (green), and Z (blue) axes from the origin point.
/// Useful for visualizing object orientation, camera position, or world origin.
///
/// **Origin**: Center point in world space.
/// **Size**: Length of each axis line in units.
///
/// ## Example
///
/// ```gleam
/// // Show world origin
/// scene.debug_axes(
///   id: "world_axes",
///   origin: vec3.Vec3(0.0, 0.0, 0.0),
///   size: 5.0,  // 5 unit length axes
/// )
///
/// // Show object local axes
/// scene.debug_axes(
///   id: "player_axes",
///   origin: player_position,
///   size: 2.0,
/// )
/// ```
pub fn debug_axes(id id: id, origin origin: Vec3(Float), size size: Float) {
  DebugAxes(id:, origin:, size:)
}

/// Create a debug ground grid visualization.
///
/// Displays a grid on the XZ plane (horizontal ground plane) centered at origin.
/// Useful for spatial reference, scale indication, or level design.
///
/// **Size**: Total width/depth of the grid in units.
/// **Divisions**: Number of grid cells (higher = finer grid).
/// **Color**: Hex color for the grid lines.
///
/// ## Example
///
/// ```gleam
/// // Create a 20x20 unit grid with 10 divisions
/// scene.debug_grid(
///   id: "ground_grid",
///   size: 20.0,  // 20 units wide
///   divisions: 10,  // 10x10 cells (2 units per cell)
///   color: 0x444444,  // Dark gray
/// )
/// ```
pub fn debug_grid(
  id id: id,
  size size: Float,
  divisions divisions: Int,
  color color: Int,
) {
  DebugGrid(id:, size:, divisions:, color:)
}

/// Create a debug point visualization.
///
/// Displays a small sphere at the specified position.
/// Useful for marking locations, waypoints, spawn points, or intersection points.
///
/// **Position**: Point location in world space.
/// **Size**: Radius of the debug sphere in units (typically small like 0.1-0.5).
/// **Color**: Hex color for the sphere.
///
/// ## Example
///
/// ```gleam
/// // Mark spawn points
/// scene.debug_point(
///   id: "spawn_1",
///   position: vec3.Vec3(10.0, 0.0, 5.0),
///   size: 0.3,  // Small sphere
///   color: 0x00ff00,  // Green
/// )
///
/// // Mark raycast hit point
/// scene.debug_point(
///   id: "hit_point",
///   position: raycast_result.point,
///   size: 0.2,
///   color: 0xff0000,  // Red
/// )
/// ```
pub fn debug_point(
  id id: id,
  position position: Vec3(Float),
  size size: Float,
  color color: Int,
) {
  DebugPoint(id:, position:, size:, color:)
}

@internal
pub type Patch(id) {
  AddNode(id: id, node: Node(id), parent_id: Option(id))
  RemoveNode(id: id)
  UpdateTransform(id: id, transform: transform.Transform)
  UpdateMaterial(id: id, material: material.Material)
  UpdateGeometry(id: id, geometry: geometry.Geometry)
  UpdateLight(id: id, light: light.Light)
  UpdateAnimation(id: id, animation: Option(AnimationPlayback))
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

    InstancedModel(_, _, prev_instances, prev_phys),
      InstancedModel(_, _, curr_instances, curr_phys)
    ->
      compare_instanced_model_fields(
        id,
        prev_instances,
        prev_phys,
        curr_instances,
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
  prev_anim: Option(AnimationPlayback),
  prev_phys: Option(physics.RigidBody),
  curr_trans: transform.Transform,
  curr_anim: Option(AnimationPlayback),
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

/// Compare InstancedModel fields using accumulator pattern
fn compare_instanced_model_fields(
  id: id,
  prev_instances: List(transform.Transform),
  prev_phys: Option(physics.RigidBody),
  curr_instances: List(transform.Transform),
  curr_phys: Option(physics.RigidBody),
) -> List(Patch(id)) {
  let patches = []

  let patches = case prev_instances != curr_instances {
    True -> [UpdateInstances(id, curr_instances), ..patches]
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

@internal
pub type Scene

@internal
pub type ThreeCamera

@internal
pub type WebGLRenderer

@internal
pub type DomElement

@internal
pub type Dimensions {
  Dimensions(width: Float, height: Float)
}

@internal
pub type RendererOptions {
  RendererOptions(antialias: Bool, alpha: Bool, dimensions: Option(Dimensions))
}

@internal
pub type RendererState(id) {
  RendererState(
    /// Three.js renderer instance
    renderer: WebGLRenderer,
    /// Three.js scene instance
    scene: Scene,
    /// Object cache (Three.js objects, mixers, actions, etc.)
    cache: CacheState,
    /// Optional physics world
    physics_world: Option(physics.PhysicsWorld(id)),
    /// Audio manager state (Gleam-managed)
    audio_manager: audio_manager.AudioManagerState,
    /// Audio listener (singleton, attached to camera)
    audio_listener: asset.Object3D,
  )
}

@external(javascript, "../threejs.ffi.mjs", "identity")
fn to_dynamic(value: a) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "identity")
fn points_to_object3d(points: Dynamic) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createRenderer")
fn create_renderer_ffi(options: RendererOptions) -> WebGLRenderer

@external(javascript, "../threejs.ffi.mjs", "getRendererDomElement")
@internal
pub fn get_dom_element(renderer: WebGLRenderer) -> DomElement

@external(javascript, "../threejs.ffi.mjs", "setCanvas")
@internal
pub fn set_canvas(canvas: DomElement) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createScene")
fn create_scene_ffi() -> Scene

@external(javascript, "../threejs.ffi.mjs", "render")
@internal
pub fn render(renderer: WebGLRenderer, scene: Scene, camera: ThreeCamera) -> Nil

@external(javascript, "../threejs.ffi.mjs", "addToScene")
fn add_to_scene_ffi(scene: Scene, object: asset.Object3D) -> Nil

@external(javascript, "../threejs.ffi.mjs", "removeFromScene")
fn remove_from_scene_ffi(scene: Scene, object: asset.Object3D) -> Nil

@external(javascript, "../threejs.ffi.mjs", "addChild")
fn add_child_ffi(parent: asset.Object3D, child: asset.Object3D) -> Nil

@external(javascript, "../threejs.ffi.mjs", "applyTransform")
fn apply_transform_ffi(
  object: asset.Object3D,
  transform: transform.Transform,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "applyTransformWithQuaternion")
fn apply_transform_with_quaternion_ffi(
  object: asset.Object3D,
  position: Dynamic,
  quaternion: Dynamic,
  scale: Dynamic,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "updateMatrixWorld")
fn update_matrix_world_ffi(object: asset.Object3D, force: Bool) -> Nil

@external(javascript, "../threejs.ffi.mjs", "applyCameraLookAt")
fn apply_camera_look_at_ffi(camera: asset.Object3D, target: Vec3(Float)) -> Nil

@external(javascript, "../threejs.ffi.mjs", "applyCameraLookAtDynamic")
fn apply_camera_look_at_dynamic_ffi(
  camera: asset.Object3D,
  target: Dynamic,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createMesh")
fn create_mesh_ffi(
  geometry: geometry.ThreeGeometry,
  material: material.ThreeMaterial,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createInstancedMesh")
fn create_instanced_mesh_ffi(
  geometry: geometry.ThreeGeometry,
  material: material.ThreeMaterial,
  count: Int,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createGroup")
fn create_group_ffi() -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createLOD")
fn create_lod_ffi() -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "cloneObject")
fn clone_object_ffi(object: asset.Object3D) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "extractMeshMaterialPairs")
fn extract_mesh_material_pairs_ffi(object: asset.Object3D) -> MeshMaterialPairs

type MeshMaterialPairs

@external(javascript, "../tiramisu.ffi.mjs", "getPairsGeometries")
fn get_pairs_geometries_ffi(
  pairs: MeshMaterialPairs,
) -> List(geometry.ThreeGeometry)

@external(javascript, "../tiramisu.ffi.mjs", "getPairsMaterials")
fn get_pairs_materials_ffi(
  pairs: MeshMaterialPairs,
) -> List(material.ThreeMaterial)

@external(javascript, "../threejs.ffi.mjs", "setObjectGeometry")
fn set_object_geometry_ffi(
  object: asset.Object3D,
  geometry: geometry.ThreeGeometry,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setObjectMaterial")
fn set_object_material_ffi(
  object: asset.Object3D,
  material: material.ThreeMaterial,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getObjectGeometry")
fn get_object_geometry_ffi(object: asset.Object3D) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "getObjectMaterial")
fn get_object_material_ffi(object: asset.Object3D) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "setShadowProperties")
fn set_shadow_properties_ffi(
  object: asset.Object3D,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "isPerspectiveCamera")
fn is_perspective_camera_ffi(object: asset.Object3D) -> Bool

@external(javascript, "../threejs.ffi.mjs", "isOrthographicCamera")
fn is_orthographic_camera_ffi(object: asset.Object3D) -> Bool

@external(javascript, "../threejs.ffi.mjs", "updateCameraProjectionMatrix")
fn update_camera_projection_matrix_ffi(camera: asset.Object3D) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setCameraUserData")
fn set_camera_user_data_ffi(
  camera: asset.Object3D,
  key: String,
  value: Dynamic,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getCameraUserData")
fn get_camera_user_data_ffi(camera: asset.Object3D, key: String) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "hasCameraUserData")
fn has_camera_user_data_ffi(camera: asset.Object3D, key: String) -> Bool

@external(javascript, "../threejs.ffi.mjs", "deleteCameraUserData")
fn delete_camera_user_data_ffi(camera: asset.Object3D, key: String) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createPerspectiveCamera")
fn create_perspective_camera_ffi(
  fov: Float,
  aspect: Float,
  near: Float,
  far: Float,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "createOrthographicCamera")
fn create_orthographic_camera_ffi(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  near: Float,
  far: Float,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "setActiveCamera")
fn set_active_camera_ffi(camera: asset.Object3D) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getCanvasClientWidth")
fn get_canvas_client_width_ffi(canvas: DomElement) -> Float

@external(javascript, "../threejs.ffi.mjs", "getCanvasClientHeight")
fn get_canvas_client_height_ffi(canvas: DomElement) -> Float

@external(javascript, "../threejs.ffi.mjs", "getWindowWidth")
fn get_window_width_ffi() -> Float

@external(javascript, "../threejs.ffi.mjs", "getWindowHeight")
fn get_window_height_ffi() -> Float

@external(javascript, "../threejs.ffi.mjs", "createAnimationMixer")
fn create_animation_mixer_ffi(object: asset.Object3D) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "updateMixer")
fn update_animation_mixer_ffi(mixer: Dynamic, delta_time: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "isInstancedMesh")
fn is_instanced_mesh_ffi(object: asset.Object3D) -> Bool

@external(javascript, "../threejs.ffi.mjs", "updateInstancedMeshTransforms")
fn update_instanced_mesh_transforms_ffi(
  mesh: asset.Object3D,
  instances: List(transform.Transform),
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "updateGroupInstancedMeshes")
fn update_group_instanced_meshes_ffi(
  group: asset.Object3D,
  instances: List(transform.Transform),
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "isLOD")
fn is_lod_ffi(object: asset.Object3D) -> Bool

@external(javascript, "../threejs.ffi.mjs", "addLODLevel")
fn add_lod_level_ffi(
  lod: asset.Object3D,
  object: asset.Object3D,
  distance: Float,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "clearLODLevels")
fn clear_lod_levels_ffi(lod: asset.Object3D) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createAudioListener")
fn create_audio_listener_ffi() -> asset.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugBox")
fn create_debug_box_ffi(
  min: Vec3(Float),
  max: Vec3(Float),
  color: Int,
) -> asset.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugSphere")
fn create_debug_sphere_ffi(
  center: Vec3(Float),
  radius: Float,
  color: Int,
) -> asset.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugLine")
fn create_debug_line_ffi(
  from: Vec3(Float),
  to: Vec3(Float),
  color: Int,
) -> asset.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugAxes")
fn create_debug_axes_ffi(origin: Vec3(Float), size: Float) -> asset.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugGrid")
fn create_debug_grid_ffi(
  size: Float,
  divisions: Int,
  color: Int,
) -> asset.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugPoint")
fn create_debug_point_ffi(
  position: Vec3(Float),
  size: Float,
  color: Int,
) -> asset.Object3D

// ============================================================================
// FFI DECLARATIONS - RESOURCE DISPOSAL
// ============================================================================

@external(javascript, "../tiramisu.ffi.mjs", "disposeGeometry")
fn dispose_geometry_ffi(geometry: asset.Object3D) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "disposeMaterial")
fn dispose_material_ffi(material: asset.Object3D) -> Nil

@internal
pub fn create(options: RendererOptions) -> RendererState(id) {
  let renderer = create_renderer_ffi(options)
  let scene = create_scene_ffi()
  let canvas = get_dom_element(renderer)
  set_canvas(canvas)

  // Create audio listener (singleton)
  let audio_listener = create_audio_listener_ffi()

  RendererState(
    renderer: renderer,
    scene: scene,
    cache: object_cache.init(),
    physics_world: option.None,
    audio_manager: audio_manager.init(),
    audio_listener: audio_listener,
  )
}

@internal
pub fn get_renderer(state: RendererState(id)) -> WebGLRenderer {
  state.renderer
}

@internal
pub fn get_scene(state: RendererState(id)) -> Scene {
  state.scene
}

@internal
pub fn set_physics_world(
  state: RendererState(id),
  world: Option(physics.PhysicsWorld(id)),
) -> RendererState(id) {
  RendererState(..state, physics_world: world)
}

@internal
pub fn get_physics_world(
  state: RendererState(id),
) -> Option(physics.PhysicsWorld(id)) {
  state.physics_world
}

@internal
pub fn resume_audio_context(state: RendererState(id)) -> RendererState(id) {
  let new_audio_manager =
    audio_manager.resume_audio_context(state.audio_manager)
  RendererState(..state, audio_manager: new_audio_manager)
}

@internal
pub fn apply_patches(
  state: RendererState(id),
  patches: List(Patch(id)),
) -> RendererState(id) {
  list.fold(patches, state, fn(st, patch) { apply_patch(st, patch) })
}

@internal
pub fn apply_patch(
  state: RendererState(id),
  patch: Patch(id),
) -> RendererState(id) {
  case patch {
    AddNode(id: id, node: node, parent_id: parent_id) ->
      handle_add_node(state, id, node, parent_id)

    RemoveNode(id: id) -> handle_remove_node(state, id)

    UpdateTransform(id: id, transform: transform) ->
      handle_update_transform(state, id, transform)

    UpdateMaterial(id: id, material: material) ->
      handle_update_material(state, id, material)

    UpdateGeometry(id: id, geometry: geometry) ->
      handle_update_geometry(state, id, geometry)

    UpdateLight(id: id, light: light) -> handle_update_light(state, id, light)

    UpdateAnimation(id: id, animation: animation) ->
      handle_update_animation(state, id, animation)

    UpdatePhysics(id: id, physics: physics) ->
      handle_update_physics(state, id, physics)

    UpdateAudio(id: id, audio: audio) -> handle_update_audio(state, id, audio)

    UpdateInstances(id: id, instances: instances) ->
      handle_update_instances(state, id, instances)

    UpdateLODLevels(id: id, levels: levels) ->
      handle_update_lod_levels(state, id, levels)

    UpdateCamera(id: id, camera_type: camera_type, look_at: look_at) ->
      handle_update_camera(state, id, camera_type, look_at)

    SetActiveCamera(id: id) -> handle_set_active_camera(state, id)

    UpdateParticleEmitter(id: id, emitter: emitter) ->
      handle_update_particle_emitter(state, id, emitter)

    UpdateParticleActive(id: id, active: active) ->
      handle_update_particle_active(state, id, active)
  }
}

// ============================================================================
// PATCH HANDLERS - ADD NODE
// ============================================================================

fn handle_add_node(
  state: RendererState(id),
  id: id,
  node: Node(id),
  parent_id: Option(id),
) -> RendererState(id) {
  case node {
    Mesh(
      id: _,
      geometry: geometry,
      material: material,
      transform: transform,
      physics: physics,
    ) ->
      handle_add_mesh(
        state,
        id,
        geometry,
        material,
        transform,
        physics,
        parent_id,
      )

    InstancedMesh(
      id: _,
      geometry: geometry,
      material: material,
      instances: instances,
    ) ->
      handle_add_instanced_mesh(
        state,
        id,
        geometry,
        material,
        instances,
        parent_id,
      )

    Light(id: _, light: light, transform: transform) ->
      handle_add_light(state, id, light, transform, parent_id)

    Group(id: _, transform: transform, children: _) ->
      handle_add_group(state, id, transform, parent_id)

    LOD(id: _, transform: transform, levels: levels) ->
      handle_add_lod(state, id, transform, levels, parent_id)

    Model3D(
      id: _,
      object: object,
      transform: transform,
      animation: animation,
      physics: physics,
    ) ->
      handle_add_model3d(
        state,
        id,
        object,
        transform,
        animation,
        physics,
        parent_id,
      )

    InstancedModel(
      id: _,
      object: object,
      instances: instances,
      physics: physics,
    ) ->
      handle_add_instanced_model(
        state,
        id,
        object,
        instances,
        physics,
        parent_id,
      )

    Audio(id: _, audio: audio) -> handle_add_audio(state, id, audio, parent_id)

    Camera(
      id: _,
      camera: camera,
      transform: transform,
      look_at: look_at,
      active: active,
      viewport: viewport,
    ) ->
      handle_add_camera(
        state,
        id,
        camera,
        transform,
        look_at,
        active,
        viewport,
        parent_id,
      )

    DebugBox(id: _, min: min, max: max, color: color) ->
      handle_add_debug_box(state, id, min, max, color, parent_id)

    DebugSphere(id: _, center: center, radius: radius, color: color) ->
      handle_add_debug_sphere(state, id, center, radius, color, parent_id)

    DebugLine(id: _, from: from, to: to, color: color) ->
      handle_add_debug_line(state, id, from, to, color, parent_id)

    DebugAxes(id: _, origin: origin, size: size) ->
      handle_add_debug_axes(state, id, origin, size, parent_id)

    DebugGrid(id: _, size: size, divisions: divisions, color: color) ->
      handle_add_debug_grid(state, id, size, divisions, color, parent_id)

    DebugPoint(id: _, position: position, size: size, color: color) ->
      handle_add_debug_point(state, id, position, size, color, parent_id)

    Particles(id: _, emitter: emitter, transform: transform, active: active) ->
      handle_add_particles(state, id, emitter, transform, active, parent_id)
  }
}

// Helper: Add object to scene or parent
fn add_to_scene_or_parent(
  state: RendererState(id),
  object: ThreeObject,
  parent_id: Option(id),
) -> Nil {
  let obj_dynamic = object_cache.unwrap_object(object)

  case parent_id {
    option.Some(pid) -> {
      case object_cache.get_object(state.cache, pid) {
        option.Some(parent_obj) -> {
          let parent_dynamic = object_cache.unwrap_object(parent_obj)
          add_child_ffi(parent_dynamic, obj_dynamic)
        }
        option.None -> {
          // Parent not found, add to scene root
          add_to_scene_ffi(state.scene, obj_dynamic)
        }
      }
    }
    option.None -> add_to_scene_ffi(state.scene, obj_dynamic)
  }
}

fn handle_add_mesh(
  state: RendererState(id),
  id: id,
  geometry: geometry.Geometry,
  material: material.Material,
  transform: transform.Transform,
  physics: Option(physics.RigidBody),
  parent_id: Option(id),
) -> RendererState(id) {
  let geometry_three = geometry.create_geometry(geometry)
  let material_three = material.create_material(material)
  let mesh = create_mesh_ffi(geometry_three, material_three)

  apply_transform_ffi(mesh, transform)
  set_shadow_properties_ffi(mesh, True, True)

  let three_obj = object_cache.wrap_object(mesh)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)

  // Create physics body if specified - update physics world in state
  let new_state = RendererState(..state, cache: new_cache)
  case physics, new_state.physics_world {
    option.Some(physics_config), option.Some(world) -> {
      let new_world = physics.create_body(world, id, physics_config, transform)
      RendererState(..new_state, physics_world: option.Some(new_world))
    }
    _, _ -> new_state
  }
}

fn handle_add_instanced_mesh(
  state: RendererState(id),
  id: id,
  geometry: geometry.Geometry,
  material: material.Material,
  instances: List(transform.Transform),
  parent_id: Option(id),
) -> RendererState(id) {
  let geometry_three = geometry.create_geometry(geometry)
  let material_three = material.create_material(material)
  let count = list.length(instances)
  let mesh = create_instanced_mesh_ffi(geometry_three, material_three, count)

  update_instanced_mesh_transforms_ffi(mesh, instances)

  let three_obj = object_cache.wrap_object(mesh)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_instanced_model(
  state: RendererState(id),
  id: id,
  object: asset.Object3D,
  instances: List(transform.Transform),
  physics: Option(physics.RigidBody),
  parent_id: Option(id),
) -> RendererState(id) {
  // Extract all mesh/material pairs from the loaded model
  let pairs = extract_mesh_material_pairs_ffi(object)
  let geometries = get_pairs_geometries_ffi(pairs)
  let materials = get_pairs_materials_ffi(pairs)

  // Create a group to hold all the instanced meshes
  let group = create_group_ffi()

  // For each unique mesh/material combination, create an InstancedMesh
  let count = list.length(instances)

  // Zip geometries and materials together and iterate
  list.zip(geometries, materials)
  |> list.each(fn(pair) {
    let #(geometry, material) = pair
    let instanced_mesh = create_instanced_mesh_ffi(geometry, material, count)

    // Update all instance transforms
    update_instanced_mesh_transforms_ffi(instanced_mesh, instances)

    // Add the instanced mesh to the group
    add_child_ffi(group, instanced_mesh)
  })

  // Add the group to the scene or parent
  let three_obj = object_cache.wrap_object(group)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  let new_state = RendererState(..state, cache: new_cache)

  // Create physics bodies for each instance if specified
  case physics, new_state.physics_world {
    option.Some(physics_config), option.Some(world) -> {
      // Create one physics body per instance with unique ID
      let new_world =
        list.index_fold(
          instances,
          world,
          fn(world_acc, instance_transform, idx) {
            // Generate unique ID for this instance's physics body
            let instance_id = generate_instance_id(id, idx)
            physics.create_body(
              world_acc,
              instance_id,
              physics_config,
              instance_transform,
            )
          },
        )
      RendererState(..new_state, physics_world: option.Some(new_world))
    }
    _, _ -> new_state
  }
}

// Helper to generate unique IDs for instance physics bodies
@external(javascript, "../tiramisu.ffi.mjs", "generateInstanceId")
fn generate_instance_id(base_id: id, index: Int) -> id

fn handle_add_light(
  state: RendererState(id),
  id: id,
  light: light.Light,
  transform: transform.Transform,
  parent_id: Option(id),
) -> RendererState(id) {
  let light = light.create_light(light)
  apply_transform_ffi(light, transform)

  let three_obj = object_cache.wrap_object(light)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_group(
  state: RendererState(id),
  id: id,
  transform: transform.Transform,
  parent_id: Option(id),
) -> RendererState(id) {
  let group = create_group_ffi()
  apply_transform_ffi(group, transform)

  let three_obj = object_cache.wrap_object(group)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_lod(
  state: RendererState(id),
  id: id,
  transform: transform.Transform,
  levels: List(LODLevel(id)),
  parent_id: Option(id),
) -> RendererState(id) {
  let lod = create_lod_ffi()
  apply_transform_ffi(lod, transform)

  // Add LOD levels
  list.each(levels, fn(level) {
    let level_obj = create_lod_level_object(level.node)
    add_lod_level_ffi(lod, level_obj, level.distance)
  })

  let three_obj = object_cache.wrap_object(lod)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

// Helper: Create Three.js object for LOD level
fn create_lod_level_object(node: Node(id)) -> asset.Object3D {
  case node {
    Mesh(
      id: _,
      geometry: geometry,
      material: material,
      transform: transform,
      physics: _,
    ) -> {
      let geometry_three = geometry.create_geometry(geometry)
      let material_three = material.create_material(material)
      let mesh = create_mesh_ffi(geometry_three, material_three)
      apply_transform_ffi(mesh, transform)
      mesh
    }
    Group(id: _, transform: transform, children: _) -> {
      let group = create_group_ffi()
      apply_transform_ffi(group, transform)
      group
    }
    Model3D(
      id: _,
      object: object,
      transform: transform,
      animation: _,
      physics: _,
    ) -> {
      let cloned = clone_object_ffi(object)
      apply_transform_ffi(cloned, transform)
      cloned
    }
    _ -> {
      // Unsupported node type for LOD level, create empty group
      create_group_ffi()
    }
  }
}

fn handle_add_model3d(
  state: RendererState(id),
  id: id,
  object: asset.Object3D,
  transform: transform.Transform,
  animation: Option(AnimationPlayback),
  physics: Option(physics.RigidBody),
  parent_id: Option(id),
) -> RendererState(id) {
  apply_transform_ffi(object, transform)

  // Create animation mixer
  let mixer_dynamic = create_animation_mixer_ffi(object)
  let mixer = object_cache.wrap_mixer(mixer_dynamic)
  let cache_with_mixer = object_cache.add_mixer(state.cache, id, mixer)

  // Setup animation if provided
  let cache_with_animation = case animation {
    option.Some(anim_playback) ->
      setup_animation(cache_with_mixer, id, mixer, anim_playback)
    option.None -> cache_with_mixer
  }

  let three_obj = object_cache.wrap_object(object)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(cache_with_animation, id, three_obj)

  // Create physics body if specified - update physics world in state
  let new_state = RendererState(..state, cache: new_cache)
  case physics, new_state.physics_world {
    option.Some(physics_config), option.Some(world) -> {
      let new_world = physics.create_body(world, id, physics_config, transform)
      RendererState(..new_state, physics_world: option.Some(new_world))
    }
    _, _ -> new_state
  }
}

fn handle_add_audio(
  state: RendererState(id),
  id: id,
  audio: audio.Audio,
  parent_id: Option(id),
) -> RendererState(id) {
  // Extract buffer and config from Audio type
  let #(buffer, config) = case audio {
    audio.GlobalAudio(buffer: buffer, config: config) -> #(buffer, config)
    audio.PositionalAudio(
      buffer: buffer,
      config: config,
      ref_distance: _,
      rolloff_factor: _,
      max_distance: _,
    ) -> #(buffer, config)
  }

  // Create audio source using Gleam audio manager with state-managed listener
  let listener_dynamic = to_dynamic(state.audio_listener)
  let #(new_audio_manager, _source_data) =
    audio_manager.create_audio_source(
      state.audio_manager,
      id,
      buffer,
      config,
      audio,
      listener_dynamic,
    )

  // Create placeholder group to track in cache
  let placeholder = create_group_ffi()
  let three_obj = object_cache.wrap_object(placeholder)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache, audio_manager: new_audio_manager)
}

// Helper: Calculate aspect ratio for camera
// Uses viewport dimensions if specified, otherwise canvas or window dimensions
fn calculate_aspect_ratio(
  viewport: Option(#(Int, Int, Int, Int)),
  canvas: DomElement,
) -> Float {
  case viewport {
    option.Some(#(_x, _y, width, height)) -> {
      int.to_float(width) /. int.to_float(height)
    }
    option.None -> {
      // Try canvas first
      let canvas_width = get_canvas_client_width_ffi(canvas)
      let canvas_height = get_canvas_client_height_ffi(canvas)

      case canvas_width >. 0.0 && canvas_height >. 0.0 {
        True -> canvas_width /. canvas_height
        False -> {
          // Fallback to window dimensions
          let window_width = get_window_width_ffi()
          let window_height = get_window_height_ffi()
          window_width /. window_height
        }
      }
    }
  }
}

fn handle_add_camera(
  state: RendererState(id),
  id: id,
  camera_type: camera.Camera,
  transform: transform.Transform,
  look_at: Option(Vec3(Float)),
  active: Bool,
  viewport: Option(#(Int, Int, Int, Int)),
  parent_id: Option(id),
) -> RendererState(id) {
  let canvas = get_dom_element(state.renderer)

  // Calculate aspect ratio for perspective cameras
  let aspect = calculate_aspect_ratio(viewport, canvas)

  // Get projection and create camera based on type
  let projection = camera.get_projection(camera_type)
  let camera_obj = case projection {
    camera.Perspective(fov: fov, aspect: _, near: near, far: far) ->
      create_perspective_camera_ffi(fov, aspect, near, far)
    camera.Orthographic(
      left: left,
      right: right,
      top: top,
      bottom: bottom,
      near: near,
      far: far,
    ) -> create_orthographic_camera_ffi(left, right, top, bottom, near, far)
  }

  // Add audio listener to camera
  add_child_ffi(camera_obj, state.audio_listener)

  apply_transform_ffi(camera_obj, transform)

  // Store lookAt target if provided (will apply after adding to scene)
  case look_at {
    option.Some(target) -> {
      set_camera_user_data_ffi(camera_obj, "lookAtTarget", to_dynamic(target))
      set_camera_user_data_ffi(
        camera_obj,
        "needsLookAtUpdate",
        to_dynamic(True),
      )
    }
    option.None -> Nil
  }

  update_camera_projection_matrix_ffi(camera_obj)

  let three_obj = object_cache.wrap_object(camera_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  // Apply lookAt after adding to scene
  case look_at {
    option.Some(target) -> {
      apply_camera_look_at_ffi(camera_obj, target)
      delete_camera_user_data_ffi(camera_obj, "needsLookAtUpdate")
    }
    option.None -> Nil
  }

  // Store viewport if specified
  let cache_with_viewport = case viewport {
    option.Some(#(x, y, width, height)) -> {
      let vp =
        object_cache.Viewport(
          x: int.to_float(x),
          y: int.to_float(y),
          width: int.to_float(width),
          height: int.to_float(height),
        )
      object_cache.set_viewport(state.cache, id, vp)
    }
    option.None -> state.cache
  }

  // Set as active camera if specified
  case active {
    True -> set_active_camera_ffi(camera_obj)
    False -> Nil
  }

  let new_cache = object_cache.add_object(cache_with_viewport, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_box(
  state: RendererState(id),
  id: id,
  min: Vec3(Float),
  max: Vec3(Float),
  color: Int,
  parent_id: Option(id),
) -> RendererState(id) {
  let debug_obj = create_debug_box_ffi(min, max, color)
  let three_obj = object_cache.wrap_object(debug_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_sphere(
  state: RendererState(id),
  id: id,
  center: Vec3(Float),
  radius: Float,
  color: Int,
  parent_id: Option(id),
) -> RendererState(id) {
  let debug_obj = create_debug_sphere_ffi(center, radius, color)
  let three_obj = object_cache.wrap_object(debug_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_line(
  state: RendererState(id),
  id: id,
  from: Vec3(Float),
  to: Vec3(Float),
  color: Int,
  parent_id: Option(id),
) -> RendererState(id) {
  let debug_obj = create_debug_line_ffi(from, to, color)
  let three_obj = object_cache.wrap_object(debug_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_axes(
  state: RendererState(id),
  id: id,
  origin: Vec3(Float),
  size: Float,
  parent_id: Option(id),
) -> RendererState(id) {
  let debug_obj = create_debug_axes_ffi(origin, size)
  let three_obj = object_cache.wrap_object(debug_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_grid(
  state: RendererState(id),
  id: id,
  size: Float,
  divisions: Int,
  color: Int,
  parent_id: Option(id),
) -> RendererState(id) {
  let debug_obj = create_debug_grid_ffi(size, divisions, color)
  let three_obj = object_cache.wrap_object(debug_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_point(
  state: RendererState(id),
  id: id,
  position: Vec3(Float),
  size: Float,
  color: Int,
  parent_id: Option(id),
) -> RendererState(id) {
  let debug_obj = create_debug_point_ffi(position, size, color)
  let three_obj = object_cache.wrap_object(debug_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_add_particles(
  state: RendererState(id),
  id: id,
  emitter: particle_emitter.ParticleEmitter,
  transform: transform.Transform,
  active: Bool,
  parent_id: Option(id),
) -> RendererState(id) {
  // Import particle_manager at the top of the file
  // Create particle system state
  let particle_state =
    particle_manager.create_particle_system(emitter, transform)
    |> particle_manager.set_active(active)

  // Get the Three.js Points object from particle system
  let points_object = particle_manager.get_points_object(particle_state)
  let points_obj3d = points_to_object3d(points_object)

  // Add to scene or parent
  let three_obj = object_cache.wrap_object(points_obj3d)
  add_to_scene_or_parent(state, three_obj, parent_id)

  // Store both the Three.js object and the particle system state
  let new_cache =
    object_cache.add_object(state.cache, id, three_obj)
    |> object_cache.add_particle_system(
      id,
      particle_manager.wrap_as_cache_entry(particle_state),
    )

  RendererState(..state, cache: new_cache)
}

// ============================================================================
// PATCH HANDLERS - REMOVE NODE
// ============================================================================

fn handle_remove_node(state: RendererState(id), id: id) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)

      // Remove from scene
      remove_from_scene_ffi(state.scene, obj_dynamic)

      // Dispose geometry and material
      let geometry = get_object_geometry_ffi(obj_dynamic)
      let material = get_object_material_ffi(obj_dynamic)
      dispose_geometry_ffi(geometry)
      dispose_material_ffi(material)

      // Stop audio if it's an audio node (using Gleam audio manager)
      let new_audio_manager =
        audio_manager.unregister_audio_source(state.audio_manager, id)

      // Remove from all caches
      let new_cache = object_cache.remove_all(state.cache, id)

      // Remove physics body if it exists - update physics world in state
      let new_state =
        RendererState(
          ..state,
          cache: new_cache,
          audio_manager: new_audio_manager,
        )
      case new_state.physics_world {
        option.Some(world) -> {
          let new_world = physics.remove_body(world, id)
          RendererState(..new_state, physics_world: option.Some(new_world))
        }
        option.None -> new_state
      }
    }
    option.None -> state
  }
}

// ============================================================================
// PATCH HANDLERS - UPDATE OPERATIONS
// ============================================================================

fn handle_update_transform(
  state: RendererState(id),
  id: id,
  transform: transform.Transform,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let object = object_cache.unwrap_object(obj)
      apply_transform_ffi(object, transform)
      update_matrix_world_ffi(object, True)

      // If this is a camera with lookAt, reapply it after transform update
      let is_camera =
        is_perspective_camera_ffi(object) || is_orthographic_camera_ffi(object)

      case is_camera {
        True -> {
          case has_camera_user_data_ffi(object, "lookAtTarget") {
            True -> {
              let target = get_camera_user_data_ffi(object, "lookAtTarget")
              // Reapply lookAt with the stored target (it's a Dynamic Vec3)
              apply_camera_look_at_dynamic_ffi(object, target)
            }
            False -> Nil
          }
        }
        False -> Nil
      }

      // If this object has a physics body, update the physics body's transform too
      let new_state = case state.physics_world {
        option.Some(world) -> {
          let new_world = physics.update_body_transform(world, id, transform)
          RendererState(..state, physics_world: option.Some(new_world))
        }
        option.None -> state
      }

      new_state
    }
    option.None -> state
  }
}

fn handle_update_material(
  state: RendererState(id),
  id: id,
  material: material.Material,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      let old_material = get_object_material_ffi(obj_dynamic)
      dispose_material_ffi(old_material)

      let new_material = material.create_material(material)
      set_object_material_ffi(obj_dynamic, new_material)

      state
    }
    option.None -> state
  }
}

fn handle_update_geometry(
  state: RendererState(id),
  id: id,
  geometry: geometry.Geometry,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      let old_geometry = get_object_geometry_ffi(obj_dynamic)
      dispose_geometry_ffi(old_geometry)

      let new_geometry = geometry.create_geometry(geometry)
      set_object_geometry_ffi(obj_dynamic, new_geometry)

      state
    }
    option.None -> state
  }
}

fn handle_update_light(
  state: RendererState(id),
  id: id,
  light: light.Light,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(old_obj) -> {
      let old_object = object_cache.unwrap_object(old_obj)

      // Get old transform
      let position = get_object_position_ffi(old_object)
      let rotation = get_object_rotation_ffi(old_object)
      let scale = get_object_scale_ffi(old_object)

      // Create new light
      let new_light = light.create_light(light)

      // Copy transform
      set_object_position_ffi(new_light, position)
      set_object_rotation_ffi(new_light, rotation)
      set_object_scale_ffi(new_light, scale)

      // Replace in scene
      remove_from_scene_ffi(state.scene, old_object)
      add_to_scene_ffi(state.scene, new_light)

      // Update cache
      let new_obj = object_cache.wrap_object(new_light)
      let new_cache = object_cache.add_object(state.cache, id, new_obj)
      RendererState(..state, cache: new_cache)
    }
    option.None -> state
  }
}

// FFI declarations for get/set position/rotation/scale
@external(javascript, "../threejs.ffi.mjs", "getObjectPosition")
fn get_object_position_ffi(object: asset.Object3D) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "getObjectRotation")
fn get_object_rotation_ffi(object: asset.Object3D) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "getObjectScale")
fn get_object_scale_ffi(object: asset.Object3D) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "setObjectPosition")
fn set_object_position_ffi(object: asset.Object3D, position: Dynamic) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setObjectRotation")
fn set_object_rotation_ffi(object: asset.Object3D, rotation: Dynamic) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setObjectScale")
fn set_object_scale_ffi(object: asset.Object3D, scale: Dynamic) -> Nil

fn handle_update_animation(
  state: RendererState(id),
  id: id,
  animation: Option(AnimationPlayback),
) -> RendererState(id) {
  case object_cache.get_mixer(state.cache, id) {
    option.Some(mixer) -> {
      case animation {
        option.Some(anim_playback) -> {
          let new_cache = setup_animation(state.cache, id, mixer, anim_playback)
          RendererState(..state, cache: new_cache)
        }
        option.None -> {
          // Stop all animations
          case object_cache.get_actions(state.cache, id) {
            option.Some(actions) -> {
              stop_actions(actions)
              let new_cache = object_cache.remove_actions(state.cache, id)
              RendererState(..state, cache: new_cache)
            }
            option.None -> state
          }
        }
      }
    }
    option.None -> state
  }
}

fn handle_update_physics(
  state: RendererState(id),
  id: id,
  physics: Option(physics.RigidBody),
) -> RendererState(id) {
  case state.physics_world {
    option.Some(world) -> {
      // First, remove existing physics body
      let world_after_remove = physics.remove_body(world, id)

      // Then, create new physics body if provided
      let new_world = case physics {
        option.Some(physics_config) -> {
          // Get current transform from Three.js object
          case object_cache.get_object(state.cache, id) {
            option.Some(obj) -> {
              let obj_dynamic = object_cache.unwrap_object(obj)
              // Get transform from object
              let position = get_object_position_ffi(obj_dynamic)
              let rotation = get_object_rotation_ffi(obj_dynamic)
              let scale = get_object_scale_ffi(obj_dynamic)

              // Convert to Transform
              let object_transform =
                dynamic_to_transform(position, rotation, scale)

              physics.create_body(
                world_after_remove,
                id,
                physics_config,
                object_transform,
              )
            }
            option.None -> world_after_remove
          }
        }
        option.None -> world_after_remove
      }

      RendererState(..state, physics_world: option.Some(new_world))
    }
    option.None -> state
  }
}

// Helper to convert Dynamic position/rotation/scale to Transform
fn dynamic_to_transform(
  position: Dynamic,
  rotation: Dynamic,
  scale: Dynamic,
) -> transform.Transform {
  // For now, use identity transform
  // TODO: Properly convert Dynamic values to Transform
  let _ = position
  let _ = rotation
  let _ = scale
  transform.identity
}

fn handle_update_audio(
  state: RendererState(id),
  id: id,
  audio: audio.Audio,
) -> RendererState(id) {
  // Extract buffer and config from Audio type
  let #(buffer, config) = case audio {
    audio.GlobalAudio(buffer: buffer, config: config) -> #(buffer, config)
    audio.PositionalAudio(
      buffer: buffer,
      config: config,
      ref_distance: _,
      rolloff_factor: _,
      max_distance: _,
    ) -> #(buffer, config)
  }

  // Update audio config using Gleam audio manager
  let new_audio_manager =
    audio_manager.update_audio_config(state.audio_manager, id, buffer, config)

  RendererState(..state, audio_manager: new_audio_manager)
}

fn handle_update_instances(
  state: RendererState(id),
  id: id,
  instances: List(transform.Transform),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      case is_instanced_mesh_ffi(obj_dynamic) {
        True -> {
          // Single InstancedMesh node
          update_instanced_mesh_transforms_ffi(obj_dynamic, instances)
          state
        }
        False -> {
          // Could be a Group containing InstancedMeshes (InstancedModel)
          // This will recursively update all InstancedMesh children
          update_group_instanced_meshes_ffi(obj_dynamic, instances)
          state
        }
      }
    }
    option.None -> state
  }
}

fn handle_update_lod_levels(
  state: RendererState(id),
  id: id,
  levels: List(LODLevel(id)),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      case is_lod_ffi(obj_dynamic) {
        True -> {
          // Clear existing levels
          clear_lod_levels_ffi(obj_dynamic)

          // Add new levels
          list.each(levels, fn(level) {
            let level_obj = create_lod_level_object(level.node)
            add_lod_level_ffi(obj_dynamic, level_obj, level.distance)
          })

          state
        }
        False -> state
      }
    }
    option.None -> state
  }
}

fn handle_update_camera(
  state: RendererState(id),
  id: id,
  _camera_type: camera.Camera,
  look_at: Option(Vec3(Float)),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      let is_camera =
        is_perspective_camera_ffi(obj_dynamic)
        || is_orthographic_camera_ffi(obj_dynamic)

      case is_camera {
        True -> {
          // Apply lookAt if provided and update stored target
          case look_at {
            option.Some(target) -> {
              apply_camera_look_at_ffi(obj_dynamic, target)
              // Update the stored lookAtTarget so it's used in future transform updates
              set_camera_user_data_ffi(
                obj_dynamic,
                "lookAtTarget",
                to_dynamic(target),
              )
            }
            option.None -> {
              // If None, remove the stored lookAtTarget so the camera can rotate freely
              delete_camera_user_data_ffi(obj_dynamic, "lookAtTarget")
            }
          }

          // Update camera projection parameters
          // TODO: Extract projection parameters from camera_type and update
          update_camera_projection_matrix_ffi(obj_dynamic)

          state
        }
        False -> state
      }
    }
    option.None -> state
  }
}

fn handle_set_active_camera(
  state: RendererState(id),
  id: id,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      set_active_camera_ffi(obj_dynamic)
      state
    }
    option.None -> state
  }
}

fn handle_update_particle_emitter(
  state: RendererState(id),
  id: id,
  emitter: particle_emitter.ParticleEmitter,
) -> RendererState(id) {
  case object_cache.get_particle_system(state.cache, id) {
    option.Some(cached_system) -> {
      // Unwrap, update emitter, and wrap back
      let particle_state =
        particle_manager.unwrap_from_cache_entry(cached_system)
      let updated_state =
        particle_manager.update_emitter(particle_state, emitter)
      let wrapped_system = particle_manager.wrap_as_cache_entry(updated_state)

      let new_cache =
        object_cache.add_particle_system(state.cache, id, wrapped_system)
      RendererState(..state, cache: new_cache)
    }
    option.None -> state
  }
}

fn handle_update_particle_active(
  state: RendererState(id),
  id: id,
  active: Bool,
) -> RendererState(id) {
  case object_cache.get_particle_system(state.cache, id) {
    option.Some(cached_system) -> {
      // Unwrap, update active state, and wrap back
      let particle_state =
        particle_manager.unwrap_from_cache_entry(cached_system)
      let updated_state = particle_manager.set_active(particle_state, active)
      let wrapped_system = particle_manager.wrap_as_cache_entry(updated_state)

      let new_cache =
        object_cache.add_particle_system(state.cache, id, wrapped_system)
      RendererState(..state, cache: new_cache)
    }
    option.None -> state
  }
}

fn setup_animation(
  cache: CacheState,
  id: id,
  mixer: AnimationMixer,
  playback: AnimationPlayback,
) -> CacheState {
  // Stop existing animations
  case object_cache.get_actions(cache, id) {
    option.Some(actions) -> stop_actions(actions)
    option.None -> Nil
  }

  case playback {
    animation.SingleAnimation(anim) -> {
      let action = create_animation_action(mixer, anim)
      let actions = object_cache.SingleAction(action)
      object_cache.set_actions(cache, id, actions)
    }

    animation.BlendedAnimations(
      from: from_anim,
      to: to_anim,
      blend_factor: blend_factor,
    ) -> {
      let from_action = create_animation_action(mixer, from_anim)
      let to_action = create_animation_action(mixer, to_anim)

      // Adjust weights based on blend factor
      adjust_action_weight(
        from_action,
        { 1.0 -. blend_factor } *. from_anim.weight,
      )
      adjust_action_weight(to_action, blend_factor *. to_anim.weight)

      let actions =
        object_cache.BlendedActions(from: from_action, to: to_action)
      object_cache.set_actions(cache, id, actions)
    }
  }
}

fn create_animation_action(
  mixer: AnimationMixer,
  animation: animation.Animation,
) -> AnimationAction {
  let mixer_dynamic = object_cache.unwrap_mixer(mixer)
  let clip_dynamic = to_dynamic(animation.clip)
  let action_dynamic = create_animation_action_ffi(mixer_dynamic, clip_dynamic)

  // Configure action
  let loop_mode = case animation.loop {
    animation.LoopRepeat -> loop_repeat()
    animation.LoopOnce -> loop_once()
  }
  set_animation_loop_ffi(action_dynamic, loop_mode)
  set_animation_time_scale_ffi(action_dynamic, animation.speed)
  set_animation_weight_ffi(action_dynamic, animation.weight)
  play_animation_action_ffi(action_dynamic)

  object_cache.wrap_action(action_dynamic)
}

fn stop_actions(actions: AnimationActions) -> Nil {
  case actions {
    object_cache.SingleAction(action) -> {
      let action_dynamic = object_cache.unwrap_action(action)
      stop_animation_action_ffi(action_dynamic)
    }
    object_cache.BlendedActions(from: from_action, to: to_action) -> {
      let from_dynamic = object_cache.unwrap_action(from_action)
      let to_dynamic = object_cache.unwrap_action(to_action)
      stop_animation_action_ffi(from_dynamic)
      stop_animation_action_ffi(to_dynamic)
    }
  }
}

fn adjust_action_weight(action: AnimationAction, weight: Float) -> Nil {
  let action_dynamic = object_cache.unwrap_action(action)
  set_animation_weight_ffi(action_dynamic, weight)
}

// Animation FFI declarations
@external(javascript, "../threejs.ffi.mjs", "clipAction")
fn create_animation_action_ffi(mixer: Dynamic, clip: Dynamic) -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "setActionLoop")
fn set_animation_loop_ffi(action: Dynamic, loop_mode: Dynamic) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setActionTimeScale")
fn set_animation_time_scale_ffi(action: Dynamic, time_scale: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setActionWeight")
fn set_animation_weight_ffi(action: Dynamic, weight: Float) -> Nil

@external(javascript, "../threejs.ffi.mjs", "playAction")
fn play_animation_action_ffi(action: Dynamic) -> Nil

@external(javascript, "../threejs.ffi.mjs", "stopAction")
fn stop_animation_action_ffi(action: Dynamic) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getLoopRepeat")
fn loop_repeat() -> Dynamic

@external(javascript, "../threejs.ffi.mjs", "getLoopOnce")
fn loop_once() -> Dynamic

@internal
pub fn update_mixers(state: RendererState(id), delta_time: Float) -> Nil {
  let mixers = object_cache.get_all_mixers(state.cache)
  list.each(mixers, fn(entry) {
    let #(_id, mixer) = entry
    let mixer_dynamic = object_cache.unwrap_mixer(mixer)
    update_animation_mixer_ffi(mixer_dynamic, delta_time)
  })
}

@internal
pub fn update_particle_systems(
  state: RendererState(id),
  delta_time: Float,
) -> RendererState(id) {
  let particle_systems = object_cache.get_all_particle_systems(state.cache)

  let updated_cache =
    list.fold(particle_systems, state.cache, fn(cache, entry) {
      let #(string_id, cached_system) = entry

      // Unwrap, update, and wrap back
      let particle_state =
        particle_manager.unwrap_from_cache_entry(cached_system)
      let updated_state =
        particle_manager.update_particles(particle_state, delta_time)
      let wrapped_system = particle_manager.wrap_as_cache_entry(updated_state)

      // Update cache with new state (using string ID directly)
      let new_particles =
        dict.insert(cache.particles, string_id, wrapped_system)
      object_cache.CacheState(..cache, particles: new_particles)
    })

  RendererState(..state, cache: updated_cache)
}

@internal
pub fn sync_physics_transforms(state: RendererState(id)) -> Nil {
  case state.physics_world {
    option.Some(world) -> {
      // Use raw quaternion data from physics to avoid conversion errors
      physics.for_each_body_raw(world, fn(id, position, quaternion, body_type) {
        // Only sync Dynamic bodies - Kinematic/Fixed are controlled by scene
        case body_type {
          physics.Dynamic -> {
            // Get the Three.js object for this body
            case object_cache.get_object(state.cache, id) {
              option.Some(obj) -> {
                let obj_dynamic = object_cache.unwrap_object(obj)
                // Use identity scale since physics doesn't affect scale
                let scale = vec3.Vec3(1.0, 1.0, 1.0)
                // Apply transform using quaternion directly (no Euler conversion)
                apply_transform_with_quaternion_ffi(
                  obj_dynamic,
                  to_dynamic(position),
                  to_dynamic(quaternion),
                  to_dynamic(scale),
                )
                update_matrix_world_ffi(obj_dynamic, True)
              }
              option.None -> Nil
            }
          }
          physics.Kinematic | physics.Fixed -> Nil
        }
      })
    }
    option.None -> Nil
  }
}

@internal
pub fn clear_cache(state: RendererState(id)) -> RendererState(id) {
  // Dispose all objects
  let objects = object_cache.get_all_objects(state.cache)
  list.each(objects, fn(entry) {
    let #(_id, obj) = entry
    let obj_dynamic = object_cache.unwrap_object(obj)
    let geometry = get_object_geometry_ffi(obj_dynamic)
    let material = get_object_material_ffi(obj_dynamic)
    dispose_geometry_ffi(geometry)
    dispose_material_ffi(material)
  })

  let new_cache = object_cache.clear(state.cache)
  RendererState(..state, cache: new_cache)
}

@internal
pub fn get_cameras_with_viewports(
  state: RendererState(id),
) -> List(#(asset.Object3D, object_cache.Viewport)) {
  object_cache.get_cameras_with_viewports(state.cache)
  |> list.map(fn(entry) {
    let #(camera_obj, viewport) = entry
    #(object_cache.unwrap_object(camera_obj), viewport)
  })
}
