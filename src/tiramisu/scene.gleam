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
import paint
import paint/encode as paint_encode
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
import tiramisu/postprocessing
import tiramisu/spritesheet
import tiramisu/texture
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
  /// Empty node for organization, pivot points, and grouping without visual representation.
  /// Replaces the old Group type with clearer intent.
  Empty(id: id, children: List(Node(id)), transform: transform.Transform)
  Mesh(
    id: id,
    children: List(Node(id)),
    transform: transform.Transform,
    geometry: geometry.Geometry,
    material: material.Material,
    physics: Option(physics.RigidBody),
  )
  InstancedMesh(
    id: id,
    children: List(Node(id)),
    geometry: geometry.Geometry,
    material: material.Material,
    instances: List(transform.Transform),
  )
  Light(
    id: id,
    children: List(Node(id)),
    transform: transform.Transform,
    light: light.Light,
  )
  Camera(
    id: id,
    children: List(Node(id)),
    camera: camera.Camera,
    transform: transform.Transform,
    look_at: Option(vec3.Vec3(Float)),
    active: Bool,
    viewport: Option(#(Int, Int, Int, Int)),
    postprocessing: Option(postprocessing.PostProcessing),
  )
  LOD(
    id: id,
    children: List(Node(id)),
    levels: List(LODLevel(id)),
    transform: transform.Transform,
  )
  Model3D(
    id: id,
    children: List(Node(id)),
    object: asset.Object3D,
    transform: transform.Transform,
    animation: Option(AnimationPlayback),
    physics: Option(physics.RigidBody),
    material: Option(material.Material),
  )
  InstancedModel(
    id: id,
    children: List(Node(id)),
    object: asset.Object3D,
    instances: List(transform.Transform),
    physics: Option(physics.RigidBody),
    material: Option(material.Material),
  )
  Audio(id: id, children: List(Node(id)), audio: audio.Audio)
  Particles(
    id: id,
    children: List(Node(id)),
    emitter: particle_emitter.ParticleEmitter,
    transform: transform.Transform,
    active: Bool,
  )
  // UI overlay nodes
  CSS2DLabel(
    id: id,
    children: List(Node(id)),
    html: String,
    transform: transform.Transform,
  )
  CSS3DLabel(
    id: id,
    children: List(Node(id)),
    html: String,
    transform: transform.Transform,
  )
  // Canvas drawings rendered to texture with depth occlusion
  Canvas(
    id: id,
    children: List(Node(id)),
    encoded_picture: String,
    texture_width: Int,
    texture_height: Int,
    width: Float,
    height: Float,
    transform: transform.Transform,
  )
  // Animated sprite with spritesheet
  AnimatedSprite(
    id: id,
    children: List(Node(id)),
    spritesheet: spritesheet.Spritesheet,
    animation: spritesheet.Animation,
    state: spritesheet.AnimationState,
    width: Float,
    height: Float,
    transform: transform.Transform,
    pixel_art: Bool,
    physics: Option(physics.RigidBody),
  )
  // Debug visualization nodes
  DebugBox(
    id: id,
    children: List(Node(id)),
    min: Vec3(Float),
    max: Vec3(Float),
    color: Int,
  )
  DebugSphere(
    id: id,
    children: List(Node(id)),
    center: Vec3(Float),
    radius: Float,
    color: Int,
  )
  DebugLine(
    id: id,
    children: List(Node(id)),
    from: Vec3(Float),
    to: Vec3(Float),
    color: Int,
  )
  DebugAxes(id: id, children: List(Node(id)), origin: Vec3(Float), size: Float)
  DebugGrid(
    id: id,
    children: List(Node(id)),
    size: Float,
    divisions: Int,
    color: Int,
  )
  DebugPoint(
    id: id,
    children: List(Node(id)),
    position: Vec3(Float),
    size: Float,
    color: Int,
  )
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
/// scene.mesh(
///   id: "player",
///   geometry: cube_geo,
///   material: red_mat,
///   transform: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
///   physics: option.None,
///   children: [],
/// )
/// ```
pub fn mesh(
  id id: id,
  geometry geometry: geometry.Geometry,
  material material: material.Material,
  transform transform: transform.Transform,
  physics physics: Option(physics.RigidBody),
) -> Node(id) {
  Mesh(id:, children: [], transform:, geometry:, material:, physics:)
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
) -> Node(id) {
  InstancedMesh(id:, geometry:, material:, instances:, children: [])
}

/// Create an empty node for organization, pivot points, or grouping.
///
/// Empty nodes don't render anything but are useful for organizing your scene hierarchy,
/// creating pivot points for rotation/animation, or grouping related objects.
///
/// This replaces the old `group` function with clearer intent.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/transform
///
/// // Group car parts together
/// scene.empty(
///   id: "car",
///   transform: car_transform,
///   children: [
///     scene.mesh(id: "body", ..., children: []),
///     scene.mesh(id: "wheel-fl", ..., children: []),
///     scene.mesh(id: "wheel-fr", ..., children: []),
///   ],
/// )
/// ```
pub fn empty(
  id id: id,
  transform transform: transform.Transform,
  children children: List(Node(id)),
) -> Node(id) {
  Empty(id:, children:, transform:)
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
/// scene.light(
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
) -> Node(id) {
  Light(id:, children: [], transform:, light:)
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
/// scene.camera(
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
/// scene.camera(
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
  postprocessing postprocessing: Option(postprocessing.PostProcessing),
) -> Node(id) {
  Camera(
    id:,
    children: [],
    camera:,
    transform:,
    look_at:,
    active:,
    viewport: option.map(viewport, fn(viewport) {
      #(viewport.x, viewport.y, viewport.width, viewport.height)
    }),
    postprocessing:,
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
/// let high_detail = scene.mesh(
///   id: "tree-high",
///   geometry: high_geo,
///   material: mat,
///   transform: transform.identity,
///   physics: option.None,
/// )
///
/// // Low detail mesh (shown far away)
/// let assert Ok(low_geo) = geometry.sphere(radius: 1.0, width_segments: 8, height_segments: 8)
/// let low_detail = scene.mesh(
///   id: "tree-low",
///   geometry: low_geo,
///   material: mat,
///   transform: transform.identity,
///   physics: option.None,
/// )
///
/// scene.lod(
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
) -> Node(id) {
  LOD(id:, levels:, transform:, children: [])
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
  material material: Option(material.Material),
) -> Node(id) {
  Model3D(
    id:,
    object:,
    transform:,
    animation:,
    physics:,
    material:,
    children: [],
  )
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
/// scene.instanced_model(
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
  material material: Option(material.Material),
) -> Node(id) {
  InstancedModel(id:, object:, instances:, physics:, material:, children: [])
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
/// scene.audio(id: "bgm", audio: background_music, children: [])
///
/// // Sound effect (from pre-loaded buffer)
/// let assert Ok(jump_buffer) = asset.get_audio(cache, "sounds/jump.mp3")
/// let jump_sound = audio.new_audio("jump")
///   |> audio.with_source(audio.Buffer(jump_buffer))
///   |> audio.with_volume(0.8)
///
/// scene.audio(id: "jump-sfx", audio: jump_sound, children: [])
/// ```
pub fn audio(id id: id, audio audio: audio.Audio) -> Node(id) {
  Audio(id:, audio:, children: [])
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
/// scene.particles(
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
) -> Node(id) {
  Particles(id:, emitter:, transform:, active:, children: [])
}

/// Create a CSS2D label that follows a 3D position in screen space.
///
/// CSS2D labels are HTML elements that follow 3D objects but always face the camera.
/// Perfect for health bars, nameplates, tooltips, or interactive UI elements.
///
/// **HTML**: Raw HTML string. Use Lustre's `element.to_string()` for type-safe HTML.
/// **Position**: Offset from parent object (or world position if top-level node).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import vec/vec3
/// import lustre/element/html
/// import lustre/element
/// import lustre/attribute
///
/// // Option 1: Using Lustre (recommended)
/// let hp_element = html.div([attribute.class("bg-red-500 text-white px-4 py-2")], [
///   html.text("HP: 100")
/// ])
/// scene.css2d_label(
///   id: "player-hp",
///   html: element.to_string(hp_element),
///   position: vec3.Vec3(0.0, 2.0, 0.0),
/// )
///
/// // Option 2: Raw HTML string
/// scene.css2d_label(
///   id: "player-name",
///   html: "<div class='text-white font-bold'>Player</div>",
///   transform: vec3.Vec3(0.0, 2.5, 0.0),
/// )
/// ```
pub fn css2d_label(
  id id: id,
  html html: String,
  transform transform: transform.Transform,
) -> Node(id) {
  CSS2DLabel(id:, html:, transform:, children: [])
}

/// Create a CSS3D label that respects 3D depth and occlusion.
///
/// CSS3D labels are HTML elements that live "in" 3D space with full transformations.
/// Unlike CSS2D labels (always on top), CSS3D labels hide behind objects and can
/// rotate in 3D. Great for immersive UI elements.
///
/// **HTML**: Raw HTML string. Use Lustre's `element.to_string()` for type-safe HTML.
/// **Position**: Offset from parent object (or world position if top-level node).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import vec/vec3
///
/// // Label that hides behind objects
/// scene.css3d_label(
///   id: "3d-sign",
///   html: "<div class='text-white text-2xl'>→ Exit</div>",
///   transform: vec3.Vec3(0.0, 2.0, 0.0),
/// )
/// ```
pub fn css3d_label(
  id id: id,
  html html: String,
  transform transform: transform.Transform,
) -> Node(id) {
  CSS3DLabel(id:, html:, transform:, children: [])
}

/// Create a canvas node with a paint.Picture drawing rendered to a texture.
///
/// Canvas nodes are Three.js planes with paint.Picture drawings rendered to canvas textures.
/// Unlike CSS2D/CSS3D, they are true 3D meshes that respect depth testing (hide behind objects).
///
/// Uses the `paint` library for canvas drawing operations.
///
/// **Picture**: A paint.Picture created using paint's drawing API
/// **Texture Width/Height**: Canvas texture resolution in pixels (higher = sharper but more memory)
/// **Width/Height**: World space size of the canvas plane
/// **Transform**: Position, rotation, scale
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec3
/// import paint as p
///
/// // Create health bar using paint
/// let health_bar = p.combine([
///   // Background
///   p.rectangle(256.0, 64.0)
///     |> p.fill(p.colour_rgb(0, 0, 0)),
///   // Health bar
///   p.rectangle(192.0, 20.0)
///     |> p.translate_xy(10.0, 22.0)
///     |> p.fill(p.colour_rgb(255, 0, 0)),
///   // Text
///   p.text("HP: 75/100", 14.0)
///     |> p.translate_xy(10.0, 50.0)
///     |> p.fill(p.colour_rgb(255, 255, 255)),
/// ])
///
/// scene.canvas(
///   id: "health",
///   picture: health_bar,
///   texture_width: 256,
///   texture_height: 64,
///   width: 2.0,
///   height: 0.5,
///   transform: transform.at(position: vec3.Vec3(0.0, 2.0, 0.0)),
/// )
/// ```
pub fn canvas(
  id id: id,
  picture picture: paint.Picture,
  texture_width texture_width: Int,
  texture_height texture_height: Int,
  width width: Float,
  height height: Float,
  transform transform: transform.Transform,
) -> Node(id) {
  // Encode picture to string for efficient comparison and storage
  let encoded_picture = paint_encode.to_string(picture)

  Canvas(
    id:,
    encoded_picture:,
    texture_width:,
    texture_height:,
    width:,
    height:,
    transform:,
    children: [],
  )
}

/// Create an animated sprite node with spritesheet animation.
///
/// Animated sprites display a textured plane that cycles through frames
/// from a spritesheet. The animation state is managed in your model and
/// updated each frame.
///
/// ## Parameters
///
/// - `id`: Unique identifier for this sprite
/// - `spritesheet`: The spritesheet definition
/// - `animation`: The animation sequence to play
/// - `state`: Current animation state (from your model)
/// - `width`: World space width of the sprite plane
/// - `height`: World space height of the sprite plane
/// - `transform`: Position, rotation, and scale
/// - `pixel_art`: If True, uses nearest-neighbor filtering for crisp pixels
///
/// ## Example
///
/// ```gleam
/// import iv
/// import tiramisu/scene
/// import tiramisu/spritesheet
///
/// // In your init()
/// let assert Ok(sheet) = spritesheet.from_grid(
///   texture: player_texture,
///   columns: 8,
///   rows: 1,
/// )
///
/// let walk_anim = spritesheet.animation(
///   name: "walk",
///   frames: iv.from_list([0, 1, 2, 3, 4, 5, 6, 7]),
///   frame_duration: 0.1,
///   loop: spritesheet.Repeat,
/// )
///
/// let model = Model(
///   player_state: spritesheet.initial_state("walk"),
///   // ...
/// )
///
/// // In your update()
/// fn update(model, msg, ctx) {
///   case msg {
///     Tick -> {
///       let new_state = spritesheet.update(
///         state: model.player_state,
///         animation: walk_anim,
///         delta_time: ctx.delta_time,
///       )
///       Model(..model, player_state: new_state)
///     }
///   }
/// }
///
/// // In your view()
/// fn view(model, _ctx) {
///   [
///     scene.animated_sprite(
///       id: Player,
///       spritesheet: sheet,
///       animation: walk_anim,
///       state: model.player_state,
///       width: 2.0,
///       height: 2.0,
///       transform: transform.at(vec3.Vec3(0.0, 0.0, 0.0)),
///       pixel_art: True,
///       physics: option.None,
///     ),
///   ]
/// }
/// ```
pub fn animated_sprite(
  id id: id,
  spritesheet spritesheet: spritesheet.Spritesheet,
  animation animation: spritesheet.Animation,
  state state: spritesheet.AnimationState,
  width width: Float,
  height height: Float,
  transform transform: transform.Transform,
  pixel_art pixel_art: Bool,
  physics physics: Option(physics.RigidBody),
) -> Node(id) {
  AnimatedSprite(
    id:,
    spritesheet:,
    animation:,
    state:,
    width:,
    height:,
    transform:,
    pixel_art:,
    physics:,
    children: [],
  )
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
) -> Node(id) {
  DebugBox(id:, min:, max:, color:, children: [])
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
///   children: [],
/// )
/// ```
pub fn debug_sphere(
  id id: id,
  center center: Vec3(Float),
  radius radius: Float,
  color color: Int,
) -> Node(id) {
  DebugSphere(id:, center:, radius:, color:, children: [])
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
///   children: [],
/// )
/// ```
pub fn debug_line(
  id id: id,
  from from: Vec3(Float),
  to to: Vec3(Float),
  color color: Int,
) -> Node(id) {
  DebugLine(id:, from:, to:, color:, children: [])
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
pub fn debug_axes(
  id id: id,
  origin origin: Vec3(Float),
  size size: Float,
) -> Node(id) {
  DebugAxes(id:, origin:, size:, children: [])
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
) -> Node(id) {
  DebugGrid(id:, size:, divisions:, color:, children: [])
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
) -> Node(id) {
  DebugPoint(id:, position:, size:, color:, children: [])
}

pub fn with_children(node: Node(id), children children: List(Node(id))) {
  case node {
    AnimatedSprite(..) -> AnimatedSprite(..node, children:)
    Audio(..) -> Audio(..node, children:)
    CSS2DLabel(..) -> CSS2DLabel(..node, children:)
    CSS3DLabel(..) -> CSS3DLabel(..node, children:)
    Camera(..) -> Camera(..node, children:)
    Canvas(..) -> Canvas(..node, children:)
    DebugAxes(..) -> DebugAxes(..node, children:)
    DebugBox(..) -> DebugBox(..node, children:)
    DebugGrid(..) -> DebugGrid(..node, children:)
    DebugLine(..) -> DebugLine(..node, children:)
    DebugPoint(..) -> DebugPoint(..node, children:)
    DebugSphere(..) -> DebugSphere(..node, children:)
    Empty(..) -> Empty(..node, children:)
    InstancedMesh(..) -> InstancedMesh(..node, children:)
    InstancedModel(..) -> InstancedModel(..node, children:)
    LOD(..) -> LOD(..node, children:)
    Light(..) -> Light(..node, children:)
    Mesh(..) -> Mesh(..node, children:)
    Model3D(..) -> Model3D(..node, children:)
    Particles(..) -> Particles(..node, children:)
  }
}

/// Apply post-processing effects to a node and its children.
///
/// Post-processing effects are applied to the entire subtree starting from this node.
/// Effects are rendered in the order they are added to the pipeline.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/postprocessing as pp
/// import gleam/option
///
/// // Apply bloom effect to a mesh
/// scene.mesh(
///   id: "glowing-cube",
///   geometry: cube_geo,
///   material: emissive_mat,
///   transform: transform.identity,
///   physics: option.None,
/// )
/// |> scene.with_postprocessing(
///   pp.new()
///   |> pp.add_pass(pp.bloom(strength: 1.5, threshold: 0.85, radius: 0.4))
/// )
///
/// // Apply multiple effects to a group
/// scene.empty(id: "scene-root", transform: transform.identity, children: [])
/// |> scene.with_children([camera, light, player])
/// |> scene.with_postprocessing(
///   pp.new()
///   |> pp.add_pass(pp.bloom(strength: 0.8, threshold: 0.9, radius: 0.3))
///   |> pp.add_pass(pp.vignette(darkness: 0.8, offset: 1.0))
///   |> pp.add_pass(pp.fxaa())
/// )
/// ```
@internal
pub type Patch(id) {
  AddNode(id: id, node: Node(id), parent_id: Option(id))
  RemoveNode(id: id)
  UpdateTransform(id: id, transform: transform.Transform)
  UpdateMaterial(id: id, material: Option(material.Material))
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
  UpdateCameraPostprocessing(
    id: id,
    postprocessing: Option(postprocessing.PostProcessing),
  )
  UpdateParticleEmitter(id: id, emitter: particle_emitter.ParticleEmitter)
  UpdateParticleActive(id: id, active: Bool)
  UpdateCSS2DLabel(id: id, html: String, transform: transform.Transform)
  UpdateCSS3DLabel(id: id, html: String, transform: transform.Transform)
  UpdateCanvas(
    id: id,
    encoded_picture: String,
    texture_width: Int,
    texture_height: Int,
    width: Float,
    height: Float,
    transform: transform.Transform,
  )
  UpdateAnimatedSprite(
    id: id,
    spritesheet: spritesheet.Spritesheet,
    animation: spritesheet.Animation,
    state: spritesheet.AnimationState,
    width: Float,
    height: Float,
    transform: transform.Transform,
    pixel_art: Bool,
  )
}

type NodeWithParent(id) {
  NodeWithParent(node: Node(id), parent_id: Option(id), depth: Int)
}

fn flatten_scene(nodes: List(Node(id))) -> dict.Dict(id, NodeWithParent(id)) {
  flatten_scene_helper(nodes, option.None, 0, dict.new())
}

fn flatten_scene_helper(
  nodes: List(Node(id)),
  parent_id: Option(id),
  current_depth: Int,
  acc: dict.Dict(id, NodeWithParent(id)),
) -> dict.Dict(id, NodeWithParent(id)) {
  list.fold(nodes, acc, fn(acc, node) {
    let node_id = node.id

    // Store node with cached depth (avoids recursive calculation later)
    let acc =
      dict.insert(acc, node_id, NodeWithParent(node, parent_id, current_depth))
    let children = node.children
    // Recurse with incremented depth
    flatten_scene_helper(children, option.Some(node_id), current_depth + 1, acc)
  })
}

@internal
pub fn diff(
  previous: Option(Node(id)),
  current: Option(Node(id)),
) -> List(Patch(id)) {
  // Early exit: if scenes are identical by reference, no work needed
  case previous == current {
    True -> {
      []
    }
    False -> {
      // Convert optional nodes to lists for flatten_scene
      let prev_list = case previous {
        option.Some(node) -> [node]
        option.None -> []
      }
      let curr_list = case current {
        option.Some(node) -> [node]
        option.None -> []
      }

      let prev_dict = flatten_scene(prev_list)
      let curr_dict = flatten_scene(curr_list)

      // Early exit: if both scenes are empty, no patches needed
      let prev_size = dict.size(prev_dict)
      let curr_size = dict.size(curr_dict)
      case prev_size == 0 && curr_size == 0 {
        True -> []
        False -> {
          // Use dict.has_key for O(log n) membership checks (no set allocation needed)
          let prev_ids = dict.keys(prev_dict)
          let curr_ids = dict.keys(curr_dict)

          // Find removals: IDs in previous but not in current
          let removals =
            list.filter(prev_ids, fn(id) { !dict.has_key(curr_dict, id) })
            |> list.map(fn(id) { RemoveNode(id) })

          // Find nodes that exist in both but have changed parents (need remove + add)
          let #(parent_changed_ids, same_parent_ids) =
            list.filter(curr_ids, fn(id) { dict.has_key(prev_dict, id) })
            |> list.partition(fn(id) {
              case dict.get(prev_dict, id), dict.get(curr_dict, id) {
                Ok(NodeWithParent(_, prev_parent, _)),
                  Ok(NodeWithParent(_, curr_parent, _))
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
                Ok(NodeWithParent(node, parent_id, _)) ->
                  Ok(AddNode(id, node, parent_id))
                Error(_) -> Error(Nil)
              }
            })

          // Find additions: IDs in current but not in previous
          // Sort additions so parents are added before children
          let additions =
            list.filter(curr_ids, fn(id) { !dict.has_key(prev_dict, id) })
            |> list.filter_map(fn(id) {
              case dict.get(curr_dict, id) {
                Ok(NodeWithParent(node, parent_id, _)) ->
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
                Ok(NodeWithParent(prev_node, _, _)),
                  Ok(NodeWithParent(curr_node, _, _))
                -> compare_nodes(id, prev_node, curr_node)
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
/// O(n) optimized: use list.append which preserves order
fn concat_patches(lists: List(List(Patch(id)))) -> List(Patch(id)) {
  // Simply flatten the list of lists while preserving order
  list.fold(lists, [], fn(acc, patches) { list.append(acc, patches) })
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
        AddNode(id, _, _) -> {
          // Look up the node's pre-computed depth from node_dict
          let depth = case dict.get(node_dict, id) {
            Ok(NodeWithParent(_, _, node_depth)) -> node_depth
            Error(_) -> 0
            // Fallback if not found
          }
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
    Mesh(
      id: _,
      children: _,
      transform: previous_transform,
      geometry: previous_geometry,
      material: previous_material,
      physics: previous_physics,
    ),
      Mesh(
        id: _,
        children: _,
        transform: current_transform,
        geometry: current_geometry,
        material: current_material,
        physics: current_physics,
      )
    ->
      compare_mesh_fields(
        id,
        previous_geometry:,
        previous_material:,
        previous_transform:,
        previous_physics:,
        current_geometry:,
        current_material:,
        current_transform:,
        current_physics:,
      )

    InstancedMesh(
      id: _,
      children: _,
      geometry: previous_geometry,
      material: previous_material,
      instances: previous_instances,
    ),
      InstancedMesh(
        id: _,
        children: _,
        geometry: current_geometry,
        material: current_material,
        instances: current_instances,
      )
    ->
      compare_instanced_mesh_fields(
        id,
        previous_geometry:,
        previous_material:,
        previous_instances:,
        current_geometry:,
        current_material:,
        current_instances:,
      )

    Light(
      id: _,
      children: _,
      transform: previous_transform,
      light: previous_light,
    ),
      Light(
        id: _,
        children: _,
        transform: current_transform,
        light: current_light,
      )
    ->
      compare_light_fields(
        id,
        previous_light:,
        previous_transform:,
        current_light:,
        current_transform:,
      )

    Empty(id: _, children: _, transform: prev_transform),
      Empty(id: _, children: _, transform: curr_transform)
    ->
      case prev_transform != curr_transform {
        True -> [UpdateTransform(id, curr_transform)]
        False -> []
      }

    Camera(
      id: _,
      children: _,
      camera: previous_camera,
      transform: previous_transform,
      look_at: previous_look_at,
      active: previous_active,
      viewport: previous_viewport,
      postprocessing: previous_postprocessing,
    ),
      Camera(
        id: _,
        children: _,
        camera: current_camera,
        transform: current_transform,
        look_at: current_look_at,
        active: current_active,
        viewport: current_viewport,
        postprocessing: current_postprocessing,
      )
    ->
      compare_camera_fields(
        id,
        previous_camera:,
        previous_transform:,
        previous_look_at:,
        previous_active:,
        previous_viewport:,
        previous_postprocessing:,
        current_camera:,
        current_transform:,
        current_look_at:,
        current_active:,
        current_viewport:,
        current_postprocessing:,
      )

    LOD(
      id: _,
      children: _,
      levels: previous_levels,
      transform: previous_transform,
    ),
      LOD(
        id: _,
        children: _,
        levels: current_levels,
        transform: current_transform,
      )
    ->
      compare_lod_fields(
        id,
        previous_levels:,
        previous_transform:,
        current_levels:,
        current_transform:,
      )

    Model3D(
      id: _,
      children: _,
      object: _,
      transform: previous_transform,
      animation: previous_animation,
      physics: previous_physics,
      material: previous_material,
    ),
      Model3D(
        id: _,
        children: _,
        object: _,
        transform: current_transform,
        animation: current_animation,
        physics: current_physics,
        material: current_material,
      )
    ->
      compare_model3d_fields(
        id,
        previous_transform:,
        previous_animation:,
        previous_physics:,
        previous_material:,
        current_transform:,
        current_animation:,
        current_physics:,
        current_material:,
      )

    InstancedModel(
      id: _,
      children: _,
      object: _,
      instances: previous_instances,
      physics: previous_physics,
      material: previous_material,
    ),
      InstancedModel(
        id: _,
        children: _,
        object: _,
        instances: current_instances,
        physics: current_physics,
        material: current_material,
      )
    ->
      compare_instanced_model_fields(
        id:,
        previous_instances:,
        previous_physics:,
        previous_material:,
        current_instances:,
        current_physics:,
        current_material:,
      )

    Audio(id: _, children: _, audio: prev_audio),
      Audio(id: _, children: _, audio: curr_audio)
    ->
      case prev_audio != curr_audio {
        True -> [UpdateAudio(id, curr_audio)]
        False -> []
      }

    Particles(
      id: _,
      children: _,
      emitter: previous_emitter,
      transform: previous_transform,
      active: previous_active,
    ),
      Particles(
        id: _,
        children: _,
        emitter: current_emitter,
        transform: current_transform,
        active: current_active,
      )
    ->
      compare_particle_fields(
        id:,
        previous_emitter:,
        previous_transform:,
        previous_active:,
        current_emitter:,
        current_transform:,
        current_active:,
      )

    CSS2DLabel(
      id: _,
      children: _,
      html: previous_html,
      transform: prev_transform,
    ),
      CSS2DLabel(id: _, children: _, html: curr_html, transform: curr_transform)
    ->
      case previous_html != curr_html || prev_transform != curr_transform {
        True -> [UpdateCSS2DLabel(id, curr_html, curr_transform)]
        False -> []
      }

    CSS3DLabel(id: _, children: _, html: prev_html, transform: prev_transform),
      CSS3DLabel(id: _, children: _, html: curr_html, transform: curr_transform)
    ->
      case prev_html != curr_html || prev_transform != curr_transform {
        True -> [UpdateCSS3DLabel(id, curr_html, curr_transform)]
        False -> []
      }

    Canvas(
      id: _,
      children: _,
      encoded_picture: prev_encoded_picture,
      texture_width: prev_tw,
      texture_height: prev_th,
      width: prev_w,
      height: prev_h,
      transform: prev_transform,
    ),
      Canvas(
        id: _,
        children: _,
        encoded_picture: curr_encoded_picture,
        texture_width: curr_tw,
        texture_height: curr_th,
        width: curr_w,
        height: curr_h,
        transform: curr_transform,
      )
    ->
      case
        prev_encoded_picture != curr_encoded_picture
        || prev_tw != curr_tw
        || prev_th != curr_th
        || prev_w != curr_w
        || prev_h != curr_h
        || prev_transform != curr_transform
      {
        True -> [
          UpdateCanvas(
            id,
            curr_encoded_picture,
            curr_tw,
            curr_th,
            curr_w,
            curr_h,
            curr_transform,
          ),
        ]
        False -> []
      }

    AnimatedSprite(
      id: _,
      children: _,
      spritesheet: previous_spritesheet,
      animation: previous_animation,
      state: previous_state,
      width: previous_width,
      height: previous_height,
      transform: previous_transform,
      pixel_art: previous_pixel_art,
      physics: previous_physics,
    ),
      AnimatedSprite(
        id: _,
        children: _,
        spritesheet: current_spritesheet,
        animation: current_animation,
        state: current_state,
        width: current_width,
        height: current_height,
        transform: current_transform,
        pixel_art: current_pixel_art,
        physics: current_physics,
      )
    ->
      compare_animated_sprite_fields(
        id:,
        previous_spritesheet:,
        previous_animation:,
        previous_state:,
        previous_width:,
        previous_height:,
        previous_transform:,
        previous_pixel_art:,
        previous_physics:,
        current_spritesheet:,
        current_animation:,
        current_state:,
        current_width:,
        current_height:,
        current_transform:,
        current_pixel_art:,
        current_physics:,
      )

    _, _ -> []
  }
}

/// Compare Mesh fields using accumulator pattern (no empty list allocations)
fn compare_mesh_fields(
  id: id,
  previous_geometry prev_geom: geometry.Geometry,
  previous_material prev_mat: material.Material,
  previous_transform prev_trans: transform.Transform,
  previous_physics prev_phys: Option(physics.RigidBody),
  current_geometry curr_geom: geometry.Geometry,
  current_material curr_mat: material.Material,
  current_transform curr_trans: transform.Transform,
  current_physics curr_phys: Option(physics.RigidBody),
) -> List(Patch(id)) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, option.Some(curr_mat)), ..patches]
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
  previous_geometry prev_geom: geometry.Geometry,
  previous_material prev_mat: material.Material,
  previous_instances prev_instances: List(transform.Transform),
  current_geometry curr_geom: geometry.Geometry,
  current_material curr_mat: material.Material,
  current_instances curr_instances: List(transform.Transform),
) -> List(Patch(id)) {
  let patches = []

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, option.Some(curr_mat)), ..patches]
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
  previous_light prev_light: light.Light,
  previous_transform prev_trans: transform.Transform,
  current_light curr_light: light.Light,
  current_transform curr_trans: transform.Transform,
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
  previous_levels prev_levels: List(LODLevel(id)),
  previous_transform prev_trans: transform.Transform,
  current_levels curr_levels: List(LODLevel(id)),
  current_transform curr_trans: transform.Transform,
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
  previous_camera prev_cam: camera.Camera,
  previous_transform prev_trans: transform.Transform,
  previous_look_at prev_look_at: Option(vec3.Vec3(Float)),
  previous_active prev_active: Bool,
  previous_viewport prev_viewport: Option(#(Int, Int, Int, Int)),
  previous_postprocessing prev_pp: Option(postprocessing.PostProcessing),
  current_camera curr_cam: camera.Camera,
  current_transform curr_trans: transform.Transform,
  current_look_at curr_look_at: Option(vec3.Vec3(Float)),
  current_active curr_active: Bool,
  current_viewport curr_viewport: Option(#(Int, Int, Int, Int)),
  current_postprocessing curr_pp: Option(postprocessing.PostProcessing),
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

  // If postprocessing changed, emit UpdateCameraPostprocessing patch
  let patches = case prev_pp != curr_pp {
    True -> [UpdateCameraPostprocessing(id, curr_pp), ..patches]
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
  previous_transform prev_trans: transform.Transform,
  previous_animation prev_anim: Option(AnimationPlayback),
  previous_physics prev_phys: Option(physics.RigidBody),
  previous_material prev_mat: Option(material.Material),
  current_transform curr_trans: transform.Transform,
  current_animation curr_anim: Option(AnimationPlayback),
  current_physics curr_phys: Option(physics.RigidBody),
  current_material curr_mat: Option(material.Material),
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

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, curr_mat), ..patches]
    False -> patches
  }

  patches
}

/// Compare InstancedModel fields using accumulator pattern
fn compare_instanced_model_fields(
  id id: id,
  previous_instances previous_instances: List(transform.Transform),
  previous_physics previous_physics: Option(physics.RigidBody),
  previous_material previous_material: Option(material.Material),
  current_instances current_instances: List(transform.Transform),
  current_physics current_physics: Option(physics.RigidBody),
  current_material current_material: Option(material.Material),
) -> List(Patch(id)) {
  let patches = []

  let patches = case previous_instances != current_instances {
    True -> [UpdateInstances(id, current_instances), ..patches]
    False -> patches
  }

  let patches = case previous_physics != current_physics {
    True -> [UpdatePhysics(id, current_physics), ..patches]
    False -> patches
  }

  let patches = case previous_material != current_material {
    True -> [UpdateMaterial(id, current_material), ..patches]
    False -> patches
  }

  patches
}

/// Compare Particles fields using accumulator pattern
fn compare_particle_fields(
  id id: id,
  previous_emitter previous_emitter: particle_emitter.ParticleEmitter,
  previous_transform previous_transform: transform.Transform,
  previous_active previous_active: Bool,
  current_emitter current_emitter: particle_emitter.ParticleEmitter,
  current_transform current_transform: transform.Transform,
  current_active current_active: Bool,
) -> List(Patch(id)) {
  let patches = []

  let patches = case previous_transform != current_transform {
    True -> [UpdateTransform(id, current_transform), ..patches]
    False -> patches
  }

  let patches = case previous_emitter != current_emitter {
    True -> [UpdateParticleEmitter(id, current_emitter), ..patches]
    False -> patches
  }

  let patches = case previous_active != current_active {
    True -> [UpdateParticleActive(id, current_active), ..patches]
    False -> patches
  }

  patches
}

/// Compare AnimatedSprite fields using accumulator pattern
fn compare_animated_sprite_fields(
  id id: id,
  previous_spritesheet previous_spritesheet: spritesheet.Spritesheet,
  previous_animation previous_animation: spritesheet.Animation,
  previous_state previous_state: spritesheet.AnimationState,
  previous_width previous_width: Float,
  previous_height previous_height: Float,
  previous_transform previous_transform: transform.Transform,
  previous_pixel_art previous_pixel_art: Bool,
  previous_physics previous_physics: Option(physics.RigidBody),
  current_spritesheet current_spritesheet: spritesheet.Spritesheet,
  current_animation current_animation: spritesheet.Animation,
  current_state current_state: spritesheet.AnimationState,
  current_width current_width: Float,
  current_height current_height: Float,
  current_transform current_transform: transform.Transform,
  current_pixel_art current_pixel_art: Bool,
  current_physics current_physics: Option(physics.RigidBody),
) -> List(Patch(id)) {
  let patches = []

  let patches = case previous_physics != current_physics {
    True -> [UpdatePhysics(id, current_physics), ..patches]
    False -> patches
  }

  case
    previous_spritesheet != current_spritesheet
    || previous_animation != current_animation
    || previous_state != current_state
    || previous_width != current_width
    || previous_height != current_height
    || previous_transform != current_transform
    || previous_pixel_art != current_pixel_art
  {
    True -> [
      UpdateAnimatedSprite(
        id,
        current_spritesheet,
        current_animation,
        current_state,
        current_width,
        current_height,
        current_transform,
        current_pixel_art,
      ),
      ..patches
    ]
    False -> patches
  }
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
  position: vec3.Vec3(Float),
  quaternion: transform.Quaternion,
  scale: vec3.Vec3(Float),
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "updateMatrixWorld")
fn update_matrix_world_ffi(object: asset.Object3D, force: Bool) -> Nil

@external(javascript, "../threejs.ffi.mjs", "applyCameraLookAt")
fn apply_camera_look_at_ffi(camera: asset.Object3D, target: Vec3(Float)) -> Nil

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

@external(javascript, "../threejs.ffi.mjs", "setPerspectiveCameraParams")
fn set_perspective_camera_params_ffi(
  camera: asset.Object3D,
  fov: Float,
  aspect: Float,
  near: Float,
  far: Float,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setOrthographicCameraParams")
fn set_orthographic_camera_params_ffi(
  camera: asset.Object3D,
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  near: Float,
  far: Float,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setCameraUserData")
fn set_camera_user_data_vec3_ffi(
  camera: asset.Object3D,
  key: String,
  value: Vec3(Float),
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getCameraUserData")
fn get_camera_user_data_vec3_ffi(
  camera: asset.Object3D,
  key: String,
) -> Vec3(Float)

@external(javascript, "../threejs.ffi.mjs", "setCameraUserData")
fn set_camera_user_data_bool_ffi(
  camera: asset.Object3D,
  key: String,
  value: Bool,
) -> Nil

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

@external(javascript, "../tiramisu.ffi.mjs", "disposeObject3D")
fn dispose_object_3d_ffi(object: asset.Object3D) -> Nil

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

    UpdateCameraPostprocessing(id: id, postprocessing: pp) ->
      handle_update_camera_postprocessing(state, id, pp)

    UpdateParticleEmitter(id: id, emitter: emitter) ->
      handle_update_particle_emitter(state, id, emitter)

    UpdateParticleActive(id: id, active: active) ->
      handle_update_particle_active(state, id, active)

    UpdateCSS2DLabel(id: id, html: html, transform: trans) ->
      handle_update_css2d_label(state, id, html, trans)

    UpdateCSS3DLabel(id: id, html: html, transform: trans) ->
      handle_update_css3d_label(state, id, html, trans)

    UpdateCanvas(
      id: id,
      encoded_picture: encoded_picture,
      texture_width: tw,
      texture_height: th,
      width: w,
      height: h,
      transform: trans,
    ) -> handle_update_canvas(state, id, encoded_picture, tw, th, w, h, trans)

    UpdateAnimatedSprite(
      id: id,
      spritesheet: sheet,
      animation: anim,
      state: anim_state,
      width: w,
      height: h,
      transform: trans,
      pixel_art: pixel_art,
    ) ->
      handle_update_animated_sprite(
        state,
        id,
        sheet,
        anim,
        anim_state,
        w,
        h,
        trans,
        pixel_art,
      )
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
      children: _,
      transform: transform,
      geometry: geometry,
      material: material,
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
      children: _,
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

    Light(id: _, children: _, transform: transform, light: light) ->
      handle_add_light(state, id, light, transform, parent_id)

    Empty(id: _, children: _, transform: transform) ->
      handle_add_group(state, id, transform, parent_id)

    LOD(id: _, children: _, levels: levels, transform: transform) ->
      handle_add_lod(state, id, transform, levels, parent_id)

    Model3D(
      id: _,
      children: _,
      object: object,
      transform: transform,
      animation: animation,
      physics: physics,
      material: material,
    ) ->
      handle_add_model3d(
        state,
        id,
        object,
        transform,
        animation,
        physics,
        material,
        parent_id,
      )

    InstancedModel(
      id: _,
      children: _,
      object: object,
      instances: instances,
      physics: physics,
      material: material,
    ) ->
      handle_add_instanced_model(
        state,
        id,
        object,
        instances,
        physics,
        material,
        parent_id,
      )

    Audio(id: _, children: _, audio: audio) ->
      handle_add_audio(state, id, audio, parent_id)

    Camera(
      id: _,
      children: _,
      camera: camera,
      transform: transform,
      look_at: look_at,
      active: active,
      viewport: viewport,
      postprocessing: postprocessing,
    ) ->
      handle_add_camera(
        state,
        id,
        camera,
        transform,
        look_at,
        active,
        viewport,
        postprocessing,
        parent_id,
      )

    DebugBox(id: _, children: _, min: min, max: max, color: color) ->
      handle_add_debug_box(state, id, min, max, color, parent_id)

    DebugSphere(
      id: _,
      children: _,
      center: center,
      radius: radius,
      color: color,
    ) -> handle_add_debug_sphere(state, id, center, radius, color, parent_id)

    DebugLine(id: _, children: _, from: from, to: to, color: color) ->
      handle_add_debug_line(state, id, from, to, color, parent_id)

    DebugAxes(id: _, children: _, origin: origin, size: size) ->
      handle_add_debug_axes(state, id, origin, size, parent_id)

    DebugGrid(
      id: _,
      children: _,
      size: size,
      divisions: divisions,
      color: color,
    ) -> handle_add_debug_grid(state, id, size, divisions, color, parent_id)

    DebugPoint(id: _, children: _, position: position, size: size, color: color) ->
      handle_add_debug_point(state, id, position, size, color, parent_id)

    Particles(
      id: _,
      children: _,
      emitter: emitter,
      transform: transform,
      active: active,
    ) -> handle_add_particles(state, id, emitter, transform, active, parent_id)

    CSS2DLabel(id: _, children: _, html: html, transform: trans) ->
      handle_add_css2d_label(state, id, html, trans, parent_id)

    CSS3DLabel(id: _, children: _, html: html, transform: trans) ->
      handle_add_css3d_label(state, id, html, trans, parent_id)

    Canvas(
      id: _,
      children: _,
      encoded_picture: encoded_picture,
      texture_width: tw,
      texture_height: th,
      width: w,
      height: h,
      transform: trans,
    ) ->
      handle_add_canvas(
        state,
        id,
        encoded_picture,
        tw,
        th,
        w,
        h,
        trans,
        parent_id,
      )

    AnimatedSprite(
      id: _,
      children: _,
      spritesheet: sheet,
      animation: anim,
      state: anim_state,
      width: w,
      height: h,
      transform: trans,
      pixel_art: pixel_art,
      physics: physics,
    ) ->
      handle_add_animated_sprite(
        state,
        id,
        sheet,
        anim,
        anim_state,
        w,
        h,
        trans,
        pixel_art,
        physics,
        parent_id,
      )
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

  // Create physics body if specified - use world transform (includes parent transforms)
  let new_state = RendererState(..state, cache: new_cache)
  case physics, new_state.physics_world {
    option.Some(physics_config), option.Some(world) -> {
      // Get world position and rotation from the Three.js object (includes parent transforms)
      let unwrapped_obj = object_cache.unwrap_object(three_obj)
      let world_pos = get_world_position_ffi(unwrapped_obj)
      let world_quat = get_world_quaternion_ffi(unwrapped_obj)
      let world_transform =
        transform.at(world_pos)
        |> transform.with_quaternion_rotation(world_quat)
      let new_world =
        physics.create_body(world, id, physics_config, world_transform)
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
  material: Option(material.Material),
  parent_id: Option(id),
) -> RendererState(id) {
  // Extract all mesh/material pairs from the loaded model
  let pairs = extract_mesh_material_pairs_ffi(object)
  let geometries = get_pairs_geometries_ffi(pairs)
  let materials = case material {
    option.Some(mat) -> {
      // Create Material for each geometry if material override is provided
      let three_mat = material.create_material(mat)
      list.repeat(three_mat, list.length(geometries))
    }
    option.None -> get_pairs_materials_ffi(pairs)
  }

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
      children: _,
      transform: transform,
      geometry: geometry,
      material: material,
      physics: _,
    ) -> {
      let geometry_three = geometry.create_geometry(geometry)
      let material_three = material.create_material(material)
      let mesh = create_mesh_ffi(geometry_three, material_three)
      apply_transform_ffi(mesh, transform)
      mesh
    }
    Empty(id: _, children: _, transform: transform) -> {
      let group = create_group_ffi()
      apply_transform_ffi(group, transform)
      group
    }
    Model3D(
      id: _,
      children: _,
      object: object,
      transform: transform,
      animation: _,
      physics: _,
      material: material,
    ) -> {
      let cloned = clone_object_ffi(object)
      apply_transform_ffi(cloned, transform)
      // Apply material if provided
      case material {
        option.Some(mat) -> apply_material_to_object_ffi(cloned, mat)
        option.None -> Nil
      }
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
  material: Option(material.Material),
  parent_id: Option(id),
) -> RendererState(id) {
  // Clone the object to avoid modifying shared assets
  let cloned = clone_object_ffi(object)
  apply_transform_ffi(cloned, transform)

  // Apply material if provided
  case material {
    option.Some(mat) -> apply_material_to_object_ffi(cloned, mat)
    option.None -> Nil
  }

  // Create animation mixer
  let mixer_dynamic = create_animation_mixer_ffi(cloned)
  let mixer = object_cache.wrap_mixer(mixer_dynamic)
  let cache_with_mixer = object_cache.add_mixer(state.cache, id, mixer)

  // Setup animation if provided
  let cache_with_animation = case animation {
    option.Some(anim_playback) ->
      setup_animation(cache_with_mixer, id, mixer, anim_playback)
    option.None -> cache_with_mixer
  }

  let three_obj = object_cache.wrap_object(cloned)
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
  postprocessing: Option(postprocessing.PostProcessing),
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
      set_camera_user_data_vec3_ffi(camera_obj, "lookAtTarget", target)
      set_camera_user_data_bool_ffi(camera_obj, "needsLookAtUpdate", True)
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

  // Store postprocessing if specified
  let cache_with_postprocessing = case postprocessing {
    option.Some(pp) ->
      object_cache.set_camera_postprocessing(cache_with_viewport, id, pp)
    option.None -> cache_with_viewport
  }

  // Mark this ID as a camera in the cache
  let cache_with_camera = object_cache.add_camera(cache_with_postprocessing, id)

  // Set as active camera if specified
  case active {
    True -> set_active_camera_ffi(camera_obj)
    False -> Nil
  }

  let new_cache = object_cache.add_object(cache_with_camera, id, three_obj)
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

      // Dispose object recursively (geometry, materials, textures, children)
      dispose_object_3d_ffi(obj_dynamic)

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
              let target = get_camera_user_data_vec3_ffi(object, "lookAtTarget")
              apply_camera_look_at_ffi(object, target)
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
  material: Option(material.Material),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)

      // Only update if material is provided
      case material {
        option.Some(mat) -> {
          let old_material = get_object_material_ffi(obj_dynamic)
          dispose_material_ffi(old_material)

          let new_material = material.create_material(mat)
          set_object_material_ffi(obj_dynamic, new_material)
        }
        option.None -> Nil
      }

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
fn get_object_position_ffi(object: asset.Object3D) -> vec3.Vec3(Float)

@external(javascript, "../threejs.ffi.mjs", "getObjectRotation")
fn get_object_rotation_ffi(object: asset.Object3D) -> vec3.Vec3(Float)

@external(javascript, "../threejs.ffi.mjs", "getObjectScale")
fn get_object_scale_ffi(object: asset.Object3D) -> vec3.Vec3(Float)

@external(javascript, "../threejs.ffi.mjs", "getObjectQuaternion")
fn get_object_quaternion_ffi(
  object: asset.Object3D,
) -> transform.Quaternion

@external(javascript, "../threejs.ffi.mjs", "setObjectPosition")
fn set_object_position_ffi(
  object: asset.Object3D,
  position: vec3.Vec3(Float),
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setObjectRotation")
fn set_object_rotation_ffi(
  object: asset.Object3D,
  rotation: vec3.Vec3(Float),
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "setObjectScale")
fn set_object_scale_ffi(object: asset.Object3D, scale: vec3.Vec3(Float)) -> Nil

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
  new_physics: Option(physics.RigidBody),
) -> RendererState(id) {
  case state.physics_world {
    option.Some(world) -> {
      // Check if body already exists
      let body_exists = physics.has_body(world, id)

      case body_exists, new_physics {
        // Body exists, new config provided -> UPDATE transform
        // TODO: In future, detect if collider shape changed and only then recreate
        // For now, we just update the transform which covers most use cases
        True, option.Some(_physics_config) -> {
          // Get current transform from Three.js object
          case object_cache.get_object(state.cache, id) {
            option.Some(obj) -> {
              let obj_dynamic = object_cache.unwrap_object(obj)
              let object_transform = object_to_transform(obj_dynamic)

              // Just update the transform (preserves velocity!)
              let new_world =
                physics.update_body_transform(world, id, object_transform)

              RendererState(..state, physics_world: option.Some(new_world))
            }
            option.None -> state
          }
        }

        // Body exists, None provided -> REMOVE it
        True, option.None -> {
          let new_world = physics.remove_body(world, id)
          RendererState(..state, physics_world: option.Some(new_world))
        }

        // No body, config provided -> CREATE it
        False, option.Some(physics_config) -> {
          case object_cache.get_object(state.cache, id) {
            option.Some(obj) -> {
              let obj_dynamic = object_cache.unwrap_object(obj)
              let object_transform = object_to_transform(obj_dynamic)

              let new_world =
                physics.create_body(world, id, physics_config, object_transform)

              RendererState(..state, physics_world: option.Some(new_world))
            }
            option.None -> state
          }
        }

        // No body, None provided -> NOP
        False, option.None -> state
      }
    }
    option.None -> state
  }
}

// Helper to build Transform from Three.js object's position/quaternion/scale
fn object_to_transform(object: asset.Object3D) -> transform.Transform {
  let position = get_object_position_ffi(object)
  let quaternion = get_object_quaternion_ffi(object)
  let scale = get_object_scale_ffi(object)

  transform.identity
  |> transform.with_position(position)
  |> transform.with_quaternion_rotation(quaternion)
  |> transform.with_scale(scale)
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
  camera_type: camera.Camera,
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
          // Update camera projection parameters from camera_type
          let projection = camera.get_projection(camera_type)
          case projection {
            camera.Perspective(fov:, aspect:, near:, far:) -> {
              set_perspective_camera_params_ffi(
                obj_dynamic,
                fov,
                aspect,
                near,
                far,
              )
            }
            camera.Orthographic(left:, right:, top:, bottom:, near:, far:) -> {
              set_orthographic_camera_params_ffi(
                obj_dynamic,
                left,
                right,
                top,
                bottom,
                near,
                far,
              )
            }
          }
          update_camera_projection_matrix_ffi(obj_dynamic)

          // Apply lookAt if provided and update stored target
          case look_at {
            option.Some(target) -> {
              apply_camera_look_at_ffi(obj_dynamic, target)
              // Update the stored lookAtTarget so it's used in future transform updates
              set_camera_user_data_vec3_ffi(obj_dynamic, "lookAtTarget", target)
            }
            option.None -> {
              // If None, remove the stored lookAtTarget so the camera can rotate freely
              delete_camera_user_data_ffi(obj_dynamic, "lookAtTarget")
            }
          }

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

fn handle_update_camera_postprocessing(
  state: RendererState(id),
  id: id,
  pp: Option(postprocessing.PostProcessing),
) -> RendererState(id) {
  // Update the postprocessing config in the cache
  let new_cache = case pp {
    option.Some(pp_config) ->
      object_cache.set_camera_postprocessing(state.cache, id, pp_config)
    option.None -> object_cache.remove_camera_postprocessing(state.cache, id)
  }

  RendererState(..state, cache: new_cache)
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

fn handle_add_css2d_label(
  state: RendererState(id),
  id: id,
  html: String,
  trans: transform.Transform,
  parent_id: Option(id),
) -> RendererState(id) {
  let css2d_obj = create_css2d_object_ffi(html)
  apply_transform_ffi(css2d_obj, trans)
  let three_obj = object_cache.wrap_object(css2d_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_update_css2d_label(
  state: RendererState(id),
  id: id,
  html: String,
  trans: transform.Transform,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_3d = object_cache.unwrap_object(obj)
      update_css2d_object_html_ffi(obj_3d, html)
      apply_transform_ffi(obj_3d, trans)
      state
    }
    option.None -> state
  }
}

fn handle_add_css3d_label(
  state: RendererState(id),
  id: id,
  html: String,
  trans: transform.Transform,
  parent_id: Option(id),
) -> RendererState(id) {
  let css3d_obj = create_css3d_object_ffi(html)
  apply_transform_ffi(css3d_obj, trans)
  let three_obj = object_cache.wrap_object(css3d_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_update_css3d_label(
  state: RendererState(id),
  id: id,
  html: String,
  trans: transform.Transform,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_3d = object_cache.unwrap_object(obj)
      update_css3d_object_html_ffi(obj_3d, html)
      apply_transform_ffi(obj_3d, trans)
      state
    }
    option.None -> state
  }
}

fn handle_add_canvas(
  state: RendererState(id),
  id: id,
  encoded_picture: String,
  texture_width: Int,
  texture_height: Int,
  width: Float,
  height: Float,
  trans: transform.Transform,
  parent_id: Option(id),
) -> RendererState(id) {
  // Picture is already encoded, use it directly
  let texture =
    create_canvas_texture_from_picture_ffi(
      encoded_picture,
      texture_width,
      texture_height,
    )

  // Create plane mesh with the texture
  let canvas_mesh = create_canvas_plane_ffi(texture, width, height)

  // Apply transform
  apply_transform_ffi(canvas_mesh, trans)

  // Cache the encoded picture to avoid recreating texture on first update
  set_canvas_cached_picture_ffi(canvas_mesh, encoded_picture)

  let three_obj = object_cache.wrap_object(canvas_mesh)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)
  RendererState(..state, cache: new_cache)
}

fn handle_update_canvas(
  state: RendererState(id),
  id: id,
  encoded_picture: String,
  texture_width: Int,
  texture_height: Int,
  width: Float,
  height: Float,
  trans: transform.Transform,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_3d = object_cache.unwrap_object(obj)

      // Performance optimization: Only create new texture if picture changed
      // Check if encoded_picture differs from cached value
      let cached_picture = get_canvas_cached_picture_ffi(obj_3d)
      let picture_changed = cached_picture != encoded_picture

      case picture_changed {
        True -> {
          // Picture changed - create new texture and cache the picture data
          let texture =
            create_canvas_texture_from_picture_ffi(
              encoded_picture,
              texture_width,
              texture_height,
            )
          update_canvas_texture_ffi(obj_3d, texture)
          set_canvas_cached_picture_ffi(obj_3d, encoded_picture)
        }
        False -> {
          // Picture unchanged - skip expensive texture creation
          Nil
        }
      }

      // Always update size and transform (cheap operations)
      update_canvas_size_ffi(obj_3d, width, height)
      apply_transform_ffi(obj_3d, trans)

      state
    }
    option.None -> state
  }
}

fn handle_add_animated_sprite(
  state: RendererState(id),
  id: id,
  sheet: spritesheet.Spritesheet,
  anim: spritesheet.Animation,
  anim_state: spritesheet.AnimationState,
  width: Float,
  height: Float,
  trans: transform.Transform,
  pixel_art: Bool,
  physics: Option(physics.RigidBody),
  parent_id: Option(id),
) -> RendererState(id) {
  // Get the base texture and clone it for independent animation
  let base_texture = spritesheet.texture(sheet)
  let sprite_texture = texture.clone(base_texture)

  // Setup texture for spritesheet animation
  let #(repeat_x, repeat_y) = spritesheet.frame_repeat(sheet)
  sprite_texture
  |> texture.set_repeat(repeat_x, repeat_y)
  |> texture.set_wrap_mode(texture.RepeatWrapping, texture.RepeatWrapping)

  // Apply pixel art filtering if requested
  case pixel_art {
    True ->
      sprite_texture
      |> texture.set_filter_mode(texture.NearestFilter, texture.NearestFilter)
    False -> sprite_texture
  }

  // Get current frame and apply offset
  let assert Ok(frame) = spritesheet.current_frame(anim_state, anim)
  let #(offset_x, offset_y) = spritesheet.frame_offset(sheet, frame)
  sprite_texture
  |> texture.set_offset(offset_x, offset_y)

  // Create plane mesh with the animated texture
  let sprite_mesh = create_canvas_plane_ffi(sprite_texture, width, height)

  // Apply transform
  apply_transform_ffi(sprite_mesh, trans)

  let three_obj = object_cache.wrap_object(sprite_mesh)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, three_obj)

  // Create physics body if specified - update physics world in state
  let new_state = RendererState(..state, cache: new_cache)
  case physics, new_state.physics_world {
    option.Some(physics_config), option.Some(world) -> {
      let new_world = physics.create_body(world, id, physics_config, trans)
      RendererState(..new_state, physics_world: option.Some(new_world))
    }
    _, _ -> new_state
  }
}

fn handle_update_animated_sprite(
  state: RendererState(id),
  id: id,
  sheet: spritesheet.Spritesheet,
  anim: spritesheet.Animation,
  anim_state: spritesheet.AnimationState,
  width: Float,
  height: Float,
  trans: transform.Transform,
  pixel_art: Bool,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    option.Some(obj) -> {
      let obj_3d = object_cache.unwrap_object(obj)

      // Get the base texture and clone it for independent animation
      let base_texture = spritesheet.texture(sheet)
      let sprite_texture = texture.clone(base_texture)

      // Setup texture for spritesheet animation
      let #(repeat_x, repeat_y) = spritesheet.frame_repeat(sheet)
      sprite_texture
      |> texture.set_repeat(repeat_x, repeat_y)
      |> texture.set_wrap_mode(texture.RepeatWrapping, texture.RepeatWrapping)

      // Apply pixel art filtering if requested
      case pixel_art {
        True ->
          sprite_texture
          |> texture.set_filter_mode(
            texture.NearestFilter,
            texture.NearestFilter,
          )
        False -> sprite_texture
      }

      // Get current frame and apply offset
      let assert Ok(frame) = spritesheet.current_frame(anim_state, anim)
      let #(offset_x, offset_y) = spritesheet.frame_offset(sheet, frame)
      sprite_texture
      |> texture.set_offset(offset_x, offset_y)

      // Update the mesh texture
      update_canvas_texture_ffi(obj_3d, sprite_texture)

      // Update size
      update_canvas_size_ffi(obj_3d, width, height)

      // Update transform
      apply_transform_ffi(obj_3d, trans)

      state
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
                  position,
                  quaternion,
                  scale,
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
  // Dispose all objects recursively (geometry, materials, textures, children)
  let objects = object_cache.get_all_objects(state.cache)
  list.each(objects, fn(entry) {
    let #(_id, obj) = entry
    let obj_dynamic = object_cache.unwrap_object(obj)
    dispose_object_3d_ffi(obj_dynamic)
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

/// Get all cameras with their viewport and postprocessing configurations
/// Returns: List of (camera_id_string, camera_object, Option(viewport), Option(postprocessing))
@internal
pub fn get_all_cameras_with_info(
  state: RendererState(id),
) -> List(
  #(
    String,
    asset.Object3D,
    Option(object_cache.Viewport),
    Option(postprocessing.PostProcessing),
  ),
) {
  object_cache.get_all_cameras_with_info(state.cache)
  |> list.map(fn(entry) {
    let #(id_string, camera_obj, viewport_opt, pp_opt) = entry
    #(id_string, object_cache.unwrap_object(camera_obj), viewport_opt, pp_opt)
  })
}

// ============================================================================
// CSS2D FFI DECLARATIONS
// ============================================================================

@external(javascript, "../threejs.ffi.mjs", "createCSS2DObject")
fn create_css2d_object_ffi(html: String) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "updateCSS2DObjectHTML")
fn update_css2d_object_html_ffi(object: asset.Object3D, html: String) -> Nil

@external(javascript, "../threejs.ffi.mjs", "createCSS3DObject")
fn create_css3d_object_ffi(html: String) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "updateCSS3DObjectHTML")
fn update_css3d_object_html_ffi(object: asset.Object3D, html: String) -> Nil

// ============================================================================
// SPRITE FFI DECLARATIONS
// ============================================================================

@external(javascript, "../threejs.ffi.mjs", "createCanvasTextureFromPicture")
fn create_canvas_texture_from_picture_ffi(
  encoded_picture: String,
  width: Int,
  height: Int,
) -> asset.Texture

@external(javascript, "../threejs.ffi.mjs", "createCanvasPlane")
fn create_canvas_plane_ffi(
  texture: asset.Texture,
  width: Float,
  height: Float,
) -> asset.Object3D

@external(javascript, "../threejs.ffi.mjs", "updateCanvasTexture")
fn update_canvas_texture_ffi(
  object: asset.Object3D,
  texture: asset.Texture,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "updateCanvasSize")
fn update_canvas_size_ffi(
  object: asset.Object3D,
  width: Float,
  height: Float,
) -> Nil

@external(javascript, "../threejs.ffi.mjs", "getCanvasCachedPicture")
fn get_canvas_cached_picture_ffi(object: asset.Object3D) -> String

@external(javascript, "../threejs.ffi.mjs", "setCanvasCachedPicture")
fn set_canvas_cached_picture_ffi(object: asset.Object3D, picture: String) -> Nil

@external(javascript, "../threejs.ffi.mjs", "applyMaterialToObject")
fn apply_material_to_object_ffi_raw(
  object: asset.Object3D,
  three_material: material.ThreeMaterial,
) -> Nil

fn apply_material_to_object_ffi(
  object: asset.Object3D,
  material: material.Material,
) -> Nil {
  let three_material = material.create_material(material)
  apply_material_to_object_ffi_raw(object, three_material)
}

@external(javascript, "../threejs.ffi.mjs", "getWorldPosition")
fn get_world_position_ffi(object: asset.Object3D) -> Vec3(Float)

@external(javascript, "../threejs.ffi.mjs", "getWorldQuaternion")
fn get_world_quaternion_ffi(object: asset.Object3D) -> transform.Quaternion
