//// Declarative scene graph for building 3D scenes.
////
//// This module provides the scene node types returned by your `view` function. The engine
//// automatically diffs and patches the Three.js scene to match your declarative description,
//// similar to how virtual DOM works in web frameworks.
////
//// ## Scene Structure
////
//// Your `view` function returns a single root `Node`. Use `scene.empty` to group multiple
//// nodes together:
////
//// ```gleam
//// fn view(model: Model, ctx: Context) -> scene.Node {
////   scene.empty(
////     id: "root",
////     transform: transform.identity,
////     children: [
////       camera_node,
////       player_mesh,
////       enemy_mesh,
////       light_node,
////     ],
////   )
//// }
//// ```
////
//// ## Node Types
////
//// - **`empty`**: Invisible grouping node for organization
//// - **`mesh`**: 3D object with geometry and material
//// - **`sprite`**: 2D billboard that always faces camera
//// - **`camera`**: Viewpoint with projection settings
//// - **`light`**: Ambient, directional, point, or spot light
//// - **`audio`**: Global or positional audio source
//// - **`model`**: Loaded GLTF/GLB 3D model with animations
//// - **`LOD`**: Level-of-detail with automatic switching
////
//// ## String IDs
////
//// All nodes require a unique string ID for efficient diffing:
////
//// ```gleam
//// scene.mesh(id: "player", ...)
//// scene.mesh(id: "enemy-" <> int.to_string(idx), ...)
//// ```
////
//// ## Physics
////
//// Attach physics bodies to meshes for simulation:
////
//// ```gleam
//// scene.mesh(
////   id: "ball",
////   geometry: sphere_geo,
////   material: ball_mat,
////   transform: transform.identity,
////   physics: Some(
////     physics.new_rigid_body(physics.Dynamic)
////     |> physics.with_collider(physics.Sphere(transform.identity, 1.0))
////     |> physics.build()
////   ),
//// )
//// ```
////

import gleam/dict
import gleam/float
import gleam/int
import gleam/javascript/array
import gleam/list
import gleam/option
import gleam/order
import gleam/time/duration
import paint
import paint/encode as paint_encode
import plinth/browser/window
import savoiardi
import vec/vec2
import vec/vec3
import vec/vec3f

import tiramisu/audio
import tiramisu/camera
import tiramisu/geometry
import tiramisu/internal/audio_manager
import tiramisu/internal/object_cache
import tiramisu/light
import tiramisu/material
import tiramisu/model
import tiramisu/physics
import tiramisu/spritesheet
import tiramisu/texture
import tiramisu/transform

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
  LODLevel(distance: Float, node: Node)
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
pub fn lod_level(distance distance: Float, node node: Node) -> LODLevel {
  LODLevel(distance: distance, node: node)
}

pub opaque type Node {
  /// Empty node for organization, pivot points, and grouping without visual representation.
  /// Replaces the old Group type with clearer intent.
  Empty(id: String, children: List(Node), transform: transform.Transform)
  Mesh(
    id: String,
    children: List(Node),
    transform: transform.Transform,
    geometry: geometry.Geometry,
    material: material.Material,
    physics: option.Option(physics.RigidBody),
  )
  InstancedMesh(
    id: String,
    children: List(Node),
    geometry: geometry.Geometry,
    material: material.Material,
    instances: List(transform.Transform),
  )
  Light(
    id: String,
    children: List(Node),
    transform: transform.Transform,
    light: light.Light,
  )
  Camera(
    id: String,
    children: List(Node),
    camera: camera.Camera,
    transform: transform.Transform,
    active: Bool,
    viewport: option.Option(#(Int, Int, Int, Int)),
    postprocessing: option.Option(camera.PostProcessing),
  )
  LOD(
    id: String,
    children: List(Node),
    levels: List(LODLevel),
    transform: transform.Transform,
  )
  Model3D(
    id: String,
    children: List(Node),
    object: model.Object3D,
    transform: transform.Transform,
    animation: option.Option(model.AnimationPlayback),
    physics: option.Option(physics.RigidBody),
    material: option.Option(material.Material),
    transparent: Bool,
  )
  InstancedModel(
    id: String,
    children: List(Node),
    object: model.Object3D,
    instances: List(transform.Transform),
    physics: option.Option(physics.RigidBody),
    material: option.Option(material.Material),
    transparent: Bool,
  )
  Audio(id: String, children: List(Node), audio: audio.Audio)
  // UI overlay nodes
  CSS2D(
    id: String,
    children: List(Node),
    html: String,
    transform: transform.Transform,
  )
  CSS3D(
    id: String,
    children: List(Node),
    html: String,
    transform: transform.Transform,
  )
  // Canvas drawings rendered to texture with depth occlusion
  Canvas(
    id: String,
    children: List(Node),
    encoded_picture: String,
    texture_width: Int,
    texture_height: Int,
    width: Float,
    height: Float,
    transform: transform.Transform,
  )
  // Animated sprite with spritesheet
  AnimatedSprite(
    id: String,
    children: List(Node),
    sprite: spritesheet.Sprite,
    width: Float,
    height: Float,
    transform: transform.Transform,
    physics: option.Option(physics.RigidBody),
  )
  // Debug visualization nodes
  DebugBox(
    id: String,
    children: List(Node),
    min: vec3.Vec3(Float),
    max: vec3.Vec3(Float),
    color: Int,
  )
  DebugSphere(
    id: String,
    children: List(Node),
    center: vec3.Vec3(Float),
    radius: Float,
    color: Int,
  )
  DebugLine(
    id: String,
    children: List(Node),
    from: vec3.Vec3(Float),
    to: vec3.Vec3(Float),
    color: Int,
  )
  DebugAxes(
    id: String,
    children: List(Node),
    origin: vec3.Vec3(Float),
    size: Float,
  )
  DebugGrid(
    id: String,
    children: List(Node),
    size: Float,
    divisions: Int,
    color: Int,
  )
  DebugPoint(
    id: String,
    children: List(Node),
    position: vec3.Vec3(Float),
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
  id id: String,
  geometry geometry: geometry.Geometry,
  material material: material.Material,
  transform transform: transform.Transform,
  physics physics: option.Option(physics.RigidBody),
) -> Node {
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
  id id: String,
  geometry geometry: geometry.Geometry,
  material material: material.Material,
  instances instances: List(transform.Transform),
) -> Node {
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
  id id: String,
  transform transform: transform.Transform,
  children children: List(Node),
) -> Node {
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
  id id: String,
  light light: light.Light,
  transform transform: transform.Transform,
) -> Node {
  Light(id:, children: [], transform:, light:)
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
///   active: True,
///   viewport: option.Some(camera.ViewPort(position: vec2.Vec2(10, 10), size: vec2.Vec2(200, 200))),
/// )
/// ```
pub fn camera(
  id id: String,
  camera camera: camera.Camera,
  transform transform: transform.Transform,
  active active: Bool,
  viewport viewport: option.Option(camera.ViewPort),
  postprocessing postprocessing: option.Option(camera.PostProcessing),
) -> Node {
  Camera(
    id:,
    children: [],
    camera:,
    transform:,
    active:,
    viewport: option.map(viewport, fn(viewport) {
      #(
        viewport.position.x,
        viewport.position.y,
        viewport.size.x,
        viewport.size.y,
      )
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
  id id: String,
  levels levels: List(LODLevel),
  transform transform: transform.Transform,
) -> Node {
  LOD(id:, levels:, transform:, children: [])
}

/// Create a 3D model node from a loaded asset (GLTF, FBX, OBJ).
///
/// Use this for models loaded via the `model` module. Supports animations and physics.
///
/// **Animation**: Optional animation playback (single or blended). See `animation` module.
/// **Physics**: Optional rigid body for physics simulation.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/model
/// import tiramisu/animation
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/option
/// import gleam/list
///
/// // After loading a GLTF model
/// let scene_object = model.get_scene(gltf_data)
/// let clips = model.get_animations(gltf_data)
///
/// // Find walk animation
/// let walk_clip = list.find(clips, fn(clip) {
///   animation.clip_name(clip) == "Walk"
/// })
///
/// let walk_anim = animation.new_animation(walk_clip)
///   |> animation.set_speed(1.2)
///   |> animation.set_loop(animation.LoopRepeat)
///
/// scene.object_3d(
///   id: "player",
///   object: scene_object,
///   transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
///   animation: option.Some(animation.SingleAnimation(walk_anim)),
///   physics: option.None,
///   material: option.None,
/// )
/// ```
pub fn object_3d(
  id id: String,
  object object: model.Object3D,
  transform transform: transform.Transform,
  animation animation: option.Option(model.AnimationPlayback),
  physics physics: option.Option(physics.RigidBody),
  material material: option.Option(material.Material),
  transparent transparent: Bool,
) -> Node {
  Model3D(
    id:,
    object:,
    transform:,
    animation:,
    physics:,
    material:,
    transparent:,
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
/// import tiramisu/model
/// import tiramisu/transform
/// import vec/vec3
/// import gleam/option
/// import gleam/list
///
/// // After loading a GLTF model
/// let rock_scene = model.get_scene(rock_gltf)
///
/// // Place 50 rocks around the scene
/// let rock_positions = list.range(0, 49)
///   |> list.map(fn(i) {
///     let angle = int.to_float(i) *. 0.125
///     let radius = 20.0
///     let x = radius *. float.cos(angle)
///     let z = radius *. float.sin(angle)
///     transform.at(position: vec3.Vec3(x, 0.0, z))
///   })
///
/// scene.instanced_model(
///   id: "rock-field",
///   object: rock_scene,
///   instances: rock_positions,
///   physics: option.None,
///   material: option.None,
/// )
/// ```
pub fn instanced_model(
  id id: String,
  object object: model.Object3D,
  instances instances: List(transform.Transform),
  physics physics: option.Option(physics.RigidBody),
  material material: option.Option(material.Material),
  transparent transparent: Bool,
) -> Node {
  InstancedModel(
    id:,
    object:,
    instances:,
    physics:,
    material:,
    transparent:,
    children: [],
  )
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
pub fn audio(id id: String, audio audio: audio.Audio) -> Node {
  Audio(id:, audio:, children: [])
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
/// scene.css2d(
///   id: "player-hp",
///   html: element.to_string(hp_element),
///   position: vec3.Vec3(0.0, 2.0, 0.0),
/// )
///
/// // Option 2: Raw HTML string
/// scene.css2d(
///   id: "player-name",
///   html: "<div class='text-white font-bold'>Player</div>",
///   transform: vec3.Vec3(0.0, 2.5, 0.0),
/// )
/// ```
pub fn css2d(
  id id: String,
  html html: String,
  transform transform: transform.Transform,
) -> Node {
  CSS2D(id:, html:, transform:, children: [])
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
/// scene.css3d(
///   id: "3d-sign",
///   html: "<div class='text-white text-2xl'>→ Exit</div>",
///   transform: vec3.Vec3(0.0, 2.0, 0.0),
/// )
/// ```
pub fn css3d(
  id id: String,
  html html: String,
  transform transform: transform.Transform,
) -> Node {
  CSS3D(id:, html:, transform:, children: [])
}

/// Create a canvas node with a paint.Picture drawing rendered to a texture.
///
/// Canvas nodes are Three.js planes with paint.Picture drawings rendered to canvas textures.
/// Unlike CSS2D/CSS3D, they are true 3D meshes that respect depth testing (hide behind objects).
///
/// Uses the `paint` library for canvas drawing operations.
///
/// **Picture**: A paint.Picture created using paint's drawing API
/// **Texture Size**: Canvas texture resolution in pixels as Vec2(width, height) (higher = sharper but more memory)
/// **Size**: World space size of the canvas plane as Vec2(width, height)
/// **Transform**: Position, rotation, scale
///
/// ## Example
///
/// ```gleam
/// import tiramisu/scene
/// import tiramisu/transform
/// import vec/vec2
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
///   texture_size: vec2.Vec2(256, 64),
///   size: vec2.Vec2(2.0, 0.5),
///   transform: transform.at(position: vec3.Vec3(0.0, 2.0, 0.0)),
/// )
/// ```
pub fn canvas(
  id id: String,
  picture picture: paint.Picture,
  texture_size texture_size: vec2.Vec2(Int),
  size size: vec2.Vec2(Float),
  transform transform: transform.Transform,
) -> Node {
  // Encode picture to string for efficient comparison and storage
  let encoded_picture = paint_encode.to_string(picture)

  Canvas(
    id:,
    encoded_picture:,
    texture_width: texture_size.x,
    texture_height: texture_size.y,
    width: size.x,
    height: size.y,
    transform:,
    children: [],
  )
}

/// Create an animated sprite node with spritesheet animation.
///
/// Animated sprites display a textured plane that cycles through frames
/// from a spritesheet. The animation is managed by an AnimationMachine
/// which handles frame advancement and animation state transitions.
///
/// ## Parameters
///
/// - `id`: Unique identifier for this sprite
/// - `sprite`: The current sprite state from your animation machine
/// - `size`: World space size of the sprite plane as Vec2(width, height)
/// - `transform`: Position, rotation, and scale
/// - `physics`: Optional physics body configuration
///
/// ## Example
///
/// ```gleam
/// import gleam/option
/// import gleam/result
/// import gleam/time/duration
/// import tiramisu/scene
/// import tiramisu/spritesheet
/// import vec/vec2
///
/// // In your init()
/// let assert Ok(machine) =
///   spritesheet.new(texture: player_texture, columns: 8, rows: 4)
///   |> result.map(spritesheet.with_animation(
///     _,
///     name: "idle",
///     frames: [0, 1, 2, 3],
///     frame_duration: duration.milliseconds(100),
///     loop: spritesheet.Repeat,
///   ))
///   |> result.map(spritesheet.with_pixel_art(_, True))
///
/// let model = Model(machine: machine, ..)
///
/// // In your update()
/// fn update(model, msg, ctx) {
///   case msg {
///     Tick -> {
///       let #(new_machine, _) =
///         spritesheet.update(model.machine, model.context, ctx.delta_time)
///       Model(..model, machine: new_machine)
///     }
///   }
/// }
///
/// // In your view()
/// fn view(model, _ctx) {
///   [
///     scene.animated_sprite(
///       id: "player",
///       sprite: spritesheet.to_sprite(model.machine),
///       size: vec2.Vec2(2.0, 2.0),
///       transform: transform.identity,
///       physics: option.None,
///     ),
///   ]
/// }
/// ```
pub fn animated_sprite(
  id id: String,
  sprite sprite: spritesheet.Sprite,
  size size: vec2.Vec2(Float),
  transform transform: transform.Transform,
  physics physics: option.Option(physics.RigidBody),
) -> Node {
  AnimatedSprite(
    id:,
    sprite:,
    width: size.x,
    height: size.y,
    transform:,
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
  id id: String,
  min min: vec3.Vec3(Float),
  max max: vec3.Vec3(Float),
  color color: Int,
) -> Node {
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
  id id: String,
  center center: vec3.Vec3(Float),
  radius radius: Float,
  color color: Int,
) -> Node {
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
  id id: String,
  from from: vec3.Vec3(Float),
  to to: vec3.Vec3(Float),
  color color: Int,
) -> Node {
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
  id id: String,
  origin origin: vec3.Vec3(Float),
  size size: Float,
) -> Node {
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
  id id: String,
  size size: Float,
  divisions divisions: Int,
  color color: Int,
) -> Node {
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
  id id: String,
  position position: vec3.Vec3(Float),
  size size: Float,
  color color: Int,
) -> Node {
  DebugPoint(id:, position:, size:, color:, children: [])
}

pub fn with_children(node: Node, children children: List(Node)) {
  case node {
    AnimatedSprite(..) -> AnimatedSprite(..node, children:)
    Audio(..) -> Audio(..node, children:)
    CSS2D(..) -> CSS2D(..node, children:)
    CSS3D(..) -> CSS3D(..node, children:)
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
pub type Patch {
  AddNode(id: String, node: Node, parent_id: option.Option(String))
  RemoveNode(id: String)
  UpdateTransform(id: String, transform: transform.Transform)
  UpdateMaterial(id: String, material: option.Option(material.Material))
  UpdateGeometry(id: String, geometry: geometry.Geometry)
  UpdateLight(id: String, light: light.Light)
  UpdateAnimation(id: String, animation: option.Option(model.AnimationPlayback))
  UpdatePhysics(id: String, physics: option.Option(physics.RigidBody))
  UpdateAudio(id: String, audio: audio.Audio)
  UpdateInstances(id: String, instances: List(transform.Transform))
  UpdateLODLevels(id: String, levels: List(LODLevel))
  UpdateCamera(id: String, camera_type: camera.Camera)
  SetActiveCamera(id: String)
  UpdateCameraPostprocessing(
    id: String,
    postprocessing: option.Option(camera.PostProcessing),
  )
  UpdateCSS2DLabel(id: String, html: String, transform: transform.Transform)
  UpdateCSS3DLabel(id: String, html: String, transform: transform.Transform)
  UpdateCanvas(
    id: String,
    encoded_picture: String,
    texture_width: Int,
    texture_height: Int,
    width: Float,
    height: Float,
    transform: transform.Transform,
  )
  UpdateAnimatedSprite(
    id: String,
    sprite: spritesheet.Sprite,
    width: Float,
    height: Float,
    transform: transform.Transform,
  )
}

/// Internal cache type for flattened scene nodes (used for optimization)
@internal
pub opaque type NodeWithParent {
  NodeWithParent(node: Node, parent_id: option.Option(String), depth: Int)
}

fn flatten_scene(nodes: List(Node)) -> dict.Dict(String, NodeWithParent) {
  flatten_scene_helper(nodes, option.None, 0, dict.new())
}

fn flatten_scene_helper(
  nodes: List(Node),
  parent_id: option.Option(String),
  current_depth: Int,
  acc: dict.Dict(String, NodeWithParent),
) -> dict.Dict(String, NodeWithParent) {
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
  previous: option.Option(Node),
  current: option.Option(Node),
  cached_prev_dict: option.Option(dict.Dict(String, NodeWithParent)),
) -> #(List(Patch), dict.Dict(String, NodeWithParent)) {
  // Early exit: if scenes are identical by reference, no work needed
  case previous == current {
    True -> {
      let empty_dict = dict.new()
      #([], cached_prev_dict |> option.unwrap(empty_dict))
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

      // OPTIMIZATION: Reuse cached prev_dict if available (saves one tree traversal!)
      let prev_dict = case cached_prev_dict {
        option.Some(cached) -> cached
        option.None -> flatten_scene(prev_list)
      }

      let curr_dict = flatten_scene(curr_list)

      // Early exit: if both scenes are empty, no patches needed
      let prev_size = dict.size(prev_dict)
      let curr_size = dict.size(curr_dict)
      case prev_size == 0 && curr_size == 0 {
        True -> #([], curr_dict)
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
          let patches =
            batch_patches(removals, parent_change_removals, updates, additions)

          // Return patches + new cached dict for next frame
          #(patches, curr_dict)
        }
      }
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
/// O(n) optimized: use list.append which preserves order
fn concat_patches(lists: List(List(Patch))) -> List(Patch) {
  // Simply flatten the list of lists while preserving order
  list.fold(lists, [], fn(acc, patches) { list.append(acc, patches) })
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

fn compare_nodes(id: String, prev: Node, curr: Node) -> List(Patch) {
  case prev == curr {
    True -> []
    False -> compare_nodes_detailed(id, prev, curr)
  }
}

/// Detailed comparison of node properties (called only when nodes differ)
/// Uses accumulator pattern to avoid empty list allocations
fn compare_nodes_detailed(id: String, prev: Node, curr: Node) -> List(Patch) {
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
      active: previous_active,
      viewport: previous_viewport,
      postprocessing: previous_postprocessing,
    ),
      Camera(
        id: _,
        children: _,
        camera: current_camera,
        transform: current_transform,
        active: current_active,
        viewport: current_viewport,
        postprocessing: current_postprocessing,
      )
    ->
      compare_camera_fields(
        id,
        previous_camera:,
        previous_transform:,
        previous_active:,
        previous_viewport:,
        previous_postprocessing:,
        current_camera:,
        current_transform:,
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
      transparent: _,
    ),
      Model3D(
        id: _,
        children: _,
        object: _,
        transform: current_transform,
        animation: current_animation,
        physics: current_physics,
        material: current_material,
        transparent: _,
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
      transparent: _,
    ),
      InstancedModel(
        id: _,
        children: _,
        object: _,
        instances: current_instances,
        physics: current_physics,
        material: current_material,
        transparent: _,
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

    CSS2D(id: _, children: _, html: previous_html, transform: prev_transform),
      CSS2D(id: _, children: _, html: curr_html, transform: curr_transform)
    ->
      case previous_html != curr_html || prev_transform != curr_transform {
        True -> [UpdateCSS2DLabel(id, curr_html, curr_transform)]
        False -> []
      }

    CSS3D(id: _, children: _, html: prev_html, transform: prev_transform),
      CSS3D(id: _, children: _, html: curr_html, transform: curr_transform)
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
      sprite: previous_sprite,
      width: previous_width,
      height: previous_height,
      transform: previous_transform,
      physics: previous_physics,
    ),
      AnimatedSprite(
        id: _,
        children: _,
        sprite: current_sprite,
        width: current_width,
        height: current_height,
        transform: current_transform,
        physics: current_physics,
      )
    ->
      compare_animated_sprite_fields(
        id:,
        previous_sprite:,
        previous_width:,
        previous_height:,
        previous_transform:,
        previous_physics:,
        current_sprite:,
        current_width:,
        current_height:,
        current_transform:,
        current_physics:,
      )

    // Debug nodes: if properties changed, remove and re-add
    DebugBox(
      id: _,
      children: _,
      min: prev_min,
      max: prev_max,
      color: prev_color,
    ),
      DebugBox(
        id: _,
        children: curr_children,
        min: curr_min,
        max: curr_max,
        color: curr_color,
      )
    ->
      case
        prev_min != curr_min || prev_max != curr_max || prev_color != curr_color
      {
        True -> [
          RemoveNode(id),
          AddNode(
            id,
            DebugBox(
              id:,
              children: curr_children,
              min: curr_min,
              max: curr_max,
              color: curr_color,
            ),
            option.None,
          ),
        ]
        False -> []
      }

    DebugSphere(
      id: _,
      children: _,
      center: prev_center,
      radius: prev_radius,
      color: prev_color,
    ),
      DebugSphere(
        id: _,
        children: curr_children,
        center: curr_center,
        radius: curr_radius,
        color: curr_color,
      )
    ->
      case
        prev_center != curr_center
        || prev_radius != curr_radius
        || prev_color != curr_color
      {
        True -> [
          RemoveNode(id),
          AddNode(
            id,
            DebugSphere(
              id:,
              children: curr_children,
              center: curr_center,
              radius: curr_radius,
              color: curr_color,
            ),
            option.None,
          ),
        ]
        False -> []
      }

    DebugLine(
      id: _,
      children: _,
      from: prev_from,
      to: prev_to,
      color: prev_color,
    ),
      DebugLine(
        id: _,
        children: curr_children,
        from: curr_from,
        to: curr_to,
        color: curr_color,
      )
    ->
      case
        prev_from != curr_from || prev_to != curr_to || prev_color != curr_color
      {
        True -> [
          RemoveNode(id),
          AddNode(
            id,
            DebugLine(
              id:,
              children: curr_children,
              from: curr_from,
              to: curr_to,
              color: curr_color,
            ),
            option.None,
          ),
        ]
        False -> []
      }

    DebugAxes(id: _, children: _, origin: prev_origin, size: prev_size),
      DebugAxes(
        id: _,
        children: curr_children,
        origin: curr_origin,
        size: curr_size,
      )
    ->
      case prev_origin != curr_origin || prev_size != curr_size {
        True -> [
          RemoveNode(id),
          AddNode(
            id,
            DebugAxes(
              id:,
              children: curr_children,
              origin: curr_origin,
              size: curr_size,
            ),
            option.None,
          ),
        ]
        False -> []
      }

    DebugGrid(
      id: _,
      children: _,
      size: prev_size,
      divisions: prev_divisions,
      color: prev_color,
    ),
      DebugGrid(
        id: _,
        children: curr_children,
        size: curr_size,
        divisions: curr_divisions,
        color: curr_color,
      )
    ->
      case
        prev_size != curr_size
        || prev_divisions != curr_divisions
        || prev_color != curr_color
      {
        True -> [
          RemoveNode(id),
          AddNode(
            id,
            DebugGrid(
              id:,
              children: curr_children,
              size: curr_size,
              divisions: curr_divisions,
              color: curr_color,
            ),
            option.None,
          ),
        ]
        False -> []
      }

    DebugPoint(
      id: _,
      children: _,
      position: prev_position,
      size: prev_size,
      color: prev_color,
    ),
      DebugPoint(
        id: _,
        children: curr_children,
        position: curr_position,
        size: curr_size,
        color: curr_color,
      )
    ->
      case
        prev_position != curr_position
        || prev_size != curr_size
        || prev_color != curr_color
      {
        True -> [
          RemoveNode(id),
          AddNode(
            id,
            DebugPoint(
              id:,
              children: curr_children,
              position: curr_position,
              size: curr_size,
              color: curr_color,
            ),
            option.None,
          ),
        ]
        False -> []
      }

    _, _ -> []
  }
}

/// Compare Mesh fields using accumulator pattern (no empty list allocations)
fn compare_mesh_fields(
  id: String,
  previous_geometry prev_geom: geometry.Geometry,
  previous_material prev_mat: material.Material,
  previous_transform prev_trans: transform.Transform,
  previous_physics prev_phys: option.Option(physics.RigidBody),
  current_geometry curr_geom: geometry.Geometry,
  current_material curr_mat: material.Material,
  current_transform curr_trans: transform.Transform,
  current_physics curr_phys: option.Option(physics.RigidBody),
) -> List(Patch) {
  let patches = []

  // Only emit UpdateTransform if transform changed
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
  id: String,
  previous_geometry prev_geom: geometry.Geometry,
  previous_material prev_mat: material.Material,
  previous_instances prev_instances: List(transform.Transform),
  current_geometry curr_geom: geometry.Geometry,
  current_material curr_mat: material.Material,
  current_instances curr_instances: List(transform.Transform),
) -> List(Patch) {
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
  id: String,
  previous_light prev_light: light.Light,
  previous_transform prev_trans: transform.Transform,
  current_light curr_light: light.Light,
  current_transform curr_trans: transform.Transform,
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
  previous_levels prev_levels: List(LODLevel),
  previous_transform prev_trans: transform.Transform,
  current_levels curr_levels: List(LODLevel),
  current_transform curr_trans: transform.Transform,
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
  previous_camera prev_cam: camera.Camera,
  previous_transform prev_trans: transform.Transform,
  previous_active prev_active: Bool,
  previous_viewport prev_viewport: option.Option(#(Int, Int, Int, Int)),
  previous_postprocessing prev_pp: option.Option(camera.PostProcessing),
  current_camera curr_cam: camera.Camera,
  current_transform curr_trans: transform.Transform,
  current_active curr_active: Bool,
  current_viewport curr_viewport: option.Option(#(Int, Int, Int, Int)),
  current_postprocessing curr_pp: option.Option(camera.PostProcessing),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_cam != curr_cam || prev_viewport != curr_viewport {
    True -> [UpdateCamera(id, curr_cam), ..patches]
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
  id: String,
  previous_transform prev_trans: transform.Transform,
  previous_animation prev_anim: option.Option(model.AnimationPlayback),
  previous_physics prev_phys: option.Option(physics.RigidBody),
  previous_material prev_mat: option.Option(material.Material),
  current_transform curr_trans: transform.Transform,
  current_animation curr_anim: option.Option(model.AnimationPlayback),
  current_physics curr_phys: option.Option(physics.RigidBody),
  current_material curr_mat: option.Option(material.Material),
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

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, curr_mat), ..patches]
    False -> patches
  }

  patches
}

/// Compare InstancedModel fields using accumulator pattern
fn compare_instanced_model_fields(
  id id: String,
  previous_instances previous_instances: List(transform.Transform),
  previous_physics previous_physics: option.Option(physics.RigidBody),
  previous_material previous_material: option.Option(material.Material),
  current_instances current_instances: List(transform.Transform),
  current_physics current_physics: option.Option(physics.RigidBody),
  current_material current_material: option.Option(material.Material),
) -> List(Patch) {
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

/// Compare AnimatedSprite fields using accumulator pattern
fn compare_animated_sprite_fields(
  id id: String,
  previous_sprite previous_sprite: spritesheet.Sprite,
  previous_width previous_width: Float,
  previous_height previous_height: Float,
  previous_transform previous_transform: transform.Transform,
  previous_physics previous_physics: option.Option(physics.RigidBody),
  current_sprite current_sprite: spritesheet.Sprite,
  current_width current_width: Float,
  current_height current_height: Float,
  current_transform current_transform: transform.Transform,
  current_physics current_physics: option.Option(physics.RigidBody),
) -> List(Patch) {
  let patches = []

  let patches = case previous_physics != current_physics {
    True -> [UpdatePhysics(id, current_physics), ..patches]
    False -> patches
  }

  case
    previous_sprite != current_sprite
    || previous_width != current_width
    || previous_height != current_height
    || previous_transform != current_transform
  {
    True -> [
      UpdateAnimatedSprite(
        id,
        current_sprite,
        current_width,
        current_height,
        current_transform,
      ),
      ..patches
    ]
    False -> patches
  }
}

@internal
pub type Scene =
  savoiardi.Scene

/// Opaque type for the WebGL renderer
pub type Renderer =
  savoiardi.Renderer

@internal
pub type DomElement =
  savoiardi.Canvas

@internal
pub type RendererState {
  RendererState(
    /// Three.js renderer instance
    renderer: Renderer,
    /// Three.js scene instance
    scene: Scene,
    /// Object cache (Three.js objects, mixers, actions, etc.)
    cache: object_cache.CacheState,
    /// Optional physics world
    physics_world: option.Option(physics.PhysicsWorld),
    /// Audio manager state (Gleam-managed)
    audio_manager: audio_manager.AudioManagerState,
    /// Audio listener (singleton, attached to camera)
    audio_listener: savoiardi.AudioListener,
    /// Cached flattened scene dictionary from previous frame (optimization)
    cached_scene_dict: option.Option(dict.Dict(String, NodeWithParent)),
    /// CSS2D renderer for HTML overlay labels
    css2d_renderer: option.Option(savoiardi.CSS2DRenderer),
  )
}

@internal
pub fn new_render_state(options: savoiardi.RendererOptions) -> RendererState {
  let renderer = savoiardi.create_renderer(options)
  let scene = savoiardi.create_scene()

  // Create audio listener (singleton)
  let audio_listener = savoiardi.create_audio_listener()

  RendererState(
    renderer: renderer,
    scene: scene,
    cache: object_cache.init(),
    physics_world: option.None,
    audio_manager: audio_manager.init(),
    audio_listener: audio_listener,
    cached_scene_dict: option.None,
    css2d_renderer: option.None,
  )
}

/// Create a headless render state for testing (no WebGL required)
///
/// This creates a real Three.js Scene (which works in Node.js) but uses
/// a mock Renderer that doesn't require WebGL. Use this in tests to
/// simulate game state without needing a browser environment.
@internal
pub fn new_headless_render_state(width: Float, height: Float) -> RendererState {
  let scene = create_headless_scene_ffi()
  let renderer = create_headless_renderer_ffi(width, height)
  let audio_listener = create_mock_audio_listener_ffi()

  RendererState(
    renderer: renderer,
    scene: scene,
    cache: object_cache.init(),
    physics_world: option.None,
    audio_manager: audio_manager.init(),
    audio_listener: audio_listener,
    cached_scene_dict: option.None,
    css2d_renderer: option.None,
  )
}

@external(javascript, "./simulate.ffi.mjs", "createHeadlessScene")
fn create_headless_scene_ffi() -> Scene

@external(javascript, "./simulate.ffi.mjs", "createHeadlessRenderer")
fn create_headless_renderer_ffi(width: Float, height: Float) -> Renderer

@external(javascript, "./simulate.ffi.mjs", "createMockAudioListener")
fn create_mock_audio_listener_ffi() -> savoiardi.AudioListener

@internal
pub fn get_renderer(state: RendererState) -> Renderer {
  state.renderer
}

@internal
pub fn get_scene(state: RendererState) -> Scene {
  state.scene
}

@internal
pub fn set_physics_world(
  state: RendererState,
  world: option.Option(physics.PhysicsWorld),
) -> RendererState {
  RendererState(..state, physics_world: world)
}

@internal
pub fn get_physics_world(
  state: RendererState,
) -> option.Option(physics.PhysicsWorld) {
  state.physics_world
}

@internal
pub fn get_cached_scene_dict(
  state: RendererState,
) -> option.Option(dict.Dict(String, NodeWithParent)) {
  state.cached_scene_dict
}

@internal
pub fn set_cached_scene_dict(
  state: RendererState,
  cache: option.Option(dict.Dict(String, NodeWithParent)),
) -> RendererState {
  RendererState(..state, cached_scene_dict: cache)
}

@internal
pub fn resume_audio_context(state: RendererState) -> RendererState {
  let new_audio_manager =
    audio_manager.resume_audio_context(
      state.audio_manager,
      state.audio_listener,
    )
  RendererState(..state, audio_manager: new_audio_manager)
}

/// Initialize CSS2D renderer and append to container
/// Must be called after the main canvas is appended to the container
@internal
pub fn init_css2d_renderer(state: RendererState, container: a) -> RendererState {
  let css2d_renderer = savoiardi.create_css2d_renderer()

  // Get initial size from the WebGL renderer
  let vec2.Vec2(width, height) = savoiardi.get_canvas_dimensions(state.renderer)
  savoiardi.set_css2d_renderer_size(
    css2d_renderer,
    float.round(width),
    float.round(height),
  )

  // Append CSS2D renderer element to container
  let css2d_element = savoiardi.get_css2d_renderer_dom_element(css2d_renderer)
  append_element_to_container(container, css2d_element)

  RendererState(..state, css2d_renderer: option.Some(css2d_renderer))
}

@external(javascript, "../tiramisu.ffi.mjs", "appendElementToContainer")
fn append_element_to_container(container: a, element: b) -> Nil

/// Render CSS2D labels (call after main render)
@internal
pub fn render_css2d(state: RendererState, camera: savoiardi.Camera) -> Nil {
  case state.css2d_renderer {
    option.Some(css2d_renderer) ->
      savoiardi.render_css2d(css2d_renderer, state.scene, camera)
    option.None -> Nil
  }
}

/// Update CSS2D renderer size (call on window resize)
@internal
pub fn update_css2d_renderer_size(state: RendererState) -> Nil {
  case state.css2d_renderer {
    option.Some(css2d_renderer) -> {
      let vec2.Vec2(width, height) =
        savoiardi.get_canvas_dimensions(state.renderer)
      savoiardi.set_css2d_renderer_size(
        css2d_renderer,
        float.round(width),
        float.round(height),
      )
    }
    option.None -> Nil
  }
}

@internal
pub fn apply_patches(
  state: RendererState,
  patches: List(Patch),
) -> RendererState {
  list.fold(patches, state, fn(st, patch) { apply_patch(st, patch) })
}

@internal
pub fn apply_patch(state: RendererState, patch: Patch) -> RendererState {
  case patch {
    AddNode(id: id_val, node: node, parent_id: parent_id) ->
      handle_add_node(state, id_val, node, parent_id)

    RemoveNode(id: id_val) -> handle_remove_node(state, id_val)

    UpdateTransform(id: id_val, transform: transform) ->
      handle_update_transform(state, id_val, transform)

    UpdateMaterial(id: id_val, material: material) ->
      handle_update_material(state, id_val, material)

    UpdateGeometry(id: id_val, geometry: geometry) ->
      handle_update_geometry(state, id_val, geometry)

    UpdateLight(id: id_val, light: light) ->
      handle_update_light(state, id_val, light)

    UpdateAnimation(id: id_val, animation: animation) ->
      handle_update_animation(state, id_val, animation)

    UpdatePhysics(id: id_val, physics: physics) ->
      handle_update_physics(state, id_val, physics)

    UpdateAudio(id: id_val, audio: audio) ->
      handle_update_audio(state, id_val, audio)

    UpdateInstances(id: id_val, instances: instances) ->
      handle_update_instances(state, id_val, instances)

    UpdateLODLevels(id: id_val, levels: levels) ->
      handle_update_lod_levels(state, id_val, levels)

    UpdateCamera(id: id_val, camera_type: camera_type) ->
      handle_update_camera(state, id_val, camera_type)

    SetActiveCamera(id: id_val) -> handle_set_active_camera(state, id_val)

    UpdateCameraPostprocessing(id: id_val, postprocessing: pp) ->
      handle_update_camera_postprocessing(state, id_val, pp)

    UpdateCSS2DLabel(id: id_val, html: html, transform: trans) ->
      handle_update_css2d(state, id_val, html, trans)

    UpdateCSS3DLabel(id: id_val, html: html, transform: trans) ->
      handle_update_css3d(state, id_val, html, trans)

    UpdateCanvas(
      id: id_val,
      encoded_picture: encoded_picture,
      texture_width: tw,
      texture_height: th,
      width: w,
      height: h,
      transform: trans,
    ) ->
      handle_update_canvas(state, id_val, encoded_picture, tw, th, w, h, trans)

    UpdateAnimatedSprite(
      id: id_val,
      sprite: spr,
      width: w,
      height: h,
      transform: trans,
    ) -> handle_update_animated_sprite(state, id_val, spr, w, h, trans)
  }
}

// ============================================================================
// PATCH HANDLERS - ADD NODE
// ============================================================================

fn handle_add_node(
  state: RendererState,
  id: String,
  node: Node,
  parent_id: option.Option(String),
) -> RendererState {
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
      transparent: transparent,
    ) ->
      handle_add_model3d(
        state,
        id,
        object,
        transform,
        animation,
        physics,
        material,
        transparent,
        parent_id,
      )

    InstancedModel(
      id: _,
      children: _,
      object: object,
      instances: instances,
      physics: physics,
      material: material,
      transparent: transparent,
    ) ->
      handle_add_instanced_model(
        state,
        id,
        object,
        instances,
        physics,
        material,
        transparent,
        parent_id,
      )

    Audio(id: _, children: _, audio: audio) ->
      handle_add_audio(state, id, audio, parent_id)

    Camera(
      id: _,
      children: _,
      camera: camera,
      transform: transform,
      active: active,
      viewport: viewport,
      postprocessing: postprocessing,
    ) ->
      handle_add_camera(
        state,
        id,
        camera,
        transform,
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

    CSS2D(id: _, children: _, html: html, transform: trans) ->
      handle_add_css2d(state, id, html, trans, parent_id)

    CSS3D(id: _, children: _, html: html, transform: trans) ->
      handle_add_css3d(state, id, html, trans, parent_id)

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
      sprite: spr,
      width: w,
      height: h,
      transform: trans,
      physics: physics,
    ) ->
      handle_add_animated_sprite(
        state,
        id,
        spr,
        w,
        h,
        trans,
        physics,
        parent_id,
      )
  }
}

// Helper: Add object to scene or parent
fn add_to_scene_or_parent(
  state: RendererState,
  object: model.Object3D,
  parent_id: option.Option(String),
) -> Nil {
  case parent_id {
    option.Some(pid) -> {
      case object_cache.get_object(state.cache, pid) {
        Ok(parent_obj) -> savoiardi.add_child(parent: parent_obj, child: object)
        Error(Nil) -> savoiardi.add_to_scene(scene: state.scene, object: object)
      }
    }
    option.None -> savoiardi.add_to_scene(scene: state.scene, object: object)
  }
}

fn handle_add_mesh(
  state: RendererState,
  id: String,
  geometry: geometry.Geometry,
  material: material.Material,
  transform: transform.Transform,
  physics: option.Option(physics.RigidBody),
  parent_id: option.Option(String),
) -> RendererState {
  let geometry_three = geometry.create_geometry(geometry)
  let material_three = material.create_material(material)
  let mesh = savoiardi.create_mesh(geometry_three, material_three)

  savoiardi.apply_transform_with_quaternion(
    object: mesh,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )
  savoiardi.set_shadow_properties(
    object: mesh,
    cast_shadow: True,
    receive_shadow: True,
  )

  add_to_scene_or_parent(state, mesh, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, mesh)

  // Create physics body if specified - use world transform (includes parent transforms)
  let new_state = RendererState(..state, cache: new_cache)
  case physics, new_state.physics_world {
    option.Some(physics_config), option.Some(world) -> {
      // Get world position and rotation from the Three.js object (includes parent transforms)
      let world_position = savoiardi.get_world_position(mesh)
      let world_rotation = savoiardi.get_world_quaternion(mesh)
      let world_transform =
        transform.at(world_position)
        |> transform.with_quaternion_rotation(world_rotation)
      let new_world =
        physics.create_body(world, id, physics_config, world_transform)
      RendererState(..new_state, physics_world: option.Some(new_world))
    }
    _, _ -> new_state
  }
}

fn handle_add_instanced_mesh(
  state: RendererState,
  id: String,
  geometry: geometry.Geometry,
  material: material.Material,
  instances: List(transform.Transform),
  parent_id: option.Option(String),
) -> RendererState {
  let geometry_three = geometry.create_geometry(geometry)
  let material_three = material.create_material(material)
  let count = list.length(instances)
  let mesh =
    savoiardi.create_instanced_mesh(geometry_three, material_three, count)

  savoiardi.update_instanced_mesh_transforms(
    mesh,
    transforms_to_tuples(instances),
  )

  add_to_scene_or_parent(state, mesh |> coerce, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, mesh |> coerce)
  RendererState(..state, cache: new_cache)
}

fn handle_add_instanced_model(
  state: RendererState,
  id: String,
  object: model.Object3D,
  instances: List(transform.Transform),
  physics: option.Option(physics.RigidBody),
  material: option.Option(material.Material),
  transparent: Bool,
  parent_id: option.Option(String),
) -> RendererState {
  // Extract all mesh/material pairs from the loaded model
  let #(geometries, materials) = savoiardi.extract_mesh_material_pairs(object)
  let materials = case material {
    option.Some(mat) -> {
      // Create Material for each geometry if material override is provided
      let three_mat = material.create_material(mat)
      list.repeat(three_mat, array.size(geometries))
    }
    option.None -> array.to_list(materials)
  }

  // Create a group to hold all the instanced meshes
  let group = savoiardi.create_group()

  // For each unique mesh/material combination, create an InstancedMesh
  let count = list.length(instances)

  // Zip geometries and materials together and iterate
  list.zip(array.to_list(geometries), materials)
  |> list.each(fn(pair) {
    let #(geometry, material) = pair
    let instanced_mesh =
      savoiardi.create_instanced_mesh(geometry, material, count)

    // Update all instance transforms
    savoiardi.update_instanced_mesh_transforms(
      instanced_mesh,
      transforms_to_tuples(instances),
    )

    // Add the instanced mesh to the group
    savoiardi.add_child(parent: group, child: coerce(instanced_mesh))
  })

  // Enable transparency if requested
  case transparent {
    True -> savoiardi.enable_transparency(group)
    False -> Nil
  }

  // Enable shadows on all meshes (cast and receive)
  savoiardi.enable_shadows(group, True, True)

  // Add the group to the scene or parent
  add_to_scene_or_parent(state, group, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, group)
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
fn generate_instance_id(base_id: String, index: Int) -> String {
  base_id <> "[" <> int.to_string(index) <> "]"
}

fn handle_add_light(
  state: RendererState,
  id: String,
  light_config: light.Light,
  transform: transform.Transform,
  parent_id: option.Option(String),
) -> RendererState {
  let light = light.create_light(light_config)
  savoiardi.apply_transform_with_quaternion(
    object: light |> coerce,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )

  add_to_scene_or_parent(state, light |> coerce, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, light |> coerce)
  RendererState(..state, cache: new_cache)
}

fn handle_add_group(
  state: RendererState,
  id: String,
  transform: transform.Transform,
  parent_id: option.Option(String),
) -> RendererState {
  let group = savoiardi.create_group()
  savoiardi.apply_transform_with_quaternion(
    object: group,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )

  add_to_scene_or_parent(state, group, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, group)
  RendererState(..state, cache: new_cache)
}

fn handle_add_lod(
  state: RendererState,
  id: String,
  transform: transform.Transform,
  levels: List(LODLevel),
  parent_id: option.Option(String),
) -> RendererState {
  let lod = savoiardi.create_lod()
  savoiardi.apply_transform_with_quaternion(
    object: lod |> coerce,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )

  // Add LOD levels
  list.each(levels, fn(level) {
    let level_obj = create_lod_level_object(level.node)
    savoiardi.add_lod_level(
      lod: lod,
      object: level_obj,
      distance: level.distance,
    )
  })

  add_to_scene_or_parent(state, lod |> coerce, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, lod |> coerce)
  RendererState(..state, cache: new_cache)
}

// Helper: Create Three.js object for LOD level
fn create_lod_level_object(node: Node) -> model.Object3D {
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
      let mesh = savoiardi.create_mesh(geometry_three, material_three)
      savoiardi.apply_transform_with_quaternion(
        object: mesh,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )
      mesh
    }
    Empty(id: _, children: _, transform: transform) -> {
      let group = savoiardi.create_group()
      savoiardi.apply_transform_with_quaternion(
        object: group,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )
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
      transparent: transparent,
    ) -> {
      let cloned = savoiardi.clone_object(object)
      savoiardi.apply_transform_with_quaternion(
        object: cloned,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )
      // Apply material if provided
      case material {
        option.Some(material) ->
          savoiardi.apply_material_to_object(
            cloned,
            material.create_material(material),
          )
        option.None -> Nil
      }
      // Enable transparency if requested
      case transparent {
        True -> savoiardi.enable_transparency(cloned)
        False -> Nil
      }
      cloned
    }
    _ -> {
      // Unsupported node type for LOD level, create empty group
      savoiardi.create_group()
    }
  }
}

fn handle_add_model3d(
  state: RendererState,
  id: String,
  object: model.Object3D,
  transform: transform.Transform,
  animation: option.Option(model.AnimationPlayback),
  physics: option.Option(physics.RigidBody),
  material: option.Option(material.Material),
  transparent: Bool,
  parent_id: option.Option(String),
) -> RendererState {
  // For models with animations, we cannot clone because the animation clips
  // reference the original skeleton. Cloning would break the bone references.
  // For models without animations, we can clone to avoid modifying shared assets.
  let model_object = case animation {
    option.Some(_) -> object
    option.None -> savoiardi.clone_object(object)
  }

  savoiardi.apply_transform_with_quaternion(
    object: model_object,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )

  // Apply material if provided
  case material {
    option.Some(mat) ->
      savoiardi.apply_material_to_object(
        model_object,
        material.create_material(mat),
      )
    option.None -> Nil
  }

  // Enable transparency if requested
  case transparent {
    True -> savoiardi.enable_transparency(model_object)
    False -> Nil
  }

  // Enable shadows on all meshes (cast and receive)
  savoiardi.enable_shadows(model_object, True, True)

  // Create animation mixer
  let mixer = savoiardi.create_animation_mixer(model_object)
  let cache_with_mixer = object_cache.add_mixer(state.cache, id, mixer)

  // Setup animation if provided
  let cache_with_animation = case animation {
    option.Some(anim_playback) ->
      setup_animation(cache_with_mixer, id, mixer, anim_playback)
    option.None -> cache_with_mixer
  }

  add_to_scene_or_parent(state, model_object, parent_id)

  let new_cache =
    object_cache.add_object(cache_with_animation, id, model_object)

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
  state: RendererState,
  id: String,
  audio: audio.Audio,
  parent_id: option.Option(String),
) -> RendererState {
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
  let #(new_audio_manager, _source_data) =
    audio_manager.create_audio_source(
      state.audio_manager,
      id,
      buffer,
      config,
      audio,
      state.audio_listener,
    )

  // Create placeholder group to track in cache
  let placeholder = savoiardi.create_group()
  add_to_scene_or_parent(state, placeholder, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, placeholder)
  RendererState(..state, cache: new_cache, audio_manager: new_audio_manager)
}

// Helper: Calculate aspect ratio for camera
// Uses viewport dimensions if specified, otherwise canvas or window dimensions
fn calculate_aspect_ratio(
  viewport: option.Option(#(Int, Int, Int, Int)),
  renderer: Renderer,
) -> Float {
  case viewport {
    option.Some(#(_x, _y, width, height)) -> {
      int.to_float(width) /. int.to_float(height)
    }
    option.None -> {
      // Try canvas first
      let vec2.Vec2(canvas_width, canvas_height) =
        savoiardi.get_canvas_dimensions(renderer)

      case canvas_width >. 0.0 && canvas_height >. 0.0 {
        True -> canvas_width /. canvas_height
        False -> {
          // Fallback to window dimensions
          let window_width = window.outer_width(window.self())
          let window_height = window.outer_height(window.self())
          int.to_float(window_width) /. int.to_float(window_height)
        }
      }
    }
  }
}

fn handle_add_camera(
  state: RendererState,
  id: String,
  camera_type: camera.Camera,
  transform: transform.Transform,
  active: Bool,
  viewport: option.Option(#(Int, Int, Int, Int)),
  postprocessing: option.Option(camera.PostProcessing),
  parent_id: option.Option(String),
) -> RendererState {
  let aspect = calculate_aspect_ratio(viewport, state.renderer)

  // Get projection and create camera based on type
  let projection = camera.get_projection(camera_type)
  let camera = case projection {
    camera.Perspective(fov: fov, aspect: _, near: near, far: far) ->
      savoiardi.create_perspective_camera(fov, aspect, near, far)
    camera.Orthographic(
      left: left,
      right: right,
      top: top,
      bottom: bottom,
      near: near,
      far: far,
    ) ->
      savoiardi.create_orthographic_camera(left, right, top, bottom, near, far)
  }

  // Add audio listener to camera
  savoiardi.add_child(
    parent: camera |> coerce,
    child: state.audio_listener |> coerce,
  )

  savoiardi.apply_transform_with_quaternion(
    object: camera |> coerce,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )

  savoiardi.update_camera_projection_matrix(camera)

  case parent_id {
    option.None ->
      savoiardi.add_to_scene(scene: state.scene, object: camera |> coerce)
    option.Some(parent_id) ->
      case object_cache.get_object(state.cache, parent_id) {
        Error(Nil) ->
          savoiardi.add_to_scene(scene: state.scene, object: camera |> coerce)
        Ok(parent_obj) ->
          savoiardi.add_child(parent: parent_obj, child: camera |> coerce)
      }
  }

  // Store viewport if specified
  let cache_with_viewport = case viewport {
    option.Some(#(x, y, width, height)) -> {
      let vp =
        camera.ViewPort(
          position: vec2.Vec2(x, y),
          size: vec2.Vec2(width, height),
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
  let cache_with_active = case active {
    True -> object_cache.set_active_camera(cache_with_camera, id)
    False -> cache_with_camera
  }

  let new_cache =
    object_cache.add_object(cache_with_active, id, camera |> coerce)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_box(
  state: RendererState,
  id: String,
  min: vec3.Vec3(Float),
  max: vec3.Vec3(Float),
  color: Int,
  parent_id: option.Option(String),
) -> RendererState {
  let debug = create_debug_box(min, max, color)
  add_to_scene_or_parent(state, debug, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, debug)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_sphere(
  state: RendererState,
  id: String,
  center: vec3.Vec3(Float),
  radius: Float,
  color: Int,
  parent_id: option.Option(String),
) -> RendererState {
  let debug = create_debug_sphere(center, radius, color)
  add_to_scene_or_parent(state, debug, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, debug)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_line(
  state: RendererState,
  id: String,
  from: vec3.Vec3(Float),
  to: vec3.Vec3(Float),
  color: Int,
  parent_id: option.Option(String),
) -> RendererState {
  let debug = create_debug_line(from, to, color)
  add_to_scene_or_parent(state, debug, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, debug)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_axes(
  state: RendererState,
  id: String,
  origin: vec3.Vec3(Float),
  size: Float,
  parent_id: option.Option(String),
) -> RendererState {
  let debug = create_debug_axes(origin, size)
  add_to_scene_or_parent(state, debug, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, debug)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_grid(
  state: RendererState,
  id: String,
  size: Float,
  divisions: Int,
  color: Int,
  parent_id: option.Option(String),
) -> RendererState {
  let debug = create_debug_grid(size, divisions, color)
  add_to_scene_or_parent(state, debug, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, debug)
  RendererState(..state, cache: new_cache)
}

fn handle_add_debug_point(
  state: RendererState,
  id: String,
  position: vec3.Vec3(Float),
  size: Float,
  color: Int,
  parent_id: option.Option(String),
) -> RendererState {
  let debug = create_debug_point(position, size, color)
  add_to_scene_or_parent(state, debug, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, debug)
  RendererState(..state, cache: new_cache)
}

// ============================================================================
// PATCH HANDLERS - REMOVE NODE
// ============================================================================

fn handle_remove_node(state: RendererState, id: String) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(obj) -> {
      // Remove from scene
      savoiardi.remove_from_scene(scene: state.scene, object: obj)

      // Dispose object recursively (geometry, materials, textures, children)
      savoiardi.dispose_object(obj)

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
    Error(Nil) -> state
  }
}

// ============================================================================
// PATCH HANDLERS - UPDATE OPERATIONS
// ============================================================================

fn handle_update_transform(
  state: RendererState,
  id: String,
  transform: transform.Transform,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      savoiardi.apply_transform_with_quaternion(
        object: object,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )
      savoiardi.update_matrix_world_force(object, True)

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
    Error(Nil) -> state
  }
}

fn handle_update_material(
  state: RendererState,
  id: String,
  material: option.Option(material.Material),
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      case material {
        option.Some(mat) -> {
          let old_material = savoiardi.get_object_material(object)
          savoiardi.dispose_material(old_material)

          let new_material = material.create_material(mat)
          savoiardi.set_object_material(object, new_material)
        }
        option.None -> Nil
      }

      state
    }
    Error(Nil) -> state
  }
}

fn handle_update_geometry(
  state: RendererState,
  id: String,
  geometry: geometry.Geometry,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      let old_geometry = savoiardi.get_object_geometry(object)
      savoiardi.dispose_geometry(old_geometry)

      let new_geometry = geometry.create_geometry(geometry)
      savoiardi.set_object_geometry(object, new_geometry)

      state
    }
    Error(Nil) -> state
  }
}

fn handle_update_light(
  state: RendererState,
  id: String,
  light: light.Light,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(old_object) -> {
      // Get old transform
      let position = savoiardi.get_object_position(old_object)
      let rotation = savoiardi.get_object_rotation(old_object)
      let scale = savoiardi.get_object_scale(old_object)

      // Create new light
      let new_light = light.create_light(light)
      let new_light_obj: model.Object3D = coerce(new_light)

      // Copy transform
      savoiardi.set_object_position(new_light_obj, position)
      savoiardi.set_object_rotation(new_light_obj, rotation)
      savoiardi.set_object_scale(new_light_obj, scale)

      // Replace in scene
      savoiardi.remove_from_scene(scene: state.scene, object: old_object)
      savoiardi.add_to_scene(scene: state.scene, object: new_light |> coerce)

      // Update cache
      let new_cache =
        object_cache.add_object(state.cache, id, new_light |> coerce)
      RendererState(..state, cache: new_cache)
    }
    Error(Nil) -> state
  }
}

fn handle_update_animation(
  state: RendererState,
  id: String,
  animation: option.Option(model.AnimationPlayback),
) -> RendererState {
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
  state: RendererState,
  id: String,
  new_physics: option.Option(physics.RigidBody),
) -> RendererState {
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
            Ok(object) -> {
              let object_transform = object_to_transform(object)

              // Just update the transform (preserves velocity!)
              let new_world =
                physics.update_body_transform(world, id, object_transform)

              RendererState(..state, physics_world: option.Some(new_world))
            }
            Error(Nil) -> state
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
            Ok(object) -> {
              let object_transform = object_to_transform(object)

              let new_world =
                physics.create_body(world, id, physics_config, object_transform)

              RendererState(..state, physics_world: option.Some(new_world))
            }
            Error(Nil) -> state
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
fn object_to_transform(object: model.Object3D) -> transform.Transform {
  let position = savoiardi.get_object_position(object)
  let quaternion = savoiardi.get_object_quaternion(object)
  let scale = savoiardi.get_object_scale(object)

  transform.identity
  |> transform.with_position(position)
  |> transform.with_quaternion_rotation(quaternion)
  |> transform.with_scale(scale)
}

fn handle_update_audio(
  state: RendererState,
  id: String,
  audio: audio.Audio,
) -> RendererState {
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
    audio_manager.update_audio_config(
      state.audio_manager,
      id,
      buffer,
      config,
      state.audio_listener,
    )

  RendererState(..state, audio_manager: new_audio_manager)
}

fn handle_update_instances(
  state: RendererState,
  id: String,
  instances: List(transform.Transform),
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      // Single InstancedMesh node
      savoiardi.update_instanced_mesh_transforms(
        coerce(object),
        transforms_to_tuples(instances),
      )
      state
    }
    Error(Nil) -> state
  }
}

fn handle_update_lod_levels(
  state: RendererState,
  id: String,
  levels: List(LODLevel),
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      // Clear existing levels
      savoiardi.clear_lod_levels(object |> coerce)

      // Add new levels
      list.each(levels, fn(level) {
        let level_obj = create_lod_level_object(level.node)
        savoiardi.add_lod_level(
          lod: object |> coerce,
          object: level_obj,
          distance: level.distance,
        )
      })

      state
    }
    Error(Nil) -> state
  }
}

fn handle_update_camera(
  state: RendererState,
  id: String,
  camera_type: camera.Camera,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      // Update camera projection parameters from camera_type
      let projection = camera.get_projection(camera_type)
      case projection {
        camera.Perspective(fov:, aspect: _, near:, far:) -> {
          // Calculate the correct aspect ratio instead of using placeholder
          let viewport = object_cache.get_viewport(state.cache, id)
          // Convert Viewport to tuple format for calculate_aspect_ratio
          let viewport_tuple = case viewport {
            option.Some(camera.ViewPort(position:, size:)) ->
              option.Some(#(position.x, position.y, size.x, size.y))
            option.None -> option.None
          }
          let calculated_aspect =
            calculate_aspect_ratio(viewport_tuple, state.renderer)

          savoiardi.set_perspective_camera_params(
            object |> coerce,
            fov,
            calculated_aspect,
            near,
            far,
          )
        }
        camera.Orthographic(left:, right:, top:, bottom:, near:, far:) -> {
          savoiardi.set_orthographic_camera_params(
            object |> coerce,
            left,
            right,
            top,
            bottom,
            near,
            far,
          )
        }
      }
      savoiardi.update_camera_projection_matrix(object |> coerce)

      state
    }
    Error(Nil) -> state
  }
}

fn handle_set_active_camera(state: RendererState, id: String) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(_object) -> {
      // Update the cache to track which camera is active
      let new_cache = object_cache.set_active_camera(state.cache, id)
      RendererState(..state, cache: new_cache)
    }
    Error(Nil) -> state
  }
}

fn handle_update_camera_postprocessing(
  state: RendererState,
  id: String,
  pp: option.Option(camera.PostProcessing),
) -> RendererState {
  // Update the postprocessing config in the cache
  let new_cache = case pp {
    option.Some(pp_config) ->
      object_cache.set_camera_postprocessing(state.cache, id, pp_config)
    option.None -> object_cache.remove_camera_postprocessing(state.cache, id)
  }

  RendererState(..state, cache: new_cache)
}

fn handle_add_css2d(
  state: RendererState,
  id: String,
  html: String,
  transform: transform.Transform,
  parent_id: option.Option(String),
) -> RendererState {
  let css2d_object = savoiardi.create_css2d_object(html)
  savoiardi.apply_transform_with_quaternion(
    object: css2d_object |> coerce,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )
  add_to_scene_or_parent(state, css2d_object |> coerce, parent_id)

  let new_cache =
    object_cache.add_object(state.cache, id, css2d_object |> coerce)
  RendererState(..state, cache: new_cache)
}

fn handle_update_css2d(
  state: RendererState,
  id: String,
  html: String,
  transform: transform.Transform,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      // TODO:  I don't like this, this should be typesafe
      savoiardi.update_css2d_object_html(object |> coerce, html)
      savoiardi.apply_transform_with_quaternion(
        object: object,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )
      state
    }
    Error(Nil) -> state
  }
}

fn handle_add_css3d(
  state: RendererState,
  id: String,
  html: String,
  transform: transform.Transform,
  parent_id: option.Option(String),
) -> RendererState {
  let css3d_object = savoiardi.create_css3d_object(html)
  savoiardi.apply_transform_with_quaternion(
    object: css3d_object |> coerce,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )
  add_to_scene_or_parent(state, css3d_object |> coerce, parent_id)

  let new_cache =
    object_cache.add_object(state.cache, id, css3d_object |> coerce)
  RendererState(..state, cache: new_cache)
}

fn handle_update_css3d(
  state: RendererState,
  id: String,
  html: String,
  transform: transform.Transform,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      savoiardi.update_css3d_object_html(object |> coerce, html)
      savoiardi.apply_transform_with_quaternion(
        object: object,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )
      state
    }
    Error(Nil) -> state
  }
}

fn handle_add_canvas(
  state: RendererState,
  id: String,
  encoded_picture: String,
  texture_width: Int,
  texture_height: Int,
  width: Float,
  height: Float,
  transform: transform.Transform,
  parent_id: option.Option(String),
) -> RendererState {
  // Picture is already encoded, use it directly
  let texture =
    create_canvas_texture_from_picture(
      encoded_picture,
      texture_width,
      texture_height,
    )

  // Create plane mesh with the texture
  let canvas_mesh = create_canvas_plane(texture, width, height)

  // Apply transform
  savoiardi.apply_transform_with_quaternion(
    object: canvas_mesh,
    position: transform.position(transform),
    quaternion: transform.rotation_quaternion(transform),
    scale: transform.scale(transform),
  )

  // Cache the encoded picture to avoid recreating texture on first update
  set_canvas_cached_picture(canvas_mesh, encoded_picture)

  add_to_scene_or_parent(state, canvas_mesh, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, canvas_mesh)
  RendererState(..state, cache: new_cache)
}

fn handle_update_canvas(
  state: RendererState,
  id: String,
  encoded_picture: String,
  texture_width: Int,
  texture_height: Int,
  width: Float,
  height: Float,
  transform: transform.Transform,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(object) -> {
      // Performance optimization: Only create new texture if picture changed
      // Check if encoded_picture differs from cached value
      let cached_picture = get_canvas_cached_picture(object)
      let picture_changed = cached_picture != encoded_picture

      case picture_changed {
        True -> {
          // Picture changed - create new texture and cache the picture data
          let texture =
            create_canvas_texture_from_picture(
              encoded_picture,
              texture_width,
              texture_height,
            )
          update_canvas_texture(object, texture)
          set_canvas_cached_picture(object, encoded_picture)
        }
        False -> {
          // Picture unchanged - skip expensive texture creation
          Nil
        }
      }

      // Always update size and transform (cheap operations)
      update_canvas_size(object, width, height)
      savoiardi.apply_transform_with_quaternion(
        object: object,
        position: transform.position(transform),
        quaternion: transform.rotation_quaternion(transform),
        scale: transform.scale(transform),
      )

      state
    }
    Error(Nil) -> state
  }
}

fn handle_add_animated_sprite(
  state: RendererState,
  id: String,
  sprite: spritesheet.Sprite,
  width: Float,
  height: Float,
  trans: transform.Transform,
  physics: option.Option(physics.RigidBody),
  parent_id: option.Option(String),
) -> RendererState {
  // Get the texture and clone it for independent animation
  let base_texture = spritesheet.sprite_texture(sprite)
  let sprite_texture = texture.clone(base_texture)

  // Setup texture for spritesheet animation
  let #(repeat_x, repeat_y) = spritesheet.sprite_frame_repeat(sprite)
  sprite_texture
  |> texture.set_repeat(repeat: vec2.Vec2(repeat_x, repeat_y))
  |> texture.set_wrap_mode(texture.RepeatWrapping, texture.RepeatWrapping)

  // Apply pixel art filtering if requested
  case spritesheet.sprite_pixel_art(sprite) {
    True ->
      sprite_texture
      |> texture.set_filter_mode(texture.NearestFilter, texture.NearestFilter)
    False -> sprite_texture
  }

  // Apply frame offset
  let #(offset_x, offset_y) = spritesheet.sprite_frame_offset(sprite)
  sprite_texture
  |> texture.set_offset(offset: vec2.Vec2(offset_x, offset_y))

  // Create plane mesh with the animated texture
  let sprite_mesh = create_canvas_plane(sprite_texture, width, height)

  // Apply transform
  savoiardi.apply_transform_with_quaternion(
    object: sprite_mesh,
    position: transform.position(trans),
    quaternion: transform.rotation_quaternion(trans),
    scale: transform.scale(trans),
  )

  add_to_scene_or_parent(state, sprite_mesh, parent_id)

  let new_cache = object_cache.add_object(state.cache, id, sprite_mesh)

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
  state: RendererState,
  id: String,
  sprite: spritesheet.Sprite,
  width: Float,
  height: Float,
  trans: transform.Transform,
) -> RendererState {
  case object_cache.get_object(state.cache, id) {
    Ok(obj) -> {
      // Get the texture and clone it for independent animation
      let base_texture = spritesheet.sprite_texture(sprite)
      let sprite_texture = texture.clone(base_texture)

      // Setup texture for spritesheet animation
      let #(repeat_x, repeat_y) = spritesheet.sprite_frame_repeat(sprite)
      sprite_texture
      |> texture.set_repeat(repeat: vec2.Vec2(repeat_x, repeat_y))
      |> texture.set_wrap_mode(texture.RepeatWrapping, texture.RepeatWrapping)

      // Apply pixel art filtering if requested
      case spritesheet.sprite_pixel_art(sprite) {
        True ->
          sprite_texture
          |> texture.set_filter_mode(
            texture.NearestFilter,
            texture.NearestFilter,
          )
        False -> sprite_texture
      }

      // Apply frame offset
      let #(offset_x, offset_y) = spritesheet.sprite_frame_offset(sprite)
      sprite_texture
      |> texture.set_offset(offset: vec2.Vec2(offset_x, offset_y))

      // Update the mesh texture
      update_canvas_texture(obj, sprite_texture)

      // Update size
      update_canvas_size(obj, width, height)

      // Update transform
      savoiardi.apply_transform_with_quaternion(
        object: obj,
        position: transform.position(trans),
        quaternion: transform.rotation_quaternion(trans),
        scale: transform.scale(trans),
      )

      state
    }
    Error(Nil) -> state
  }
}

fn setup_animation(
  cache: object_cache.CacheState,
  id: String,
  mixer: object_cache.AnimationMixer,
  playback: model.AnimationPlayback,
) -> object_cache.CacheState {
  // Get current animation state to compare clip names
  let current_state = object_cache.get_animation_state(cache, id)

  case playback {
    model.SingleAnimation(anim) -> {
      let new_clip_name = model.clip_name(anim.clip)
      let new_state = object_cache.SingleState(new_clip_name)

      // Check if we can just update existing action
      case current_state, object_cache.get_actions(cache, id) {
        option.Some(object_cache.SingleState(current_clip_name)),
          option.Some(object_cache.SingleAction(existing_action))
          if current_clip_name == new_clip_name
        -> {
          // Same clip - just update weight/speed on existing action
          set_animation_weight_ffi(existing_action, anim.weight)
          set_animation_time_scale_ffi(existing_action, anim.speed)
          // Update state but keep same actions
          object_cache.set_animation_state(cache, id, new_state)
        }
        _, _ -> {
          // Different clip or no existing action - recreate
          case object_cache.get_actions(cache, id) {
            option.Some(actions) -> stop_actions(actions)
            option.None -> Nil
          }
          let action = create_animation_action(mixer, anim)
          let actions = object_cache.SingleAction(action)
          cache
          |> object_cache.set_actions(id, actions)
          |> object_cache.set_animation_state(id, new_state)
        }
      }
    }

    model.BlendedAnimations(
      from: from_anim,
      to: to_anim,
      blend_factor: blend_factor,
    ) -> {
      let from_clip_name = model.clip_name(from_anim.clip)
      let to_clip_name = model.clip_name(to_anim.clip)
      let new_state = object_cache.BlendedState(from_clip_name, to_clip_name)

      // Check if we can just update weights on existing actions
      case current_state, object_cache.get_actions(cache, id) {
        option.Some(object_cache.BlendedState(current_from, current_to)),
          option.Some(object_cache.BlendedActions(
            from: existing_from_action,
            to: existing_to_action,
          ))
          if current_from == from_clip_name && current_to == to_clip_name
        -> {
          // Same clips - just update weights on existing actions (preserves playback time!)
          set_animation_weight_ffi(
            existing_from_action,
            { 1.0 -. blend_factor } *. from_anim.weight,
          )
          set_animation_weight_ffi(
            existing_to_action,
            blend_factor *. to_anim.weight,
          )
          // Update state but keep same actions
          object_cache.set_animation_state(cache, id, new_state)
        }
        _, _ -> {
          // Different clips or no existing actions - recreate
          case object_cache.get_actions(cache, id) {
            option.Some(actions) -> stop_actions(actions)
            option.None -> Nil
          }
          let from_action = create_animation_action(mixer, from_anim)
          let to_action = create_animation_action(mixer, to_anim)

          // Adjust weights based on blend factor
          set_animation_weight_ffi(
            from_action,
            { 1.0 -. blend_factor } *. from_anim.weight,
          )
          set_animation_weight_ffi(to_action, blend_factor *. to_anim.weight)

          let actions =
            object_cache.BlendedActions(from: from_action, to: to_action)
          cache
          |> object_cache.set_actions(id, actions)
          |> object_cache.set_animation_state(id, new_state)
        }
      }
    }
  }
}

fn create_animation_action(
  mixer: object_cache.AnimationMixer,
  animation: model.Animation,
) -> object_cache.AnimationAction {
  let three_animation_action =
    create_animation_action_ffi(mixer, animation.clip)

  // Configure action
  let loop_mode = case animation.loop {
    model.LoopRepeat -> savoiardi.LoopRepeat
    model.LoopOnce -> savoiardi.LoopOnce
  }
  savoiardi.set_action_loop(three_animation_action, loop_mode)
  set_animation_time_scale_ffi(three_animation_action, animation.speed)
  set_animation_weight_ffi(three_animation_action, animation.weight)
  play_animation_action_ffi(three_animation_action)

  three_animation_action
}

fn stop_actions(actions: object_cache.AnimationActions) -> Nil {
  case actions {
    object_cache.SingleAction(action) -> {
      stop_animation_action_ffi(action)
    }
    object_cache.BlendedActions(from: from_action, to: to_action) -> {
      stop_animation_action_ffi(from_action)
      stop_animation_action_ffi(to_action)
    }
  }
}

// Animation wrapper functions
fn create_animation_action_ffi(
  mixer: object_cache.AnimationMixer,
  clip: savoiardi.AnimationClip,
) -> object_cache.AnimationAction {
  savoiardi.clip_action(mixer, clip)
}

fn set_animation_time_scale_ffi(
  action: object_cache.AnimationAction,
  time_scale: Float,
) -> Nil {
  savoiardi.set_action_time_scale(action, time_scale)
}

fn set_animation_weight_ffi(
  action: object_cache.AnimationAction,
  weight: Float,
) -> Nil {
  savoiardi.set_action_weight(action, weight)
}

fn play_animation_action_ffi(action: object_cache.AnimationAction) -> Nil {
  savoiardi.play_action(action)
}

fn stop_animation_action_ffi(action: object_cache.AnimationAction) -> Nil {
  savoiardi.stop_action(action)
}

@internal
pub fn update_mixers(state: RendererState, delta_time: duration.Duration) -> Nil {
  let delta_time_seconds = duration.to_seconds(delta_time)

  let mixers = object_cache.get_all_mixers(state.cache)
  list.each(mixers, fn(entry) {
    let #(_id, mixer) = entry
    // Three.js AnimationMixer.update expects seconds
    savoiardi.update_mixer(mixer, delta_time_seconds)
  })
}

@internal
pub fn sync_physics_transforms(state: RendererState) -> Nil {
  case state.physics_world {
    option.Some(world) -> {
      // Use raw quaternion data from physics to avoid conversion errors
      physics.for_each_body_raw(world, fn(id, position, quaternion, body_type) {
        // Only sync Dynamic bodies - Kinematic/Fixed are controlled by scene
        case body_type {
          physics.Dynamic -> {
            // Get the Three.js object for this body
            case object_cache.get_object(state.cache, id) {
              Ok(obj) -> {
                // Apply transform using quaternion directly (no Euler conversion)
                savoiardi.apply_transform_with_quaternion(
                  object: obj,
                  position: position,
                  quaternion: quaternion,
                  scale: vec3f.one,
                )
                savoiardi.update_matrix_world_force(obj, True)
              }
              Error(Nil) -> Nil
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
pub fn clear_cache(state: RendererState) -> RendererState {
  // Dispose all objects recursively (geometry, materials, textures, children)
  let objects = object_cache.get_all_objects(state.cache)
  list.each(objects, fn(entry) {
    let #(_id, obj) = entry
    savoiardi.dispose_object(obj)
  })

  RendererState(..state, cache: object_cache.init())
}

@internal
pub fn get_cameras_with_viewports(
  state: RendererState,
) -> List(#(model.Object3D, camera.ViewPort)) {
  object_cache.get_cameras_with_viewports(state.cache)
  |> list.map(fn(entry) {
    let #(camera_obj, viewport) = entry
    #(camera_obj, viewport)
  })
}

/// Get all cameras with their viewport and postprocessing configurations
/// Returns: List of (camera_id_string, camera_object, Option(viewport), Option(postprocessing), is_active)
@internal
pub fn get_all_cameras_with_info(
  state: RendererState,
) -> List(
  #(
    String,
    model.Object3D,
    option.Option(camera.ViewPort),
    option.Option(camera.PostProcessing),
    Bool,
  ),
) {
  object_cache.get_all_cameras_with_info(state.cache)
  |> list.map(fn(entry) {
    let #(id_string, camera_obj, viewport_opt, pp_opt, is_active) = entry
    #(id_string, camera_obj, viewport_opt, pp_opt, is_active)
  })
}

@external(javascript, "../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

// Canvas/Paint integration functions - tiramisu-specific, not pure Three.js bindings
@external(javascript, "../tiramisu.ffi.mjs", "createCanvasTextureFromPicture")
fn create_canvas_texture_from_picture(
  encoded_picture: String,
  width: Int,
  height: Int,
) -> texture.Texture

@external(javascript, "../tiramisu.ffi.mjs", "createCanvasPlane")
fn create_canvas_plane(
  texture: texture.Texture,
  width: Float,
  height: Float,
) -> model.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "updateCanvasTexture")
fn update_canvas_texture(
  object: model.Object3D,
  texture: texture.Texture,
) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "updateCanvasSize")
fn update_canvas_size(
  object: model.Object3D,
  width: Float,
  height: Float,
) -> Nil

@external(javascript, "../tiramisu.ffi.mjs", "getCanvasCachedPicture")
fn get_canvas_cached_picture(object: model.Object3D) -> String

@external(javascript, "../tiramisu.ffi.mjs", "setCanvasCachedPicture")
fn set_canvas_cached_picture(
  object: model.Object3D,
  encoded_picture: String,
) -> Nil

// Debug visualization helpers - tiramisu-specific, not pure Three.js bindings
@external(javascript, "../tiramisu.ffi.mjs", "createDebugBox")
fn create_debug_box(
  min: vec3.Vec3(Float),
  max: vec3.Vec3(Float),
  color: Int,
) -> model.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugSphere")
fn create_debug_sphere(
  center: vec3.Vec3(Float),
  radius: Float,
  color: Int,
) -> model.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugLine")
fn create_debug_line(
  from: vec3.Vec3(Float),
  to: vec3.Vec3(Float),
  color: Int,
) -> model.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugAxes")
fn create_debug_axes(origin: vec3.Vec3(Float), size: Float) -> model.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugGrid")
fn create_debug_grid(size: Float, divisions: Int, color: Int) -> model.Object3D

@external(javascript, "../tiramisu.ffi.mjs", "createDebugPoint")
fn create_debug_point(
  position: vec3.Vec3(Float),
  size: Float,
  color: Int,
) -> model.Object3D

/// Convert a list of Transforms to the tuple format expected by savoiardi
fn transforms_to_tuples(
  transforms: List(transform.Transform),
) -> List(#(vec3.Vec3(Float), vec3.Vec3(Float), vec3.Vec3(Float))) {
  list.map(transforms, fn(t) {
    #(transform.position(t), transform.rotation(t), transform.scale(t))
  })
}
