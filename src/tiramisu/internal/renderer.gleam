/// Internal renderer module
///
/// This module handles all patch application logic, converting declarative scene
/// graph changes into imperative Three.js operations. It manages:
/// - Scene graph synchronization (adding/removing nodes)
/// - Transform updates
/// - Material and geometry updates
/// - Animation coordination
/// - Physics synchronization
/// - Audio management
/// - Camera management
/// - Particle system integration
///
/// Architecture:
/// - Business logic lives here in Gleam (pattern matching, coordination)
/// - Pure imperative Three.js operations live in threejs.ffi.mjs
/// - Cache management uses Gleam Dict (via object_cache module)
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import tiramisu/audio.{type Audio}
import tiramisu/camera
import tiramisu/geometry.{type Geometry}
import tiramisu/internal/audio_manager
import tiramisu/internal/object_cache.{
  type AnimationAction, type AnimationActions, type AnimationMixer,
  type CacheState, type ThreeObject,
}
import tiramisu/internal/particle_manager
import tiramisu/light.{type Light}
import tiramisu/material.{type Material}
import tiramisu/object3d.{type AnimationPlayback}
import tiramisu/particle_emitter
import tiramisu/physics.{type RigidBody}
import tiramisu/scene.{type LODLevel, type Node, type Patch}
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}

// ============================================================================
// OPAQUE TYPES
// ============================================================================

/// Internal Three.js Scene type (not exposed to users)
pub type Scene

/// Internal Three.js Camera type (not exposed to users)
pub type ThreeCamera

/// WebGL renderer
pub type WebGLRenderer

/// Canvas DOM element
pub type DomElement

// ============================================================================
// RENDERER CONFIGURATION
// ============================================================================

/// Canvas dimensions
pub type Dimensions {
  Dimensions(width: Float, height: Float)
}

/// Configuration options for the renderer
pub type RendererOptions {
  RendererOptions(antialias: Bool, alpha: Bool, dimensions: Option(Dimensions))
}

// ============================================================================
// RENDERER STATE
// ============================================================================

/// Complete renderer state including caches and references
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
    audio_listener: object3d.Object3D,
  )
}

// ============================================================================
// FFI DECLARATIONS - TYPE CONVERSION
// ============================================================================

/// Convert any type to Dynamic (identity function)
@external(javascript, "../../threejs.ffi.mjs", "identity")
fn to_dynamic(value: a) -> Dynamic

/// Convert Dynamic to Object3D (identity function for type casting)
@external(javascript, "../../threejs.ffi.mjs", "identity")
fn points_to_object3d(points: Dynamic) -> object3d.Object3D

// ============================================================================
// FFI DECLARATIONS - RENDERER CREATION
// ============================================================================

/// Create a new WebGL renderer
@external(javascript, "../../threejs.ffi.mjs", "createRenderer")
fn create_renderer_ffi(options: RendererOptions) -> WebGLRenderer

/// Get the canvas DOM element from renderer
@external(javascript, "../../threejs.ffi.mjs", "getRendererDomElement")
pub fn get_dom_element(renderer: WebGLRenderer) -> DomElement

/// Set the canvas reference for camera aspect ratio calculation
@external(javascript, "../../threejs.ffi.mjs", "setCanvas")
pub fn set_canvas(canvas: DomElement) -> Nil

/// Create a new Three.js scene
@external(javascript, "../../threejs.ffi.mjs", "createScene")
fn create_scene_ffi() -> Scene

/// Render a scene with a camera
@external(javascript, "../../threejs.ffi.mjs", "render")
pub fn render(renderer: WebGLRenderer, scene: Scene, camera: ThreeCamera) -> Nil

// ============================================================================
// FFI DECLARATIONS - SCENE GRAPH OPERATIONS
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "addToScene")
fn add_to_scene_ffi(scene: Scene, object: object3d.Object3D) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "removeFromScene")
fn remove_from_scene_ffi(scene: Scene, object: object3d.Object3D) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "addChild")
fn add_child_ffi(parent: object3d.Object3D, child: object3d.Object3D) -> Nil

// ============================================================================
// FFI DECLARATIONS - TRANSFORM OPERATIONS
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "applyTransform")
fn apply_transform_ffi(object: object3d.Object3D, transform: Transform) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "applyTransformWithQuaternion")
fn apply_transform_with_quaternion_ffi(
  object: object3d.Object3D,
  position: Dynamic,
  quaternion: Dynamic,
  scale: Dynamic,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "updateMatrixWorld")
fn update_matrix_world_ffi(object: object3d.Object3D, force: Bool) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "applyCameraLookAt")
fn apply_camera_look_at_ffi(
  camera: object3d.Object3D,
  target: Vec3(Float),
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "applyCameraLookAtDynamic")
fn apply_camera_look_at_dynamic_ffi(
  camera: object3d.Object3D,
  target: Dynamic,
) -> Nil

// ============================================================================
// FFI DECLARATIONS - OBJECT CREATION
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "createMesh")
fn create_mesh_ffi(
  geometry: geometry.ThreeGeometry,
  material: material.ThreeMaterial,
) -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "createInstancedMesh")
fn create_instanced_mesh_ffi(
  geometry: geometry.ThreeGeometry,
  material: material.ThreeMaterial,
  count: Int,
) -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "createGroup")
fn create_group_ffi() -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "createLOD")
fn create_lod_ffi() -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "cloneObject")
fn clone_object_ffi(object: object3d.Object3D) -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "extractMeshMaterialPairs")
fn extract_mesh_material_pairs_ffi(
  object: object3d.Object3D,
) -> MeshMaterialPairs

// Opaque type for mesh/material pairs from FFI
pub type MeshMaterialPairs

// Access the arrays from the pairs object
@external(javascript, "../../tiramisu.ffi.mjs", "getPairsGeometries")
fn get_pairs_geometries_ffi(
  pairs: MeshMaterialPairs,
) -> List(geometry.ThreeGeometry)

@external(javascript, "../../tiramisu.ffi.mjs", "getPairsMaterials")
fn get_pairs_materials_ffi(
  pairs: MeshMaterialPairs,
) -> List(material.ThreeMaterial)

// ============================================================================
// FFI DECLARATIONS - MATERIAL AND GEOMETRY
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "setObjectGeometry")
fn set_object_geometry_ffi(
  object: object3d.Object3D,
  geometry: geometry.ThreeGeometry,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setObjectMaterial")
fn set_object_material_ffi(
  object: object3d.Object3D,
  material: material.ThreeMaterial,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "getObjectGeometry")
fn get_object_geometry_ffi(object: object3d.Object3D) -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "getObjectMaterial")
fn get_object_material_ffi(object: object3d.Object3D) -> object3d.Object3D

// ============================================================================
// FFI DECLARATIONS - LIGHT
// ============================================================================

// Light creation is handled by tiramisu/light.gleam's create_light function

// ============================================================================
// FFI DECLARATIONS - SHADOWS
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "setShadowProperties")
fn set_shadow_properties_ffi(
  object: object3d.Object3D,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> Nil

// ============================================================================
// FFI DECLARATIONS - CAMERA OPERATIONS
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "isPerspectiveCamera")
fn is_perspective_camera_ffi(object: object3d.Object3D) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "isOrthographicCamera")
fn is_orthographic_camera_ffi(object: object3d.Object3D) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "updateCameraProjectionMatrix")
fn update_camera_projection_matrix_ffi(camera: object3d.Object3D) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setCameraUserData")
fn set_camera_user_data_ffi(
  camera: object3d.Object3D,
  key: String,
  value: Dynamic,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "getCameraUserData")
fn get_camera_user_data_ffi(camera: object3d.Object3D, key: String) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "hasCameraUserData")
fn has_camera_user_data_ffi(camera: object3d.Object3D, key: String) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "deleteCameraUserData")
fn delete_camera_user_data_ffi(camera: object3d.Object3D, key: String) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "createPerspectiveCamera")
fn create_perspective_camera_ffi(
  fov: Float,
  aspect: Float,
  near: Float,
  far: Float,
) -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "createOrthographicCamera")
fn create_orthographic_camera_ffi(
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  near: Float,
  far: Float,
) -> object3d.Object3D

@external(javascript, "../../threejs.ffi.mjs", "setActiveCamera")
fn set_active_camera_ffi(camera: object3d.Object3D) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "getCanvasClientWidth")
fn get_canvas_client_width_ffi(canvas: DomElement) -> Float

@external(javascript, "../../threejs.ffi.mjs", "getCanvasClientHeight")
fn get_canvas_client_height_ffi(canvas: DomElement) -> Float

@external(javascript, "../../threejs.ffi.mjs", "getWindowWidth")
fn get_window_width_ffi() -> Float

@external(javascript, "../../threejs.ffi.mjs", "getWindowHeight")
fn get_window_height_ffi() -> Float

// ============================================================================
// FFI DECLARATIONS - ANIMATION
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "createAnimationMixer")
fn create_animation_mixer_ffi(object: object3d.Object3D) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "updateMixer")
fn update_animation_mixer_ffi(mixer: Dynamic, delta_time: Float) -> Nil

// ============================================================================
// FFI DECLARATIONS - INSTANCED MESH
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "isInstancedMesh")
fn is_instanced_mesh_ffi(object: object3d.Object3D) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "updateInstancedMeshTransforms")
fn update_instanced_mesh_transforms_ffi(
  mesh: object3d.Object3D,
  instances: List(Transform),
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "updateGroupInstancedMeshes")
fn update_group_instanced_meshes_ffi(
  group: object3d.Object3D,
  instances: List(Transform),
) -> Nil

// ============================================================================
// FFI DECLARATIONS - LOD
// ============================================================================

@external(javascript, "../../threejs.ffi.mjs", "isLOD")
fn is_lod_ffi(object: object3d.Object3D) -> Bool

@external(javascript, "../../threejs.ffi.mjs", "addLODLevel")
fn add_lod_level_ffi(
  lod: object3d.Object3D,
  object: object3d.Object3D,
  distance: Float,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "clearLODLevels")
fn clear_lod_levels_ffi(lod: object3d.Object3D) -> Nil

// ============================================================================
// FFI DECLARATIONS - AUDIO
// ============================================================================

// Audio listener creation (singleton, attached to camera)
@external(javascript, "../../threejs.ffi.mjs", "createAudioListener")
fn create_audio_listener_ffi() -> object3d.Object3D

// ============================================================================
// FFI DECLARATIONS - DEBUG VISUALIZATION
// ============================================================================

@external(javascript, "../../tiramisu.ffi.mjs", "createDebugBox")
fn create_debug_box_ffi(
  min: Vec3(Float),
  max: Vec3(Float),
  color: Int,
) -> object3d.Object3D

@external(javascript, "../../tiramisu.ffi.mjs", "createDebugSphere")
fn create_debug_sphere_ffi(
  center: Vec3(Float),
  radius: Float,
  color: Int,
) -> object3d.Object3D

@external(javascript, "../../tiramisu.ffi.mjs", "createDebugLine")
fn create_debug_line_ffi(
  from: Vec3(Float),
  to: Vec3(Float),
  color: Int,
) -> object3d.Object3D

@external(javascript, "../../tiramisu.ffi.mjs", "createDebugAxes")
fn create_debug_axes_ffi(origin: Vec3(Float), size: Float) -> object3d.Object3D

@external(javascript, "../../tiramisu.ffi.mjs", "createDebugGrid")
fn create_debug_grid_ffi(
  size: Float,
  divisions: Int,
  color: Int,
) -> object3d.Object3D

@external(javascript, "../../tiramisu.ffi.mjs", "createDebugPoint")
fn create_debug_point_ffi(
  position: Vec3(Float),
  size: Float,
  color: Int,
) -> object3d.Object3D

// ============================================================================
// FFI DECLARATIONS - RESOURCE DISPOSAL
// ============================================================================

@external(javascript, "../../tiramisu.ffi.mjs", "disposeGeometry")
fn dispose_geometry_ffi(geometry: object3d.Object3D) -> Nil

@external(javascript, "../../tiramisu.ffi.mjs", "disposeMaterial")
fn dispose_material_ffi(material: object3d.Object3D) -> Nil

// ============================================================================
// PUBLIC API - RENDERER INITIALIZATION
// ============================================================================

/// Create a new renderer with full state management
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
    physics_world: None,
    audio_manager: audio_manager.init(),
    audio_listener: audio_listener,
  )
}

/// Get the WebGLRenderer from RendererState
pub fn get_renderer(state: RendererState(id)) -> WebGLRenderer {
  state.renderer
}

/// Get the Scene from RendererState
pub fn get_scene(state: RendererState(id)) -> Scene {
  state.scene
}

/// Set the physics world for the renderer
pub fn set_physics_world(
  state: RendererState(id),
  world: Option(physics.PhysicsWorld(id)),
) -> RendererState(id) {
  RendererState(..state, physics_world: world)
}

/// Get the physics world from the renderer
pub fn get_physics_world(
  state: RendererState(id),
) -> Option(physics.PhysicsWorld(id)) {
  state.physics_world
}

/// Resume AudioContext and play any pending audio sources
/// Call this after user interaction to enable audio playback
pub fn resume_audio_context(state: RendererState(id)) -> RendererState(id) {
  let new_audio_manager =
    audio_manager.resume_audio_context(state.audio_manager)
  RendererState(..state, audio_manager: new_audio_manager)
}

// ============================================================================
// PUBLIC API - PATCH APPLICATION
// ============================================================================

/// Apply multiple patches to update the scene
pub fn apply_patches(
  state: RendererState(id),
  patches: List(Patch(id)),
) -> RendererState(id) {
  list.fold(patches, state, fn(st, patch) { apply_patch(st, patch) })
}

/// Apply a single patch to the scene
pub fn apply_patch(
  state: RendererState(id),
  patch: Patch(id),
) -> RendererState(id) {
  case patch {
    scene.AddNode(id: id, node: node, parent_id: parent_id) ->
      handle_add_node(state, id, node, parent_id)

    scene.RemoveNode(id: id) -> handle_remove_node(state, id)

    scene.UpdateTransform(id: id, transform: transform) ->
      handle_update_transform(state, id, transform)

    scene.UpdateMaterial(id: id, material: material) ->
      handle_update_material(state, id, material)

    scene.UpdateGeometry(id: id, geometry: geometry) ->
      handle_update_geometry(state, id, geometry)

    scene.UpdateLight(id: id, light: light) ->
      handle_update_light(state, id, light)

    scene.UpdateAnimation(id: id, animation: animation) ->
      handle_update_animation(state, id, animation)

    scene.UpdatePhysics(id: id, physics: physics) ->
      handle_update_physics(state, id, physics)

    scene.UpdateAudio(id: id, audio: audio) ->
      handle_update_audio(state, id, audio)

    scene.UpdateInstances(id: id, instances: instances) ->
      handle_update_instances(state, id, instances)

    scene.UpdateLODLevels(id: id, levels: levels) ->
      handle_update_lod_levels(state, id, levels)

    scene.UpdateCamera(id: id, camera_type: camera_type, look_at: look_at) ->
      handle_update_camera(state, id, camera_type, look_at)

    scene.SetActiveCamera(id: id) -> handle_set_active_camera(state, id)

    scene.UpdateParticleEmitter(id: id, emitter: emitter) ->
      handle_update_particle_emitter(state, id, emitter)

    scene.UpdateParticleActive(id: id, active: active) ->
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
    scene.Mesh(
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

    scene.InstancedMesh(
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

    scene.Light(id: _, light: light, transform: transform) ->
      handle_add_light(state, id, light, transform, parent_id)

    scene.Group(id: _, transform: transform, children: _) ->
      handle_add_group(state, id, transform, parent_id)

    scene.LOD(id: _, transform: transform, levels: levels) ->
      handle_add_lod(state, id, transform, levels, parent_id)

    scene.Model3D(
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

    scene.InstancedModel(
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

    scene.Audio(id: _, audio: audio) ->
      handle_add_audio(state, id, audio, parent_id)

    scene.Camera(
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

    scene.DebugBox(id: _, min: min, max: max, color: color) ->
      handle_add_debug_box(state, id, min, max, color, parent_id)

    scene.DebugSphere(id: _, center: center, radius: radius, color: color) ->
      handle_add_debug_sphere(state, id, center, radius, color, parent_id)

    scene.DebugLine(id: _, from: from, to: to, color: color) ->
      handle_add_debug_line(state, id, from, to, color, parent_id)

    scene.DebugAxes(id: _, origin: origin, size: size) ->
      handle_add_debug_axes(state, id, origin, size, parent_id)

    scene.DebugGrid(id: _, size: size, divisions: divisions, color: color) ->
      handle_add_debug_grid(state, id, size, divisions, color, parent_id)

    scene.DebugPoint(id: _, position: position, size: size, color: color) ->
      handle_add_debug_point(state, id, position, size, color, parent_id)

    scene.Particles(
      id: _,
      emitter: emitter,
      transform: transform,
      active: active,
    ) -> handle_add_particles(state, id, emitter, transform, active, parent_id)
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
    Some(pid) -> {
      case object_cache.get_object(state.cache, pid) {
        Some(parent_obj) -> {
          let parent_dynamic = object_cache.unwrap_object(parent_obj)
          add_child_ffi(parent_dynamic, obj_dynamic)
        }
        None -> {
          // Parent not found, add to scene root
          add_to_scene_ffi(state.scene, obj_dynamic)
        }
      }
    }
    None -> add_to_scene_ffi(state.scene, obj_dynamic)
  }
}

fn handle_add_mesh(
  state: RendererState(id),
  id: id,
  geometry: Geometry,
  material: Material,
  transform: Transform,
  physics: Option(RigidBody),
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
    Some(physics_config), Some(world) -> {
      let new_world = physics.create_body(world, id, physics_config, transform)
      RendererState(..new_state, physics_world: Some(new_world))
    }
    _, _ -> new_state
  }
}

fn handle_add_instanced_mesh(
  state: RendererState(id),
  id: id,
  geometry: Geometry,
  material: Material,
  instances: List(Transform),
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
  object: object3d.Object3D,
  instances: List(Transform),
  physics: Option(RigidBody),
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
    Some(physics_config), Some(world) -> {
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
      RendererState(..new_state, physics_world: Some(new_world))
    }
    _, _ -> new_state
  }
}

// Helper to generate unique IDs for instance physics bodies
@external(javascript, "../../tiramisu.ffi.mjs", "generateInstanceId")
fn generate_instance_id(base_id: id, index: Int) -> id

fn handle_add_light(
  state: RendererState(id),
  id: id,
  light: Light,
  transform: Transform,
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
  transform: Transform,
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
  transform: Transform,
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
fn create_lod_level_object(node: Node(id)) -> object3d.Object3D {
  case node {
    scene.Mesh(
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
    scene.Group(id: _, transform: transform, children: _) -> {
      let group = create_group_ffi()
      apply_transform_ffi(group, transform)
      group
    }
    scene.Model3D(
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
  object: object3d.Object3D,
  transform: Transform,
  animation: Option(AnimationPlayback),
  physics: Option(RigidBody),
  parent_id: Option(id),
) -> RendererState(id) {
  apply_transform_ffi(object, transform)

  // Create animation mixer
  let mixer_dynamic = create_animation_mixer_ffi(object)
  let mixer = object_cache.wrap_mixer(mixer_dynamic)
  let cache_with_mixer = object_cache.add_mixer(state.cache, id, mixer)

  // Setup animation if provided
  let cache_with_animation = case animation {
    Some(anim_playback) ->
      setup_animation(cache_with_mixer, id, mixer, anim_playback)
    None -> cache_with_mixer
  }

  let three_obj = object_cache.wrap_object(object)
  add_to_scene_or_parent(state, three_obj, parent_id)

  let new_cache = object_cache.add_object(cache_with_animation, id, three_obj)

  // Create physics body if specified - update physics world in state
  let new_state = RendererState(..state, cache: new_cache)
  case physics, new_state.physics_world {
    Some(physics_config), Some(world) -> {
      let new_world = physics.create_body(world, id, physics_config, transform)
      RendererState(..new_state, physics_world: Some(new_world))
    }
    _, _ -> new_state
  }
}

fn handle_add_audio(
  state: RendererState(id),
  id: id,
  audio: Audio,
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
    Some(#(_x, _y, width, height)) -> {
      int.to_float(width) /. int.to_float(height)
    }
    None -> {
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
  transform: Transform,
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
    Some(target) -> {
      set_camera_user_data_ffi(camera_obj, "lookAtTarget", to_dynamic(target))
      set_camera_user_data_ffi(
        camera_obj,
        "needsLookAtUpdate",
        to_dynamic(True),
      )
    }
    None -> Nil
  }

  update_camera_projection_matrix_ffi(camera_obj)

  let three_obj = object_cache.wrap_object(camera_obj)
  add_to_scene_or_parent(state, three_obj, parent_id)

  // Apply lookAt after adding to scene
  case look_at {
    Some(target) -> {
      apply_camera_look_at_ffi(camera_obj, target)
      delete_camera_user_data_ffi(camera_obj, "needsLookAtUpdate")
    }
    None -> Nil
  }

  // Store viewport if specified
  let cache_with_viewport = case viewport {
    Some(#(x, y, width, height)) -> {
      let vp =
        object_cache.Viewport(
          x: int.to_float(x),
          y: int.to_float(y),
          width: int.to_float(width),
          height: int.to_float(height),
        )
      object_cache.set_viewport(state.cache, id, vp)
    }
    None -> state.cache
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
  transform: Transform,
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
    Some(obj) -> {
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
        Some(world) -> {
          let new_world = physics.remove_body(world, id)
          RendererState(..new_state, physics_world: Some(new_world))
        }
        None -> new_state
      }
    }
    None -> state
  }
}

// ============================================================================
// PATCH HANDLERS - UPDATE OPERATIONS
// ============================================================================

fn handle_update_transform(
  state: RendererState(id),
  id: id,
  transform: Transform,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
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
        Some(world) -> {
          let new_world = physics.update_body_transform(world, id, transform)
          RendererState(..state, physics_world: Some(new_world))
        }
        None -> state
      }

      new_state
    }
    None -> state
  }
}

fn handle_update_material(
  state: RendererState(id),
  id: id,
  material: Material,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      let old_material = get_object_material_ffi(obj_dynamic)
      dispose_material_ffi(old_material)

      let new_material = material.create_material(material)
      set_object_material_ffi(obj_dynamic, new_material)

      state
    }
    None -> state
  }
}

fn handle_update_geometry(
  state: RendererState(id),
  id: id,
  geometry: Geometry,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      let old_geometry = get_object_geometry_ffi(obj_dynamic)
      dispose_geometry_ffi(old_geometry)

      let new_geometry = geometry.create_geometry(geometry)
      set_object_geometry_ffi(obj_dynamic, new_geometry)

      state
    }
    None -> state
  }
}

fn handle_update_light(
  state: RendererState(id),
  id: id,
  light: Light,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(old_obj) -> {
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
    None -> state
  }
}

// FFI declarations for get/set position/rotation/scale
@external(javascript, "../../threejs.ffi.mjs", "getObjectPosition")
fn get_object_position_ffi(object: object3d.Object3D) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "getObjectRotation")
fn get_object_rotation_ffi(object: object3d.Object3D) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "getObjectScale")
fn get_object_scale_ffi(object: object3d.Object3D) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "setObjectPosition")
fn set_object_position_ffi(object: object3d.Object3D, position: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setObjectRotation")
fn set_object_rotation_ffi(object: object3d.Object3D, rotation: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setObjectScale")
fn set_object_scale_ffi(object: object3d.Object3D, scale: Dynamic) -> Nil

fn handle_update_animation(
  state: RendererState(id),
  id: id,
  animation: Option(AnimationPlayback),
) -> RendererState(id) {
  case object_cache.get_mixer(state.cache, id) {
    Some(mixer) -> {
      case animation {
        Some(anim_playback) -> {
          let new_cache = setup_animation(state.cache, id, mixer, anim_playback)
          RendererState(..state, cache: new_cache)
        }
        None -> {
          // Stop all animations
          case object_cache.get_actions(state.cache, id) {
            Some(actions) -> {
              stop_actions(actions)
              let new_cache = object_cache.remove_actions(state.cache, id)
              RendererState(..state, cache: new_cache)
            }
            None -> state
          }
        }
      }
    }
    None -> state
  }
}

fn handle_update_physics(
  state: RendererState(id),
  id: id,
  physics: Option(RigidBody),
) -> RendererState(id) {
  case state.physics_world {
    Some(world) -> {
      // First, remove existing physics body
      let world_after_remove = physics.remove_body(world, id)

      // Then, create new physics body if provided
      let new_world = case physics {
        Some(physics_config) -> {
          // Get current transform from Three.js object
          case object_cache.get_object(state.cache, id) {
            Some(obj) -> {
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
            None -> world_after_remove
          }
        }
        None -> world_after_remove
      }

      RendererState(..state, physics_world: Some(new_world))
    }
    None -> state
  }
}

// Helper to convert Dynamic position/rotation/scale to Transform
fn dynamic_to_transform(
  position: Dynamic,
  rotation: Dynamic,
  scale: Dynamic,
) -> Transform {
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
  audio: Audio,
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
  instances: List(Transform),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
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
    None -> state
  }
}

fn handle_update_lod_levels(
  state: RendererState(id),
  id: id,
  levels: List(LODLevel(id)),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
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
    None -> state
  }
}

fn handle_update_camera(
  state: RendererState(id),
  id: id,
  _camera_type: camera.Camera,
  look_at: Option(Vec3(Float)),
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      let is_camera =
        is_perspective_camera_ffi(obj_dynamic)
        || is_orthographic_camera_ffi(obj_dynamic)

      case is_camera {
        True -> {
          // Apply lookAt if provided and update stored target
          case look_at {
            Some(target) -> {
              apply_camera_look_at_ffi(obj_dynamic, target)
              // Update the stored lookAtTarget so it's used in future transform updates
              set_camera_user_data_ffi(
                obj_dynamic,
                "lookAtTarget",
                to_dynamic(target),
              )
            }
            None -> {
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
    None -> state
  }
}

fn handle_set_active_camera(
  state: RendererState(id),
  id: id,
) -> RendererState(id) {
  case object_cache.get_object(state.cache, id) {
    Some(obj) -> {
      let obj_dynamic = object_cache.unwrap_object(obj)
      set_active_camera_ffi(obj_dynamic)
      state
    }
    None -> state
  }
}

fn handle_update_particle_emitter(
  state: RendererState(id),
  id: id,
  emitter: particle_emitter.ParticleEmitter,
) -> RendererState(id) {
  case object_cache.get_particle_system(state.cache, id) {
    Some(cached_system) -> {
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
    None -> state
  }
}

fn handle_update_particle_active(
  state: RendererState(id),
  id: id,
  active: Bool,
) -> RendererState(id) {
  case object_cache.get_particle_system(state.cache, id) {
    Some(cached_system) -> {
      // Unwrap, update active state, and wrap back
      let particle_state =
        particle_manager.unwrap_from_cache_entry(cached_system)
      let updated_state = particle_manager.set_active(particle_state, active)
      let wrapped_system = particle_manager.wrap_as_cache_entry(updated_state)

      let new_cache =
        object_cache.add_particle_system(state.cache, id, wrapped_system)
      RendererState(..state, cache: new_cache)
    }
    None -> state
  }
}

// ============================================================================
// ANIMATION HELPERS
// ============================================================================

fn setup_animation(
  cache: CacheState,
  id: id,
  mixer: AnimationMixer,
  playback: AnimationPlayback,
) -> CacheState {
  // Stop existing animations
  case object_cache.get_actions(cache, id) {
    Some(actions) -> stop_actions(actions)
    None -> Nil
  }

  case playback {
    object3d.SingleAnimation(anim) -> {
      let action = create_animation_action(mixer, anim)
      let actions = object_cache.SingleAction(action)
      object_cache.set_actions(cache, id, actions)
    }

    object3d.BlendedAnimations(
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
  animation: object3d.Animation,
) -> AnimationAction {
  let mixer_dynamic = object_cache.unwrap_mixer(mixer)
  let clip_dynamic = to_dynamic(animation.clip)
  let action_dynamic = create_animation_action_ffi(mixer_dynamic, clip_dynamic)

  // Configure action
  let loop_mode = case animation.loop {
    object3d.LoopRepeat -> loop_repeat()
    object3d.LoopOnce -> loop_once()
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
@external(javascript, "../../threejs.ffi.mjs", "clipAction")
fn create_animation_action_ffi(mixer: Dynamic, clip: Dynamic) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "setActionLoop")
fn set_animation_loop_ffi(action: Dynamic, loop_mode: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setActionTimeScale")
fn set_animation_time_scale_ffi(action: Dynamic, time_scale: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setActionWeight")
fn set_animation_weight_ffi(action: Dynamic, weight: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "playAction")
fn play_animation_action_ffi(action: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "stopAction")
fn stop_animation_action_ffi(action: Dynamic) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "getLoopRepeat")
fn loop_repeat() -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "getLoopOnce")
fn loop_once() -> Dynamic

// ============================================================================
// PUBLIC API - MIXER UPDATES
// ============================================================================

/// Update all animation mixers with delta time
pub fn update_mixers(state: RendererState(id), delta_time: Float) -> Nil {
  let mixers = object_cache.get_all_mixers(state.cache)
  list.each(mixers, fn(entry) {
    let #(_id, mixer) = entry
    let mixer_dynamic = object_cache.unwrap_mixer(mixer)
    update_animation_mixer_ffi(mixer_dynamic, delta_time)
  })
}

/// Update all particle systems with delta time
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

// ============================================================================
// PUBLIC API - PHYSICS SYNC
// ============================================================================

/// Sync physics body transforms to Three.js objects
///
/// Uses quaternions directly from Rapier to avoid rotation errors
/// caused by quaternion-to-Euler-to-quaternion conversion.
///
/// Only syncs Dynamic bodies - Kinematic and Fixed bodies are controlled
/// programmatically via scene transforms, not by the physics simulation.
pub fn sync_physics_transforms(state: RendererState(id)) -> Nil {
  case state.physics_world {
    Some(world) -> {
      // Use raw quaternion data from physics to avoid conversion errors
      physics.for_each_body_raw(world, fn(id, position, quaternion, body_type) {
        // Only sync Dynamic bodies - Kinematic/Fixed are controlled by scene
        case body_type {
          physics.Dynamic -> {
            // Get the Three.js object for this body
            case object_cache.get_object(state.cache, id) {
              Some(obj) -> {
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
              None -> Nil
            }
          }
          physics.Kinematic | physics.Fixed -> Nil
        }
      })
    }
    None -> Nil
  }
}

// ============================================================================
// PUBLIC API - CLEANUP
// ============================================================================

/// Clear all caches and dispose resources
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

// ============================================================================
// PUBLIC API - VIEWPORT CAMERAS
// ============================================================================

/// Get all cameras with viewports for multi-viewport rendering
/// Returns list of (camera, viewport) tuples
pub fn get_cameras_with_viewports(
  state: RendererState(id),
) -> List(#(object3d.Object3D, object_cache.Viewport)) {
  object_cache.get_cameras_with_viewports(state.cache)
  |> list.map(fn(entry) {
    let #(camera_obj, viewport) = entry
    #(object_cache.unwrap_object(camera_obj), viewport)
  })
}
