//// Runtime registry for Tiramisu web components.
////
//// This module provides a registry to store savoiardi Three.js objects
//// and look them up by string IDs. This is necessary because Lustre's
//// context system uses JSON encoding, so we can't pass opaque types
//// directly between components.
////
//// All actual Three.js operations use savoiardi's typed wrappers.

// IMPORTS ---------------------------------------------------------------------

import gleam/option.{type Option}
import savoiardi.{
  type Camera, type Geometry, type Light, type Material, type Object3D,
  type Renderer, type Scene,
}

// TYPES -----------------------------------------------------------------------

/// Opaque reference to a registered scene.
pub type SceneRef {
  SceneRef(id: String)
}

/// Opaque reference to a registered renderer.
pub type RendererRef {
  RendererRef(id: String)
}

/// Opaque reference to a registered object (mesh, group, etc.).
pub type ObjectRef {
  ObjectRef(id: String)
}

/// Opaque reference to a registered camera.
pub type CameraRef {
  CameraRef(id: String)
}

// SCENE OPERATIONS ------------------------------------------------------------

/// Create a new scene and register it with an auto-generated ID.
pub fn create_scene() -> SceneRef {
  let scene = savoiardi.create_scene()
  let id = register_scene_ffi(scene, "")
  SceneRef(id:)
}

/// Create a new scene with a user-defined ID.
/// If the ID is empty, an auto-generated ID will be used.
pub fn create_scene_with_id(custom_id: String) -> SceneRef {
  let scene = savoiardi.create_scene()
  let id = register_scene_ffi(scene, custom_id)
  SceneRef(id:)
}

/// Get a scene by its reference.
pub fn get_scene(ref: SceneRef) -> Option(Scene) {
  get_scene_ffi(ref.id)
}

/// Dispose of a scene and unregister it.
pub fn dispose_scene(ref: SceneRef) -> Nil {
  dispose_scene_ffi(ref.id)
}

// RENDERER OPERATIONS ---------------------------------------------------------

/// Renderer configuration options.
pub type RendererConfig {
  RendererConfig(
    width: Option(Int),
    height: Option(Int),
    background: String,
    antialias: Bool,
    alpha: Bool,
  )
}

/// Create a new renderer attached to a container element.
pub fn create_renderer(container: a, config: RendererConfig) -> RendererRef {
  let renderer =
    savoiardi.create_renderer(savoiardi.RendererOptions(
      antialias: config.antialias,
      alpha: config.alpha,
      dimensions: option.None,
    ))

  let id = register_renderer_ffi(renderer, container, config)
  RendererRef(id:)
}

/// Resize a renderer.
pub fn resize_renderer(ref: RendererRef, width: Int, height: Int) -> Nil {
  resize_renderer_ffi(ref.id, width, height)
}

/// Dispose of a renderer.
pub fn dispose_renderer(ref: RendererRef) -> Nil {
  dispose_renderer_ffi(ref.id)
}

// RENDER LOOP -----------------------------------------------------------------

/// Start the render loop for a renderer/scene pair.
pub fn start_render_loop(renderer: RendererRef, scene: SceneRef) -> Nil {
  start_render_loop_ffi(renderer.id, scene.id)
}

/// Stop the render loop.
pub fn stop_render_loop(renderer: RendererRef) -> Nil {
  stop_render_loop_ffi(renderer.id)
}

// MESH OPERATIONS -------------------------------------------------------------

/// Create a mesh with the given geometry and material, add to parent.
pub fn create_mesh(
  scene: SceneRef,
  parent_id: String,
  id: String,
  geometry: Geometry,
  material: Material,
) -> ObjectRef {
  let mesh = savoiardi.create_mesh(geometry, material)
  let object_id = register_and_add_object_ffi(scene.id, parent_id, id, mesh)
  ObjectRef(id: object_id)
}

// CAMERA OPERATIONS -----------------------------------------------------------

/// Create a perspective camera.
pub fn create_perspective_camera(
  scene: SceneRef,
  parent_id: String,
  id: String,
  fov: Float,
  aspect: Float,
  near: Float,
  far: Float,
  active: Bool,
) -> CameraRef {
  let camera = savoiardi.create_perspective_camera(fov, aspect, near, far)
  let cam_id = register_camera_ffi(scene.id, parent_id, id, camera, active)
  CameraRef(id: cam_id)
}

/// Create an orthographic camera.
pub fn create_orthographic_camera(
  scene: SceneRef,
  parent_id: String,
  id: String,
  left: Float,
  right: Float,
  top: Float,
  bottom: Float,
  near: Float,
  far: Float,
  active: Bool,
) -> CameraRef {
  let camera =
    savoiardi.create_orthographic_camera(left, right, top, bottom, near, far)
  let cam_id = register_camera_ffi(scene.id, parent_id, id, camera, active)
  CameraRef(id: cam_id)
}

/// Set camera as active.
pub fn set_camera_active(ref: CameraRef, active: Bool) -> Nil {
  set_camera_active_ffi(ref.id, active)
}

/// Get a camera from the registry by its reference.
/// Returns the Three.js Camera for use with savoiardi functions.
pub fn get_camera(ref: CameraRef) -> Option(Camera) {
  get_camera_ffi(ref.id)
}

// LIGHT OPERATIONS ------------------------------------------------------------

/// Create an ambient light.
pub fn create_ambient_light(
  scene: SceneRef,
  parent_id: String,
  id: String,
  color: Int,
  intensity: Float,
) -> ObjectRef {
  let light = savoiardi.create_ambient_light(color, intensity)
  let obj_id = register_and_add_light_ffi(scene.id, parent_id, id, light)
  ObjectRef(id: obj_id)
}

/// Create a directional light.
pub fn create_directional_light(
  scene: SceneRef,
  parent_id: String,
  id: String,
  color: Int,
  intensity: Float,
  cast_shadow: Bool,
) -> ObjectRef {
  let shadow_config =
    savoiardi.DirectionalShadowConfig(
      resolution: 1024,
      bias: -0.0001,
      normal_bias: 0.0,
      camera_left: -10.0,
      camera_right: 10.0,
      camera_top: 10.0,
      camera_bottom: -10.0,
      camera_near: 0.5,
      camera_far: 500.0,
    )
  let light =
    savoiardi.create_directional_light(
      color,
      intensity,
      cast_shadow,
      shadow_config,
    )
  let obj_id = register_and_add_light_ffi(scene.id, parent_id, id, light)
  ObjectRef(id: obj_id)
}

/// Create a point light.
pub fn create_point_light(
  scene: SceneRef,
  parent_id: String,
  id: String,
  color: Int,
  intensity: Float,
  distance: Float,
  cast_shadow: Bool,
) -> ObjectRef {
  let shadow_config =
    savoiardi.ShadowConfig(resolution: 1024, bias: -0.0001, normal_bias: 0.0)
  let light =
    savoiardi.create_point_light(
      color,
      intensity,
      distance,
      cast_shadow,
      shadow_config,
    )
  let obj_id = register_and_add_light_ffi(scene.id, parent_id, id, light)
  ObjectRef(id: obj_id)
}

// GROUP OPERATIONS ------------------------------------------------------------

/// Create a group (empty node for hierarchy).
pub fn create_group(scene: SceneRef, parent_id: String, id: String) -> ObjectRef {
  let group = savoiardi.create_group()
  let obj_id = register_and_add_object_ffi(scene.id, parent_id, id, group)
  ObjectRef(id: obj_id)
}

// LOADED OBJECT OPERATIONS ----------------------------------------------------

/// Add an already-created Object3D to the scene.
/// Used for externally loaded models (GLTF/FBX).
pub fn add_object_to_scene(
  scene: SceneRef,
  parent_id: String,
  id: String,
  object: Object3D,
) -> ObjectRef {
  let obj_id = register_and_add_object_ffi(scene.id, parent_id, id, object)
  ObjectRef(id: obj_id)
}

// TRANSFORM OPERATIONS --------------------------------------------------------

/// Set an object's position.
pub fn set_position(ref: ObjectRef, x: Float, y: Float, z: Float) -> Nil {
  set_position_ffi(ref.id, x, y, z)
}

/// Set an object's rotation (Euler angles in radians).
pub fn set_rotation(ref: ObjectRef, x: Float, y: Float, z: Float) -> Nil {
  set_rotation_ffi(ref.id, x, y, z)
}

/// Set an object's rotation using a quaternion (x, y, z, w).
pub fn set_quaternion(
  ref: ObjectRef,
  x: Float,
  y: Float,
  z: Float,
  w: Float,
) -> Nil {
  set_quaternion_ffi(ref.id, x, y, z, w)
}

/// Set an object's scale.
pub fn set_scale(ref: ObjectRef, x: Float, y: Float, z: Float) -> Nil {
  set_scale_ffi(ref.id, x, y, z)
}

// OBJECT MANAGEMENT -----------------------------------------------------------

/// Get an object from the registry by its reference.
/// Returns the Three.js Object3D for use with savoiardi functions.
pub fn get_object(ref: ObjectRef) -> Option(Object3D) {
  get_object_ffi(ref.id)
}

/// Get a light from the registry by its reference.
/// Returns the Three.js Light for use with savoiardi functions.
pub fn get_light(ref: ObjectRef) -> Option(Light) {
  get_light_ffi(ref.id)
}

/// Get a material from an object in the registry.
/// Returns the Three.js Material for use with savoiardi functions.
pub fn get_material(ref: ObjectRef) -> Option(Material) {
  get_material_ffi(ref.id)
}

/// Remove an object and dispose its resources.
pub fn remove_object(ref: ObjectRef) -> Nil {
  remove_object_ffi(ref.id)
}

/// Set object visibility.
pub fn set_visible(ref: ObjectRef, visible: Bool) -> Nil {
  set_visible_ffi(ref.id, visible)
}

// FFI DECLARATIONS ------------------------------------------------------------

// Scene registry
@external(javascript, "./runtime.ffi.mjs", "registerScene")
fn register_scene_ffi(scene: Scene, custom_id: String) -> String

@external(javascript, "./runtime.ffi.mjs", "getScene")
fn get_scene_ffi(id: String) -> Option(Scene)

@external(javascript, "./runtime.ffi.mjs", "disposeScene")
fn dispose_scene_ffi(id: String) -> Nil

// Renderer registry
@external(javascript, "./runtime.ffi.mjs", "registerRenderer")
fn register_renderer_ffi(
  renderer: Renderer,
  container: a,
  config: RendererConfig,
) -> String

@external(javascript, "./runtime.ffi.mjs", "resizeRenderer")
fn resize_renderer_ffi(id: String, width: Int, height: Int) -> Nil

@external(javascript, "./runtime.ffi.mjs", "disposeRenderer")
fn dispose_renderer_ffi(id: String) -> Nil

// Render loop
@external(javascript, "./runtime.ffi.mjs", "startRenderLoop")
fn start_render_loop_ffi(renderer_id: String, scene_id: String) -> Nil

@external(javascript, "./runtime.ffi.mjs", "stopRenderLoop")
fn stop_render_loop_ffi(renderer_id: String) -> Nil

// Object registry
@external(javascript, "./runtime.ffi.mjs", "registerAndAddObject")
fn register_and_add_object_ffi(
  scene_id: String,
  parent_id: String,
  id: String,
  object: Object3D,
) -> String

@external(javascript, "./runtime.ffi.mjs", "registerCamera")
fn register_camera_ffi(
  scene_id: String,
  parent_id: String,
  id: String,
  camera: Camera,
  active: Bool,
) -> String

@external(javascript, "./runtime.ffi.mjs", "setCameraActive")
fn set_camera_active_ffi(id: String, active: Bool) -> Nil

@external(javascript, "./runtime.ffi.mjs", "registerAndAddLight")
fn register_and_add_light_ffi(
  scene_id: String,
  parent_id: String,
  id: String,
  light: Light,
) -> String

// Transform
@external(javascript, "./runtime.ffi.mjs", "setPosition")
fn set_position_ffi(id: String, x: Float, y: Float, z: Float) -> Nil

@external(javascript, "./runtime.ffi.mjs", "setRotation")
fn set_rotation_ffi(id: String, x: Float, y: Float, z: Float) -> Nil

@external(javascript, "./runtime.ffi.mjs", "setQuaternion")
fn set_quaternion_ffi(id: String, x: Float, y: Float, z: Float, w: Float) -> Nil

@external(javascript, "./runtime.ffi.mjs", "setScale")
fn set_scale_ffi(id: String, x: Float, y: Float, z: Float) -> Nil

// Object management
@external(javascript, "./runtime.ffi.mjs", "getObject")
fn get_object_ffi(id: String) -> Option(Object3D)

@external(javascript, "./runtime.ffi.mjs", "getLight")
fn get_light_ffi(id: String) -> Option(Light)

@external(javascript, "./runtime.ffi.mjs", "getMaterial")
fn get_material_ffi(id: String) -> Option(Material)

@external(javascript, "./runtime.ffi.mjs", "getCamera")
fn get_camera_ffi(id: String) -> Option(Camera)

@external(javascript, "./runtime.ffi.mjs", "removeObject")
fn remove_object_ffi(id: String) -> Nil

@external(javascript, "./runtime.ffi.mjs", "setVisible")
fn set_visible_ffi(id: String, visible: Bool) -> Nil
