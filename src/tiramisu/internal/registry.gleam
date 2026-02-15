//// Instance-scoped registry for Tiramisu web components.
////
//// Each `<tiramisu-renderer>` owns its own Registry. All CRUD operations
//// are pure Gleam on MutableMaps — no global state, no collisions between
//// multiple renderer instances on the same page.
////
//// Three.js operations go through savoiardi. Only browser APIs
//// (requestAnimationFrame, DOM events, DOM container manipulation)
//// remain in FFI.

// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/option.{type Option}
import savoiardi.{
  type Audio, type AudioListener, type Camera, type Light, type Material,
  type Object3D, type PositionalAudio, type Renderer, type Scene,
}
import tiramisu/internal/mutable_map.{type MutableMap}
import vec/vec2
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Metadata about a registered object.
pub type ObjectEntry {
  ObjectEntry(object: Object3D, parent_id: String, kind: ObjectKind)
}

/// What kind of object is registered.
pub type ObjectKind {
  MeshObject
  CameraObject
  LightObject
  GroupObject
  AudioObject
}

/// Metadata about a registered camera.
pub type CameraEntry {
  CameraEntry(camera: Camera, parent_id: String, active: Bool)
}

/// Instance-scoped registry. One per `<tiramisu-renderer>`.
pub type Registry {
  Registry(
    scene: Scene,
    scene_id: String,
    renderer: Renderer,
    objects: MutableMap(String, ObjectEntry),
    cameras: MutableMap(String, CameraEntry),
    audio_listeners: MutableMap(String, AudioListener),
    render_loop_id: Option(Int),
    id_counter: Int,
  )
}

// CONSTRUCTOR -----------------------------------------------------------------

/// Create a new empty registry for a renderer instance.
pub fn new(scene: Scene, scene_id: String, renderer: Renderer) -> Registry {
  Registry(
    scene:,
    scene_id:,
    renderer:,
    objects: mutable_map.new(),
    cameras: mutable_map.new(),
    audio_listeners: mutable_map.new(),
    render_loop_id: option.None,
    id_counter: 0,
  )
}

// ID GENERATION ---------------------------------------------------------------

/// Generate a unique ID within this registry using scene_id as prefix.
pub fn generate_id(registry: Registry, prefix: String) -> #(Registry, String) {
  let next = registry.id_counter + 1
  let id = registry.scene_id <> "_" <> prefix <> "_" <> int.to_string(next)
  #(Registry(..registry, id_counter: next), id)
}

// OBJECT CRUD -----------------------------------------------------------------

/// Get an Object3D by its ID.
pub fn get_object(registry: Registry, id: String) -> Result(Object3D, Nil) {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> Ok(entry.object)
    Error(Nil) -> Error(Nil)
  }
}

/// Get the full ObjectEntry by ID.
pub fn get_object_entry(
  registry: Registry,
  id: String,
) -> Result(ObjectEntry, Nil) {
  mutable_map.get(registry.objects, id)
}

/// Register an object and add it to the scene graph under its parent.
/// Sets the object's name via savoiardi for debugging in Three.js inspector.
pub fn register_and_add_object(
  registry: Registry,
  id: String,
  object: Object3D,
  parent_id: String,
  kind: ObjectKind,
) -> Registry {
  savoiardi.set_object_name(object, id)
  let parent = resolve_parent(registry, parent_id)
  savoiardi.add_child(parent:, child: object)
  let _ =
    mutable_map.insert(
      registry.objects,
      id,
      ObjectEntry(object:, parent_id:, kind:),
    )
  // Store on DOM element so external integrations (cacao physics) can find it
  store_object_on_dom(id, object)
  registry
}

/// Remove an object from the scene graph and dispose its resources.
pub fn remove_object(registry: Registry, id: String) -> Registry {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> {
      // Remove from Three.js scene graph
      let parent = resolve_parent(registry, entry.parent_id)
      savoiardi.remove_child(parent, entry.object)
      // Dispose resources
      savoiardi.dispose_object(entry.object)
      // Remove from maps
      let _ = mutable_map.delete(registry.objects, id)
      let _ = mutable_map.delete(registry.cameras, id)
      // Clear DOM reference
      clear_object_from_dom(id)
      registry
    }
    Error(Nil) -> registry
  }
}

/// Reparent an object to a new parent.
/// Three.js add() automatically removes from the old parent.
pub fn reparent_object(
  registry: Registry,
  id: String,
  new_parent_id: String,
) -> Registry {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> {
      let new_parent = resolve_parent(registry, new_parent_id)
      // Three.js add() auto-removes from old parent
      savoiardi.add_child(parent: new_parent, child: entry.object)
      // Update the entry's parent_id
      let _ =
        mutable_map.insert(
          registry.objects,
          id,
          ObjectEntry(..entry, parent_id: new_parent_id),
        )
      registry
    }
    Error(Nil) -> registry
  }
}

/// Set an object's visibility.
pub fn set_visible(registry: Registry, id: String, visible: Bool) -> Nil {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_visible(entry.object, visible)
    Error(Nil) -> Nil
  }
}

// CAMERA CRUD -----------------------------------------------------------------

/// Get a Camera by its ID.
pub fn get_camera(registry: Registry, id: String) -> Result(Camera, Nil) {
  case mutable_map.get(registry.cameras, id) {
    Ok(entry) -> Ok(entry.camera)
    Error(Nil) -> Error(Nil)
  }
}

/// Get the full CameraEntry by ID.
pub fn get_camera_entry(
  registry: Registry,
  id: String,
) -> Result(CameraEntry, Nil) {
  mutable_map.get(registry.cameras, id)
}

/// Register a camera and add it to the scene graph.
/// Cameras are stored in both the objects map (as Object3D) and the cameras map.
pub fn register_camera(
  registry: Registry,
  id: String,
  camera: Camera,
  parent_id: String,
  active: Bool,
) -> Registry {
  savoiardi.set_object_name(savoiardi.camera_to_object3d(camera), id)
  let parent = resolve_parent(registry, parent_id)
  savoiardi.add_child(parent:, child: savoiardi.camera_to_object3d(camera))
  let _ =
    mutable_map.insert(
      registry.cameras,
      id,
      CameraEntry(camera:, parent_id:, active:),
    )
  // Also store in objects map so transforms work
  let obj = savoiardi.camera_to_object3d(camera)
  let _ =
    mutable_map.insert(
      registry.objects,
      id,
      ObjectEntry(object: obj, parent_id:, kind: CameraObject),
    )
  store_object_on_dom(id, obj)
  registry
}

/// Set whether a camera is active.
pub fn set_camera_active(registry: Registry, id: String, active: Bool) -> Nil {
  case mutable_map.get(registry.cameras, id) {
    Ok(entry) -> {
      mutable_map.insert(registry.cameras, id, CameraEntry(..entry, active:))
      Nil
    }
    Error(Nil) -> Nil
  }
}

/// Find the active camera for this registry's scene.
pub fn find_active_camera(registry: Registry) -> Option(Camera) {
  mutable_map.fold(registry.cameras, option.None, fn(acc, _id, entry) {
    case entry.active {
      True -> option.Some(entry.camera)
      False -> acc
    }
  })
}

// LIGHT HELPERS ---------------------------------------------------------------

/// Get a Light by its ID. Returns Some only if the object's kind is LightObject.
pub fn get_light(registry: Registry, id: String) -> Option(Light) {
  case mutable_map.get(registry.objects, id) {
    Ok(ObjectEntry(object:, kind: LightObject, ..)) ->
      option.Some(savoiardi.object3d_to_light(object))
    _ -> option.None
  }
}

// MATERIAL HELPERS ------------------------------------------------------------

/// Get a material from a registered object.
pub fn get_material(registry: Registry, id: String) -> Result(Material, Nil) {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> Ok(savoiardi.get_object_material(entry.object))
    Error(Nil) -> Error(Nil)
  }
}

// AUDIO -----------------------------------------------------------------------

/// Store an AudioListener for this scene.
pub fn store_audio_listener(
  registry: Registry,
  listener: AudioListener,
) -> Registry {
  let _ =
    mutable_map.insert(registry.audio_listeners, registry.scene_id, listener)
  registry
}

/// Get the stored AudioListener for this scene.
pub fn get_audio_listener(registry: Registry) -> Result(AudioListener, Nil) {
  mutable_map.get(registry.audio_listeners, registry.scene_id)
}

/// Attach an AudioListener to the active camera.
/// Falls back to the scene root if no active camera is found.
pub fn attach_listener_to_camera(
  registry: Registry,
  listener: AudioListener,
) -> Nil {
  case find_active_camera(registry) {
    option.Some(camera) ->
      savoiardi.add_child(
        parent: savoiardi.camera_to_object3d(camera),
        child: savoiardi.audio_listener_to_object3d(listener),
      )
    option.None ->
      savoiardi.add_child(
        parent: savoiardi.scene_to_object3d(registry.scene),
        child: savoiardi.audio_listener_to_object3d(listener),
      )
  }
}

/// Register an Audio object in the scene graph.
pub fn register_audio(
  registry: Registry,
  parent_id: String,
  id: String,
  audio: Audio,
) -> Registry {
  register_and_add_object(
    registry,
    id,
    savoiardi.audio_to_object3d(audio),
    parent_id,
    AudioObject,
  )
}

/// Register a PositionalAudio object in the scene graph.
pub fn register_positional_audio(
  registry: Registry,
  parent_id: String,
  id: String,
  audio: PositionalAudio,
) -> Registry {
  register_and_add_object(
    registry,
    id,
    savoiardi.positional_audio_to_object3d(audio),
    parent_id,
    AudioObject,
  )
}

/// Get an Audio object from the registry by ID.
/// Audio extends Object3D so we retrieve it and cast.
pub fn get_audio(registry: Registry, id: String) -> Result(Audio, Nil) {
  case mutable_map.get(registry.objects, id) {
    Ok(ObjectEntry(object:, kind: AudioObject, ..)) ->
      Ok(savoiardi.object3d_to_audio(object))
    _ -> Error(Nil)
  }
}

/// Get a PositionalAudio object from the registry by ID.
pub fn get_positional_audio(
  registry: Registry,
  id: String,
) -> Result(PositionalAudio, Nil) {
  case mutable_map.get(registry.objects, id) {
    Ok(ObjectEntry(object:, kind: AudioObject, ..)) ->
      Ok(savoiardi.object3d_to_positional_audio(object))
    _ -> Error(Nil)
  }
}

// TRANSFORM -------------------------------------------------------------------

/// Set an object's position.
pub fn set_position(
  registry: Registry,
  id: String,
  x: Float,
  y: Float,
  z: Float,
) -> Nil {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_position(entry.object, vec3.Vec3(x, y, z))
    Error(Nil) -> Nil
  }
}

/// Set an object's rotation (Euler angles in radians).
pub fn set_rotation(
  registry: Registry,
  id: String,
  x: Float,
  y: Float,
  z: Float,
) -> Nil {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_rotation(entry.object, vec3.Vec3(x, y, z))
    Error(Nil) -> Nil
  }
}

/// Set an object's rotation using quaternion components.
pub fn set_quaternion(
  registry: Registry,
  id: String,
  x: Float,
  y: Float,
  z: Float,
  w: Float,
) -> Nil {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_quaternion_xyzw(entry.object, x, y, z, w)
    Error(Nil) -> Nil
  }
}

/// Set an object's scale.
pub fn set_scale(
  registry: Registry,
  id: String,
  x: Float,
  y: Float,
  z: Float,
) -> Nil {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_scale(entry.object, vec3.Vec3(x, y, z))
    Error(Nil) -> Nil
  }
}

// ASPECT RATIO ----------------------------------------------------------------

/// Get the aspect ratio of this registry's renderer canvas.
/// Falls back to 16:9 if canvas dimensions are invalid.
pub fn get_renderer_aspect_ratio(registry: Registry) -> Float {
  let vec2.Vec2(x: width, y: height) =
    savoiardi.get_canvas_dimensions(registry.renderer)
  case height {
    0.0 -> 16.0 /. 9.0
    h -> width /. h
  }
}

// RESIZE ----------------------------------------------------------------------

/// Resize the renderer and update all perspective cameras.
pub fn resize(registry: Registry, width: Int, height: Int) -> Nil {
  savoiardi.set_renderer_size(registry.renderer, width, height)
  // Update all perspective cameras' aspect ratio
  let w = int.to_float(width)
  let h = int.to_float(height)
  let aspect = case h {
    0.0 -> 16.0 /. 9.0
    _ -> w /. h
  }
  mutable_map.fold(registry.cameras, Nil, fn(_, _id, entry) {
    case savoiardi.is_perspective_camera(entry.camera) {
      True -> {
        savoiardi.set_camera_aspect(entry.camera, aspect)
        savoiardi.update_camera_projection_matrix(entry.camera)
      }
      False -> Nil
    }
  })
}

// RENDER LOOP -----------------------------------------------------------------

/// Start the render loop for this registry. Returns registry with loop ID.
pub fn start_render_loop(registry: Registry) -> Registry {
  let loop_id = start_render_loop_ffi(registry, render_frame)
  Registry(..registry, render_loop_id: option.Some(loop_id))
}

/// Stop the render loop.
pub fn stop_render_loop(registry: Registry) -> Registry {
  case registry.render_loop_id {
    option.Some(id) -> {
      stop_render_loop_ffi(id)
      Registry(..registry, render_loop_id: option.None)
    }
    option.None -> registry
  }
}

/// The per-frame render callback. Called by requestAnimationFrame via FFI.
/// Finds the active camera and renders using savoiardi.
fn render_frame(registry: Registry) -> Nil {
  case find_active_camera(registry) {
    option.Some(camera) ->
      savoiardi.render(registry.renderer, registry.scene, camera)
    option.None -> {
      // Fall back to traversing the scene for any camera
      // (handled by savoiardi if needed, but we do nothing here
      //  since there's no camera to render with)
      Nil
    }
  }
}

// MODEL LOADING ---------------------------------------------------------------

/// Replace an existing object's 3D model with a newly loaded one.
/// Preserves position, rotation, scale, and visibility from the old object.
/// Updates the registry entry to point to the new object.
pub fn replace_object_model(
  registry: Registry,
  id: String,
  new_object: Object3D,
) -> Registry {
  case mutable_map.get(registry.objects, id) {
    Ok(entry) -> {
      let replaced = replace_object_model_ffi(entry.object, new_object, id)
      let _ =
        mutable_map.insert(
          registry.objects,
          id,
          ObjectEntry(..entry, object: replaced),
        )
      // Update DOM reference to point to the new object
      store_object_on_dom(id, replaced)
      registry
    }
    Error(Nil) -> registry
  }
}

// DOM EVENTS ------------------------------------------------------------------

/// Dispatch a custom event on a DOM element found by ID.
/// Used to notify Lustre apps about model load status.
pub fn dispatch_mesh_event(mesh_id: String, event_name: String) -> Nil {
  dispatch_mesh_event_ffi(mesh_id, event_name)
}

// PARENT RESOLUTION -----------------------------------------------------------

/// Resolve a parent ID to an Object3D.
/// If the parent_id matches the scene_id, returns the scene root.
/// Otherwise looks up the object in the registry, falling back to scene root.
fn resolve_parent(registry: Registry, parent_id: String) -> Object3D {
  case parent_id == registry.scene_id {
    True -> savoiardi.scene_to_object3d(registry.scene)
    False ->
      case mutable_map.get(registry.objects, parent_id) {
        Ok(entry) -> entry.object
        // Fallback to scene root
        Error(Nil) -> savoiardi.scene_to_object3d(registry.scene)
      }
  }
}

// FFI DECLARATIONS ------------------------------------------------------------

@external(javascript, "./registry.ffi.mjs", "startRenderLoop")
fn start_render_loop_ffi(
  registry: Registry,
  on_frame: fn(Registry) -> Nil,
) -> Int

@external(javascript, "./registry.ffi.mjs", "stopRenderLoop")
fn stop_render_loop_ffi(animation_id: Int) -> Nil

@external(javascript, "./registry.ffi.mjs", "dispatchMeshEvent")
fn dispatch_mesh_event_ffi(mesh_id: String, event_name: String) -> Nil

@external(javascript, "./registry.ffi.mjs", "appendCanvasToContainer")
pub fn append_canvas_to_container(container: a, canvas: b) -> Nil

@external(javascript, "./registry.ffi.mjs", "getDevicePixelRatio")
pub fn get_device_pixel_ratio() -> Float

@external(javascript, "./registry.ffi.mjs", "replaceObjectModel")
fn replace_object_model_ffi(
  old_object: Object3D,
  new_object: Object3D,
  name: String,
) -> Object3D

@external(javascript, "./registry.ffi.mjs", "storeObjectOnDom")
fn store_object_on_dom(mesh_id: String, object: Object3D) -> Nil

@external(javascript, "./registry.ffi.mjs", "clearObjectFromDom")
fn clear_object_from_dom(mesh_id: String) -> Nil
