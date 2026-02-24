//// Instance-scoped registry for Tiramisu web components.
////
//// Each `<tiramisu-renderer>` owns its own Registry. All CRUD operations
//// use Gleam's stdlib Dict — no global state, no collisions between
//// multiple renderer instances on the same page.
////
//// Three.js operations go through savoiardi. Only browser APIs
//// (DOM events, DOM container manipulation) remain in FFI.
////
//// The Registry is purely functional — it lives as `Option(Registry)` in
//// the Lustre Model and is updated through Lustre's message dispatch cycle.
//// Per-frame mutable state (active camera) is owned by the RenderLoop,
//// not the Registry.

// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option}
import quaternion
import savoiardi.{
  type Audio, type AudioListener, type Camera, type InstancedMesh, type Light,
  type Material, type Object3D, type PositionalAudio, type Renderer, type Scene,
}
import tiramisu/internal/dom
import tiramisu/transform
import vec/vec2
import vec/vec3

// TYPES -----------------------------------------------------------------------

/// Metadata about a registered object.
pub type ObjectEntry {
  ObjectEntry(object: Object3D, parent_id: String, kind: ObjectKind)
}

/// What kind of object is registered (the element's tag name).
pub type ObjectKind {
  ObjectKind(tag: String)
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
    objects: Dict(String, ObjectEntry),
    cameras: Dict(String, CameraEntry),
    audio_listeners: Dict(String, AudioListener),
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
    objects: dict.new(),
    cameras: dict.new(),
    audio_listeners: dict.new(),
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
  case dict.get(registry.objects, id) {
    Ok(entry) -> Ok(entry.object)
    Error(Nil) -> Error(Nil)
  }
}

/// Get the full ObjectEntry by ID.
pub fn get_object_entry(
  registry: Registry,
  id: String,
) -> Result(ObjectEntry, Nil) {
  dict.get(registry.objects, id)
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
  let objects =
    dict.insert(registry.objects, id, ObjectEntry(object:, parent_id:, kind:))
  // Store on DOM element so external integrations (cacao physics) can find it
  dom.store_object_on_dom(id, object)
  Registry(..registry, objects: objects)
}

/// Remove an object from the scene graph and dispose its resources.
pub fn remove_object(registry: Registry, id: String) -> Registry {
  case dict.get(registry.objects, id) {
    Ok(entry) -> {
      // Remove from Three.js scene graph
      let parent = resolve_parent(registry, entry.parent_id)
      savoiardi.remove_child(parent, entry.object)
      // Dispose resources
      savoiardi.dispose_object(entry.object)
      // Remove from maps
      let objects = dict.delete(registry.objects, id)
      let cameras = dict.delete(registry.cameras, id)
      // Clear DOM reference
      dom.clear_object_from_dom(id)
      Registry(..registry, objects:, cameras:)
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
  case dict.get(registry.objects, id) {
    Ok(entry) -> {
      let new_parent = resolve_parent(registry, new_parent_id)
      // Three.js add() auto-removes from old parent
      savoiardi.add_child(parent: new_parent, child: entry.object)
      // Update the entry's parent_id
      let objects =
        dict.insert(
          registry.objects,
          id,
          ObjectEntry(..entry, parent_id: new_parent_id),
        )
      Registry(..registry, objects:)
    }
    Error(Nil) -> registry
  }
}

/// Set an object's visibility.
pub fn set_visible(registry: Registry, id: String, visible: Bool) -> Nil {
  case dict.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_visible(entry.object, visible)
    Error(Nil) -> Nil
  }
}

/// Set castShadow/receiveShadow on a mesh via the registry.
pub fn set_mesh_shadow(
  reg: Registry,
  id: String,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> Nil {
  case get_object(reg, id) {
    Ok(object) -> savoiardi.enable_shadows(object, cast_shadow, receive_shadow)
    Error(Nil) -> Nil
  }
}

// CAMERA CRUD -----------------------------------------------------------------

/// Get a Camera by its ID.
pub fn get_camera(registry: Registry, id: String) -> Result(Camera, Nil) {
  case dict.get(registry.cameras, id) {
    Ok(entry) -> Ok(entry.camera)
    Error(Nil) -> Error(Nil)
  }
}

/// Get the full CameraEntry by ID.
pub fn get_camera_entry(
  registry: Registry,
  id: String,
) -> Result(CameraEntry, Nil) {
  dict.get(registry.cameras, id)
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
  let cameras =
    dict.insert(registry.cameras, id, CameraEntry(camera:, parent_id:, active:))
  // Also store in objects map so transforms work
  let obj = savoiardi.camera_to_object3d(camera)
  let objects =
    dict.insert(
      registry.objects,
      id,
      ObjectEntry(object: obj, parent_id:, kind: ObjectKind("tiramisu-camera")),
    )
  dom.store_object_on_dom(id, obj)
  Registry(..registry, cameras: cameras, objects: objects)
}

/// Set whether a camera is active.
pub fn set_camera_active(
  registry: Registry,
  id: String,
  active: Bool,
) -> Registry {
  case dict.get(registry.cameras, id) {
    Ok(entry) -> {
      let cameras =
        dict.insert(registry.cameras, id, CameraEntry(..entry, active: active))
      Registry(..registry, cameras: cameras)
    }
    Error(Nil) -> registry
  }
}

/// Find the active camera for this registry's scene.
pub fn find_active_camera(registry: Registry) -> Option(Camera) {
  dict.fold(registry.cameras, option.None, fn(acc, _id, entry) {
    case entry.active {
      True -> option.Some(entry.camera)
      False -> acc
    }
  })
}

// LIGHT HELPERS ---------------------------------------------------------------

/// Get a Light by its ID. Returns Some only if the object's kind is a light.
pub fn get_light(registry: Registry, id: String) -> Option(Light) {
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(object:, kind: ObjectKind(tag: "tiramisu-light"), ..)) ->
      option.Some(savoiardi.object3d_to_light(object))
    _ -> option.None
  }
}

// MATERIAL HELPERS ------------------------------------------------------------

/// Get a material from a registered object.
pub fn get_material(registry: Registry, id: String) -> Result(Material, Nil) {
  case dict.get(registry.objects, id) {
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
  let audio_listeners =
    dict.insert(registry.audio_listeners, registry.scene_id, listener)
  Registry(..registry, audio_listeners:)
}

/// Get the stored AudioListener for this scene.
pub fn get_audio_listener(registry: Registry) -> Result(AudioListener, Nil) {
  dict.get(registry.audio_listeners, registry.scene_id)
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
    ObjectKind("tiramisu-global-audio"),
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
    ObjectKind("tiramisu-positional-audio"),
  )
}

/// Get an Audio object from the registry by ID.
/// Audio extends Object3D so we retrieve it and cast.
pub fn get_audio(registry: Registry, id: String) -> Result(Audio, Nil) {
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(object:, kind: ObjectKind(tag: "tiramisu-global-audio"), ..)) ->
      Ok(savoiardi.object3d_to_audio(object))
    _ -> Error(Nil)
  }
}

/// Get a PositionalAudio object from the registry by ID.
pub fn get_positional_audio(
  registry: Registry,
  id: String,
) -> Result(PositionalAudio, Nil) {
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(
      object:,
      kind: ObjectKind(tag: "tiramisu-positional-audio"),
      ..,
    )) -> Ok(savoiardi.object3d_to_positional_audio(object))
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
  case dict.get(registry.objects, id) {
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
  case dict.get(registry.objects, id) {
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
  case dict.get(registry.objects, id) {
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
  case dict.get(registry.objects, id) {
    Ok(entry) -> savoiardi.set_object_scale(entry.object, vec3.Vec3(x, y, z))
    Error(Nil) -> Nil
  }
}

/// Apply a full transform (position, rotation, scale) to an object.
pub fn set_transform(reg: Registry, id: String, t: transform.Transform) -> Nil {
  let vec3.Vec3(px, py, pz) = t.position
  let quaternion.Quaternion(qx, qy, qz, qw) = t.rotation
  let vec3.Vec3(sx, sy, sz) = t.scale

  set_position(reg, id, px, py, pz)
  set_quaternion(reg, id, qx, qy, qz, qw)
  set_scale(reg, id, sx, sy, sz)
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

// CSS OBJECT ACCESSORS --------------------------------------------------------

/// Get an InstancedMesh from the registry by ID.
pub fn get_instanced_mesh(
  registry: Registry,
  id: String,
) -> Result(InstancedMesh, Nil) {
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(
      object:,
      kind: ObjectKind(tag: "tiramisu-instanced-mesh"),
      ..,
    )) -> Ok(savoiardi.object3d_to_instanced_mesh(object))
    _ -> Error(Nil)
  }
}

/// Set how many instances are rendered (must be <= max capacity).
pub fn set_instanced_mesh_count(mesh: InstancedMesh, count: Int) -> Nil {
  savoiardi.set_instanced_mesh_count(mesh, count)
}

/// Get the max capacity of an InstancedMesh (the count passed to constructor).
pub fn get_instanced_mesh_max_count(mesh: InstancedMesh) -> Int {
  savoiardi.get_instanced_mesh_max_count(mesh)
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
  let _ =
    dict.fold(registry.cameras, Nil, fn(_, _id, entry) {
      case savoiardi.is_perspective_camera(entry.camera) {
        True -> {
          savoiardi.set_camera_aspect(entry.camera, aspect)
          savoiardi.update_camera_projection_matrix(entry.camera)
        }
        False -> Nil
      }
    })
  Nil
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
  case dict.get(registry.objects, id) {
    Ok(entry) -> {
      let replaced =
        savoiardi.replace_object_model(entry.object, new_object, id)
      let objects =
        dict.insert(
          registry.objects,
          id,
          ObjectEntry(..entry, object: replaced),
        )
      // Update DOM reference to point to the new object
      dom.store_object_on_dom(id, replaced)
      Registry(..registry, objects:)
    }
    Error(Nil) -> registry
  }
}

// DOM EVENTS ------------------------------------------------------------------

/// Dispatch a custom event on a DOM element found by ID.
/// Used to notify Lustre apps about model load status.
pub fn dispatch_mesh_event(mesh_id: String, event_name: String) -> Nil {
  dom.dispatch_mesh_event(mesh_id, event_name)
}

// PARENT RESOLUTION -----------------------------------------------------------

/// Resolve a parent ID to an Object3D.
/// If the parent_id matches the scene_id, returns the scene root.
/// Otherwise looks up the object in the registry, falling back to scene root.
fn resolve_parent(registry: Registry, parent_id: String) -> Object3D {
  case parent_id == registry.scene_id {
    True -> savoiardi.scene_to_object3d(registry.scene)
    False ->
      case dict.get(registry.objects, parent_id) {
        Ok(entry) -> entry.object
        // Fallback to scene root
        Error(Nil) -> savoiardi.scene_to_object3d(registry.scene)
      }
  }
}
