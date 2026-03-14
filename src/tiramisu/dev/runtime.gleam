//// Instance-scoped runtime for Tiramisu web components.
////
//// Each `<tiramisu-renderer>` owns one runtime value containing the root scene,
//// renderer, active camera, and registered objects.
////
//// This module is intended for extension authors. It exposes the capabilities an
//// extension needs to interact with the runtime without exposing the runtime's
//// internal storage layout.

// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import savoiardi.{type Camera, type Object3D, type Renderer, type Scene}
import tiramisu/internal/element

/// Metadata about a registered object.
@internal
pub type Entry {
  ObjectEntry(parent_id: String, tag: String, object: Object3D)
}

/// Instance-scoped runtime. One per `<tiramisu-renderer>`.
pub opaque type Runtime {
  Runtime(
    scene: Scene,
    scene_id: String,
    threejs_renderer: Renderer,
    active_camera: Option(#(String, Camera)),
    objects: Dict(String, Entry),
  )
}

/// Create a new empty runtime for a renderer instance.
@internal
pub fn new(
  scene: Scene,
  scene_id: String,
  threejs_renderer: Renderer,
) -> Runtime {
  Runtime(
    scene:,
    scene_id:,
    threejs_renderer:,
    active_camera: None,
    objects: dict.new(),
  )
}

/// Get the Three.js renderer owned by this runtime.
pub fn threejs_renderer(runtime: Runtime) -> Renderer {
  runtime.threejs_renderer
}

/// Get the root scene owned by this runtime.
pub fn scene(runtime: Runtime) -> Scene {
  runtime.scene
}

/// Get all registered node entries.
@internal
pub fn entries(runtime: Runtime) -> Dict(String, Entry) {
  runtime.objects
}

/// Get the currently active camera, if one is set.
pub fn active_camera(runtime: Runtime) -> Result(Camera, Nil) {
  case runtime.active_camera {
    Some(#(_, camera)) -> Ok(camera)
    None -> Error(Nil)
  }
}

// OBJECT OPERATIONS -----------------------------------------------------------

/// Get a registered object by its node id.
///
/// Extension authors should prefer this over inspecting internal entries.
pub fn object(runtime: Runtime, id: String) -> Result(Object3D, Nil) {
  case dict.get(runtime.objects, id) {
    Ok(ObjectEntry(object:, ..)) -> Ok(object)
    Error(Nil) -> Error(Nil)
  }
}

/// Get a registered node entry by id.
@internal
pub fn find_entry(runtime: Runtime, id: String) -> Result(Entry, Nil) {
  case dict.get(runtime.objects, id) {
    Ok(entry) -> Ok(entry)
    Error(Nil) -> Error(Nil)
  }
}

/// Register an object and add it to the scene graph under its parent.
/// Sets the object's name via savoiardi for debugging in Three.js inspector.
pub fn add_object(
  runtime: Runtime,
  id: String,
  object object: Object3D,
  parent_id parent_id: String,
  tag tag: String,
) -> Runtime {
  savoiardi.set_object_name(object, id)
  let parent = resolve_parent(runtime, parent_id)
  savoiardi.add_child(parent:, child: object)
  let objects =
    dict.insert(runtime.objects, id, ObjectEntry(object:, parent_id:, tag:))
  // Store on DOM element so external integrations (cacao physics) can find it
  let _ = element.store_object(id, object)
  Runtime(..runtime, objects:)
}

/// Remove an object from the scene graph and dispose its resources.
pub fn remove_object(
  runtime: Runtime,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Runtime {
  // Remove from Three.js scene graph
  let parent = resolve_parent(runtime, parent_id)
  savoiardi.remove_child(parent, object)
  // Dispose resources
  savoiardi.dispose_object(object)
  // Remove from maps
  let objects = dict.delete(runtime.objects, id)
  // Clear DOM reference
  let _ = element.clear_object(id)
  Runtime(..runtime, objects:)
}

/// Move an existing object under a new parent.
pub fn reparent_object(
  runtime: Runtime,
  id: String,
  parent_id: String,
) -> Runtime {
  case dict.get(runtime.objects, id) {
    Ok(ObjectEntry(object: child, ..) as entry) -> {
      let parent = resolve_parent(runtime, parent_id:)
      savoiardi.add_child(parent:, child:)
      // Update the entry's parent_id
      let objects =
        dict.insert(runtime.objects, id, ObjectEntry(..entry, parent_id:))
      Runtime(..runtime, objects:)
    }
    Error(Nil) -> runtime
  }
}

// MODEL LOADING ---------------------------------------------------------------

/// Replace an existing object's 3D model with a newly loaded one.
/// Preserves position, rotation, scale, and visibility from the old object.
/// Updates the registry entry to point to the new object.
pub fn replace_object(
  runtime: Runtime,
  id: String,
  new_object: Object3D,
) -> Runtime {
  case dict.get(runtime.objects, id) {
    Ok(ObjectEntry(object:, ..) as entry) -> {
      let replaced = savoiardi.replace_object_model(object, new_object, id)
      let objects =
        dict.insert(runtime.objects, id, ObjectEntry(..entry, object: replaced))
      // Update DOM reference to point to the new object
      let _ = element.store_object(id, replaced)
      Runtime(..runtime, objects:)
    }
    _ -> runtime
  }
}

/// Mark a camera as the active camera for the runtime.
pub fn activate_camera(runtime: Runtime, id: String, camera: Camera) -> Runtime {
  Runtime(..runtime, active_camera: Some(#(id, camera)))
}

/// Clear the active camera if the given node currently owns it.
pub fn deactivate_camera(runtime: Runtime, id: String) -> Runtime {
  case runtime.active_camera {
    Some(#(active_id, _)) if active_id == id ->
      Runtime(..runtime, active_camera: None)

    _ -> runtime
  }
}

// PARENT RESOLUTION -----------------------------------------------------------

/// Resolve a parent ID to an Object3D.
/// If the parent_id matches the scene_id, returns the scene root.
/// Otherwise looks up the object in the registry, falling back to scene root.
fn resolve_parent(runtime: Runtime, parent_id parent_id: String) -> Object3D {
  case parent_id == runtime.scene_id {
    True -> savoiardi.scene_to_object3d(runtime.scene)
    False ->
      case find_entry(runtime, parent_id) {
        Ok(entry) -> entry.object
        // Fallback to scene root
        Error(Nil) -> savoiardi.scene_to_object3d(runtime.scene)
      }
  }
}
