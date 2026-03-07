//// Instance-scoped registry for Tiramisu web components.
////
//// Each `<tiramisu-scene>` owns its own Registry. All CRUD operations
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import savoiardi.{type Object3D, type Renderer, type Scene}
import tiramisu/internal/dom

// TYPES -----------------------------------------------------------------------

/// Metadata about a registered object.
pub type Entry {
  ObjectEntry(parent_id: String, tag: String, object: Object3D)
}

/// Instance-scoped registry. One per `<tiramisu-renderer>`.
pub type Registry {
  Registry(
    scene: Scene,
    scene_id: String,
    renderer: Renderer,
    objects: Dict(String, Entry),
    id_counter: Int,
  )
}

// CONSTRUCTOR -----------------------------------------------------------------

/// Create a new empty registry for a renderer instance.
pub fn new(scene: Scene, scene_id: String, renderer: Renderer) -> Registry {
  Registry(scene:, scene_id:, renderer:, objects: dict.new(), id_counter: 0)
}

// OBJECT CRUD -----------------------------------------------------------------

/// Get an Object3D by its ID.
pub fn get_object(
  registry: Registry,
  id: String,
) -> Result(#(Object3D, String), Nil) {
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(object:, parent_id:, ..)) -> Ok(#(object, parent_id))
    Error(Nil) -> Error(Nil)
  }
}

/// Register an object and add it to the scene graph under its parent.
/// Sets the object's name via savoiardi for debugging in Three.js inspector.
pub fn register_and_add_object(
  registry: Registry,
  id: String,
  object: Object3D,
  parent_id parent_id: String,
  tag tag: String,
) -> Registry {
  savoiardi.set_object_name(object, id)
  let parent = resolve_parent(registry, parent_id)
  savoiardi.add_child(parent:, child: object)
  let objects =
    dict.insert(registry.objects, id, ObjectEntry(object:, parent_id:, tag:))
  // Store on DOM element so external integrations (cacao physics) can find it
  dom.store_object_on_dom(id, object)
  Registry(..registry, objects: objects)
}

/// Remove an object from the scene graph and dispose its resources.
pub fn remove_object(
  registry: Registry,
  id: String,
  parent_id: String,
  object: Object3D,
) -> Registry {
  // Remove from Three.js scene graph
  let parent = resolve_parent(registry, parent_id)
  savoiardi.remove_child(parent, object)
  // Dispose resources
  savoiardi.dispose_object(object)
  // Remove from maps
  let objects = dict.delete(registry.objects, id)
  // Clear DOM reference
  dom.clear_object_from_dom(id)
  Registry(..registry, objects:)
}

@internal
pub fn reparent_object(
  registry: Registry,
  id: String,
  parent_id: String,
) -> Registry {
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(object: child, ..) as entry) -> {
      let parent = resolve_parent(registry, parent_id:)
      savoiardi.add_child(parent:, child:)
      // Update the entry's parent_id
      let objects =
        dict.insert(registry.objects, id, ObjectEntry(..entry, parent_id:))
      Registry(..registry, objects:)
    }
    Error(Nil) -> registry
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
  case dict.get(registry.objects, id) {
    Ok(ObjectEntry(object:, ..) as entry) -> {
      let replaced = savoiardi.replace_object_model(object, new_object, id)
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
    _ -> registry
  }
}

// PARENT RESOLUTION -----------------------------------------------------------

/// Resolve a parent ID to an Object3D.
/// If the parent_id matches the scene_id, returns the scene root.
/// Otherwise looks up the object in the registry, falling back to scene root.
fn resolve_parent(registry: Registry, parent_id parent_id: String) -> Object3D {
  case parent_id == registry.scene_id {
    True -> savoiardi.scene_to_object3d(registry.scene)
    False ->
      case get_object(registry, parent_id) {
        Ok(#(object, _)) -> object
        // Fallback to scene root
        Error(Nil) -> savoiardi.scene_to_object3d(registry.scene)
      }
  }
}
