/// Internal object cache management
///
/// This module manages all Three.js objects created by the renderer.
/// It maintains several caches:
/// - Main object cache: Maps IDs to Three.js objects (meshes, lights, groups, etc.)
/// - Animation mixers: Maps IDs to Three.js AnimationMixer instances
/// - Animation actions: Maps IDs to active animation actions
/// - Camera viewports: Maps camera IDs to viewport configurations
/// - Particle systems: Maps IDs to particle system data
///
/// All caches use string IDs directly.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/set.{type Set}
import savoiardi
import tiramisu/camera

pub type AnimationAction =
  savoiardi.AnimationAction

pub type AnimationMixer =
  savoiardi.AnimationMixer

/// Actions can be single or blended (array of 2)
pub type AnimationActions {
  SingleAction(AnimationAction)
  BlendedActions(from: AnimationAction, to: AnimationAction)
}

/// Stored animation state for diffing (clip names to detect changes)
pub type AnimationState {
  SingleState(clip_name: String)
  BlendedState(from_clip_name: String, to_clip_name: String)
}

/// Complete cache state
pub type CacheState {
  CacheState(
    /// Main cache of Three.js objects by ID
    objects: Dict(String, savoiardi.Object3D),
    /// Animation mixers by node ID
    mixers: Dict(String, AnimationMixer),
    /// Current animation actions by node ID
    actions: Dict(String, AnimationActions),
    /// Current animation state by node ID (for diffing clip names)
    animation_states: Dict(String, AnimationState),
    /// Camera viewport configurations by camera ID
    viewports: Dict(String, camera.ViewPort),
    /// Particle systems by node ID
    /// Camera postprocessing configurations by camera ID
    camera_postprocessing: Dict(String, camera.PostProcessing),
    /// Set of camera IDs (to distinguish cameras from other objects)
    cameras: Set(String),
    /// Currently active camera ID (for main rendering)
    active_camera: Option(String),
  )
}

// ============================================================================
// INITIALIZATION
// ============================================================================

/// Create an empty cache state
pub fn init() -> CacheState {
  CacheState(
    objects: dict.new(),
    mixers: dict.new(),
    actions: dict.new(),
    animation_states: dict.new(),
    viewports: dict.new(),
    camera_postprocessing: dict.new(),
    cameras: set.new(),
    active_camera: option.None,
  )
}

// ============================================================================
// OBJECT CACHE OPERATIONS
// ============================================================================

/// Add a Three.js object to the cache
pub fn add_object(
  cache: CacheState,
  id,
  object: savoiardi.Object3D,
) -> CacheState {
  // id is already a string
  CacheState(..cache, objects: dict.insert(cache.objects, id, object))
}

/// Get a Three.js object from the cache
pub fn get_object(cache: CacheState, id) -> Result(savoiardi.Object3D, Nil) {
  dict.get(cache.objects, id)
}

/// Remove a Three.js object from the cache
pub fn remove_object(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(..cache, objects: dict.delete(cache.objects, id))
}

/// Get all cached objects as a list of (ID, Object) tuples
pub fn get_all_objects(cache: CacheState) -> List(#(String, savoiardi.Object3D)) {
  dict.to_list(cache.objects)
}

// ============================================================================
// ANIMATION MIXER OPERATIONS
// ============================================================================

/// Add an animation mixer to the cache
pub fn add_mixer(cache: CacheState, id, mixer: AnimationMixer) -> CacheState {
  // id is already a string
  CacheState(..cache, mixers: dict.insert(cache.mixers, id, mixer))
}

/// Get an animation mixer from the cache
pub fn get_mixer(cache: CacheState, id) -> Option(AnimationMixer) {
  // id is already a string
  dict.get(cache.mixers, id) |> option.from_result
}

/// Remove an animation mixer from the cache
pub fn remove_mixer(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(..cache, mixers: dict.delete(cache.mixers, id))
}

/// Get all mixers as a list
pub fn get_all_mixers(cache: CacheState) -> List(#(String, AnimationMixer)) {
  dict.to_list(cache.mixers)
}

// ============================================================================
// ANIMATION ACTION OPERATIONS
// ============================================================================

/// Set the current animation actions for a node
pub fn set_actions(
  cache: CacheState,
  id: String,
  actions: AnimationActions,
) -> CacheState {
  // id is already a string
  CacheState(..cache, actions: dict.insert(cache.actions, id, actions))
}

/// Get the current animation actions for a node
pub fn get_actions(cache: CacheState, id) -> Option(AnimationActions) {
  // id is already a string
  dict.get(cache.actions, id) |> option.from_result
}

/// Remove animation actions for a node
pub fn remove_actions(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(..cache, actions: dict.delete(cache.actions, id))
}

// ============================================================================
// ANIMATION STATE OPERATIONS
// ============================================================================

/// Set the animation state for a node (for diffing clip names)
pub fn set_animation_state(
  cache: CacheState,
  id: String,
  state: AnimationState,
) -> CacheState {
  CacheState(
    ..cache,
    animation_states: dict.insert(cache.animation_states, id, state),
  )
}

/// Get the animation state for a node
pub fn get_animation_state(
  cache: CacheState,
  id: String,
) -> Option(AnimationState) {
  dict.get(cache.animation_states, id) |> option.from_result
}

/// Remove animation state for a node
pub fn remove_animation_state(cache: CacheState, id: String) -> CacheState {
  CacheState(..cache, animation_states: dict.delete(cache.animation_states, id))
}

// ============================================================================
// VIEWPORT OPERATIONS
// ============================================================================

/// Set viewport configuration for a camera
pub fn set_viewport(
  cache: CacheState,
  id,
  viewport: camera.ViewPort,
) -> CacheState {
  // id is already a string
  CacheState(..cache, viewports: dict.insert(cache.viewports, id, viewport))
}

/// Get viewport configuration for a camera
pub fn get_viewport(cache: CacheState, id) -> Option(camera.ViewPort) {
  // id is already a string
  dict.get(cache.viewports, id) |> option.from_result
}

/// Remove viewport configuration for a camera
pub fn remove_viewport(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(..cache, viewports: dict.delete(cache.viewports, id))
}

/// Get all cameras with viewports
pub fn get_cameras_with_viewports(
  cache: CacheState,
) -> List(#(savoiardi.Object3D, camera.ViewPort)) {
  dict.to_list(cache.viewports)
  |> list.filter_map(fn(entry) {
    let #(id, viewport) = entry
    case dict.get(cache.objects, id) {
      Ok(camera_obj) -> Ok(#(camera_obj, viewport))
      Error(_) -> Error(Nil)
    }
  })
}

// ============================================================================
// CAMERA POSTPROCESSING OPERATIONS
// ============================================================================

/// Set postprocessing configuration for a camera
pub fn set_camera_postprocessing(
  cache: CacheState,
  id: String,
  pp: camera.PostProcessing,
) -> CacheState {
  // id is already a string
  CacheState(
    ..cache,
    camera_postprocessing: dict.insert(cache.camera_postprocessing, id, pp),
  )
}

/// Get postprocessing configuration for a camera
pub fn get_camera_postprocessing(
  cache: CacheState,
  id: String,
) -> Result(camera.PostProcessing, Nil) {
  // id is already a string
  dict.get(cache.camera_postprocessing, id)
}

/// Remove postprocessing configuration for a camera
pub fn remove_camera_postprocessing(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(
    ..cache,
    camera_postprocessing: dict.delete(cache.camera_postprocessing, id),
  )
}

// ============================================================================
// CAMERA TRACKING OPERATIONS
// ============================================================================

/// Add a camera ID to the cameras set
pub fn add_camera(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(..cache, cameras: set.insert(cache.cameras, id))
}

/// Remove a camera ID from the cameras set
pub fn remove_camera(cache: CacheState, id) -> CacheState {
  // id is already a string
  CacheState(..cache, cameras: set.delete(cache.cameras, id))
}

/// Set the active camera ID
pub fn set_active_camera(cache: CacheState, id: String) -> CacheState {
  CacheState(..cache, active_camera: option.Some(id))
}

/// Get the active camera ID
pub fn get_active_camera(cache: CacheState) -> Option(String) {
  cache.active_camera
}

/// Get all cameras with their postprocessing configurations
/// Returns list of tuples: (camera_id_string, camera_object, Option(viewport), Option(postprocessing), is_active)
pub fn get_all_cameras_with_info(
  cache: CacheState,
) -> List(
  #(
    String,
    savoiardi.Object3D,
    Option(camera.ViewPort),
    Option(camera.PostProcessing),
    Bool,
  ),
) {
  // Iterate over camera IDs and look up their info
  set.to_list(cache.cameras)
  |> list.filter_map(fn(camera_id) {
    // Get camera object
    case dict.get(cache.objects, camera_id) {
      Ok(camera_obj) -> {
        // Get viewport and postprocessing (both optional)
        let viewport_opt =
          dict.get(cache.viewports, camera_id) |> option.from_result
        let pp_opt =
          dict.get(cache.camera_postprocessing, camera_id)
          |> option.from_result
        // Check if this is the active camera
        let is_active = cache.active_camera == option.Some(camera_id)

        Ok(#(camera_id, camera_obj, viewport_opt, pp_opt, is_active))
      }
      Error(_) -> Error(Nil)
    }
  })
}

// ============================================================================
// CLEANUP OPERATIONS
// ============================================================================

/// Remove all cached data for a given ID (object, mixer, actions, animation_state, viewport, particles, camera, postprocessing)
/// This is used when a node is removed from the scene
pub fn remove_all(cache: CacheState, id: String) -> CacheState {
  cache
  |> remove_object(id)
  |> remove_mixer(id)
  |> remove_actions(id)
  |> remove_animation_state(id)
  |> remove_viewport(id)
  |> remove_camera(id)
  |> remove_camera_postprocessing(id)
}
