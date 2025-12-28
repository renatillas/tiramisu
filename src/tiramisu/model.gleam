////
//// 3D Model loading and manipulation.
////
//// This module provides functions for loading 3D models in various formats
//// (GLTF, OBJ, FBX) and extracting their components like scenes and animations.
////
//// ## Supported Formats
////
//// - **GLTF/GLB** (recommended) - Modern, efficient, well-supported format
//// - **OBJ** - Simple mesh format, no animations
//// - **FBX** - Common in game development, supports animations
////
//// ## Example
////
//// ```gleam
//// import tiramisu/model
//// import gleam/javascript/promise
////
//// // Load a GLTF model
//// use result <- promise.await(model.load_gltf("/models/character.glb"))
//// case result {
////   Ok(gltf) -> {
////     // Get the scene (root Object3D)
////     let scene = model.get_scene(gltf)
////
////     // Get animations if any
////     let clips = model.get_animations(gltf)
////   }
////   Error(Nil) -> io.println("Failed to load model")
//// }
//// ```
////

import gleam/javascript/promise
import savoiardi
import tiramisu/effect
import tiramisu/scene

// ============================================================================
// TYPES
// ============================================================================

/// Loaded GLTF/GLB model data.
///
/// Contains the scene graph, animations, cameras, and other assets.
/// GLTF is the recommended format for 3D models on the web.
pub type GLTFData =
  savoiardi.GLTFData

/// Loaded FBX model data.
///
/// Contains the scene graph and animations. FBX is commonly used
/// in game development tools like Unity and Unreal Engine.
pub type FBXData =
  savoiardi.FBXData

pub fn load_gltf(
  from url: String,
  on_success on_success: fn(GLTFData) -> msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  let promise =
    savoiardi.load_gltf(url)
    |> promise.map(fn(result) {
      case result {
        Ok(data) -> on_success(data)
        Error(Nil) -> on_error
      }
    })

  effect.from_promise(promise)
}

pub fn load_obj(
  from url: String,
  on_success on_success: fn(scene.Object3D) -> msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  let promise =
    savoiardi.load_obj(url)
    |> promise.map(fn(result) {
      case result {
        Ok(data) -> on_success(data)
        Error(Nil) -> on_error
      }
    })

  effect.from_promise(promise)
}

pub fn load_fbx(
  from url: String,
  on_success on_success: fn(FBXData) -> msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  let promise =
    savoiardi.load_fbx(url)
    |> promise.map(fn(result) {
      case result {
        Ok(data) -> on_success(data)
        Error(Nil) -> on_error
      }
    })

  effect.from_promise(promise)
}

// ============================================================================
// GLTF DATA EXTRACTION
// ============================================================================

/// Get the root scene (Object3D) from loaded GLTF data.
///
/// This is the main 3D object hierarchy that you can add to your scene.
///
/// ## Example
///
/// ```gleam
/// let scene = model.get_scene(gltf_data)
/// // Use with scene.model() node
/// ```
@external(javascript, "../model.ffi.mjs", "getGLTFScene")
pub fn get_scene(gltf: GLTFData) -> savoiardi.Object3D

/// Get all animation clips from loaded GLTF data.
///
/// Returns an empty list if the model has no animations.
///
/// ## Example
///
/// ```gleam
/// let clips = model.get_animations(gltf_data)
/// // Use clips with animation.create_mixer() and animation.play()
/// ```
@external(javascript, "../model.ffi.mjs", "getGLTFAnimations")
pub fn get_animations(gltf: GLTFData) -> List(savoiardi.AnimationClip)

/// Get all cameras from loaded GLTF data.
///
/// GLTF files can include camera definitions. Returns an empty list
/// if the model has no cameras.
@external(javascript, "../model.ffi.mjs", "getGLTFCameras")
pub fn get_cameras(gltf: GLTFData) -> List(savoiardi.Object3D)

// ============================================================================
// FBX DATA EXTRACTION
// ============================================================================

/// Get the root scene (Object3D) from loaded FBX data.
///
/// FBX data is itself an Object3D (THREE.Group), so this just returns it.
///
/// ## Example
///
/// ```gleam
/// let scene = model.get_fbx_scene(fbx_data)
/// // Use with scene.model() node
/// ```
@external(javascript, "../model.ffi.mjs", "getFBXScene")
pub fn get_fbx_scene(fbx: FBXData) -> savoiardi.Object3D

/// Get all animation clips from loaded FBX data.
///
/// Returns an empty list if the model has no animations.
///
/// ## Example
///
/// ```gleam
/// let clips = model.get_fbx_animations(fbx_data)
/// // Use clips with animation.create_mixer() and animation.play()
/// ```
@external(javascript, "../model.ffi.mjs", "getFBXAnimations")
pub fn get_fbx_animations(fbx: FBXData) -> List(savoiardi.AnimationClip)
