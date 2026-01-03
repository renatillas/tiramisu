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

import gleam/float
import gleam/javascript/promise
import gleam/time/duration
import savoiardi
import tiramisu/effect
import tiramisu/texture

// ============================================================================
// TYPES
// ============================================================================

pub type Object3D =
  savoiardi.Object3D

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

pub type Clip =
  savoiardi.AnimationClip

/// Controls how an animation loops.
pub type LoopMode {
  /// Play the animation once and stop at the end
  LoopOnce
  /// Play the animation repeatedly in a loop
  LoopRepeat
}

/// Configuration for a single animation clip.
///
/// Use `new_animation()` to create an animation with defaults, then use the builder
/// functions (`set_loop()`, `set_speed()`, etc.) to configure it.
pub type Animation {
  Animation(
    clip: savoiardi.AnimationClip,
    loop: LoopMode,
    /// Playback speed multiplier (1.0 = normal, 2.0 = double speed, 0.5 = half speed)
    speed: Float,
    /// Animation weight for blending (0.0 to 1.0, where 1.0 = full influence)
    weight: Float,
  )
}

/// Animation playback configuration for a Model scene node.
///
/// You can play a single animation or blend between two animations for smooth transitions.
pub type AnimationPlayback {
  /// Play a single animation
  SingleAnimation(Animation)
  /// Blend between two animations with a blend factor (0.0 = fully 'from', 1.0 = fully 'to')
  BlendedAnimations(from: Animation, to: Animation, blend_factor: Float)
}

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
  on_success on_success: fn(Object3D) -> msg,
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

// ============================================================================
// OBJECT3D MANIPULATION
// ============================================================================

/// Center an Object3D so its geometric center is at the origin.
///
/// Computes the bounding box of the entire object hierarchy and adjusts
/// all children's positions so the center of the bounding box is at (0, 0, 0).
///
/// This is useful for loaded models (FBX, GLTF, OBJ) where the origin may not
/// be at the geometric center of the mesh.
///
/// ## Example
///
/// ```gleam
/// let fbx_data = model.get_fbx_scene(loaded_fbx)
/// let centered = model.center_object(fbx_data)
/// // Now the model's center is at (0, 0, 0)
/// ```
///
/// ## Notes
///
/// - This mutates the object in place and returns it for convenience
/// - The children's positions are adjusted, not the root object's position
pub fn center_object(object: Object3D) -> Object3D {
  savoiardi.center_object(object)
}

/// Apply a texture to all meshes in an Object3D hierarchy.
///
/// This is useful for loaded models that reference external textures,
/// or when you want to override the model's textures.
///
/// ## Parameters
///
/// - `object` - The object hierarchy to apply the texture to
/// - `tex` - The texture to apply
/// - `filter_mode` - Texture filtering mode for pixel art or smooth textures
///
/// ## Example
///
/// ```gleam
/// let floor = model.get_fbx_scene(loaded_fbx)
/// model.apply_texture(floor, dungeon_texture, texture.NearestFilter)
/// ```
pub fn apply_texture(
  object: Object3D,
  tex: texture.Texture,
  filter_mode: texture.FilterMode,
) -> Nil {
  let savoiardi_filter = case filter_mode {
    texture.NearestFilter -> savoiardi.NearestFilter
    texture.LinearFilter -> savoiardi.LinearFilter
  }
  savoiardi.apply_texture_to_object(object, tex, savoiardi_filter)
}

/// Create an animation from a clip with default settings.
///
/// Defaults: loop repeat, normal speed (1.0x), full weight (1.0).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/animation
/// import tiramisu/model
///
/// // After loading a GLTF model
/// let clips = model.get_animations(gltf_data)
/// let walk_clip = list.find(clips, fn(clip) { animation.clip_name(clip) == "Walk" })
///
/// let walk_animation = animation.new_animation(walk_clip)
/// ```
pub fn new_animation(clip: Clip) -> Animation {
  Animation(clip: clip, loop: LoopRepeat, speed: 1.0, weight: 1.0)
}

/// Set the loop mode for an animation.
///
/// ## Example
///
/// ```gleam
/// let jump_animation = animation.new_animation(jump_clip)
///   |> animation.set_loop(animation.LoopOnce)  // Play once, don't loop
/// ```
pub fn set_loop(anim: Animation, mode: LoopMode) -> Animation {
  Animation(..anim, loop: mode)
}

/// Set the playback speed multiplier.
///
/// The `speed` multiplier affects how fast the animation plays:
/// - `1.0` = normal speed
/// - `2.0` = double speed (twice as fast)
/// - `0.5` = half speed (slow motion)
/// - Negative values play the animation in reverse
///
/// ## Example
///
/// ```gleam
/// let run_animation = animation.new_animation(run_clip)
///   |> animation.set_speed(1.5)  // 50% faster running
/// ```
pub fn set_speed(anim: Animation, speed: Float) -> Animation {
  Animation(..anim, speed: speed)
}

/// Set the animation weight for blending.
///
/// The `weight` value ranges from 0.0 to 1.0:
/// - `1.0` = full influence (default)
/// - `0.5` = half influence (useful for blending)
/// - `0.0` = no influence (animation has no effect)
///
/// ## Example
///
/// ```gleam
/// // Blend two animations manually
/// let walk = animation.new_animation(walk_clip) |> animation.set_weight(0.7)
/// let idle = animation.new_animation(idle_clip) |> animation.set_weight(0.3)
///
/// // Or use BlendedAnimations with a blend factor
/// animation.BlendedAnimations(from: walk, to: idle, blend_factor: 0.5)
/// ```
pub fn set_weight(anim: Animation, weight: Float) -> Animation {
  Animation(..anim, weight: weight)
}

/// Get the name of an animation clip.
///
/// Useful for finding specific animations by name when loading from GLTF files.
///
/// ## Example
///
/// ```gleam
/// import gleam/list
/// import tiramisu/model
///
/// // After loading a GLTF model
/// let clips = model.get_animations(gltf_data)
///
/// let walk_clip = list.find(clips, fn(clip) {
///   animation.clip_name(clip) == "Walk"
/// })
/// ```
pub fn clip_name(clip: savoiardi.AnimationClip) -> String {
  savoiardi.get_clip_name(clip)
}

/// Get the duration of an animation clip.
///
/// The duration is in **seconds**.
///
/// ## Example
///
/// ```gleam
/// let clip = animation.new_animation(walk_clip)
/// let duration_seconds = animation.clip_duration(walk_clip)
/// // Use this to sync game events with animation timing
/// ```
pub fn clip_duration(clip: savoiardi.AnimationClip) -> duration.Duration {
  savoiardi.get_clip_duration(clip)
  |> float.multiply(1_000_000_000.0)
  |> float.round
  |> duration.nanoseconds
}
