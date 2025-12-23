//// <script>
//// const docs = [
////   {
////     header: "Model animations",
////     functions: [
////       "new_animation",
////       "clip_duration",
////       "clip_name",
////       "set_loop",
////       "set_speed",
////       "set_weight"
////     ]
////   }
//// ]
//// 
//// const callback = () => {
////   const list = document.querySelector(".sidebar > ul:last-of-type")
////   const sortedLists = document.createDocumentFragment()
////   const sortedMembers = document.createDocumentFragment()
//// 
////   for (const section of docs) {
////     sortedLists.append((() => {
////       const node = document.createElement("h3")
////       node.append(section.header)
////       return node
////     })())
////     sortedMembers.append((() => {
////       const node = document.createElement("h2")
////       node.append(section.header)
////       return node
////     })())
//// 
////     const sortedList = document.createElement("ul")
////     sortedLists.append(sortedList)
//// 
//// 
////     for (const funcName of section.functions) {
////       const href = `#${funcName}`
////       const member = document.querySelector(
////         `.member:has(h2 > a[href="${href}"])`
////       )
////       const sidebar = list.querySelector(`li:has(a[href="${href}"])`)
////       sortedList.append(sidebar)
////       sortedMembers.append(member)
////     }
////   }
//// 
////   document.querySelector(".sidebar").insertBefore(sortedLists, list)
////   document
////     .querySelector(".module-members:has(#module-values)")
////     .insertBefore(
////       sortedMembers,
////       document.querySelector("#module-values").nextSibling
////     )
//// }
//// 
//// document.readyState !== "loading"
////   ? callback()
////   : document.addEventListener(
////     "DOMContentLoaded",
////     callback,
////     { once: true }
////   )
//// </script>
//// Three.js model animation system.
////
//// This module provides control over animations loaded from 3D model files (GLTF, FBX).
//// For generic value tweening, see the `tiramisu/tween` module.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/animation
//// import tiramisu/asset
//// import tiramisu/scene
//// import gleam/list
//// import gleam/option
////
//// // Load a GLTF model with animations
//// let assert Ok(model_data) = asset.get_model(cache, "character.glb")
//// let clips = asset.model_animations(model_data)
////
//// // Find the walk animation
//// let assert Ok(walk_clip) = list.find(clips, fn(clip) {
////   animation.clip_name(clip) == "Walk"
//// })
////
//// // Configure the animation
//// let walk_anim = animation.new_animation(walk_clip)
////   |> animation.set_speed(1.5)  // 1.5x speed
////   |> animation.set_loop(animation.LoopRepeat)
////
//// // Add to scene node
//// scene.model_3d(
////   id: "character",
////   object: model_data.scene,
////   transform: transform.identity,
////   animation: option.Some(animation.SingleAnimation(walk_anim)),
////   physics: option.None,
////   material: option.None,
//// )
//// ```

/// An opaque reference to a Three.js AnimationClip.
///
/// Animation clips are typically loaded from GLTF model files using `asset.model_animations()`.
/// Each clip contains keyframe data for animating a 3D model.
pub type AnimationClip

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
    clip: AnimationClip,
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

/// Create an animation from a clip with default settings.
///
/// Defaults: loop repeat, normal speed (1.0x), full weight (1.0).
///
/// ## Example
///
/// ```gleam
/// import tiramisu/animation
/// import tiramisu/asset
///
/// let model = asset.get_model(assets, "character")
/// let clips = asset.model_animations(model)
/// let walk_clip = list.find(clips, fn(clip) { animation.clip_name(clip) == "Walk" })
///
/// let walk_animation = animation.new_animation(walk_clip)
/// ```
pub fn new_animation(clip: AnimationClip) -> Animation {
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
/// import tiramisu/asset
///
/// let model = asset.get_model(assets, "character")
/// let clips = asset.model_animations(model)
///
/// let walk_clip = list.find(clips, fn(clip) {
///   animation.clip_name(clip) == "Walk"
/// })
/// ```
@external(javascript, "../threejs.ffi.mjs", "getClipName")
pub fn clip_name(clip: AnimationClip) -> String

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
@external(javascript, "../threejs.ffi.mjs", "getClipDuration")
pub fn clip_duration(clip: AnimationClip) -> Float
