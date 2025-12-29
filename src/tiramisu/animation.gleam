//// 3D model animation playback and blending.
////
//// This module handles animation clips loaded from 3D models (GLTF, FBX).
//// Use it to configure and play skeletal animations with configurable
//// loop modes, speed, and blending.
////
//// ## Loading Animations
////
//// Animations are loaded as part of 3D models. Extract clips using the
//// `model` module:
////
//// ```gleam
//// import tiramisu/model
//// import tiramisu/animation
////
//// // After loading a GLTF model
//// let clips = model.get_animations(gltf_data)
////
//// // Find a specific animation by name
//// let walk_clip = list.find(clips, fn(clip) {
////   animation.clip_name(clip) == "Walk"
//// })
////
//// // Create animation configuration
//// let walk_animation = animation.new_animation(walk_clip)
////   |> animation.set_loop(animation.LoopRepeat)
////   |> animation.set_speed(1.0)
//// ```
////
//// ## Animation Playback
////
//// Use `AnimationPlayback` with `scene.model()` to play animations:
////
//// ```gleam
//// scene.model(
////   id: "character",
////   object: my_model,
////   transform: transform.identity,
////   animation: option.Some(animation.SingleAnimation(walk_animation)),
////   physics: option.None,
//// )
//// ```
////
//// ## Blending Animations
////
//// Smoothly transition between animations:
////
//// ```gleam
//// animation.BlendedAnimations(
////   from: walk_animation,
////   to: run_animation,
////   blend_factor: 0.5,  // Halfway between walk and run
//// )
//// ```
////

import savoiardi

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
pub fn new_animation(clip: savoiardi.AnimationClip) -> Animation {
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
pub fn clip_duration(clip: savoiardi.AnimationClip) -> Float {
  savoiardi.get_clip_duration(clip)
}
