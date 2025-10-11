/// Opaque type for Three.js Object3D (loaded 3D models)
pub type Object3D

/// Opaque type for Three.js AnimationClip
pub type AnimationClip

/// Animation loop mode
pub type LoopMode {
  LoopOnce
  LoopRepeat
}

/// Declarative animation state - describes what animation should be playing
pub type Animation {
  Animation(clip: AnimationClip, loop: LoopMode, speed: Float, weight: Float)
}

/// Animation playback configuration - single animation or blend
pub type AnimationPlayback {
  /// Play a single animation
  SingleAnimation(Animation)
  /// Blend between two animations
  BlendedAnimations(from: Animation, to: Animation, blend_factor: Float)
}

/// Create an animation from a clip with default settings (loop repeat, normal speed, full weight)
pub fn new_animation(clip: AnimationClip) -> Animation {
  Animation(clip: clip, loop: LoopRepeat, speed: 1.0, weight: 1.0)
}

/// Set the loop mode
pub fn set_loop(anim: Animation, mode: LoopMode) -> Animation {
  Animation(..anim, loop: mode)
}

/// Set the animation speed (1.0 = normal, 2.0 = double speed, 0.5 = half speed)
pub fn set_speed(anim: Animation, speed: Float) -> Animation {
  Animation(..anim, speed: speed)
}

/// Set the animation weight (0.0 to 1.0, for blending animations)
pub fn set_weight(anim: Animation, weight: Float) -> Animation {
  Animation(..anim, weight: weight)
}

/// Get the name of an animation clip
@external(javascript, "../threejs.ffi.mjs", "getClipName")
pub fn clip_name(clip: AnimationClip) -> String

/// Get the duration of an animation clip (in seconds)
@external(javascript, "../threejs.ffi.mjs", "getClipDuration")
pub fn clip_duration(clip: AnimationClip) -> Float
