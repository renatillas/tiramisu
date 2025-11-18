//// <script>
//// const docs = [
////   {
////     header: "Tweening",
////     functions: [
////       "ease",
////       "tween",
////       "update_tween",
////       "get_tween_value",
////       "is_tween_complete",
////       "tween_vec3",
////       "tween_float",
////       "tween_transform",
////       "reset_tween",
////       "reverse_tween"
////     ]
////   },
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
//// </script>//// Tiramisu game engine main module - immutable game loop with effect system.
////
//// Animation and tweening system for smooth transitions and Three.js model animations.
////
//// This module provides two main systems:
//// - **Tweening**: Interpolate values over time with easing functions (positions, rotations, etc.)
//// - **Model Animations**: Play and blend Three.js animation clips loaded from GLTF models
////
//// ## Tweening Example
////
//// ```gleam
//// import tiramisu/animation
//// import vec/vec3
////
//// type Model {
////   Model(position_tween: animation.Tween(vec3.Vec3))
//// }
////
//// // Create a tween from one position to another over 2 seconds
//// let tween = animation.tween_vec3(
////   start: vec3.Vec3(0.0, 0.0, 0.0),
////   end: vec3.Vec3(5.0, 10.0, 0.0),
////   duration: 2000.0,  // milliseconds
////   easing: animation.EaseInOutQuad,
//// )
////
//// // Update each frame
//// fn update(model: Model, ctx: Context) {
////   let updated_tween = animation.update_tween(model.position_tween, ctx.delta_time)
////   let current_position = animation.get_tween_value(updated_tween)
////   Model(position_tween: updated_tween)
//// }
//// ```
////
//// ## Model Animation Example
////
//// ```gleam
//// import tiramisu/animation
//// import tiramisu/scene
////
//// // Assuming you loaded a GLTF model with animations
//// let model = asset.get_model(assets, "character")
//// let clips = asset.model_animations(model)
//// let walk_clip = list.find(clips, fn(clip) { animation.clip_name(clip) == "Walk" })
////
//// // Create animation and configure it
//// let walk_anim = animation.new_animation(walk_clip)
////   |> animation.set_speed(1.5)  // 1.5x speed
////   |> animation.set_loop(animation.LoopRepeat)
////
//// // Add to scene node
//// scene.Model(
////   id: "character",
////   model: model,
////   transform: transform.identity,
////   animation: option.Some(animation.SingleAnimation(walk_anim)),
////   physics: option.None,
//// )
//// ```

import gleam/float
import gleam_community/maths
import tiramisu/transform
import vec/vec3

/// Easing functions for smooth animations.
///
/// Easing functions control the rate of change over time, making animations
/// feel more natural. Use `ease()` to apply an easing function to a value in [0, 1].
pub type Easing {
  Linear
  EaseInQuad
  EaseOutQuad
  EaseInOutQuad
  EaseInCubic
  EaseOutCubic
  EaseInOutCubic
  EaseInSine
  EaseOutSine
  EaseInOutSine
}

/// Apply an easing function to a normalized value.
///
/// The value `t` is automatically clamped to the range [0, 1].
///
/// ## Example
///
/// ```gleam
/// // Linear progression
/// animation.ease(animation.Linear, 0.5)  // => 0.5
///
/// // Ease in quad - slow start, fast end
/// animation.ease(animation.EaseInQuad, 0.5)  // => 0.25
///
/// // Ease out quad - fast start, slow end
/// animation.ease(animation.EaseOutQuad, 0.5)  // => 0.75
/// ```
pub fn ease(easing: Easing, t: Float) -> Float {
  // Clamp t to [0, 1]
  let t = float.clamp(t, 0.0, 1.0)

  case easing {
    Linear -> t
    EaseInQuad -> t *. t
    EaseOutQuad -> t *. { 2.0 -. t }
    EaseInOutQuad ->
      case t <. 0.5 {
        True -> 2.0 *. t *. t
        False -> {
          let t_adj = t -. 1.0
          -1.0 *. { 2.0 *. t_adj *. t_adj -. 1.0 }
        }
      }
    EaseInCubic -> t *. t *. t
    EaseOutCubic -> {
      let t_adj = t -. 1.0
      t_adj *. t_adj *. t_adj +. 1.0
    }
    EaseInOutCubic ->
      case t <. 0.5 {
        True -> 4.0 *. t *. t *. t
        False -> {
          let t_adj = 2.0 *. t -. 2.0
          { t_adj *. t_adj *. t_adj +. 2.0 } /. 2.0
        }
      }
    EaseInSine -> {
      let angle = t *. 1.5707963267948966
      1.0 -. maths.cos(angle)
    }
    EaseOutSine -> {
      let angle = t *. 1.5707963267948966
      maths.sin(angle)
    }
    EaseInOutSine -> {
      let angle = 3.141592653589793 *. t
      { 1.0 -. maths.cos(angle) } /. 2.0
    }
  }
}

/// A tween that interpolates between two values over time.
///
/// Tweens are generic over any type `a` that can be interpolated (Float, Vec3, Transform, etc.).
/// Use the convenience constructors like `tween_float()`, `tween_vec3()`, or `tween_transform()`
/// for common types.
///
/// The `duration` is in **milliseconds**, and `elapsed` tracks the current progress.
pub type Tween(a) {
  Tween(
    start_value: a,
    end_value: a,
    /// Duration of the tween in milliseconds
    duration: Float,
    /// Time elapsed since tween started in milliseconds
    elapsed: Float,
    easing: Easing,
    lerp_fn: fn(a, a, Float) -> a,
  )
}

/// Create a new tween with a custom interpolation function.
///
/// For most use cases, prefer `tween_float()`, `tween_vec3()`, or `tween_transform()`.
///
/// ## Example
///
/// ```gleam
/// // Custom tween for a color value
/// type Color {
///   Color(r: Float, g: Float, b: Float)
/// }
///
/// let color_tween = animation.tween(
///   start: Color(1.0, 0.0, 0.0),  // Red
///   end: Color(0.0, 0.0, 1.0),    // Blue
///   duration: 1000.0,  // milliseconds
///   easing: animation.EaseInOutQuad,
///   lerp_fn: fn(a, b, t) {
///     Color(
///       r: a.r +. { b.r -. a.r } *. t,
///       g: a.g +. { b.g -. a.g } *. t,
///       b: a.b +. { b.b -. a.b } *. t,
///     )
///   },
/// )
/// ```
pub fn tween(
  start: a,
  end: a,
  duration duration: Float,
  easing easing: Easing,
  lerp_fn lerp_fn: fn(a, a, Float) -> a,
) -> Tween(a) {
  Tween(
    start_value: start,
    end_value: end,
    duration: duration,
    elapsed: 0.0,
    easing: easing,
    lerp_fn: lerp_fn,
  )
}

/// Update a tween by advancing its elapsed time.
///
/// The `delta` parameter is in **milliseconds** (typically from `ctx.delta_time`).
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   // Advance the tween by the frame delta
///   let updated_tween = animation.update_tween(model.tween, ctx.delta_time)
///   Model(..model, tween: updated_tween)
/// }
/// ```
pub fn update_tween(tween: Tween(a), delta delta: Float) -> Tween(a) {
  Tween(..tween, elapsed: tween.elapsed +. delta)
}

/// Get the current interpolated value of a tween.
///
/// Applies the easing function and returns the value at the current elapsed time.
/// Once the tween completes, this returns the end value.
///
/// ## Example
///
/// ```gleam
/// let tween = animation.tween_float(0.0, 100.0, 1000.0, animation.Linear)
///   |> animation.update_tween(500.0)  // Halfway through
///
/// let value = animation.get_tween_value(tween)  // => 50.0
/// ```
pub fn get_tween_value(tween: Tween(a)) -> a {
  let t = case tween.elapsed >=. tween.duration {
    True -> 1.0
    False -> tween.elapsed /. tween.duration
  }

  let eased_t = ease(tween.easing, t)
  tween.lerp_fn(tween.start_value, tween.end_value, eased_t)
}

/// Check if a tween has finished playing.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   let updated_tween = animation.update_tween(model.tween, ctx.delta_time)
///
///   case animation.is_tween_complete(updated_tween) {
///     True -> {
///       // Tween finished, trigger next animation or event
///       #(Model(..model, tween: animation.reset_tween(updated_tween)), effect.none())
///     }
///     False -> #(Model(..model, tween: updated_tween), effect.none())
///   }
/// }
/// ```
pub fn is_tween_complete(tween: Tween(a)) -> Bool {
  tween.elapsed >=. tween.duration
}

// --- Convenience tween creators ---

/// Create a tween that interpolates a Float value.
///
/// The `duration` is in **milliseconds**.
///
/// ## Example
///
/// ```gleam
/// // Fade from 0.0 to 1.0 over 2 seconds
/// let fade_tween = animation.tween_float(
///   start: 0.0,
///   end: 1.0,
///   duration: 2000.0,  // milliseconds
///   easing: animation.EaseInOutQuad,
/// )
/// ```
pub fn tween_float(
  start: Float,
  end: Float,
  duration duration: Float,
  easing easing: Easing,
) -> Tween(Float) {
  tween(start, end, duration:, easing:, lerp_fn: fn(a, b, t) {
    a +. { b -. a } *. t
  })
}

/// Create a tween that interpolates a Vec3 value (useful for positions).
///
/// The `duration` is in **milliseconds**.
///
/// ## Example
///
/// ```gleam
/// import vec/vec3
///
/// // Move from origin to (10, 5, 0) over 3 seconds
/// let position_tween = animation.tween_vec3(
///   start: vec3.Vec3(0.0, 0.0, 0.0),
///   end: vec3.Vec3(10.0, 5.0, 0.0),
///   duration: 3000.0,  // milliseconds
///   easing: animation.EaseOutQuad,
/// )
/// ```
pub fn tween_vec3(
  start: vec3.Vec3(Float),
  end: vec3.Vec3(Float),
  duration duration: Float,
  easing easing: Easing,
) -> Tween(vec3.Vec3(Float)) {
  tween(start, end, duration:, easing:, lerp_fn: fn(a, b, t) {
    vec3.Vec3(
      a.x +. { b.x -. a.x } *. t,
      a.y +. { b.y -. a.y } *. t,
      a.z +. { b.z -. a.z } *. t,
    )
  })
}

/// Create a tween that interpolates a Transform (position, rotation, and scale).
///
/// The `duration` is in **milliseconds**.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/transform
/// import vec/vec3
///
/// let start_transform = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
///   |> transform.scale(1.0)
///
/// let end_transform = transform.at(position: vec3.Vec3(5.0, 0.0, 0.0))
///   |> transform.scale(2.0)
///   |> transform.rotate_y(3.14159)
///
/// let transform_tween = animation.tween_transform(
///   start: start_transform,
///   end: end_transform,
///   duration: 1500.0,  // milliseconds
///   easing: animation.EaseInOutCubic,
/// )
/// ```
pub fn tween_transform(
  start: transform.Transform,
  end: transform.Transform,
  duration duration: Float,
  easing easing: Easing,
) -> Tween(transform.Transform) {
  tween(start, end, duration:, easing:, lerp_fn: transform.lerp)
}

/// Reset a tween back to the beginning (elapsed time = 0).
///
/// ## Example
///
/// ```gleam
/// // Play the tween again from the start
/// let reset = animation.reset_tween(completed_tween)
/// ```
pub fn reset_tween(tween: Tween(a)) -> Tween(a) {
  Tween(..tween, elapsed: 0.0)
}

/// Reverse a tween by swapping its start and end values.
///
/// The elapsed time is preserved, so the tween continues from where it was
/// but in the opposite direction.
///
/// ## Example
///
/// ```gleam
/// // Create a bouncing animation
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   let updated_tween = animation.update_tween(model.tween, ctx.delta_time)
///
///   case animation.is_tween_complete(updated_tween) {
///     True -> {
///       // Bounce back by reversing the tween
///       let reversed = animation.reverse_tween(updated_tween)
///         |> animation.reset_tween()
///       Model(..model, tween: reversed)
///     }
///     False -> Model(..model, tween: updated_tween)
///   }
/// }
/// ```
pub fn reverse_tween(tween: Tween(a)) -> Tween(a) {
  Tween(..tween, start_value: tween.end_value, end_value: tween.start_value)
}

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
