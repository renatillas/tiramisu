//// Animation system - tweens and easing functions for smooth interpolation.
////
//// Provides easing functions and tween helpers for animating values over time.
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/animation
////
//// // Create a tween from 0 to 100 over 2 seconds
//// let tween = animation.tween_float(
////   from: 0.0,
////   to: 100.0,
////   duration: 2.0,
////   easing: animation.EaseOutQuad,
//// )
////
//// // Update in game loop
//// let tween = animation.update_tween(tween, ctx.delta_time)
//// let current_value = animation.tween_value(tween)
//// let done = animation.is_tween_complete(tween)
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

/// Apply easing function to a value t in [0, 1]
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

/// Tween state
pub type Tween(a) {
  Tween(
    start_value: a,
    end_value: a,
    duration: Float,
    elapsed: Float,
    easing: Easing,
    lerp_fn: fn(a, a, Float) -> a,
  )
}

/// Create a new tween
pub fn tween(
  start: a,
  end: a,
  duration: Float,
  easing: Easing,
  lerp_fn: fn(a, a, Float) -> a,
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

/// Update a tween with delta time
pub fn update_tween(tween: Tween(a), delta: Float) -> Tween(a) {
  Tween(..tween, elapsed: tween.elapsed +. delta)
}

/// Get the current value of a tween
pub fn get_tween_value(tween: Tween(a)) -> a {
  let t = case tween.elapsed >=. tween.duration {
    True -> 1.0
    False -> tween.elapsed /. tween.duration
  }

  let eased_t = ease(tween.easing, t)
  tween.lerp_fn(tween.start_value, tween.end_value, eased_t)
}

/// Check if a tween is complete
pub fn is_tween_complete(tween: Tween(a)) -> Bool {
  tween.elapsed >=. tween.duration
}

// --- Convenience tween creators ---

/// Tween a Float value
pub fn tween_float(
  start: Float,
  end: Float,
  duration: Float,
  easing: Easing,
) -> Tween(Float) {
  tween(start, end, duration, easing, fn(a, b, t) { a +. { b -. a } *. t })
}

pub fn tween_vec3(
  start: vec3.Vec3(Float),
  end: vec3.Vec3(Float),
  duration: Float,
  easing: Easing,
) -> Tween(vec3.Vec3(Float)) {
  tween(start, end, duration, easing, fn(a, b, t) {
    vec3.Vec3(
      a.x +. { b.x -. a.x } *. t,
      a.y +. { b.y -. a.y } *. t,
      a.z +. { b.z -. a.z } *. t,
    )
  })
}

pub fn tween_transform(
  start: transform.Transform,
  end: transform.Transform,
  duration: Float,
  easing: Easing,
) -> Tween(transform.Transform) {
  tween(start, end, duration, easing, transform.lerp)
}

/// Reset a tween to start
pub fn reset_tween(tween: Tween(a)) -> Tween(a) {
  Tween(..tween, elapsed: 0.0)
}

/// Reverse a tween (swap start and end)
pub fn reverse_tween(tween: Tween(a)) -> Tween(a) {
  Tween(..tween, start_value: tween.end_value, end_value: tween.start_value)
}
