import gleam/float
import gleam_community/maths
import tiramisu/transform
import tiramisu/vec3

/// Easing functions for smooth animations
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

/// Tween a Vec3 value
pub fn tween_vec3(
  start: vec3.Vec3,
  end: vec3.Vec3,
  duration: Float,
  easing: Easing,
) -> Tween(vec3.Vec3) {
  tween(start, end, duration, easing, vec3.lerp)
}

/// Tween a Transform
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
