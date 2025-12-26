import gleam/order
import gleam/time/duration.{type Duration}
import quaternion
import tiramisu/transform
import vec/vec3

/// A tween that interpolates between two values over time.
///
/// Tweens are generic over any type `a` that can be interpolated (Float, Vec3, Transform, etc.).
/// Use the convenience constructors like `tween_float()`, `tween_vec3()`, or `tween_transform()`
/// for common types.
///
/// The `duration` is a Duration type, and `elapsed` tracks the current progress.
pub type Tween(a) {
  Tween(
    start_value: a,
    end_value: a,
    /// Duration of the tween
    duration: Duration,
    /// Time elapsed since tween started
    elapsed: Duration,
    easing: Easing,
    lerp_fn: fn(a, a, Float) -> a,
  )
}

/// Easing function that takes a normalized time value (0.0 to 1.0)
/// and returns an eased value (typically 0.0 to 1.0).
///
/// Common easing functions can be found in animation libraries or you can write your own.
pub type Easing =
  fn(Float) -> Float

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
/// let color_tween = tween.tween(
///   start: Color(1.0, 0.0, 0.0),  // Red
///   end: Color(0.0, 0.0, 1.0),    // Blue
///   duration: duration.seconds(1),
///   easing: fn(t) { t *. t },     // Ease in quad
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
  duration duration: Duration,
  easing easing: Easing,
  lerp_fn lerp_fn: fn(a, a, Float) -> a,
) -> Tween(a) {
  Tween(
    start_value: start,
    end_value: end,
    duration: duration,
    elapsed: duration.nanoseconds(0),
    easing: easing,
    lerp_fn: lerp_fn,
  )
}

/// Update a tween by advancing its elapsed time.
///
/// The `delta` parameter is a Duration (typically from `ctx.delta_time`).
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   // Advance the tween by the frame delta
///   let updated_tween = tween.update(model.tween, ctx.delta_time)
///   Model(..model, tween: updated_tween)
/// }
/// ```
pub fn update(tween: Tween(a), delta delta: Duration) -> Tween(a) {
  let new_elapsed = duration.add(tween.elapsed, delta)
  Tween(..tween, elapsed: new_elapsed)
}

/// Get the current interpolated value of a tween.
///
/// Applies the easing function and returns the value at the current elapsed time.
/// Once the tween completes, this returns the end value.
///
/// ## Example
///
/// ```gleam
/// let my_tween = tween.tween_float(0.0, 100.0, duration.seconds(1), fn(t) { t })
///   |> tween.update(duration.milliseconds(500))  // Halfway through
///
/// let value = tween.get_value(my_tween)  // => 50.0
/// ```
pub fn get_value(tween: Tween(a)) -> a {
  let t = case duration.compare(tween.elapsed, tween.duration) {
    order.Lt -> {
      // Convert to seconds for division
      let elapsed_s = duration.to_seconds(tween.elapsed)
      let total_s = duration.to_seconds(tween.duration)
      elapsed_s /. total_s
    }
    order.Eq | order.Gt -> 1.0
  }

  let eased_t = tween.easing(t)
  tween.lerp_fn(tween.start_value, tween.end_value, eased_t)
}

/// Check if a tween has finished playing.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: Context) {
///   let updated_tween = tween.update(model.tween, ctx.delta_time)
///
///   case tween.is_complete(updated_tween) {
///     True -> {
///       // Tween finished, trigger next animation or event
///       #(Model(..model, tween: tween.reset(updated_tween)), effect.none())
///     }
///     False -> #(Model(..model, tween: updated_tween), effect.none())
///   }
/// }
/// ```
pub fn is_complete(tween: Tween(a)) -> Bool {
  case duration.compare(tween.elapsed, tween.duration) {
    order.Lt -> False
    order.Eq | order.Gt -> True
  }
}

// --- Convenience tween creators ---

/// Create a tween that interpolates a Float value.
///
/// The `duration` is a Duration type.
///
/// ## Example
///
/// ```gleam
/// // Fade from 0.0 to 1.0 over 2 seconds
/// let fade_tween = tween.tween_float(
///   start: 0.0,
///   end: 1.0,
///   duration: duration.seconds(2),
///   easing: fn(t) { t *. t *. t },  // Ease in cubic
/// )
/// ```
pub fn tween_float(
  start: Float,
  end: Float,
  duration duration: Duration,
  easing easing: Easing,
) -> Tween(Float) {
  tween(start, end, duration:, easing:, lerp_fn: fn(a, b, t) {
    a +. { b -. a } *. t
  })
}

/// Create a tween that interpolates a Vec3 value (useful for positions).
///
/// The `duration` is a Duration type.
///
/// ## Example
///
/// ```gleam
/// import vec/vec3
///
/// // Move from origin to (10, 5, 0) over 3 seconds
/// let position_tween = tween.tween_vec3(
///   start: vec3.Vec3(0.0, 0.0, 0.0),
///   end: vec3.Vec3(10.0, 5.0, 0.0),
///   duration: duration.seconds(3),
///   easing: fn(t) { 1.0 -. { 1.0 -. t } *. { 1.0 -. t } },  // Ease out quad
/// )
/// ```
pub fn tween_vec3(
  start: vec3.Vec3(Float),
  end: vec3.Vec3(Float),
  duration duration: Duration,
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

/// Create a tween that interpolates between two quaternions using spherical linear interpolation (slerp).
///
/// The `duration` is a Duration type.
///
/// ## Example
///
/// ```gleam
/// import quaternion
/// import vec/vec3
///
/// let start_rotation = quaternion.from_euler(vec3.Vec3(0.0, 0.0, 0.0))
/// let end_rotation = quaternion.from_euler(vec3.Vec3(0.0, 3.14159, 0.0))
///
/// let rotation_tween = tween.tween_quaternion(
///   start: start_rotation,
///   end: end_rotation,
///   duration: duration.seconds(2),
///   easing: fn(t) { t },  // Linear
/// )
/// ```
pub fn tween_quaternion(
  start: quaternion.Quaternion,
  end: quaternion.Quaternion,
  duration duration: Duration,
  easing easing: Easing,
) -> Tween(quaternion.Quaternion) {
  tween(start, end, duration:, easing:, lerp_fn: fn(a, b, t) {
    quaternion.spherical_linear_interpolation(from: a, to: b, t: t)
  })
}

/// Create a tween that interpolates a Transform (position, rotation, and scale).
///
/// The `duration` is a Duration type.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/transform
/// import vec/vec3
///
/// let start_transform = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
///   |> transform.scale_uniform(1.0)
///
/// let end_transform = transform.at(position: vec3.Vec3(5.0, 0.0, 0.0))
///   |> transform.scale_uniform(2.0)
///   |> transform.rotate_y(3.14159)
///
/// let transform_tween = tween.tween_transform(
///   start: start_transform,
///   end: end_transform,
///   duration: duration.seconds(1.5),
///   easing: fn(t) { t *. t *. { 3.0 -. 2.0 *. t } },  // Smooth step
/// )
/// ```
pub fn tween_transform(
  start: transform.Transform,
  end: transform.Transform,
  duration duration: Duration,
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
/// let reset_tween = tween.reset(completed_tween)
/// ```
pub fn reset(tween: Tween(a)) -> Tween(a) {
  Tween(..tween, elapsed: duration.nanoseconds(0))
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
///   let updated_tween = tween.update(model.tween, ctx.delta_time)
///
///   case tween.is_complete(updated_tween) {
///     True -> {
///       // Bounce back by reversing the tween
///       let reversed = tween.reverse(updated_tween)
///         |> tween.reset()
///       Model(..model, tween: reversed)
///     }
///     False -> Model(..model, tween: updated_tween)
///   }
/// }
/// ```
pub fn reverse(tween: Tween(a)) -> Tween(a) {
  Tween(..tween, start_value: tween.end_value, end_value: tween.start_value)
}
