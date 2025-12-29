import gleam/float
import gleam/int
import gleam/time/duration
import quaternion
import tiramisu/transform
import tiramisu/tween
import vec/vec3

// ============================================================================
// Float Tween Tests
// ============================================================================

pub fn tween_float_initial_value_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })

  // At t=0, value should be start value
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 0.0, tolerating: 0.001)
}

pub fn tween_float_midpoint_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(2), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(1))

  // At t=0.5 (1 second into 2 second tween), value should be 50
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 50.0, tolerating: 0.001)
}

pub fn tween_float_complete_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(1))

  // At t=1.0, value should be end value
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 100.0, tolerating: 0.001)
}

pub fn tween_float_overshoot_clamps_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(2))

  // Even after 2 seconds (past duration), value should be clamped to end
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 100.0, tolerating: 0.001)
}

pub fn tween_float_with_easing_test() {
  // Ease-in quadratic: starts slow, ends fast
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(2), easing: fn(x) {
      x *. x
    })
    |> tween.update(delta: duration.seconds(1))

  // At t=0.5 with quadratic easing, value should be 0.5^2 * 100 = 25
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 25.0, tolerating: 0.001)
}

pub fn tween_float_negative_values_test() {
  let t =
    tween.tween_float(
      -50.0,
      50.0,
      duration: duration.seconds(2),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  // Midpoint should be 0
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 0.0, tolerating: 0.001)
}

// ============================================================================
// Is Complete Tests
// ============================================================================

pub fn is_complete_before_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })

  assert tween.is_complete(t) == False
}

pub fn is_complete_during_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(2), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(1))

  assert tween.is_complete(t) == False
}

pub fn is_complete_at_end_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(1))

  assert tween.is_complete(t) == True
}

pub fn is_complete_after_end_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(5))

  assert tween.is_complete(t) == True
}

// ============================================================================
// Reset Tests
// ============================================================================

pub fn reset_restores_to_start_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.seconds(1))
    |> tween.reset()

  // After reset, value should be back to start
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 0.0, tolerating: 0.001)
  assert tween.is_complete(t) == False
}

// ============================================================================
// Reverse Tests
// ============================================================================

pub fn reverse_swaps_start_end_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(2), easing: fn(x) {
      x
    })
    |> tween.reverse()

  // After reverse, start value should now be 100 (the original end)
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 100.0, tolerating: 0.001)
}

pub fn reverse_and_update_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(2), easing: fn(x) {
      x
    })
    |> tween.reverse()
    |> tween.update(delta: duration.seconds(1))

  // After reverse and halfway through, should be at 50 (going from 100 to 0)
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 50.0, tolerating: 0.001)
}

// ============================================================================
// Vec3 Tween Tests
// ============================================================================

pub fn tween_vec3_initial_test() {
  let t =
    tween.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )

  let value = tween.get_value(t)
  assert value == vec3.Vec3(0.0, 0.0, 0.0)
}

pub fn tween_vec3_midpoint_test() {
  let t =
    tween.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      duration: duration.seconds(2),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  assert float.loosely_equals(value.x, 5.0, tolerating: 0.001)
  assert float.loosely_equals(value.y, 10.0, tolerating: 0.001)
  assert float.loosely_equals(value.z, 15.0, tolerating: 0.001)
}

pub fn tween_vec3_complete_test() {
  let t =
    tween.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  assert value == vec3.Vec3(10.0, 20.0, 30.0)
}

// ============================================================================
// Quaternion Tween Tests
// ============================================================================

pub fn tween_quaternion_initial_test() {
  let start = quaternion.identity
  let end = quaternion.from_euler(vec3.Vec3(0.0, 3.14159, 0.0))

  let t =
    tween.tween_quaternion(
      start,
      end,
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )

  let value = tween.get_value(t)
  assert quaternion.loosely_equals(value, start, tolerating: 0.001)
}

pub fn tween_quaternion_complete_test() {
  let start = quaternion.identity
  let end = quaternion.from_euler(vec3.Vec3(0.0, 3.14159, 0.0))

  let t =
    tween.tween_quaternion(
      start,
      end,
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  assert quaternion.loosely_equals(value, end, tolerating: 0.001)
}

// ============================================================================
// Transform Tween Tests
// ============================================================================

pub fn tween_transform_initial_test() {
  let start = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let end = transform.at(position: vec3.Vec3(10.0, 0.0, 0.0))

  let t =
    tween.tween_transform(
      start,
      end,
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )

  let value = tween.get_value(t)
  assert transform.position(value) == vec3.Vec3(0.0, 0.0, 0.0)
}

pub fn tween_transform_midpoint_test() {
  let start = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let end = transform.at(position: vec3.Vec3(10.0, 0.0, 0.0))

  let t =
    tween.tween_transform(
      start,
      end,
      duration: duration.seconds(2),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  let pos = transform.position(value)
  assert float.loosely_equals(pos.x, 5.0, tolerating: 0.001)
}

pub fn tween_transform_complete_test() {
  let start = transform.at(position: vec3.Vec3(0.0, 0.0, 0.0))
  let end = transform.at(position: vec3.Vec3(10.0, 0.0, 0.0))

  let t =
    tween.tween_transform(
      start,
      end,
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  assert transform.position(value) == vec3.Vec3(10.0, 0.0, 0.0)
}

pub fn tween_transform_with_scale_test() {
  let start =
    transform.identity
    |> transform.with_scale(vec3.Vec3(1.0, 1.0, 1.0))
  let end =
    transform.identity
    |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))

  let t =
    tween.tween_transform(
      start,
      end,
      duration: duration.seconds(2),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  let scale = transform.scale(value)
  assert float.loosely_equals(scale.x, 1.5, tolerating: 0.001)
  assert float.loosely_equals(scale.y, 1.5, tolerating: 0.001)
  assert float.loosely_equals(scale.z, 1.5, tolerating: 0.001)
}

// ============================================================================
// Multiple Updates Test
// ============================================================================

pub fn multiple_small_updates_test() {
  let t =
    tween.tween_float(0.0, 100.0, duration: duration.seconds(1), easing: fn(x) {
      x
    })
    |> tween.update(delta: duration.milliseconds(250))
    |> tween.update(delta: duration.milliseconds(250))
    |> tween.update(delta: duration.milliseconds(250))
    |> tween.update(delta: duration.milliseconds(250))

  // After 4 x 250ms = 1 second, should be complete
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 100.0, tolerating: 0.001)
  assert tween.is_complete(t) == True
}

// ============================================================================
// Custom Tween Tests
// ============================================================================

pub fn custom_tween_test() {
  // Custom color-like type
  let lerp_int = fn(a: Int, b: Int, t: Float) -> Int {
    let af = int.to_float(a)
    let bf = int.to_float(b)
    { af +. { { bf -. af } *. t } } |> float.round
  }

  let t =
    tween.new(0, 255, duration: duration.seconds(2), easing: fn(x) { x }, lerp_fn: lerp_int)
    |> tween.update(delta: duration.seconds(1))

  let value = tween.get_value(t)
  // At t=0.5, should be halfway: 127 or 128
  assert value == 127 || value == 128
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn zero_duration_tween_test() {
  let t =
    tween.tween_float(
      0.0,
      100.0,
      duration: duration.nanoseconds(0),
      easing: fn(x) { x },
    )

  // With zero duration, should immediately be complete
  assert tween.is_complete(t) == True
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 100.0, tolerating: 0.001)
}

pub fn same_start_end_test() {
  let t =
    tween.tween_float(
      50.0,
      50.0,
      duration: duration.seconds(1),
      easing: fn(x) { x },
    )
    |> tween.update(delta: duration.milliseconds(500))

  // When start equals end, value should always be that value
  let value = tween.get_value(t)
  assert float.loosely_equals(value, 50.0, tolerating: 0.001)
}
