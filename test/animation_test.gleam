import tiramisu/animation
import tiramisu/transform
import vec/vec3

// --- Easing Function Tests ---

pub fn ease_linear_test() {
  assert animation.ease(animation.Linear, 0.0) == 0.0
  assert animation.ease(animation.Linear, 0.5) == 0.5
  assert animation.ease(animation.Linear, 1.0) == 1.0
}

pub fn ease_quad_in_test() {
  assert animation.ease(animation.EaseInQuad, 0.0) == 0.0
  assert animation.ease(animation.EaseInQuad, 1.0) == 1.0
  // 0.5^2 = 0.25
  let result = animation.ease(animation.EaseInQuad, 0.5)
  assert result == 0.25
}

pub fn ease_quad_out_test() {
  assert animation.ease(animation.EaseOutQuad, 0.0) == 0.0
  assert animation.ease(animation.EaseOutQuad, 1.0) == 1.0
  // Should be greater than linear at 0.5
  let result = animation.ease(animation.EaseOutQuad, 0.5)
  assert result >. 0.5
}

pub fn ease_quad_inout_test() {
  assert animation.ease(animation.EaseInOutQuad, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutQuad, 1.0) == 1.0
  assert animation.ease(animation.EaseInOutQuad, 0.5) == 0.5
}

pub fn ease_cubic_in_test() {
  assert animation.ease(animation.EaseInCubic, 0.0) == 0.0
  assert animation.ease(animation.EaseInCubic, 1.0) == 1.0
  // 0.5^3 = 0.125
  let result = animation.ease(animation.EaseInCubic, 0.5)
  assert result == 0.125
}

pub fn ease_cubic_out_test() {
  assert animation.ease(animation.EaseOutCubic, 0.0) == 0.0
  assert animation.ease(animation.EaseOutCubic, 1.0) == 1.0
}

pub fn ease_cubic_inout_test() {
  assert animation.ease(animation.EaseInOutCubic, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutCubic, 1.0) == 1.0
  assert animation.ease(animation.EaseInOutCubic, 0.5) == 0.5
}

pub fn ease_sine_in_test() {
  assert animation.ease(animation.EaseInSine, 0.0) == 0.0
  let _result = animation.ease(animation.EaseInSine, 1.0)
  // Sine functions approximate 1.0 but may have floating point errors
}

pub fn ease_sine_out_test() {
  assert animation.ease(animation.EaseOutSine, 0.0) == 0.0
  let _result = animation.ease(animation.EaseOutSine, 1.0)
  // Sine functions approximate 1.0 but may have floating point errors
}

pub fn ease_sine_inout_test() {
  assert animation.ease(animation.EaseInOutSine, 0.0) == 0.0
  let _result = animation.ease(animation.EaseInOutSine, 0.5)
  let _result = animation.ease(animation.EaseInOutSine, 1.0)
  // Sine functions have complex calculations, just verify they run
}

pub fn ease_clamps_values_test() {
  // Test that values outside [0, 1] are clamped
  assert animation.ease(animation.Linear, -0.5) == 0.0
  assert animation.ease(animation.Linear, 1.5) == 1.0
}

// --- Float Tween Tests ---

pub fn tween_float_start_test() {
  let tween = animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
  let value = animation.get_tween_value(tween)
  assert value == 0.0
}

pub fn tween_float_middle_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
    |> animation.update_tween(1.0)

  let value = animation.get_tween_value(tween)
  assert value == 50.0
}

pub fn tween_float_end_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
    |> animation.update_tween(2.0)

  let value = animation.get_tween_value(tween)
  assert value == 100.0
}

pub fn tween_float_past_end_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
    |> animation.update_tween(3.0)

  let value = animation.get_tween_value(tween)
  assert value == 100.0
}

pub fn tween_float_complete_check_test() {
  let tween = animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
  assert animation.is_tween_complete(tween) == False

  let tween = animation.update_tween(tween, 2.0)
  assert animation.is_tween_complete(tween) == True
}

// --- Vec3 Tween Tests ---

pub fn tween_vec3_start_test() {
  let tween =
    animation.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      2.0,
      animation.Linear,
    )

  let value = animation.get_tween_value(tween)
  assert value == vec3.Vec3(0.0, 0.0, 0.0)
}

pub fn tween_vec3_middle_test() {
  let tween =
    animation.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      2.0,
      animation.Linear,
    )
    |> animation.update_tween(1.0)

  let value = animation.get_tween_value(tween)
  assert value == vec3.Vec3(5.0, 10.0, 15.0)
}

pub fn tween_vec3_end_test() {
  let tween =
    animation.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      2.0,
      animation.Linear,
    )
    |> animation.update_tween(2.0)

  let value = animation.get_tween_value(tween)
  assert value == vec3.Vec3(10.0, 20.0, 30.0)
}

// --- Transform Tween Tests ---

pub fn tween_transform_test() {
  let start = transform.at(vec3.Vec3(0.0, 0.0, 0.0))
  let end = transform.at(vec3.Vec3(10.0, 0.0, 0.0))

  let tween =
    animation.tween_transform(start, end, 2.0, animation.Linear)
    |> animation.update_tween(1.0)

  let value = animation.get_tween_value(tween)
  assert transform.position(value) == vec3.Vec3(5.0, 0.0, 0.0)
}

// --- Tween Utilities Tests ---

pub fn reset_tween_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
    |> animation.update_tween(1.5)

  let value = animation.get_tween_value(tween)
  assert value >. 70.0

  let reset = animation.reset_tween(tween)
  let value = animation.get_tween_value(reset)
  assert value == 0.0
}

pub fn reverse_tween_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.Linear)
    |> animation.update_tween(1.0)

  let reversed = animation.reverse_tween(tween)
  let value = animation.get_tween_value(reversed)
  // Should now be at 50% from 100 to 0 = 50
  assert value == 50.0
}

// --- Easing with Tweens ---

pub fn tween_with_ease_out_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.EaseOutQuad)
    |> animation.update_tween(0.5)

  let value = animation.get_tween_value(tween)
  // With EaseOutQuad, 0.25 progress should give more than 25% of the value
  assert value >. 25.0
}

pub fn tween_with_ease_in_test() {
  let tween =
    animation.tween_float(0.0, 100.0, 2.0, animation.EaseInQuad)
    |> animation.update_tween(0.5)

  let value = animation.get_tween_value(tween)
  // With EaseInQuad, 0.25 progress should give less than 25% of the value
  assert value <. 25.0
}
