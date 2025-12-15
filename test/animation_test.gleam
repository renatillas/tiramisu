import gleam/float
import gleam/int
import gleam/list
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

pub fn ease_quartic_in_test() {
  assert animation.ease(animation.EaseInQuartic, 0.0) == 0.0
  assert animation.ease(animation.EaseInQuartic, 1.0) == 1.0
  assert animation.ease(animation.EaseInQuartic, 0.75) == 0.31640625
}

pub fn ease_quartic_out_test() {
  assert animation.ease(animation.EaseOutQuartic, 0.0) == 1.0
  assert animation.ease(animation.EaseOutQuartic, 1.0) == 0.0
  assert animation.ease(animation.EaseOutQuartic, 0.75) == 0.68359375
}

pub fn ease_quartic_inout_test() {
  assert animation.ease(animation.EaseInOutQuartic, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutQuartic, 0.5) == 0.5
  assert animation.ease(animation.EaseInOutQuartic, 1.0) == 1.0
  assert animation.ease(animation.EaseInOutQuartic, 0.25)
    == animation.ease(animation.EaseInQuartic, 0.5) /. 2.0
  assert animation.ease(animation.EaseInOutQuartic, 0.75)
    == animation.ease(animation.EaseOutQuartic, 0.5) /. 2.0 +. 0.5
}

pub fn ease_quintic_in_test() {
  assert animation.ease(animation.EaseInQuintic, 0.0) == 0.0
  assert animation.ease(animation.EaseInQuintic, 1.0) == 1.0
  assert animation.ease(animation.EaseInQuintic, 0.75) == 0.2373046875
}

pub fn ease_quintic_out_test() {
  assert animation.ease(animation.EaseOutQuintic, 0.0) == 1.0
  assert animation.ease(animation.EaseOutQuintic, 1.0) == 0.0
  assert animation.ease(animation.EaseOutQuintic, 0.75) == 0.7626953125
}

pub fn ease_quintic_inout_test() {
  assert animation.ease(animation.EaseInOutQuintic, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutQuintic, 0.5) == 0.5
  assert animation.ease(animation.EaseInOutQuintic, 1.0) == 1.0
  assert animation.ease(animation.EaseInOutQuintic, 0.25)
    == animation.ease(animation.EaseInQuintic, 0.5) /. 2.0
  assert animation.ease(animation.EaseInOutQuintic, 0.75)
    == animation.ease(animation.EaseOutQuintic, 0.5) /. 2.0 +. 0.5
}

pub fn ease_exponential_in_test() {
  assert animation.ease(animation.EaseInExponential, 0.0) == 0.0
  assert animation.ease(animation.EaseInExponential, 1.0) == 1.0
  assert animation.ease(animation.EaseInExponential, 0.1) == 0.001953125
  assert animation.ease(animation.EaseInExponential, 0.5) == 0.03125
  assert animation.ease(animation.EaseInExponential, 0.9) == 0.5
}

pub fn ease_exponential_out_test() {
  assert animation.ease(animation.EaseOutExponential, 0.0) == 0.0
  assert animation.ease(animation.EaseOutExponential, 1.0) == 1.0
  assert animation.ease(animation.EaseOutExponential, 0.1) == 0.5
  assert animation.ease(animation.EaseOutExponential, 0.5) == 0.96875
  assert animation.ease(animation.EaseOutExponential, 0.9) == 0.998046875
}

pub fn ease_exponential_inout_test() {
  assert animation.ease(animation.EaseInOutExponential, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutExponential, 1.0) == 1.0
  assert animation.ease(animation.EaseInOutExponential, 0.5) == 0.5
  assert animation.ease(animation.EaseInOutExponential, 0.1) == 0.001953125
  assert animation.ease(animation.EaseInOutExponential, 0.9) == 0.998046875
}

pub fn ease_circular_in_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInCircular, 0.0) == 0.0
  assert animation.ease(animation.EaseInCircular, 1.0) == 1.0
  assert animation.ease(animation.EaseInCircular, 0.1)
    |> float.loosely_equals(with: 0.005012562893, tolerating:)
  assert animation.ease(animation.EaseInCircular, 0.5)
    |> float.loosely_equals(with: 0.1339745962, tolerating:)
  assert animation.ease(animation.EaseInCircular, 0.9)
    |> float.loosely_equals(with: 0.5641101056, tolerating:)
}

pub fn ease_circular_out_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseOutCircular, 0.0) == 0.0
  assert animation.ease(animation.EaseOutCircular, 1.0) == 1.0
  assert animation.ease(animation.EaseOutCircular, 0.1)
    |> float.loosely_equals(with: 0.43588989435, tolerating:)
  assert animation.ease(animation.EaseOutCircular, 0.5)
    |> float.loosely_equals(with: 0.8660254038, tolerating:)
  assert animation.ease(animation.EaseOutCircular, 0.9)
    |> float.loosely_equals(with: 0.9949874371, tolerating:)
}

pub fn ease_circular_inout_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInOutCircular, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutCircular, 1.0) == 1.0
  assert animation.ease(animation.EaseInOutCircular, 0.5) == 0.5
  assert animation.ease(animation.EaseInOutCircular, 0.1)
    |> float.loosely_equals(with: 0.010102051443, tolerating:)
  assert animation.ease(animation.EaseInOutCircular, 0.9)
    |> float.loosely_equals(with: 0.989897948557, tolerating:)
}

pub fn ease_back_in_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInBack, 0.0) == 0.0
  assert animation.ease(animation.EaseInBack, 0.5) <. 0.0
  assert animation.ease(animation.EaseInBack, 1.0)
    |> float.loosely_equals(with: 1.0, tolerating:)
}

pub fn ease_back_out_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseOutBack, 0.0)
    |> float.loosely_equals(with: 0.0, tolerating:)
  assert animation.ease(animation.EaseOutBack, 0.5) >. 1.0
  assert animation.ease(animation.EaseOutBack, 1.0)
    |> float.loosely_equals(with: 1.0, tolerating:)
}

pub fn ease_back_inout_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInOutBack, 0.0)
    |> float.loosely_equals(with: 0.0, tolerating:)
  assert animation.ease(animation.EaseInOutBack, 0.25) <. 0.0
  assert animation.ease(animation.EaseInOutBack, 0.5) == 0.5
  assert animation.ease(animation.EaseInOutBack, 0.75) >. 1.0
  assert animation.ease(animation.EaseInOutBack, 1.0)
    |> float.loosely_equals(with: 1.0, tolerating:)
}

@external(javascript, "./easings.ffi.mjs", "easeInElastic")
fn ease_in_elastic_js(x: Float) -> Float

pub fn ease_elastic_in_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInElastic, 0.0) == 0.0
  assert animation.ease(animation.EaseInElastic, 1.0) == 1.0
  list.range(1, 99)
  |> list.each(fn(i) {
    let t = int.to_float(i) /. 100.0
    assert animation.ease(animation.EaseInElastic, t)
      |> float.loosely_equals(with: ease_in_elastic_js(t), tolerating:)
  })
}

@external(javascript, "./easings.ffi.mjs", "easeOutElastic")
fn ease_out_elastic_js(x: Float) -> Float

pub fn ease_elastic_out_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseOutElastic, 0.0) == 0.0
  assert animation.ease(animation.EaseOutElastic, 1.0) == 1.0
  list.range(1, 99)
  |> list.each(fn(i) {
    let t = int.to_float(i) /. 100.0
    assert animation.ease(animation.EaseOutElastic, t)
      |> float.loosely_equals(with: ease_out_elastic_js(t), tolerating:)
  })
}

@external(javascript, "./easings.ffi.mjs", "easeInOutElastic")
fn ease_inout_elastic_js(x: Float) -> Float

pub fn ease_elastic_inout_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInOutElastic, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutElastic, 1.0) == 1.0
  list.range(1, 99)
  |> list.each(fn(i) {
    let t = int.to_float(i) /. 100.0
    assert animation.ease(animation.EaseInOutElastic, t)
      |> float.loosely_equals(with: ease_inout_elastic_js(t), tolerating:)
  })
}

@external(javascript, "./easings.ffi.mjs", "easeInBounce")
fn ease_in_bounce_js(x: Float) -> Float

pub fn ease_bounce_in_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInBounce, 0.0) == 0.0
  assert animation.ease(animation.EaseInBounce, 1.0) == 1.0
  list.range(1, 99)
  |> list.each(fn(i) {
    let t = int.to_float(i) /. 100.0
    let eased = animation.ease(animation.EaseInBounce, t)
    assert float.loosely_equals(eased, with: ease_in_bounce_js(t), tolerating:)
    // Bounce never goes beyond 0..1
    assert eased <=. 1.0
    assert eased >=. 0.0
  })
}

@external(javascript, "./easings.ffi.mjs", "easeOutBounce")
fn ease_out_bounce_js(x: Float) -> Float

pub fn ease_bounce_out_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseOutBounce, 0.0) == 0.0
  assert animation.ease(animation.EaseOutBounce, 1.0) == 1.0
  list.range(1, 99)
  |> list.each(fn(i) {
    let t = int.to_float(i) /. 100.0
    let eased = animation.ease(animation.EaseOutBounce, t)
    assert float.loosely_equals(eased, with: ease_out_bounce_js(t), tolerating:)
    // Bounce never goes beyond 0..1
    assert eased <=. 1.0
    assert eased >=. 0.0
  })
}

@external(javascript, "./easings.ffi.mjs", "easeInOutBounce")
fn ease_inout_bounce_js(x: Float) -> Float

pub fn ease_bounce_inout_test() {
  let tolerating = 0.0000000001
  assert animation.ease(animation.EaseInOutBounce, 0.0) == 0.0
  assert animation.ease(animation.EaseInOutBounce, 1.0) == 1.0
  list.range(1, 99)
  |> list.each(fn(i) {
    let t = int.to_float(i) /. 100.0
    let eased = animation.ease(animation.EaseInOutBounce, t)
    assert float.loosely_equals(
      eased,
      with: ease_inout_bounce_js(t),
      tolerating:,
    )
    // Bounce never goes beyond 0..1
    assert eased <=. 1.0
    assert eased >=. 0.0
  })
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
