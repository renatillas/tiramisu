import tiramisu/animation
import tiramisu/transform
import vec/vec3

// --- Easing Function Tests ---

// Test: Linear easing at boundaries
pub fn linear_easing_boundaries_test() {
  assert animation.ease(animation.Linear, 0.0) == 0.0
  assert animation.ease(animation.Linear, 1.0) == 1.0
}

// Test: Linear easing at midpoint
pub fn linear_easing_midpoint_test() {
  let result = animation.ease(animation.Linear, 0.5)
  assert result == 0.5
}

// Test: EaseInQuad produces expected curve
pub fn ease_in_quad_test() {
  let result = animation.ease(animation.EaseInQuad, 0.5)
  // 0.5 * 0.5 = 0.25
  assert result == 0.25
}

// Test: EaseOutQuad produces expected curve
pub fn ease_out_quad_test() {
  let result = animation.ease(animation.EaseOutQuad, 0.5)
  // 0.5 * (2.0 - 0.5) = 0.5 * 1.5 = 0.75
  assert result == 0.75
}

// Test: Easing clamps values below 0
pub fn easing_clamps_below_zero_test() {
  let result = animation.ease(animation.Linear, -0.5)
  assert result == 0.0
}

// Test: Easing clamps values above 1
pub fn easing_clamps_above_one_test() {
  let result = animation.ease(animation.Linear, 1.5)
  assert result == 1.0
}

// Test: All easing functions return 0 at t=0
pub fn all_easing_start_at_zero_test() {
  assert animation.ease(animation.Linear, 0.0) == 0.0
  assert animation.ease(animation.EaseInQuad, 0.0) == 0.0
  assert animation.ease(animation.EaseOutQuad, 0.0) == 0.0
  assert animation.ease(animation.EaseInCubic, 0.0) == 0.0
  assert animation.ease(animation.EaseOutCubic, 0.0) == 0.0
}

// Test: All easing functions return 1 at t=1
pub fn all_easing_end_at_one_test() {
  assert animation.ease(animation.Linear, 1.0) == 1.0
  assert animation.ease(animation.EaseInQuad, 1.0) == 1.0
  assert animation.ease(animation.EaseOutQuad, 1.0) == 1.0
  assert animation.ease(animation.EaseInCubic, 1.0) == 1.0
  assert animation.ease(animation.EaseOutCubic, 1.0) == 1.0
}

// --- Tween Creation Tests ---

// Test: Create float tween
pub fn create_float_tween_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)

  assert tw.start_value == 0.0
  assert tw.end_value == 10.0
  assert tw.duration == 1.0
  assert tw.elapsed == 0.0
}

// Test: Create vec3 tween
pub fn create_vec3_tween_test() {
  let start = vec3.Vec3(0.0, 0.0, 0.0)
  let end = vec3.Vec3(10.0, 10.0, 10.0)
  let tw = animation.tween_vec3(start, end, 2.0, animation.Linear)

  assert tw.start_value == start
  assert tw.end_value == end
  assert tw.duration == 2.0
  assert tw.elapsed == 0.0
}

// Test: Create transform tween
pub fn create_transform_tween_test() {
  let start = transform.identity
  let end = transform.at(position: vec3.Vec3(5.0, 5.0, 5.0))
  let tw = animation.tween_transform(start, end, 1.5, animation.EaseInQuad)

  assert tw.duration == 1.5
  assert tw.elapsed == 0.0
}

// --- Tween State Transitions Tests ---

// Test: Update tween advances elapsed time
pub fn update_tween_advances_time_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.5)

  assert tw.elapsed == 0.5
}

// Test: Multiple updates accumulate time
pub fn multiple_updates_accumulate_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.3)
  let tw = animation.update_tween(tw, 0.4)
  let tw = animation.update_tween(tw, 0.2)

  assert float_approx_equal(tw.elapsed, 0.9, 0.001)
}

// Test: Tween is not complete initially
pub fn tween_not_complete_initially_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  assert !animation.is_tween_complete(tw)
}

// Test: Tween is complete when elapsed >= duration
pub fn tween_complete_at_duration_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 1.0)

  assert animation.is_tween_complete(tw)
}

// Test: Tween is complete when elapsed > duration
pub fn tween_complete_past_duration_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 1.5)

  assert animation.is_tween_complete(tw)
}

// Test: Get tween value at start
pub fn get_tween_value_at_start_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let value = animation.get_tween_value(tw)

  assert value == 0.0
}

// Test: Get tween value at midpoint
pub fn get_tween_value_at_midpoint_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.5)
  let value = animation.get_tween_value(tw)

  assert value == 5.0
}

// Test: Get tween value at end
pub fn get_tween_value_at_end_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 1.0)
  let value = animation.get_tween_value(tw)

  assert value == 10.0
}

// Test: Get tween value past end (clamped)
pub fn get_tween_value_past_end_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 2.0)
  let value = animation.get_tween_value(tw)

  // Should clamp to end value
  assert value == 10.0
}

// Test: Eased tween value (EaseInQuad)
pub fn eased_tween_value_test() {
  let tw = animation.tween_float(0.0, 100.0, 1.0, animation.EaseInQuad)
  let tw = animation.update_tween(tw, 0.5)
  let value = animation.get_tween_value(tw)

  // With EaseInQuad at t=0.5: 0.5*0.5 = 0.25
  // So value = 0 + (100-0) * 0.25 = 25
  assert value == 25.0
}

// Test: Reset tween resets elapsed time
pub fn reset_tween_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.7)
  assert tw.elapsed == 0.7

  let tw = animation.reset_tween(tw)
  assert tw.elapsed == 0.0
}

// Test: Reset preserves other tween properties
pub fn reset_preserves_properties_test() {
  let tw = animation.tween_float(5.0, 15.0, 2.0, animation.EaseInCubic)
  let tw = animation.update_tween(tw, 1.0)
  let tw = animation.reset_tween(tw)

  assert tw.start_value == 5.0
  assert tw.end_value == 15.0
  assert tw.duration == 2.0
}

// Test: Reverse tween swaps start and end
pub fn reverse_tween_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.reverse_tween(tw)

  assert tw.start_value == 10.0
  assert tw.end_value == 0.0
}

// Test: Reverse preserves duration
pub fn reverse_preserves_duration_test() {
  let tw = animation.tween_float(0.0, 10.0, 3.5, animation.Linear)
  let tw = animation.reverse_tween(tw)

  assert tw.duration == 3.5
}

// Test: Vec3 tween interpolation
pub fn vec3_tween_interpolation_test() {
  let start = vec3.Vec3(0.0, 0.0, 0.0)
  let end = vec3.Vec3(10.0, 20.0, 30.0)
  let tw = animation.tween_vec3(start, end, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.5)
  let value = animation.get_tween_value(tw)

  assert value.x == 5.0
  assert value.y == 10.0
  assert value.z == 15.0
}

// --- Complex State Transition Tests ---

// Test: Reset and replay tween
pub fn reset_and_replay_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 1.0)
  assert animation.is_tween_complete(tw)

  let tw = animation.reset_tween(tw)
  assert !animation.is_tween_complete(tw)

  let tw = animation.update_tween(tw, 0.3)
  let value = animation.get_tween_value(tw)
  assert value == 3.0
}

// Test: Reverse mid-animation
pub fn reverse_mid_animation_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.6)
  let value1 = animation.get_tween_value(tw)
  assert value1 == 6.0

  // Reverse (but keep elapsed time)
  let tw = animation.reverse_tween(tw)
  // Now start=10, end=0, elapsed=0.6
  let value2 = animation.get_tween_value(tw)
  // At t=0.6: 10 + (0-10)*0.6 = 10 - 6 = 4
  assert value2 == 4.0
}

// Test: Chain of state transitions
pub fn chain_state_transitions_test() {
  // Start -> Update -> Complete -> Reset -> Update again
  let tw = animation.tween_float(0.0, 100.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.25)
  let v1 = animation.get_tween_value(tw)
  assert v1 == 25.0

  let tw = animation.update_tween(tw, 0.75)
  assert animation.is_tween_complete(tw)
  let v2 = animation.get_tween_value(tw)
  assert v2 == 100.0

  let tw = animation.reset_tween(tw)
  let tw = animation.update_tween(tw, 0.5)
  let v3 = animation.get_tween_value(tw)
  assert v3 == 50.0
}

// Test: Zero duration tween
pub fn zero_duration_tween_test() {
  let tw = animation.tween_float(0.0, 10.0, 0.0, animation.Linear)
  let value = animation.get_tween_value(tw)

  // Should immediately be at end value
  assert value == 10.0
  assert animation.is_tween_complete(tw)
}

// Test: Negative values tween correctly
pub fn negative_values_tween_test() {
  let tw = animation.tween_float(-10.0, -5.0, 1.0, animation.Linear)
  let tw = animation.update_tween(tw, 0.5)
  let value = animation.get_tween_value(tw)

  // -10 + (-5 - -10) * 0.5 = -10 + 2.5 = -7.5
  assert value == -7.5
}

// Test: Reverse then reverse returns to original
pub fn double_reverse_test() {
  let tw = animation.tween_float(0.0, 10.0, 1.0, animation.Linear)
  let tw = animation.reverse_tween(tw)
  let tw = animation.reverse_tween(tw)

  assert tw.start_value == 0.0
  assert tw.end_value == 10.0
}

// --- Helper Functions ---

fn float_approx_equal(a: Float, b: Float, epsilon: Float) -> Bool {
  let diff = case a >. b {
    True -> a -. b
    False -> b -. a
  }
  diff <. epsilon
}
