import gleam/function
import gleam/time/duration
import tiramisu/transform
import tiramisu/tween
import vec/vec3

// --- Float Tween Tests ---

pub fn tween_float_start_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
  let value = tween.get_value(my_tween)
  assert value == 0.0
}

pub fn tween_float_middle_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
    |> tween.update(duration.seconds(1))

  let value = tween.get_value(my_tween)
  assert value == 50.0
}

pub fn tween_float_end_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
    |> tween.update(duration.seconds(2))

  let value = tween.get_value(my_tween)
  assert value == 100.0
}

pub fn tween_float_past_end_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
    |> tween.update(duration.seconds(3))

  let value = tween.get_value(my_tween)
  assert value == 100.0
}

pub fn tween_float_complete_check_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
  assert tween.is_complete(my_tween) == False

  let my_tween = tween.update(my_tween, duration.seconds(2))
  assert tween.is_complete(my_tween) == True
}

// --- Vec3 Tween Tests ---

pub fn tween_vec3_start_test() {
  let my_tween =
    tween.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      duration.seconds(2),
      function.identity,
    )

  let value = tween.get_value(my_tween)
  assert value == vec3.Vec3(0.0, 0.0, 0.0)
}

pub fn tween_vec3_middle_test() {
  let my_tween =
    tween.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      duration.seconds(2),
      function.identity,
    )
    |> tween.update(duration.seconds(1))

  let value = tween.get_value(my_tween)
  assert value == vec3.Vec3(5.0, 10.0, 15.0)
}

pub fn tween_vec3_end_test() {
  let my_tween =
    tween.tween_vec3(
      vec3.Vec3(0.0, 0.0, 0.0),
      vec3.Vec3(10.0, 20.0, 30.0),
      duration.seconds(2),
      function.identity,
    )
    |> tween.update(duration.seconds(2))

  let value = tween.get_value(my_tween)
  assert value == vec3.Vec3(10.0, 20.0, 30.0)
}

// --- Transform Tween Tests ---

pub fn tween_transform_test() {
  let start = transform.at(vec3.Vec3(0.0, 0.0, 0.0))
  let end = transform.at(vec3.Vec3(10.0, 0.0, 0.0))

  let my_tween =
    tween.tween_transform(start, end, duration.seconds(2), function.identity)
    |> tween.update(duration.seconds(1))

  let value = tween.get_value(my_tween)
  assert transform.position(value) == vec3.Vec3(5.0, 0.0, 0.0)
}

// --- Tween Utilities Tests ---

pub fn reset_tween_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
    |> tween.update(duration.milliseconds(1500))

  let value = tween.get_value(my_tween)
  assert value >. 70.0

  let reset = tween.reset(my_tween)
  let value = tween.get_value(reset)
  assert value == 0.0
}

pub fn reverse_tween_test() {
  let my_tween =
    tween.tween_float(0.0, 100.0, duration.seconds(2), function.identity)
    |> tween.update(duration.seconds(1))

  let reversed = tween.reverse(my_tween)
  let value = tween.get_value(reversed)
  // Should now be at 50% from 100 to 0 = 50
  assert value == 50.0
}
