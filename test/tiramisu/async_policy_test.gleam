import tiramisu/internal/async_policy
import gleam/option

pub fn model_event_policy_test() {
  let assert async_policy.EmitModelEvent =
    async_policy.model_event_outcome(
      is_current: True,
      owner_exists: True,
    )

  let assert async_policy.DropModelEvent =
    async_policy.model_event_outcome(
      is_current: False,
      owner_exists: True,
    )

  let assert async_policy.DropModelEvent =
    async_policy.model_event_outcome(
      is_current: True,
      owner_exists: False,
    )

  let assert async_policy.DropModelEvent =
    async_policy.model_event_outcome(
      is_current: False,
      owner_exists: False,
    )
}

pub fn request_resolution_policy_test() {
  let assert async_policy.ApplyNow =
    async_policy.request_resolution_outcome(
      is_current: True,
      in_flight_generation: False,
      runtime_available: True,
      owner_exists: True,
    )

  let assert async_policy.EnqueueForLater =
    async_policy.request_resolution_outcome(
      is_current: True,
      in_flight_generation: True,
      runtime_available: True,
      owner_exists: True,
    )

  let assert async_policy.EnqueueForLater =
    async_policy.request_resolution_outcome(
      is_current: True,
      in_flight_generation: False,
      runtime_available: False,
      owner_exists: True,
    )

  let assert async_policy.DropRequestResolution =
    async_policy.request_resolution_outcome(
      is_current: False,
      in_flight_generation: False,
      runtime_available: True,
      owner_exists: True,
    )

  let assert async_policy.DropRequestResolution =
    async_policy.request_resolution_outcome(
      is_current: True,
      in_flight_generation: False,
      runtime_available: True,
      owner_exists: False,
    )

  let assert async_policy.DropRequestResolution =
    async_policy.request_resolution_outcome(
      is_current: False,
      in_flight_generation: True,
      runtime_available: False,
      owner_exists: False,
    )
}

pub fn pending_request_policy_test() {
  assert
    async_policy.should_apply_pending_request(
      is_current: True,
      owner_exists: True,
    )

  assert !async_policy.should_apply_pending_request(
      is_current: False,
      owner_exists: True,
  )

  assert !async_policy.should_apply_pending_request(
      is_current: True,
      owner_exists: False,
  )
}

pub fn background_reload_policy_test() {
  assert async_policy.should_reload_background(
    previous_signature: option.None,
    next_signature: "texture:background.jpg::srgb",
  )

  assert !async_policy.should_reload_background(
    previous_signature: option.Some("texture:background.jpg::srgb"),
    next_signature: "texture:background.jpg::srgb",
  )

  assert async_policy.should_reload_background(
    previous_signature: option.Some("texture:background.jpg::srgb"),
    next_signature: "texture:background.jpg::linear-srgb",
  )

  assert async_policy.should_reload_background(
    previous_signature: option.Some("texture:background.jpg::srgb"),
    next_signature: "none::srgb",
  )
}
