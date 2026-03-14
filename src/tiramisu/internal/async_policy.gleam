import gleam/option.{type Option, None, Some}

pub type RequestResolutionOutcome {
  DropRequestResolution
  EnqueueForLater
  ApplyNow
}

pub type ModelEventOutcome {
  DropModelEvent
  EmitModelEvent
}

pub fn request_resolution_outcome(
  is_current is_current: Bool,
  in_flight_generation in_flight_generation: Bool,
  runtime_available runtime_available: Bool,
  owner_exists owner_exists: Bool,
) -> RequestResolutionOutcome {
  case is_current && owner_exists {
    False -> DropRequestResolution
    True ->
      case in_flight_generation || !runtime_available {
        True -> EnqueueForLater
        False -> ApplyNow
      }
  }
}

pub fn should_apply_pending_request(
  is_current is_current: Bool,
  owner_exists owner_exists: Bool,
) -> Bool {
  is_current && owner_exists
}

pub fn model_event_outcome(
  is_current is_current: Bool,
  owner_exists owner_exists: Bool,
) -> ModelEventOutcome {
  case is_current && owner_exists {
    True -> EmitModelEvent
    False -> DropModelEvent
  }
}

pub fn should_reload_background(
  previous_signature previous_signature: Option(String),
  next_signature next_signature: String,
) -> Bool {
  case previous_signature {
    None -> True
    Some(previous) -> previous != next_signature
  }
}
