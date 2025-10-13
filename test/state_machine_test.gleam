import gleam/option
import tiramisu/state_machine

// Define test states
type State {
  Idle
  Walking
  Running
}

// Define test context
type Context {
  Context(speed: Float, jump_pressed: Bool)
}

// Since AnimationClip is opaque and requires FFI, we can't create real ones in tests
// But we can test the state machine logic with placeholder animations

// --- State Machine Creation Tests ---

pub fn new_state_machine_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.current_state(machine) == Idle
  assert state_machine.is_blending(machine) == False
  assert state_machine.state_count(machine) == 0
}

// --- State Count Tests ---

pub fn state_count_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.state_count(machine) == 0
}

pub fn transition_count_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.transition_count(machine) == 0
}

// --- State IDs Tests ---

pub fn state_ids_empty_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.state_ids(machine) == []
}

// --- Current State Tests ---

pub fn current_state_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.current_state(machine) == Idle
}

// --- Blending Tests ---

pub fn not_blending_initially_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.is_blending(machine) == False
}

pub fn blend_progress_none_when_not_blending_test() {
  let machine = state_machine.new(Idle)
  assert state_machine.blend_progress(machine) == option.None
}

// --- Default Blend Tests ---

pub fn set_default_blend_test() {
  let machine =
    state_machine.new(Idle)
    |> state_machine.set_default_blend(0.5)

  // Default blend is set (we can't directly check opaque type, but it doesn't error)
  assert state_machine.current_state(machine) == Idle
}

// --- Transition Tests ---

pub fn manual_transition_test() {
  let machine =
    state_machine.new(Idle)
    |> state_machine.transition_to(Walking, option.None)

  // After manual transition, we should be in Walking state (or blending to it)
  assert state_machine.is_blending(machine) == True
}

pub fn manual_transition_with_custom_blend_test() {
  let machine =
    state_machine.new(Idle)
    |> state_machine.transition_to(Walking, option.Some(0.1))

  assert state_machine.is_blending(machine) == True
}

// --- Condition Tests ---
// Note: Cannot directly test conditions as they require context types

// --- Animation Output Tests ---

pub fn animation_output_none_test() {
  let machine = state_machine.new(Idle)
  let output = state_machine.get_current_animation(machine)
  // No states added yet, should return None
  assert output == state_machine.None
}

pub fn animation_output_to_playback_none_test() {
  assert state_machine.to_playback(state_machine.None) == option.None
}

// Note: We can't fully test the state machine with real animations since
// AnimationClip is an opaque FFI type. The tests above verify the core
// state machine logic works correctly.

// --- State Machine Properties ---

pub fn state_machine_preserves_initial_state_test() {
  let machine = state_machine.new(Running)
  assert state_machine.current_state(machine) == Running
}

pub fn multiple_transitions_test() {
  let machine =
    state_machine.new(Idle)
    |> state_machine.transition_to(Walking, option.None)
    |> state_machine.transition_to(Running, option.None)

  // Should be blending (last transition takes effect)
  assert state_machine.is_blending(machine) == True
}

// --- Update Tests (without real animations) ---

pub fn update_without_states_test() {
  let machine = state_machine.new(Idle)
  let context = Context(speed: 0.0, jump_pressed: False)
  let #(updated, transitioned) = state_machine.update(machine, context, 0.016)

  // Should not transition (no states or transitions defined)
  assert transitioned == False
  assert state_machine.current_state(updated) == Idle
}

pub fn update_maintains_state_test() {
  let machine = state_machine.new(Walking)
  let context = Context(speed: 5.0, jump_pressed: False)
  let #(updated, _) = state_machine.update(machine, context, 0.016)

  assert state_machine.current_state(updated) == Walking
}
