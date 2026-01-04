import gleam/time/duration
import gleeunit
import tiramisu/spritesheet

pub fn main() {
  gleeunit.main()
}

// Note: Most spritesheet tests are skipped because the AnimationMachine
// requires a Texture (opaque FFI type) that can only be created in a
// browser environment with Three.js loaded.
//
// The tests below focus on the public API that can be tested without FFI.

// ============================================================================
// Condition Constructor Tests
// ============================================================================

pub fn always_creates_always_condition_test() {
  // Just verify the function exists and returns a condition
  let _condition = spritesheet.always()
  Nil
}

pub fn after_duration_creates_duration_condition_test() {
  let _condition = spritesheet.after_duration(duration.seconds(2))
  Nil
}

pub fn custom_creates_custom_condition_test() {
  let _condition = spritesheet.custom(fn(_ctx) { True })
  Nil
}

// ============================================================================
// Loop Mode Tests
// ============================================================================

pub fn loop_modes_exist_test() {
  // Verify all loop modes are accessible
  let _ = spritesheet.Once
  let _ = spritesheet.Repeat
  let _ = spritesheet.PingPong
  Nil
}

// ============================================================================
// Error Type Tests
// ============================================================================

pub fn error_types_exist_test() {
  // Verify all error types are accessible
  let _ = spritesheet.InvalidColumns
  let _ = spritesheet.InvalidRows
  let _ = spritesheet.InvalidFrameCount
  let _ = spritesheet.FrameCountExceedsGrid
  Nil
}

// ============================================================================
// FrameData Type Tests
// ============================================================================

pub fn frame_data_types_exist_test() {
  // Verify FrameData constructors are accessible
  let single = spritesheet.SingleFrame(frame_index: 0)
  let assert spritesheet.SingleFrame(frame_index: 0) = single

  let blend =
    spritesheet.BlendingFrames(
      from_frame_index: 0,
      to_frame_index: 1,
      blend_factor: 0.5,
    )
  let assert spritesheet.BlendingFrames(
    from_frame_index: 0,
    to_frame_index: 1,
    blend_factor: 0.5,
  ) = blend

  Nil
}
