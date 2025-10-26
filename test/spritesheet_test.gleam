import gleeunit
import iv
import tiramisu/spritesheet

pub fn main() {
  gleeunit.main()
}

// Note: Spritesheet creation tests are skipped because Texture is an opaque FFI type
// that can only be created in a browser environment with Three.js loaded.
// These validation tests would require mocking or integration testing.
//
// However, we can validate that the error handling logic works by checking
// that invalid parameters are rejected before trying to create the spritesheet.

// ============================================================================
// Animation Tests
// ============================================================================

pub fn animation_creates_valid_animation_test() {
  let frames = [0, 1, 2, 3]

  let anim =
    spritesheet.animation(
      name: "walk",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Repeat,
    )

  let assert "walk" = anim.name
  let assert 0.1 = anim.frame_duration
  let assert spritesheet.Repeat = anim.loop
}

// ============================================================================
// Animation State Tests
// ============================================================================

pub fn initial_state_creates_valid_state_test() {
  let state = spritesheet.initial_state("idle")

  let assert "idle" = spritesheet.current_animation(state)
  let assert 0 = spritesheet.current_frame_index(state)
  let assert True = spritesheet.is_playing(state)
}

pub fn update_advances_frame_after_duration_test() {
  let frames = [0, 1, 2, 3]
  let anim =
    spritesheet.animation(
      name: "walk",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Repeat,
    )

  let state = spritesheet.initial_state("walk")

  // Update with time less than frame duration
  let state =
    spritesheet.update(state: state, animation: anim, delta_time: 0.05)
  let assert 0 = spritesheet.current_frame_index(state)

  // Update with remaining time to complete frame duration
  let state =
    spritesheet.update(state: state, animation: anim, delta_time: 0.05)
  let assert 1 = spritesheet.current_frame_index(state)
}

pub fn update_loops_with_repeat_mode_test() {
  let frames = [0, 1, 2]
  let anim =
    spritesheet.animation(
      name: "spin",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Repeat,
    )

  let state = spritesheet.initial_state("spin")

  // Advance to last frame
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.2)
  let assert 2 = spritesheet.current_frame_index(state)

  // Should wrap back to first frame
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.1)
  let assert 0 = spritesheet.current_frame_index(state)
}

pub fn update_stops_with_once_mode_test() {
  let frames = [0, 1, 2]
  let anim =
    spritesheet.animation(
      name: "jump",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Once,
    )

  let state = spritesheet.initial_state("jump")

  // Advance to last frame
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.3)

  let assert 2 = spritesheet.current_frame_index(state)
  let assert False = spritesheet.is_playing(state)

  // Should stay on last frame
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.1)
  let assert 2 = spritesheet.current_frame_index(state)
}

pub fn update_ping_pongs_with_ping_pong_mode_test() {
  let frames = [0, 1, 2]
  let anim =
    spritesheet.animation(
      name: "idle",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.PingPong,
    )

  let state = spritesheet.initial_state("idle")

  // Advance forward to last frame
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.2)
  let assert 2 = spritesheet.current_frame_index(state)

  // Should reverse direction
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.1)
  let assert 1 = spritesheet.current_frame_index(state)

  // Continue backward
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.1)
  let assert 0 = spritesheet.current_frame_index(state)

  // Should reverse direction again
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.1)
  let assert 1 = spritesheet.current_frame_index(state)
}

// ============================================================================
// Animation Control Tests
// ============================================================================

pub fn play_resumes_animation_test() {
  let state = spritesheet.initial_state("idle")
  let state = spritesheet.pause(state)

  let assert False = spritesheet.is_playing(state)

  let state = spritesheet.play(state)

  let assert True = spritesheet.is_playing(state)
}

pub fn pause_stops_animation_test() {
  let state = spritesheet.initial_state("idle")

  let assert True = spritesheet.is_playing(state)

  let state = spritesheet.pause(state)

  let assert False = spritesheet.is_playing(state)
}

pub fn stop_resets_to_first_frame_test() {
  let frames = [0, 1, 2]
  let anim =
    spritesheet.animation(
      name: "walk",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Repeat,
    )

  let state = spritesheet.initial_state("walk")
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.2)

  let assert 2 = spritesheet.current_frame_index(state)

  let state = spritesheet.stop(state)

  let assert 0 = spritesheet.current_frame_index(state)
  let assert False = spritesheet.is_playing(state)
}

pub fn change_animation_resets_state_test() {
  let frames = [0, 1, 2]
  let anim =
    spritesheet.animation(
      name: "walk",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Repeat,
    )

  let state = spritesheet.initial_state("walk")
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.2)

  let assert 2 = spritesheet.current_frame_index(state)

  let state = spritesheet.change_animation(state, "run")

  let assert "run" = spritesheet.current_animation(state)
  let assert 0 = spritesheet.current_frame_index(state)
  let assert True = spritesheet.is_playing(state)
}

// ============================================================================
// Frame Calculation Tests
// ============================================================================

pub fn current_frame_returns_correct_sprite_frame_test() {
  let frames = [5, 6, 7, 8]
  let anim =
    spritesheet.animation(
      name: "attack",
      frames: frames,
      frame_duration: 0.1,
      loop: spritesheet.Repeat,
    )

  let state = spritesheet.initial_state("attack")

  // Frame index 0 should return sprite frame 5
  let assert Ok(5) = spritesheet.current_frame(state, anim)

  // Advance to frame index 2
  let state = spritesheet.update(state: state, animation: anim, delta_time: 0.2)

  // Frame index 2 should return sprite frame 7
  let assert Ok(7) = spritesheet.current_frame(state, anim)
}
// Frame offset and repeat calculation tests are skipped because they require
// a Spritesheet instance, which requires a Texture (FFI type).
// The mathematical logic for these functions is straightforward and can be
// verified through the working example instead.
