import gleam/bool

/// Three.js AnimationMixer (opaque type)
pub type AnimationMixer

/// Three.js AnimationAction (opaque type)
pub type AnimationAction

/// Three.js AnimationClip (opaque type)
pub type AnimationClip

/// Looping mode for animations
pub type LoopMode {
  LoopOnce
  LoopRepeat
  LoopPingPong
}

/// Animation error
pub type AnimationError {
  InvalidClipName(String)
  InvalidDuration(Float)
  MixerNotFound
}

/// Create a new AnimationMixer for an object
@external(javascript, "../three/ffi/animation.mjs", "createMixer")
pub fn create_mixer(object: a) -> AnimationMixer

/// Update the mixer (call this in your game loop with delta time)
@external(javascript, "../three/ffi/animation.mjs", "updateMixer")
pub fn update(mixer: AnimationMixer, delta: Float) -> AnimationMixer

/// Get an action from a clip
@external(javascript, "../three/ffi/animation.mjs", "clipAction")
fn ffi_clip_action(mixer: AnimationMixer, clip: AnimationClip) -> AnimationAction

/// Play an animation action
@external(javascript, "../three/ffi/animation.mjs", "playAction")
fn ffi_play_action(action: AnimationAction) -> AnimationAction

/// Stop an animation action
@external(javascript, "../three/ffi/animation.mjs", "stopAction")
fn ffi_stop_action(action: AnimationAction) -> AnimationAction

/// Pause an animation action
@external(javascript, "../three/ffi/animation.mjs", "pauseAction")
fn ffi_pause_action(action: AnimationAction) -> AnimationAction

/// Set action time scale (speed)
@external(javascript, "../three/ffi/animation.mjs", "setTimeScale")
fn ffi_set_time_scale(action: AnimationAction, scale: Float) -> AnimationAction

/// Set action loop mode
@external(javascript, "../three/ffi/animation.mjs", "setLoop")
fn ffi_set_loop(
  action: AnimationAction,
  mode: LoopMode,
  repetitions: Int,
) -> AnimationAction

/// Check if action is running
@external(javascript, "../three/ffi/animation.mjs", "isRunning")
pub fn is_running(action: AnimationAction) -> Bool

/// Get all clips from an object (like loaded GLTF)
@external(javascript, "../three/ffi/animation.mjs", "getClips")
pub fn get_clips(object: a) -> List(AnimationClip)

/// Find clip by name
@external(javascript, "../three/ffi/animation.mjs", "findClipByName")
fn ffi_find_clip(clips: List(AnimationClip), name: String) -> Result(
  AnimationClip,
  Nil,
)

/// Create an action from a clip
pub fn clip_action(
  mixer: AnimationMixer,
  clip: AnimationClip,
) -> AnimationAction {
  ffi_clip_action(mixer, clip)
}

/// Find and create an action for a named animation clip
pub fn find_action(
  mixer: AnimationMixer,
  clips: List(AnimationClip),
  name: String,
) -> Result(AnimationAction, AnimationError) {
  case ffi_find_clip(clips, name) {
    Ok(clip) -> Ok(clip_action(mixer, clip))
    Error(_) -> Error(InvalidClipName(name))
  }
}

/// Play an animation action
pub fn play(action: AnimationAction) -> AnimationAction {
  ffi_play_action(action)
}

/// Stop an animation action
pub fn stop(action: AnimationAction) -> AnimationAction {
  ffi_stop_action(action)
}

/// Pause an animation action
pub fn pause(action: AnimationAction) -> AnimationAction {
  ffi_pause_action(action)
}

/// Set animation playback speed (1.0 = normal, 2.0 = double speed, 0.5 = half speed)
pub fn set_time_scale(action: AnimationAction, scale: Float) -> AnimationAction {
  ffi_set_time_scale(action, scale)
}

/// Set animation loop mode
pub fn set_loop(
  action: AnimationAction,
  mode: LoopMode,
  repetitions: Int,
) -> Result(AnimationAction, AnimationError) {
  use <- bool.guard(repetitions < 0, Error(InvalidDuration(0.0)))
  Ok(ffi_set_loop(action, mode, repetitions))
}

/// Set action to loop once
pub fn loop_once(action: AnimationAction) -> AnimationAction {
  ffi_set_loop(action, LoopOnce, 1)
}

/// Set action to loop indefinitely
pub fn loop_repeat(action: AnimationAction) -> AnimationAction {
  ffi_set_loop(action, LoopRepeat, 999_999)
}

/// Set action to loop with ping-pong (forward then backward)
pub fn loop_ping_pong(action: AnimationAction) -> AnimationAction {
  ffi_set_loop(action, LoopPingPong, 999_999)
}
