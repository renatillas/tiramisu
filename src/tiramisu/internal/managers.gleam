// Managers module - Wraps FFI manager classes as opaque types
// This provides type-safe access to mutable state managers from Gleam

import tiramisu/input
import tiramisu/internal/renderer

/// External type wrapping the JavaScript InputManager class
/// Manages all input state (keyboard, mouse, touch, gamepad)
pub type InputManager

/// Create a new InputManager instance
/// @param canvas The canvas element to attach input listeners to
@external(javascript, "../../tiramisu.ffi.mjs", "createInputManager")
pub fn new_input_manager(canvas: renderer.DomElement) -> InputManager

// FFI helper functions to call methods on the manager instance
// These need to be defined in tiramisu.ffi.mjs

/// Capture the current input state as an immutable snapshot
/// @param manager The InputManager instance
/// @returns An immutable InputState for use in Gleam
@external(javascript, "../../tiramisu.ffi.mjs", "inputManagerCaptureState")
pub fn capture_input_state(manager: InputManager) -> input.InputState

/// Clear per-frame input state
/// Should be called at the end of each frame
/// @param manager The InputManager instance
@external(javascript, "../../tiramisu.ffi.mjs", "inputManagerClearFrameState")
pub fn clear_input_frame_state(manager: InputManager) -> Nil

/// Cleanup the InputManager and remove event listeners
/// @param manager The InputManager instance
@external(javascript, "../../tiramisu.ffi.mjs", "inputManagerDestroy")
pub fn destroy_input_manager(manager: InputManager) -> Nil

/// External type wrapping the JavaScript AudioManager class
/// Manages all audio state (sources, groups, context)
pub type AudioManager

/// Create a new AudioManager instance
@external(javascript, "../../tiramisu.ffi.mjs", "createAudioManager")
pub fn new_audio_manager() -> AudioManager

/// Register an audio source with the manager
/// @param manager The AudioManager instance
/// @param id Audio source ID
/// @param source Audio source object
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerRegisterSource")
pub fn register_audio_source(
  manager: AudioManager,
  id: id,
  source: source,
) -> Nil

/// Unregister an audio source from the manager
/// @param manager The AudioManager instance
/// @param id Audio source ID
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerUnregisterSource")
pub fn unregister_audio_source(manager: AudioManager, id: id) -> Nil

/// Get an audio source from the manager
/// @param manager The AudioManager instance
/// @param id Audio source ID
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerGetSource")
pub fn get_audio_source(manager: AudioManager, id: id) -> source

/// Set group volume
/// @param manager The AudioManager instance
/// @param group Group name
/// @param volume Volume (0.0 to 1.0)
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerSetGroupVolume")
pub fn set_group_volume(manager: AudioManager, group: String, volume: Float) -> Nil

/// Get group volume
/// @param manager The AudioManager instance
/// @param group Group name
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerGetGroupVolume")
pub fn get_group_volume(manager: AudioManager, group: String) -> Float

/// Mute a group
/// @param manager The AudioManager instance
/// @param group Group name
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerMuteGroup")
pub fn mute_group(manager: AudioManager, group: String) -> Nil

/// Unmute a group
/// @param manager The AudioManager instance
/// @param group Group name
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerUnmuteGroup")
pub fn unmute_group(manager: AudioManager, group: String) -> Nil

/// Destroy the audio manager
/// @param manager The AudioManager instance
@external(javascript, "../../tiramisu.ffi.mjs", "audioManagerDestroy")
pub fn destroy_audio_manager(manager: AudioManager) -> Nil
