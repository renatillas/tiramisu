import {
  createInputManager as new_input_manager,
  inputManagerCaptureState as capture_input_state,
  inputManagerClearFrameState as clear_input_frame_state,
  inputManagerDestroy as destroy_input_manager,
  createAudioManager as new_audio_manager,
  audioManagerRegisterSource as register_audio_source,
  audioManagerUnregisterSource as unregister_audio_source,
  audioManagerGetSource as get_audio_source,
  audioManagerSetGroupVolume as set_group_volume,
  audioManagerGetGroupVolume as get_group_volume,
  audioManagerMuteGroup as mute_group,
  audioManagerUnmuteGroup as unmute_group,
  audioManagerDestroy as destroy_audio_manager,
} from "../../tiramisu.ffi.mjs";
import * as $input from "../../tiramisu/input.mjs";
import * as $renderer from "../../tiramisu/internal/renderer.mjs";

export {
  capture_input_state,
  clear_input_frame_state,
  destroy_audio_manager,
  destroy_input_manager,
  get_audio_source,
  get_group_volume,
  mute_group,
  new_audio_manager,
  new_input_manager,
  register_audio_source,
  set_group_volume,
  unmute_group,
  unregister_audio_source,
};
