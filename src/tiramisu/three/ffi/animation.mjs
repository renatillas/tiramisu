import * as THREE from 'three';
import { Ok, Error as GleamError, toList } from '../../../gleam.mjs';

// Create a new AnimationMixer
export function createMixer(object) {
  return new THREE.AnimationMixer(object);
}

// Update the mixer with delta time
export function updateMixer(mixer, delta) {
  mixer.update(delta);
  return mixer;
}

// Get an action for a clip
export function clipAction(mixer, clip) {
  return mixer.clipAction(clip);
}

// Play an action
export function playAction(action) {
  action.play();
  return action;
}

// Stop an action
export function stopAction(action) {
  action.stop();
  return action;
}

// Pause an action
export function pauseAction(action) {
  action.paused = true;
  return action;
}

// Set time scale (playback speed)
export function setTimeScale(action, scale) {
  action.timeScale = scale;
  return action;
}

// Helper to convert Gleam LoopMode to THREE.js constant
function getLoopMode(modeEnum) {
  if (modeEnum.constructor.name === 'LoopOnce') {
    return THREE.LoopOnce;
  } else if (modeEnum.constructor.name === 'LoopRepeat') {
    return THREE.LoopRepeat;
  } else if (modeEnum.constructor.name === 'LoopPingPong') {
    return THREE.LoopPingPong;
  }
  return THREE.LoopRepeat; // default
}

// Set loop mode
export function setLoop(action, mode, repetitions) {
  action.setLoop(getLoopMode(mode), repetitions);
  return action;
}

// Check if action is running
export function isRunning(action) {
  return action.isRunning();
}

// Get all animation clips from an object
export function getClips(object) {
  // For GLTF models, clips are usually in object.animations
  const clips = object.animations || [];
  return toList(clips);
}

// Find a clip by name
export function findClipByName(clips, name) {
  // Convert Gleam list to JS array
  const clipsArray = clips.toArray();

  const clip = clipsArray.find(c => c.name === name);

  if (clip) {
    return new Ok(clip);
  } else {
    return new GleamError(undefined);
  }
}
