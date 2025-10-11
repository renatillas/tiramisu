/// Audio Playback FFI
///
/// Handles audio source control (play, pause, stop, volume, etc.)
/// Uses THREE.Audio and THREE.PositionalAudio
import * as THREE from 'three';
import { getAudioListener } from './asset.mjs';

// Convert Gleam ID to string for use as Map key
// This ensures IDs can be properly compared across frames
// We include the constructor name to differentiate between types with no fields
function idToString(id) {
  // Get constructor name (Gleam custom types compile to JavaScript classes)
  const typeName = id.constructor.name;
  // Serialize the object's properties
  const props = JSON.stringify(id);
  // Combine both for a unique key
  return `${typeName}:${props}`;
}

// Global registry of audio sources by ID (using string keys)
const audioSources = new Map();

// Audio group volumes (sfx, music, voice, ambient, custom)
const groupVolumes = new Map([
  ['sfx', 1.0],
  ['music', 1.0],
  ['voice', 1.0],
  ['ambient', 1.0],
]);

// Muted groups
const mutedGroups = new Set();

// Track if AudioContext has been resumed
let audioContextResumed = false;
let pendingPlaybacks = []; // Store {id, source, fadeDuration} instead of just sources

/**
 * Resume AudioContext after user interaction
 */
function resumeAudioContext() {
  if (audioContextResumed) return;

  const listener = getAudioListener();
  if (listener.context.state === 'suspended') {
    listener.context.resume().then(() => {
      audioContextResumed = true;

      // Play any pending audio sources
      pendingPlaybacks.forEach(({id, source, fadeDuration}) => {
        if (source.buffer && !source.isPlaying) {
          try {
            if (fadeDuration > 0) {
              playWithFadeInternal(source, fadeDuration);
            } else {
              source.play();
            }
          } catch (error) {
            console.error(`[Tiramisu] Failed to play queued audio ${id}:`, error);
          }
        }
      });
      pendingPlaybacks = [];
    });
  } else {
    audioContextResumed = true;
  }
}

// Runtime guards namespace - prevents duplicate event listeners across hot reloads
// Stored on window object to persist across module reloads
if (typeof window !== 'undefined' && !window.__tiramisu) {
  window.__tiramisu = {
    initialized: {
      audioContextListeners: false,
      resizeListener: false,
    }
  };
}

// Add event listeners for user interaction (only once, survives hot reload)
if (typeof document !== 'undefined' && !window.__tiramisu.initialized.audioContextListeners) {
  window.__tiramisu.initialized.audioContextListeners = true;
  ['click', 'touchstart', 'keydown'].forEach(eventType => {
    document.addEventListener(eventType, resumeAudioContext, { once: true });
  });
}

/**
 * Register an audio source
 * @param {string} id - Unique identifier
 * @param {THREE.Audio | THREE.PositionalAudio} source - Audio source
 */
export function registerAudioSource(id, source) {
  audioSources.set(idToString(id), source);
}

/**
 * Unregister an audio source
 * @param {string} id - Unique identifier
 */
export function unregisterAudioSource(id) {
  const idStr = idToString(id);
  const source = audioSources.get(idStr);
  if (source && source.isPlaying) {
    // Only stop looping sounds or music
    // One-shot sounds (loop=false) should play to completion
    if (source.loop) {
      source.stop();
    }
  }
  audioSources.delete(idStr);
}

/**
 * Get an audio source by ID
 */
function getAudioSource(id) {
  return audioSources.get(idToString(id));
}

/**
 * Get an audio source for cleanup purposes (exported for renderer)
 * @param {string} id - Audio source ID
 * @returns {THREE.Audio | null}
 */
export function getAudioSourceForCleanup(id) {
  return getAudioSource(id);
}

/**
 * Create and configure an audio source from a scene Audio node
 * @param {string} id - Audio node ID
 * @param {AudioBuffer} buffer - Audio buffer
 * @param {AudioConfig} config - Audio configuration
 * @param {AudioType} audioType - GlobalAudio or PositionalAudio
 */
export function playAudio(id, buffer, config, audioType) {
  // Remove existing source if it exists
  const idStr = idToString(id);
  if (audioSources.has(idStr)) {
    stopAudio(id);
    unregisterAudioSource(id);
  }

  // Get or create audio listener
  const listener = getAudioListener();

  // Create appropriate audio source based on type
  let source;
  const typeName = audioType.constructor.name;

  if (typeName === 'GlobalAudio') {
    // Create global 2D audio
    source = new THREE.Audio(listener);
  } else if (typeName === 'PositionalAudio') {
    // Create positional 3D audio
    source = new THREE.PositionalAudio(listener);
    // Set position from audio type
    if (audioType.position) {
      source.position.set(audioType.position.x, audioType.position.y, audioType.position.z);
    }
    // Set distance properties
    source.setRefDistance(audioType.ref_distance || 1.0);
    source.setMaxDistance(audioType.max_distance || 10000.0);
    source.setRolloffFactor(audioType.rolloff_factor || 1.0);
  }

  // Store the buffer in userData instead of calling setBuffer() immediately
  // We'll call setBuffer() right before playing to avoid issues with suspended AudioContext
  source.userData = source.userData || {};
  source.userData.pendingBuffer = buffer;

  // Store base volume, group, previous state, and on_end callback in userData
  source.userData = source.userData || {};
  source.userData.baseVolume = config.volume;
  source.userData.previousState = null; // Initial state (null means not initialized yet)

  // Store on_end callback if provided
  // NOTE: We store it in userData and will set up the listener when audio plays
  // DO NOT override source.onEnded as it's used internally by Three.js
  if (config.on_end && config.on_end[0]) {
    source.userData.onEnd = config.on_end[0];
  } else {
    source.userData.onEnd = null;
  }

  // Get group from config if it exists
  if (config.group && config.group[0]) {
    const groupObj = config.group[0];
    let groupName;

    // Convert Gleam AudioGroup to string
    switch (groupObj.constructor.name) {
      case 'SFX':
        groupName = 'sfx';
        break;
      case 'Music':
        groupName = 'music';
        break;
      case 'Voice':
        groupName = 'voice';
        break;
      case 'Ambient':
        groupName = 'ambient';
        break;
      case 'Custom':
        groupName = groupObj[0]; // Custom group name
        break;
      default:
        groupName = null;
    }

    if (groupName) {
      source.userData.group = groupName;

      // Apply group volume and mute state
      const groupVolume = groupVolumes.get(groupName) || 1.0;
      const isMuted = mutedGroups.has(groupName);
      const effectiveVolume = isMuted ? 0.0 : config.volume * groupVolume;
      source.setVolume(effectiveVolume);
    } else {
      source.setVolume(config.volume);
    }
  } else {
    source.setVolume(config.volume);
  }

  source.setLoop(config.loop);
  source.setPlaybackRate(config.playback_rate);

  // Register the source
  registerAudioSource(id, source);

  // Set initial state
  const initialStateName = config.state.constructor.name;
  if (initialStateName === 'Stopped' || initialStateName === 'Paused') {
    source.userData.previousState = initialStateName;
  } else {
    applyAudioState(id, source, config);
  }

  return source;
}

/**
 * Apply audio state transitions declaratively
 * Compares previous state with new state to determine action
 * @param {string} id - Audio source ID
 * @param {THREE.Audio} source - Audio source
 * @param {AudioConfig} config - Audio configuration
 */
function applyAudioState(id, source, config) {
  // Extract current state from config
  const stateName = config.state.constructor.name;
  const previousState = source.userData.previousState || 'Stopped';

  // Extract fade config
  const fadeConfig = config.fade;
  const fadeDuration = (fadeConfig && fadeConfig.duration_ms) || 0;

  const listener = getAudioListener();

  // State transition logic
  if (stateName === 'Playing' && previousState !== 'Playing') {
    // Set buffer right before playing (if not already set)
    if (!source.buffer && source.userData.pendingBuffer) {
      source.setBuffer(source.userData.pendingBuffer);
    }

    // Check if source has a buffer before attempting to play
    if (!source.buffer) {
      console.warn(`[Tiramisu] Cannot play audio without buffer: ${id}`);
      return;
    }

    // Transition to Playing state
    if (listener.context.state === 'suspended') {
      // AudioContext is suspended, add to pending list
      pendingPlaybacks.push({ id, source, fadeDuration });
    } else {
      // Check if already playing - don't play again
      if (source.isPlaying) {
        source.userData.previousState = 'Playing';
        return;
      }

      try {
        // When resuming from Paused, don't apply fade - just resume
        const isResumingFromPause = previousState === 'Paused';

        if (fadeDuration > 0 && !isResumingFromPause) {
          playWithFadeInternal(source, fadeDuration);
        } else {
          source.play();
          // Restore volume when resuming from pause (no fade)
          if (isResumingFromPause) {
            const groupVolume = source.userData.group
              ? groupVolumes.get(source.userData.group) || 1.0
              : 1.0;
            const isMuted = source.userData.group
              ? mutedGroups.has(source.userData.group)
              : false;
            const baseVolume = source.userData.baseVolume || 1.0;
            const effectiveVolume = isMuted ? 0.0 : baseVolume * groupVolume;
            source.setVolume(effectiveVolume);
          }
        }
      } catch (error) {
        console.error(`[Tiramisu] Failed to play audio ${id}:`, error);
      }
    }
    source.userData.previousState = 'Playing';
  } else if (stateName === 'Stopped' && previousState !== 'Stopped') {
    // Transition to Stopped state
    if (source.isPlaying) {
      if (fadeDuration > 0) {
        stopWithFadeInternal(source, fadeDuration, false);
      } else {
        source.stop();
      }
    }
    source.userData.previousState = 'Stopped';
  } else if (stateName === 'Paused' && previousState !== 'Paused') {
    // Transition to Paused state
    if (source.isPlaying) {
      source.pause();
    }
    source.userData.previousState = 'Paused';
  }
}

/**
 * Stop an audio source (resets to beginning)
 * @param {string} sourceId - Audio source ID
 */
export function stopAudio(sourceId) {
  const source = getAudioSource(sourceId);
  if (source) {
    if (source.isPlaying) {
      source.stop();
    }
    unregisterAudioSource(sourceId);
  }
}


/**
 * Update audio configuration for an existing audio source (declarative)
 * @param {string} id - Audio source ID
 * @param {AudioConfig} config - New audio configuration
 */
export function updateAudioConfig(id, config) {
  const source = getAudioSource(id);
  if (!source) {
    return;
  }

  // Update base volume and other properties
  source.userData.baseVolume = config.volume;

  // Update on_end callback if changed
  if (config.on_end && config.on_end[0]) {
    source.userData.onEnd = config.on_end[0];
  } else {
    source.userData.onEnd = null;
  }

  // Apply group volume if source belongs to a group
  if (source.userData.group) {
    const groupVolume = groupVolumes.get(source.userData.group) || 1.0;
    const isMuted = mutedGroups.has(source.userData.group);
    const effectiveVolume = isMuted ? 0.0 : config.volume * groupVolume;
    source.setVolume(effectiveVolume);
  } else {
    source.setVolume(config.volume);
  }

  source.setLoop(config.loop);
  source.setPlaybackRate(config.playback_rate);

  // Apply state transitions declaratively
  applyAudioState(id, source, config);
}

// --- Fading Functions ---

/**
 * Internal function: Play audio with fade-in
 * @param {THREE.Audio} source - Audio source
 * @param {number} fadeInMs - Fade-in duration in milliseconds
 */
function playWithFadeInternal(source, fadeInMs) {
  const targetVolume = source.userData?.baseVolume || source.getVolume() || 1.0;

  if (!source.buffer) {
    console.warn('[Tiramisu] Cannot play audio without buffer');
    return;
  }

  source.setVolume(0.0);
  if (!source.isPlaying) {
    try {
      source.play();
    } catch (error) {
      console.error('[Tiramisu] Failed to play audio:', error);
      return;
    }
  }

  const startTime = Date.now();
  const fadeInterval = setInterval(() => {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / fadeInMs, 1.0);
    const currentVolume = progress * targetVolume;
    source.setVolume(currentVolume);

    if (progress >= 1.0) {
      clearInterval(fadeInterval);
    }
  }, 16);
}

/**
 * Internal function: Stop audio with fade-out
 * @param {THREE.Audio} source - Audio source
 * @param {number} fadeOutMs - Fade-out duration in milliseconds
 * @param {boolean} pauseInsteadOfStop - If true, pause instead of stop
 */
function stopWithFadeInternal(source, fadeOutMs, pauseInsteadOfStop = false) {
  if (!source.isPlaying) return;

  const startVolume = source.getVolume();
  const startTime = Date.now();

  const fadeInterval = setInterval(() => {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / fadeOutMs, 1.0);
    const currentVolume = startVolume * (1.0 - progress);
    source.setVolume(currentVolume);

    if (progress >= 1.0) {
      clearInterval(fadeInterval);
      if (pauseInsteadOfStop) {
        source.pause();
        source.offset = 0;
      } else {
        source.stop();
      }
      source.setVolume(startVolume);
    }
  }, 16);
}

// --- Audio Groups ---

/**
 * Set volume for an audio group
 * @param {string} group - Group name (sfx, music, voice, ambient, or custom)
 * @param {number} volume - Volume (0.0 to 1.0)
 */
export function setGroupVolume(group, volume) {
  const clampedVolume = Math.max(0.0, Math.min(1.0, volume));
  groupVolumes.set(group, clampedVolume);

  // Update all audio sources in this group
  audioSources.forEach((source) => {
    if (source.userData && source.userData.group === group) {
      const baseVolume = source.userData.baseVolume || 1.0;
      const effectiveVolume = mutedGroups.has(group) ? 0.0 : baseVolume * clampedVolume;
      source.setVolume(effectiveVolume);
    }
  });
}

/**
 * Get current volume for an audio group
 * @param {string} group - Group name
 * @returns {number} Volume (0.0 to 1.0)
 */
export function getGroupVolume(group) {
  return groupVolumes.get(group) || 1.0;
}

/**
 * Mute an audio group
 * @param {string} group - Group name
 */
export function muteGroup(group) {
  mutedGroups.add(group);

  audioSources.forEach((source) => {
    if (source.userData && source.userData.group === group) {
      source.setVolume(0.0);
    }
  });
}

/**
 * Unmute an audio group
 * @param {string} group - Group name
 */
export function unmuteGroup(group) {
  mutedGroups.delete(group);

  const groupVolume = groupVolumes.get(group) || 1.0;
  audioSources.forEach((source) => {
    if (source.userData && source.userData.group === group) {
      const baseVolume = source.userData.baseVolume || 1.0;
      source.setVolume(baseVolume * groupVolume);
    }
  });
}
