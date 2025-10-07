/// Audio Playback FFI
///
/// Handles audio source control (play, pause, stop, volume, etc.)
/// Uses THREE.Audio and THREE.PositionalAudio
import * as THREE from 'three';
import { getAudioListener } from './asset.mjs';

// Global registry of audio sources by ID
const audioSources = new Map();

// Track if AudioContext has been resumed
let audioContextResumed = false;
let pendingAudioSources = [];

/**
 * Resume AudioContext after user interaction
 */
function resumeAudioContext() {
  if (audioContextResumed) return;

  const listener = getAudioListener();
  if (listener.context.state === 'suspended') {
    listener.context.resume().then(() => {
      console.log('[Tiramisu] AudioContext resumed after user interaction');
      audioContextResumed = true;

      // Play any pending audio sources
      pendingAudioSources.forEach(source => {
        if (!source.isPlaying) {
          source.play();
        }
      });
      pendingAudioSources = [];
    });
  } else {
    audioContextResumed = true;
  }
}

// Add event listeners for user interaction
if (typeof document !== 'undefined') {
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
  audioSources.set(id, source);
  console.log(`[Tiramisu] Audio source registered: ${id}`);
}

/**
 * Unregister an audio source
 * @param {string} id - Unique identifier
 */
export function unregisterAudioSource(id) {
  const source = audioSources.get(id);
  if (source && source.isPlaying) {
    source.stop();
  }
  audioSources.delete(id);
  console.log(`[Tiramisu] Audio source unregistered: ${id}`);
}

/**
 * Get an audio source by ID
 */
function getAudioSource(id) {
  return audioSources.get(id);
}

/**
 * Create and play an audio source from a scene Audio node
 * @param {string} id - Audio node ID
 * @param {AudioBuffer} buffer - Audio buffer
 * @param {AudioConfig} config - Audio configuration
 * @param {AudioType} audioType - GlobalAudio or PositionalAudio
 */
export function playAudio(id, buffer, config, audioType) {
  // Remove existing source if it exists
  if (audioSources.has(id)) {
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

  // Set buffer
  source.setBuffer(buffer);

  // Apply configuration
  source.setVolume(config.volume);
  source.setLoop(config.loop);
  source.setPlaybackRate(config.playback_rate);

  // Register the source
  registerAudioSource(id, source);

  // Start playing if autoplay is enabled
  if (config.autoplay) {
    if (listener.context.state === 'suspended') {
      // AudioContext is suspended, add to pending list
      pendingAudioSources.push(source);
      console.log(`[Tiramisu] Audio queued (waiting for user interaction): ${id}`);
      console.log('[Tiramisu] Click, tap, or press any key to enable audio');
    } else {
      source.play();
      console.log(`[Tiramisu] Auto-playing audio: ${id}`);
    }
  }

  return source;
}

/**
 * Resume playing an existing audio source
 * @param {string} sourceId - Audio source ID
 */
export function resumeAudio(sourceId) {
  const source = getAudioSource(sourceId);
  if (source) {
    if (!source.isPlaying) {
      source.play();
      console.log(`[Tiramisu] Playing audio: ${sourceId}`);
    }
  } else {
    console.warn(`[Tiramisu] Audio source not found: ${sourceId}`);
  }
}

/**
 * Pause an audio source
 * @param {string} sourceId - Audio source ID
 */
export function pauseAudio(sourceId) {
  const source = getAudioSource(sourceId);
  if (source) {
    if (source.isPlaying) {
      source.pause();
      console.log(`[Tiramisu] Paused audio: ${sourceId}`);
    }
  } else {
    console.warn(`[Tiramisu] Audio source not found: ${sourceId}`);
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
      console.log(`[Tiramisu] Stopped audio: ${sourceId}`);
    }
    // Unregister after stopping
    unregisterAudioSource(sourceId);
  }
  // Don't warn if source not found - many nodes don't have audio
}

/**
 * Set volume of an audio source
 * @param {string} sourceId - Audio source ID
 * @param {number} volume - Volume (0.0 to 1.0)
 */
export function setAudioVolume(sourceId, volume) {
  const source = getAudioSource(sourceId);
  if (source) {
    source.setVolume(volume);
  } else {
    console.warn(`[Tiramisu] Audio source not found: ${sourceId}`);
  }
}

/**
 * Check if an audio source is playing
 * @param {string} sourceId - Audio source ID
 * @returns {boolean}
 */
export function isAudioPlaying(sourceId) {
  const source = getAudioSource(sourceId);
  if (source) {
    return source.isPlaying;
  }
  return false;
}

/**
 * Get current playback time
 * @param {string} sourceId - Audio source ID
 * @returns {number} Current time in seconds
 */
export function getAudioTime(sourceId) {
  const source = getAudioSource(sourceId);
  if (source && source.source) {
    return source.source.context.currentTime - source.source.startTime;
  }
  return 0.0;
}

/**
 * Set current playback time
 * @param {string} sourceId - Audio source ID
 * @param {number} time - Time in seconds
 */
export function setAudioTime(sourceId, time) {
  const source = getAudioSource(sourceId);
  if (source) {
    const wasPlaying = source.isPlaying;
    if (wasPlaying) {
      source.stop();
    }
    source.offset = time;
    if (wasPlaying) {
      source.play();
    }
  } else {
    console.warn(`[Tiramisu] Audio source not found: ${sourceId}`);
  }
}

/**
 * Get all registered audio source IDs
 * @returns {string[]}
 */
export function getAllAudioSourceIds() {
  return Array.from(audioSources.keys());
}

/**
 * Update audio configuration for an existing audio source
 * @param {string} id - Audio source ID
 * @param {AudioConfig} config - New audio configuration
 */
export function updateAudioConfig(id, config) {
  const source = getAudioSource(id);
  if (source) {
    const wasPlaying = source.isPlaying;

    // Update configuration
    source.setVolume(config.volume);
    source.setLoop(config.loop);
    source.setPlaybackRate(config.playback_rate);

    // Handle autoplay changes
    if (config.autoplay && !wasPlaying) {
      source.play();
      console.log(`[Tiramisu] Started audio due to autoplay: ${id}`);
    } else if (!config.autoplay && wasPlaying) {
      source.stop();
      console.log(`[Tiramisu] Stopped audio due to autoplay off: ${id}`);
    }

    console.log(`[Tiramisu] Updated audio config: ${id}`);
  } else {
    console.warn(`[Tiramisu] Audio source not found for update: ${id}`);
  }
}
