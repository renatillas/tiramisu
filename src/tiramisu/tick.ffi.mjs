// Tick subscription FFI for animation frame timing and input capture.
//
// This module manages per-scene tick subscriptions that get called
// from the render loop with frame timing and input information.
//
// Multiple subscribers can register for the same scene ID — each gets
// a unique subscription key (sceneId + subscriberId). broadcastTick
// calls ALL handlers whose key starts with the given sceneId.

import * as $duration from "../../gleam_time/gleam/time/duration.mjs";
import * as $timestamp from "../../gleam_time/gleam/time/timestamp.mjs";
import { TickContext$TickContext } from "./tick.mjs";

let subscriptions = new Map();
let subscriberCounter = 0;

/**
 * Subscribe to tick updates for a scene.
 * Returns a unique subscription key that can be used to unsubscribe.
 * Multiple handlers can subscribe to the same sceneId.
 * @param {string} sceneId - Scene identifier (use "" for global)
 * @param {function} handler - Called with TickContext on each frame
 * @returns {string} Unique subscription key
 */
export function subscribeToTicks(sceneId, handler) {
  const subId = ++subscriberCounter;
  const key = `${sceneId}::${subId}`;
  subscriptions.set(key, { sceneId, handler });
  return key;
}

/**
 * Unsubscribe ALL handlers for a given scene.
 * @param {string} sceneId - The scene ID to unsubscribe from
 */
export function unsubscribeFromTicks(sceneId) {
  for (const [key, sub] of subscriptions) {
    if (sub.sceneId === sceneId) {
      subscriptions.delete(key);
    }
  }
}

/**
 * Broadcast a tick to all subscribers for a given scene.
 * Called by the render loop with the delta time since last frame.
 * @param {string} sceneId - Scene that is ticking
 * @param {number} deltaMs - Milliseconds since last frame
 */
export function broadcastTick(sceneId, deltaMs) {
  // Create typed Duration and Timestamp using gleam_time constructors
  const deltaTime = $duration.milliseconds(deltaMs);
  const timestamp = $timestamp.system_time();

  const ctx = TickContext$TickContext(deltaTime, timestamp);

  // Call ALL handlers subscribed to this scene
  for (const [, sub] of subscriptions) {
    if (sub.sceneId === sceneId) {
      sub.handler(ctx);
    }
  }
}
