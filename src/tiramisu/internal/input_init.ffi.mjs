// Minimal FFI for input initialization - extracts data from browser events
//
// All event listener attachment is done via Plinth in Gleam.
// This file only extracts typed data from JavaScript event objects.

/**
 * Extract mouse button from mouse event
 */
export function getMouseButton(event) {
  return event.button || 0;
}

/**
 * Extract mouse position and delta from mouse event
 * Returns [x, y, deltaX, deltaY]
 */
export function getMousePosition(canvas, event) {
  // Check if pointer lock is active
  const isPointerLocked = 
    document.pointerLockElement === canvas ||
    document.webkitPointerLockElement === canvas ||
    document.mozPointerLockElement === canvas;

  if (isPointerLocked) {
    // In pointer lock mode, use movement deltas
    const deltaX = event.movementX || event.webkitMovementX || event.mozMovementX || 0;
    const deltaY = event.movementY || event.webkitMovementY || event.mozMovementY || 0;
    return [0.0, 0.0, deltaX, deltaY]; // Position is meaningless in pointer lock
  } else {
    // Normal mode - calculate position relative to canvas
    const rect = canvas.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    
    // Delta is calculated in Gleam by comparing to last position
    return [x, y, 0.0, 0.0];
  }
}

/**
 * Extract wheel delta from wheel event
 */
export function getWheelDelta(event) {
  return event.deltaY || 0.0;
}

/**
 * Extract changed touches from touch event
 * Returns array of [id, x, y] tuples
 */
export function getChangedTouches(canvas, event) {
  const rect = canvas.getBoundingClientRect();
  const touches = [];
  
  for (let i = 0; i < event.changedTouches.length; i++) {
    const touch = event.changedTouches[i];
    const x = Math.max(0, Math.min(rect.width, touch.clientX - rect.left));
    const y = Math.max(0, Math.min(rect.height, touch.clientY - rect.top));
    touches.push([touch.identifier, x, y]);
  }
  
  return touches;
}

/**
 * Extract changed touch IDs from touch event
 * Returns array of touch identifiers
 */
export function getChangedTouchIds(event) {
  const ids = [];
  
  for (let i = 0; i < event.changedTouches.length; i++) {
    ids.push(event.changedTouches[i].identifier);
  }
  
  return ids;
}
