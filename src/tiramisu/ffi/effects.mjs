// Effects for game loop integration

// Store reference to the camera (set by renderer.mjs)
let currentCamera = null;

export function setCamera(camera) {
  currentCamera = camera;
}

export function getCamera() {
  return currentCamera;
}

/**
 * Request animation frame effect
 * Schedules a message to be dispatched on the next animation frame
 */
export function requestAnimationFrame(msg) {
  return {
    perform: (dispatch) => {
      window.requestAnimationFrame(() => {
        dispatch(msg);
      });
    }
  };
}

/**
 * Update camera position effect
 * Moves the camera to a new position while maintaining its viewing direction
 */
export function updateCameraPosition(position) {
  return {
    perform: (_dispatch) => {
      if (currentCamera) {
        currentCamera.position.set(position.x, position.y, position.z);
        // No lookAt call - this preserves the camera's orientation/rotation
        // The camera will maintain whatever direction it was facing
      } else {
        console.warn('[Effect] Camera not available for position update');
      }
    }
  };
}
