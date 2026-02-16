// Render loop FFI — owns minimal mutable state for 60fps rendering.
//
// The loop runs requestAnimationFrame and manages:
// - Active camera reference (set/cleared by Gleam on camera changes)

import { broadcastTick } from "../tick.ffi.mjs";

export function start(scene, renderer, sceneId) {
  let previousTimestamp = null;
  let animationId = null;
  let activeCamera = null;

  function render(timestamp) {
    animationId = requestAnimationFrame(render);
    const deltaMs =
      previousTimestamp === null ? 16.67 : timestamp - previousTimestamp;
    previousTimestamp = timestamp;

    broadcastTick(sceneId, Math.round(deltaMs));

    if (activeCamera) {
      renderer.render(scene, activeCamera);
    }
  }

  animationId = requestAnimationFrame(render);

  return {
    stop() {
      if (animationId !== null) {
        cancelAnimationFrame(animationId);
        animationId = null;
      }
    },
    setActiveCamera(camera) {
      activeCamera = camera;
    },
    clearActiveCamera() {
      activeCamera = null;
    },
  };
}

export function stop(loop) {
  loop.stop();
}

export function setActiveCamera(loop, camera) {
  loop.setActiveCamera(camera);
}

export function clearActiveCamera(loop) {
  loop.clearActiveCamera();
}
