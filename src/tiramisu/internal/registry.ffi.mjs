// Minimal FFI for the instance-scoped registry.
// Only browser APIs that can't be done in pure Gleam live here.

import { broadcastTick } from "../tick.ffi.mjs";

export function startRenderLoop(registry, onFrame) {
  let previousTimestamp = null;
  let animationId = null;

  function render(timestamp) {
    animationId = requestAnimationFrame(render);
    const deltaMs =
      previousTimestamp === null ? 16.67 : timestamp - previousTimestamp;
    previousTimestamp = timestamp;

    // Broadcast tick to subscribers for this scene
    broadcastTick(registry.scene_id, Math.round(deltaMs));

    // Call the Gleam render callback
    onFrame(registry);
  }

  animationId = requestAnimationFrame(render);
  return animationId;
}

export function stopRenderLoop(animationId) {
  cancelAnimationFrame(animationId);
}

export function dispatchMeshEvent(meshId, eventName) {
  const el = document.getElementById(meshId);
  if (el) {
    el.dispatchEvent(
      new CustomEvent(eventName, {
        bubbles: true,
        composed: true,
        detail: { id: meshId },
      })
    );
  }
}

export function appendCanvasToContainer(container, canvas) {
  container.appendChild(canvas);
  canvas.style.display = "block";
}

export function getDevicePixelRatio() {
  return Math.min(window.devicePixelRatio, 2);
}

// Store a Three.js object reference on its corresponding DOM element.
// This enables external integrations (cacao physics, etc.) to find
// the Three.js object via document.getElementById(meshId)._object3d.
export function storeObjectOnDom(meshId, object) {
  const el = document.getElementById(meshId);
  if (el) el._object3d = object;
}

export function clearObjectFromDom(meshId) {
  const el = document.getElementById(meshId);
  if (el) delete el._object3d;
}

// Replace an existing object's 3D model with a newly loaded one.
// Preserves position, rotation, scale, and visibility from the old object.
// This must stay in FFI because it needs direct Three.js property copying
// and child traversal for disposal.
export function replaceObjectModel(oldObject, newObject, name) {
  newObject.position.copy(oldObject.position);
  newObject.quaternion.copy(oldObject.quaternion);
  newObject.scale.copy(oldObject.scale);
  newObject.visible = oldObject.visible;
  newObject.name = name;

  if (oldObject.parent) {
    oldObject.parent.add(newObject);
    oldObject.parent.remove(oldObject);
  }

  // Dispose old geometry/materials
  oldObject.traverse((obj) => {
    if (obj.geometry) obj.geometry.dispose();
    if (obj.material) {
      if (Array.isArray(obj.material))
        obj.material.forEach((m) => m.dispose());
      else obj.material.dispose();
    }
  });

  return newObject;
}
