// Runtime registry FFI for Tiramisu web components.
//
// This is a thin registry layer that stores savoiardi Three.js objects
// by string IDs. All actual Three.js operations are done by savoiardi
// in Gleam code - this just provides the ID-based lookup needed for
// cross-component communication via Lustre's context system.

import { Option$Some, Option$None, Option$Some$0, Option$isSome } from "../../../gleam_stdlib/gleam/option.mjs";
import { broadcastTick } from "../tick.ffi.mjs";

// REGISTRIES ------------------------------------------------------------------

const scenes = new Map();
const renderers = new Map();
const objects = new Map();
const cameras = new Map();
const renderLoops = new Map();

let idCounter = 0;

function generateId(prefix) {
  return `${prefix}_${++idCounter}_${Date.now().toString(36)}`;
}

// SCENE REGISTRY --------------------------------------------------------------

export function registerScene(scene, customId) {
  // Use custom ID if provided, otherwise generate one
  const id = customId && customId.length > 0 ? customId : generateId("scene");
  scenes.set(id, scene);
  return id;
}

export function getScene(id) {
  const scene = scenes.get(id);
  return scene ? Option$Some(scene) : Option$None();
}

export function disposeScene(id) {
  const scene = scenes.get(id);
  if (scene) {
    // Dispose all children
    scene.traverse((object) => {
      if (object.geometry) object.geometry.dispose();
      if (object.material) {
        if (Array.isArray(object.material)) {
          object.material.forEach((m) => m.dispose());
        } else {
          object.material.dispose();
        }
      }
    });
    scenes.delete(id);
  }
}

// RENDERER REGISTRY -----------------------------------------------------------

export function registerRenderer(renderer, container, config) {
  const id = generateId("renderer");

  // Use container from config if available (the actual div inside shadow root),
  // otherwise fall back to the passed container
  const actualContainer = config.container || container;

  // Set initial size from config or container
  const width = Option$isSome(config.width)
    ? Option$Some$0(config.width)
    : actualContainer.clientWidth || 800;
  const height = Option$isSome(config.height)
    ? Option$Some$0(config.height)
    : actualContainer.clientHeight || 600;

  renderer.setSize(width, height);
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));

  // Set background color
  if (config.background) {
    renderer.setClearColor(parseInt(config.background.replace("#", ""), 16));
  }

  // Append canvas to container
  if (actualContainer.appendChild) {
    actualContainer.appendChild(renderer.domElement);
  }

  // Style the canvas
  renderer.domElement.style.display = "block";

  renderers.set(id, { renderer, container: actualContainer });
  return id;
}

export function resizeRenderer(id, width, height) {
  const info = renderers.get(id);
  if (info) {
    info.renderer.setSize(width, height);
    // Update all cameras
    cameras.forEach((camData) => {
      const cam = camData.camera;
      if (cam.isPerspectiveCamera) {
        cam.aspect = width / height;
        cam.updateProjectionMatrix();
      }
    });
  }
}

export function disposeRenderer(id) {
  const info = renderers.get(id);
  if (info) {
    stopRenderLoop(id);
    if (info.renderer.domElement.parentElement) {
      info.renderer.domElement.parentElement.removeChild(info.renderer.domElement);
    }
    info.renderer.dispose();
    renderers.delete(id);
  }
}

// RENDER LOOP -----------------------------------------------------------------

export function startRenderLoop(rendererId, sceneId) {
  const renderInfo = renderers.get(rendererId);
  const scene = scenes.get(sceneId);

  if (!renderInfo || !scene) {
    return;
  }

  stopRenderLoop(rendererId);

  let animationId = null;
  let previousTimestamp = null;

  function render(timestamp) {
    animationId = requestAnimationFrame(render);

    // Calculate delta time in milliseconds
    const deltaMs =
      previousTimestamp === null ? 16.67 : timestamp - previousTimestamp;
    previousTimestamp = timestamp;

    // Broadcast tick to subscribers
    broadcastTick(sceneId, Math.round(deltaMs));

    // Find active camera
    let activeCamera = null;
    cameras.forEach((camData) => {
      if (camData.sceneId === sceneId && camData.active) {
        activeCamera = camData.camera;
      }
    });

    // Fall back to first camera in scene
    if (!activeCamera) {
      scene.traverse((obj) => {
        if (obj.isCamera && !activeCamera) {
          activeCamera = obj;
        }
      });
    }

    if (activeCamera) {
      renderInfo.renderer.render(scene, activeCamera);
    }
  }

  // Start the loop via requestAnimationFrame to get a proper timestamp
  animationId = requestAnimationFrame(render);
  renderLoops.set(rendererId, animationId);
}

export function stopRenderLoop(rendererId) {
  const animationId = renderLoops.get(rendererId);
  if (animationId) {
    cancelAnimationFrame(animationId);
    renderLoops.delete(rendererId);
  }
}

// OBJECT REGISTRY -------------------------------------------------------------

function getParentObject(sceneId, parentId) {
  if (parentId === sceneId) {
    return scenes.get(sceneId);
  }
  const objData = objects.get(parentId);
  return objData?.object || scenes.get(sceneId);
}

export function registerAndAddObject(sceneId, parentId, id, object) {
  const parent = getParentObject(sceneId, parentId);
  if (!parent) {
    console.error("Cannot add object: parent not found", parentId);
    return null;
  }

  const objectId = id || generateId("object");
  object.name = objectId;

  parent.add(object);

  objects.set(objectId, {
    object,
    sceneId,
    parentId,
    type: "object",
  });

  return objectId;
}

export function registerCamera(sceneId, parentId, id, camera, active) {
  const parent = getParentObject(sceneId, parentId);
  if (!parent) {
    console.error("Cannot add camera: parent not found");
    return null;
  }

  const cameraId = id || generateId("camera");
  camera.name = cameraId;

  parent.add(camera);

  cameras.set(cameraId, {
    camera,
    sceneId,
    parentId,
    active,
  });

  objects.set(cameraId, {
    object: camera,
    sceneId,
    parentId,
    type: "camera",
  });

  return cameraId;
}

export function setCameraActive(id, active) {
  const camData = cameras.get(id);
  if (camData) {
    camData.active = active;
  }
}

export function registerAndAddLight(sceneId, parentId, id, light) {
  const parent = getParentObject(sceneId, parentId);
  if (!parent) {
    console.error("Cannot add light: parent not found");
    return null;
  }

  const lightId = id || generateId("light");
  light.name = lightId;

  parent.add(light);

  objects.set(lightId, {
    object: light,
    sceneId,
    parentId,
    type: "light",
  });

  return lightId;
}

// TRANSFORM -------------------------------------------------------------------

export function setPosition(id, x, y, z) {
  const data = objects.get(id);
  if (data) {
    data.object.position.set(x, y, z);
  }
}

export function setRotation(id, x, y, z) {
  const data = objects.get(id);
  if (data) {
    data.object.rotation.set(x, y, z);
  }
}

export function setQuaternion(id, x, y, z, w) {
  const data = objects.get(id);
  if (data) {
    data.object.quaternion.set(x, y, z, w);
  }
}

export function setScale(id, x, y, z) {
  const data = objects.get(id);
  if (data) {
    data.object.scale.set(x, y, z);
  }
}

export function lookAt(id, x, y, z) {
  const data = objects.get(id);
  if (data) {
    data.object.lookAt(x, y, z);
  }
}

// OBJECT MANAGEMENT -----------------------------------------------------------

export function removeObject(id) {
  const data = objects.get(id);
  if (!data) return;

  const { object, type } = data;

  if (object.parent) {
    object.parent.remove(object);
  }

  if (object.geometry) {
    object.geometry.dispose();
  }
  if (object.material) {
    if (Array.isArray(object.material)) {
      object.material.forEach((m) => m.dispose());
    } else {
      object.material.dispose();
    }
  }

  objects.delete(id);
  if (type === "camera") {
    cameras.delete(id);
  }
}

export function setVisible(id, visible) {
  const data = objects.get(id);
  if (data) {
    data.object.visible = visible;
  }
}

// UTILITY EXPORTS -------------------------------------------------------------

export function getObject(id) {
  const data = objects.get(id);
  return data ? Option$Some(data.object) : Option$None();
}

export function getLight(id) {
  const data = objects.get(id);
  if (data && data.object.isLight) {
    return Option$Some(data.object);
  }
  return Option$None();
}

export function getMaterial(id) {
  const data = objects.get(id);
  if (data && data.object.material) {
    return Option$Some(data.object.material);
  }
  return Option$None();
}

export function getCamera(id) {
  const camData = cameras.get(id);
  return camData ? Option$Some(camData.camera) : Option$None();
}

// Reparent an object to a new parent
export function reparentObject(id, newParentId, sceneId) {
  const data = objects.get(id);
  if (!data) return;

  const newParent = getParentObject(sceneId, newParentId);
  if (!newParent) return;

  // Three.js add() automatically removes from old parent
  newParent.add(data.object);
  data.parentId = newParentId;
}

// Set renderer background color
export function setRendererBackground(id, color) {
  const info = renderers.get(id);
  if (info) {
    info.renderer.setClearColor(parseInt(color.replace("#", ""), 16));
  }
}

// AUDIO LISTENER REGISTRY -----------------------------------------------------

const audioListeners = new Map();

export function storeAudioListener(sceneId, listener) {
  audioListeners.set(sceneId, listener);
}

export function getStoredAudioListener(sceneId) {
  const listener = audioListeners.get(sceneId);
  return listener ? Option$Some(listener) : Option$None();
}

// Attach an AudioListener to the active camera for a given scene.
// Uses camera.add() on objects created by savoiardi (no direct THREE import).
export function attachListenerToActiveCamera(sceneId, listener) {
  for (const [, camData] of cameras) {
    if (camData.sceneId === sceneId && camData.active) {
      camData.camera.add(listener);
      return;
    }
  }
  // Fallback: attach to scene root so audio still works
  const scene = scenes.get(sceneId);
  if (scene) scene.add(listener);
}

// MODEL LOADING HELPERS -------------------------------------------------------

// Replace an existing object's 3D model with a newly loaded one.
// Preserves position, rotation, scale, and visibility from the old object.
export function replaceObjectModel(id, newObject) {
  const data = objects.get(id);
  if (!data) return;

  const old = data.object;
  newObject.position.copy(old.position);
  newObject.quaternion.copy(old.quaternion);
  newObject.scale.copy(old.scale);
  newObject.visible = old.visible;
  newObject.name = id;

  if (old.parent) {
    old.parent.add(newObject);
    old.parent.remove(old);
  }

  // Dispose old geometry/materials
  old.traverse((obj) => {
    if (obj.geometry) obj.geometry.dispose();
    if (obj.material) {
      if (Array.isArray(obj.material)) obj.material.forEach((m) => m.dispose());
      else obj.material.dispose();
    }
  });

  data.object = newObject;
}

// Dispatch a custom event on a DOM element found by ID.
// Used to notify Lustre apps when models finish loading.
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
