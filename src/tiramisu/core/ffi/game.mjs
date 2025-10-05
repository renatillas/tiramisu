import * as keyboard from '../../input/ffi/keyboard.mjs';
import * as mouse from '../../input/ffi/mouse.mjs';
import * as touch from '../../input/ffi/touch.mjs';
import { applyPatches } from '../../scene/ffi/renderer.mjs';
import { diff } from '../../scene/diff.mjs';
import { Empty } from '../../../gleam.mjs';

export function appendToDom(element) {
  document.body.appendChild(element);
}

export function applyInitialScene(scene, nodes) {
  // Diff from empty scene to initial nodes
  const patches = diff(new Empty(), nodes);
  applyPatches(scene, patches);
  scene.children.forEach((child, i) => {
    console.log(`[Game] Child ${i}: type=${child.type}, position=(${child.position.x}, ${child.position.y}, ${child.position.z})`);
  });
}

export function startDeclarativeLoop(
  state,
  prevNodes,
  context,
  scene,
  renderer,
  camera,
  updateFn,
  viewFn
) {
  let lastTime = performance.now();
  let currentState = state;
  let previousNodes = prevNodes;

  function loop(currentTime) {
    const deltaTime = (currentTime - lastTime) / 1000.0;
    lastTime = currentTime;

    // Update context with delta time
    const updatedContext = {
      camera: context.camera,
      delta_time: deltaTime,
    };

    // Update game state
    currentState = updateFn(currentState, updatedContext);

    // Get new scene nodes from view function
    const currentNodes = viewFn(currentState);

    // Diff the scene trees
    const patches = diff(previousNodes, currentNodes);

    // Apply patches to Three.js scene (Gleam lists have countLength method)
    if (patches.countLength && patches.countLength() > 0) {
      applyPatches(scene, patches);
    }

    // Store current nodes for next frame
    previousNodes = currentNodes;

    // Render
    renderer.render(scene, camera);

    // Clear per-frame input state
    keyboard.clearFrameState();
    mouse.clearFrameState();
    touch.clearFrameState();

    requestAnimationFrame(loop);
  }
  requestAnimationFrame(loop);
}
