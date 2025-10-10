import * as THREE from 'three';
import * as DEBUG_GLEAM from '../debug.mjs';

// --- Debug Rendering Helpers ---

/// Create a wireframe box between min and max points
export function createDebugBox(min, max, color) {
  const geometry = new THREE.BoxGeometry(
    max.x - min.x,
    max.y - min.y,
    max.z - min.z
  );
  const edges = new THREE.EdgesGeometry(geometry);
  const material = new THREE.LineBasicMaterial({ color: color });
  const wireframe = new THREE.LineSegments(edges, material);

  // Position at center of box
  wireframe.position.set(
    (min.x + max.x) / 2,
    (min.y + max.y) / 2,
    (min.z + max.z) / 2
  );

  geometry.dispose(); // Clean up the geometry since we only need edges
  return wireframe;
}

/// Create a wireframe sphere
export function createDebugSphere(center, radius, color) {
  const geometry = new THREE.SphereGeometry(radius, 16, 12);
  const edges = new THREE.EdgesGeometry(geometry);
  const material = new THREE.LineBasicMaterial({ color: color });
  const wireframe = new THREE.LineSegments(edges, material);

  wireframe.position.set(center.x, center.y, center.z);

  geometry.dispose();
  return wireframe;
}

/// Create a line between two points
export function createDebugLine(from, to, color) {
  const points = [
    new THREE.Vector3(from.x, from.y, from.z),
    new THREE.Vector3(to.x, to.y, to.z)
  ];
  const geometry = new THREE.BufferGeometry().setFromPoints(points);
  const material = new THREE.LineBasicMaterial({ color: color });
  return new THREE.Line(geometry, material);
}

/// Create coordinate axes (X=red, Y=green, Z=blue)
export function createDebugAxes(origin, size) {
  const axes = new THREE.Group();

  // X axis (red)
  const xPoints = [
    new THREE.Vector3(origin.x, origin.y, origin.z),
    new THREE.Vector3(origin.x + size, origin.y, origin.z)
  ];
  const xGeometry = new THREE.BufferGeometry().setFromPoints(xPoints);
  const xMaterial = new THREE.LineBasicMaterial({ color: 0xff0000 });
  axes.add(new THREE.Line(xGeometry, xMaterial));

  // Y axis (green)
  const yPoints = [
    new THREE.Vector3(origin.x, origin.y, origin.z),
    new THREE.Vector3(origin.x, origin.y + size, origin.z)
  ];
  const yGeometry = new THREE.BufferGeometry().setFromPoints(yPoints);
  const yMaterial = new THREE.LineBasicMaterial({ color: 0x00ff00 });
  axes.add(new THREE.Line(yGeometry, yMaterial));

  // Z axis (blue)
  const zPoints = [
    new THREE.Vector3(origin.x, origin.y, origin.z),
    new THREE.Vector3(origin.x, origin.y, origin.z + size)
  ];
  const zGeometry = new THREE.BufferGeometry().setFromPoints(zPoints);
  const zMaterial = new THREE.LineBasicMaterial({ color: 0x0000ff });
  axes.add(new THREE.Line(zGeometry, zMaterial));

  return axes;
}

/// Create a grid on the XZ plane
export function createDebugGrid(size, divisions, color) {
  return new THREE.GridHelper(size, divisions, color, color);
}

/// Create a point marker (small sphere)
export function createDebugPoint(position, size, color) {
  const geometry = new THREE.SphereGeometry(size, 8, 6);
  const material = new THREE.MeshBasicMaterial({ color: color });
  const sphere = new THREE.Mesh(geometry, material);

  sphere.position.set(position.x, position.y, position.z);

  return sphere;
}

// --- Performance Monitoring ---

let performanceStats = {
  fps: 0,
  frameTime: 0,
  drawCalls: 0,
  triangles: 0,
  memoryMB: 0,
};

let frameCount = 0;
let lastFpsUpdate = performance.now();
let frameTimes = [];

export function startPerformanceMonitoring() {
  frameCount = 0;
  lastFpsUpdate = performance.now();
  frameTimes = [];
}

export function updatePerformanceStats(deltaTime) {
  frameCount++;
  frameTimes.push(deltaTime * 1000); // Convert to ms

  // Keep only last 60 frames
  if (frameTimes.length > 60) {
    frameTimes.shift();
  }

  // Update FPS every second
  const now = performance.now();
  if (now - lastFpsUpdate >= 1000) {
    performanceStats.fps = frameCount;
    frameCount = 0;
    lastFpsUpdate = now;
  }

  // Average frame time
  const avgFrameTime = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  performanceStats.frameTime = avgFrameTime;

  // Memory (if available)
  if (performance.memory) {
    performanceStats.memoryMB = performance.memory.usedJSHeapSize / (1024 * 1024);
  }
}

export function getPerformanceStats() {
  return new DEBUG_GLEAM.PerformanceStats(
    performanceStats.fps,
    performanceStats.frameTime,
    performanceStats.drawCalls,
    performanceStats.triangles,
    performanceStats.memoryMB
  );
}

export function setRenderStats(info) {
  if (info) {
    performanceStats.drawCalls = info.render?.calls || 0;
    performanceStats.triangles = info.render?.triangles || 0;
  }
}

// --- Physics Debugger ---

import { getPhysicsWorld, getBodyMap } from './physics.mjs';

let debugColliderMeshes = new Map(); // Map of body ID to debug mesh
let debugScene = null; // Will be set by setDebugScene
let collidersEnabled = false; // Track if visualization is enabled

/**
 * Set the Three.js scene for debug rendering
 * @param {THREE.Scene} scene
 */
export function setDebugScene(scene) {
  debugScene = scene;
}

/**
 * Enable/disable collision shape visualization for a specific physics world
 * @param {PhysicsWorld} physicsWorld - The Gleam PhysicsWorld (passed for explicitness)
 * @param {boolean} enabled
 *
 * Note: The physics world parameter makes the API declarative and explicit about
 * which world is being debugged. Internally, we access the global Rapier world
 * since Tiramisu currently supports one physics world per game.
 */
export function showColliders(enabled) {
  collidersEnabled = enabled;

  if (enabled) {
    console.log('[Tiramisu] Collision shape visualization enabled for physics world');
    updateColliderVisualization();
  } else {
    console.log('[Tiramisu] Collision shape visualization disabled');
    clearColliderVisualization();
  }
}

/**
 * Update collider visualization (call each frame when enabled)
 */
export function updateColliderVisualization() {
  if (!collidersEnabled || !debugScene) return;

  // Access the global Rapier world and body map
  const physicsWorld = getPhysicsWorld();
  const bodyMap = getBodyMap();

  if (!physicsWorld || !bodyMap) return;

  // Get current body IDs
  const currentBodyIds = new Set(bodyMap.keys());

  // Remove debug meshes for bodies that no longer exist
  for (const [bodyId, debugMesh] of debugColliderMeshes) {
    if (!currentBodyIds.has(bodyId)) {
      debugScene.remove(debugMesh);
      debugMesh.geometry.dispose();
      debugMesh.material.dispose();
      debugColliderMeshes.delete(bodyId);
    }
  }

  // Create or update debug meshes for each body
  for (const [bodyId, rigidBody] of bodyMap) {
    const numColliders = rigidBody.numColliders();

    for (let i = 0; i < numColliders; i++) {
      const collider = rigidBody.collider(i);
      const meshKey = `${bodyId}_${i}`;

      // Get collider shape and create/update debug mesh
      let debugMesh = debugColliderMeshes.get(meshKey);

      if (!debugMesh) {
        debugMesh = createColliderDebugMesh(collider, physicsWorld);
        if (debugMesh) {
          debugScene.add(debugMesh);
          debugColliderMeshes.set(meshKey, debugMesh);
        }
      }

      if (debugMesh) {
        // Update position and rotation from rigid body
        const translation = rigidBody.translation();
        const rotation = rigidBody.rotation();

        debugMesh.position.set(translation.x, translation.y, translation.z);
        debugMesh.quaternion.set(rotation.x, rotation.y, rotation.z, rotation.w);
      }
    }
  }
}

/**
 * Create a debug mesh for a Rapier collider
 * @param {RAPIER.Collider} collider
 * @param {RAPIER.World} world - Physics world needed to compute AABB
 * @returns {THREE.Object3D|null}
 */
function createColliderDebugMesh(collider, world) {
  const shape = collider.shape;
  const shapeType = shape.type;

  let geometry = null;

  try {
    // Based on console output:
    // Type 0 = Ball (has radius property)
    // Type 1 = Cuboid (has halfExtents property with x, y, z)

    switch (shapeType) {
      case 0: // Ball (Sphere)
        if (shape.radius !== undefined) {
          geometry = new THREE.SphereGeometry(shape.radius, 16, 12);
        } else {
          console.error('[Tiramisu Debug] Ball shape missing radius');
          return null;
        }
        break;

      case 1: // Cuboid (Box)
        const he = shape.halfExtents;
        if (he && he.x !== undefined) {
          geometry = new THREE.BoxGeometry(he.x * 2, he.y * 2, he.z * 2);
        } else {
          console.error('[Tiramisu Debug] Cuboid shape missing halfExtents');
          return null;
        }
        break;

      case 2: // Capsule
        if (shape.radius !== undefined && shape.halfHeight !== undefined) {
          geometry = new THREE.CapsuleGeometry(shape.radius, shape.halfHeight * 2, 8, 16);
        } else {
          console.error('[Tiramisu Debug] Capsule shape missing properties');
          return null;
        }
        break;

      case 3: // Cylinder
        if (shape.radius !== undefined && shape.halfHeight !== undefined) {
          geometry = new THREE.CylinderGeometry(shape.radius, shape.radius, shape.halfHeight * 2, 16);
        } else {
          console.error('[Tiramisu Debug] Cylinder shape missing properties');
          return null;
        }
        break;

      default:
        console.warn(`[Tiramisu Debug] Unsupported collider shape type ${shapeType}`);
        return null;
    }
  } catch (error) {
    console.error('[Tiramisu Debug] Error creating collider debug mesh:', error);
    return null;
  }

  if (!geometry) return null;

  // Create wireframe
  const edges = new THREE.EdgesGeometry(geometry);
  const material = new THREE.LineBasicMaterial({
    color: 0x00ff00,
    linewidth: 2,
    transparent: true,
    opacity: 0.8
  });
  const wireframe = new THREE.LineSegments(edges, material);

  geometry.dispose();
  return wireframe;
}

/**
 * Clear all collider debug visualization
 */
function clearColliderVisualization() {
  if (!debugScene) return;

  for (const [_, debugMesh] of debugColliderMeshes) {
    debugScene.remove(debugMesh);
    debugMesh.geometry.dispose();
    debugMesh.material.dispose();
  }

  debugColliderMeshes.clear();
}
