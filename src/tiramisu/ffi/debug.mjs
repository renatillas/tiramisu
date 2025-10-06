import * as THREE from 'three';
import { PerformanceStats } from '../debug.mjs';

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
  return new PerformanceStats(
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
