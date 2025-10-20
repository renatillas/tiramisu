/**
 * ThreeJS FFI - Pure 1:1 bindings to Three.js library
 *
 * This module provides direct, minimal wrappers around Three.js API calls.
 * No game logic should exist here - this is purely a binding layer.
 *
 * All higher-level logic should be in the Gleam declarative layer.
 */

import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import { OBJLoader } from 'three/addons/loaders/OBJLoader.js';
import { STLLoader } from 'three/addons/loaders/STLLoader.js';
import { CSS2DRenderer, CSS2DObject } from 'three/addons/renderers/CSS2DRenderer.js';
import { FontLoader } from 'three/addons/loaders/FontLoader.js';
import { TextGeometry } from 'three/addons/geometries/TextGeometry.js';
import { Quaternion } from './tiramisu/transform.mjs';
import { Vec3 } from '../vec/vec/vec3.mjs';

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Identity function - returns its argument unchanged
 * Useful for type conversions in Gleam FFI
 * @param {any} value
 * @returns {any}
 */
export function identity(value) {
  return value;
}

// ============================================================================
// ID SERIALIZATION - Utility for converting Gleam IDs to stable strings
// ============================================================================

/**
 * Serialize a Gleam ID to a stable string representation
 *
 * This function converts any Gleam custom type instance into a unique string
 * that can be used as a Map/Dict key. It combines the type constructor name
 * with JSON-serialized properties to ensure uniqueness even for types with
 * no fields (e.g., Player vs Enemy).
 *
 * @param {any} id - Any Gleam custom type instance
 * @returns {string} - Stable string representation
 *
 * @example
 * // In Gleam: type GameId { Player(Int) | Enemy(Int) }
 * serializeId(Player(1))  // Returns "Player:{\"0\":1}"
 * serializeId(Enemy(1))   // Returns "Enemy:{\"0\":1}"
 */
export function serializeId(id) {
  // Get constructor name (Gleam custom types compile to JavaScript classes)
  const typeName = id.constructor.name;

  // Serialize the object's properties
  const props = JSON.stringify(id);

  // Combine both for a unique key
  return `${typeName}:${props}`;
}

// ============================================================================
// CORE - Scene, Renderer, Camera
// ============================================================================

/**
 * Create a Three.js Scene
 * @returns {THREE.Scene}
 */
export function createScene() {
  return new THREE.Scene();
}

/**
 * Set scene background color
 * @param {THREE.Scene} scene
 * @param {number} color - Hex color
 */
export function setSceneBackgroundColor(scene, color) {
  scene.background = new THREE.Color(color);
}

/**
 * Set scene background texture
 * @param {THREE.Scene} scene
 * @param {THREE.Texture} texture
 */
export function setSceneBackgroundTexture(scene, texture) {
  scene.background = texture;
}

/**
 * Set scene background cube texture (skybox)
 * @param {THREE.Scene} scene
 * @param {THREE.CubeTexture} cubeTexture
 */
export function setSceneBackgroundCubeTexture(scene, cubeTexture) {
  scene.background = cubeTexture;
}

/**
 * Create a WebGLRenderer
 * @param {Object} options - {antialias: boolean, alpha: boolean, dimensions: Option<{width, height}>}
 * @returns {THREE.WebGLRenderer}
 */
export function createRenderer(options) {
  const renderer = new THREE.WebGLRenderer({
    antialias: options.antialias,
    alpha: options.alpha
  });
  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;

  // Set renderer size based on dimensions or fullscreen
  const dimensions = optionToNull(options.dimensions);
  if (dimensions) {
    // Fixed size
    renderer.setSize(dimensions.width, dimensions.height);
    renderer.setPixelRatio(window.devicePixelRatio || 1);
  } else {
    // Fullscreen mode
    renderer.setSize(window.innerWidth, window.innerHeight);
    renderer.setPixelRatio(window.devicePixelRatio || 1);

    // Add resize listener for fullscreen mode
    window.addEventListener('resize', () => {
      renderer.setSize(window.innerWidth, window.innerHeight);
      renderer.setPixelRatio(window.devicePixelRatio || 1);

      // Update active camera aspect ratio if it exists
      const camera = getActiveCamera();
      if (camera && camera.isPerspectiveCamera) {
        camera.aspect = window.innerWidth / window.innerHeight;
        camera.updateProjectionMatrix();
      }
    });
  }

  return renderer;
}

/**
 * Set renderer size
 * @param {THREE.WebGLRenderer} renderer
 * @param {number} width
 * @param {number} height
 */
export function setRendererSize(renderer, width, height) {
  renderer.setSize(width, height);
}

/**
 * Set renderer pixel ratio
 * @param {THREE.WebGLRenderer} renderer
 * @param {number} ratio
 */
export function setRendererPixelRatio(renderer, ratio) {
  renderer.setPixelRatio(ratio);
}

/**
 * Get renderer DOM element (canvas)
 * @param {THREE.WebGLRenderer} renderer
 * @returns {HTMLCanvasElement}
 */
export function getRendererDomElement(renderer) {
  return renderer.domElement;
}

// Global canvas reference for camera aspect ratio calculations
let globalCanvas = null;

/**
 * Set the global canvas reference
 * @param {HTMLCanvasElement} canvas
 */
export function setCanvas(canvas) {
  globalCanvas = canvas;
}

/**
 * Get the global canvas reference
 * @returns {HTMLCanvasElement|null}
 */
export function getCanvas() {
  return globalCanvas;
}

/**
 * Get canvas client width (CSS width)
 * @param {HTMLCanvasElement} canvas
 * @returns {number}
 */
export function getCanvasClientWidth(canvas) {
  return canvas.clientWidth;
}

/**
 * Get canvas client height (CSS height)
 * @param {HTMLCanvasElement} canvas
 * @returns {number}
 */
export function getCanvasClientHeight(canvas) {
  return canvas.clientHeight;
}

/**
 * Get window inner width
 * @returns {number}
 */
export function getWindowWidth() {
  return window.innerWidth;
}

/**
 * Get window inner height
 * @returns {number}
 */
export function getWindowHeight() {
  return window.innerHeight;
}

// Global active camera reference
let globalCamera = null;

/**
 * Set the global active camera
 * @param {THREE.Camera} camera
 */
export function setActiveCamera(camera) {
  globalCamera = camera;
}

/**
 * Get the global active camera
 * @returns {THREE.Camera|null}
 */
export function getActiveCamera() {
  return globalCamera;
}

// Global game scene reference for effects
let globalGameScene = null;

/**
 * Set the global game scene reference
 * @param {THREE.Scene} scene
 */
export function setGameScene(scene) {
  globalGameScene = scene;
}

/**
 * Get the global game scene reference
 * @returns {THREE.Scene|null}
 */
export function getGameScene() {
  return globalGameScene;
}

/**
 * Render a scene with camera
 * @param {THREE.WebGLRenderer} renderer
 * @param {THREE.Scene} scene
 * @param {THREE.Camera} camera
 */
export function render(renderer, scene, camera) {
  renderer.render(scene, camera);
}

/**
 * Clear the renderer
 * @param {THREE.WebGLRenderer} renderer
 */
export function clearRenderer(renderer) {
  renderer.clear();
}

/**
 * Set viewport
 * @param {THREE.WebGLRenderer} renderer
 * @param {number} x
 * @param {number} y
 * @param {number} width
 * @param {number} height
 */
export function setViewport(renderer, x, y, width, height) {
  renderer.setViewport(x, y, width, height);
}

/**
 * Set scissor
 * @param {THREE.WebGLRenderer} renderer
 * @param {number} x
 * @param {number} y
 * @param {number} width
 * @param {number} height
 */
export function setScissor(renderer, x, y, width, height) {
  renderer.setScissor(x, y, width, height);
}

/**
 * Enable/disable scissor test
 * @param {THREE.WebGLRenderer} renderer
 * @param {boolean} enabled
 */
export function setScissorTest(renderer, enabled) {
  renderer.setScissorTest(enabled);
}

/**
 * Get renderer info for stats
 * @param {THREE.WebGLRenderer} renderer
 * @returns {Object}
 */
export function getRendererInfo(renderer) {
  return renderer.info;
}

/**
 * Check if WebGL context is valid
 * @param {THREE.WebGLRenderer} renderer
 * @returns {boolean}
 */
export function isContextValid(renderer) {
  const gl = renderer.getContext();
  return gl && !gl.isContextLost();
}

// ============================================================================
// CAMERAS
// ============================================================================

/**
 * Create a perspective camera
 * @param {number} fov - Field of view in degrees
 * @param {number} aspect - Aspect ratio
 * @param {number} near - Near clipping plane
 * @param {number} far - Far clipping plane
 * @returns {THREE.PerspectiveCamera}
 */
export function createPerspectiveCamera(fov, aspect, near, far) {
  return new THREE.PerspectiveCamera(fov, aspect, near, far);
}

/**
 * Create an orthographic camera
 * @param {number} left
 * @param {number} right
 * @param {number} top
 * @param {number} bottom
 * @param {number} near
 * @param {number} far
 * @returns {THREE.OrthographicCamera}
 */
export function createOrthographicCamera(left, right, top, bottom, near, far) {
  return new THREE.OrthographicCamera(left, right, top, bottom, near, far);
}

/**
 * Update camera projection matrix
 * @param {THREE.Camera} camera
 */
export function updateProjectionMatrix(camera) {
  camera.updateProjectionMatrix();
}

/**
 * Set camera aspect ratio
 * @param {THREE.PerspectiveCamera} camera
 * @param {number} aspect
 */
export function setCameraAspect(camera, aspect) {
  camera.aspect = aspect;
}

/**
 * Set camera to look at a point
 * @param {THREE.Camera} camera
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function cameraLookAt(camera, x, y, z) {
  camera.updateMatrixWorld(true);
  camera.lookAt(x, y, z);
}

// ============================================================================
// GEOMETRIES
// ============================================================================

/**
 * Create box geometry
 * @param {number} width
 * @param {number} height
 * @param {number} depth
 * @returns {THREE.BoxGeometry}
 */
export function createBoxGeometry(width, height, depth) {
  return new THREE.BoxGeometry(width, height, depth);
}

/**
 * Create sphere geometry
 * @param {number} radius
 * @param {number} widthSegments
 * @param {number} heightSegments
 * @returns {THREE.SphereGeometry}
 */
export function createSphereGeometry(radius, widthSegments, heightSegments) {
  return new THREE.SphereGeometry(radius, widthSegments, heightSegments);
}

/**
 * Create cone geometry
 * @param {number} radius
 * @param {number} height
 * @param {number} segments
 * @returns {THREE.ConeGeometry}
 */
export function createConeGeometry(radius, height, segments) {
  return new THREE.ConeGeometry(radius, height, segments);
}

/**
 * Create plane geometry
 * @param {number} width
 * @param {number} height
 * @returns {THREE.PlaneGeometry}
 */
export function createPlaneGeometry(width, height) {
  return new THREE.PlaneGeometry(width, height);
}

/**
 * Create circle geometry
 * @param {number} radius
 * @param {number} segments
 * @returns {THREE.CircleGeometry}
 */
export function createCircleGeometry(radius, segments) {
  return new THREE.CircleGeometry(radius, segments);
}

/**
 * Create cylinder geometry
 * @param {number} radiusTop
 * @param {number} radiusBottom
 * @param {number} height
 * @param {number} radialSegments
 * @returns {THREE.CylinderGeometry}
 */
export function createCylinderGeometry(radiusTop, radiusBottom, height, radialSegments) {
  return new THREE.CylinderGeometry(radiusTop, radiusBottom, height, radialSegments);
}

/**
 * Create torus geometry
 * @param {number} radius
 * @param {number} tube
 * @param {number} radialSegments
 * @param {number} tubularSegments
 * @returns {THREE.TorusGeometry}
 */
export function createTorusGeometry(radius, tube, radialSegments, tubularSegments) {
  return new THREE.TorusGeometry(radius, tube, radialSegments, tubularSegments);
}

/**
 * Create tetrahedron geometry
 * @param {number} radius
 * @param {number} detail
 * @returns {THREE.TetrahedronGeometry}
 */
export function createTetrahedronGeometry(radius, detail) {
  return new THREE.TetrahedronGeometry(radius, detail);
}

/**
 * Create icosahedron geometry
 * @param {number} radius
 * @param {number} detail
 * @returns {THREE.IcosahedronGeometry}
 */
export function createIcosahedronGeometry(radius, detail) {
  return new THREE.IcosahedronGeometry(radius, detail);
}

/**
 * Dispose geometry
 * @param {THREE.BufferGeometry} geometry
 */
export function disposeGeometry(geometry) {
  geometry.dispose();
}

/**
 * Load font for text geometry
 * @param {string} url - URL to typeface.json font file
 * @returns {Promise<Font>}
 */
export function loadFont(url) {
  const loader = new FontLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      url,
      (font) => resolve(font),
      undefined,
      (error) => reject(error)
    );
  });
}

/**
 * Create text geometry
 * @param {string} text - The text to render
 * @param {Font} font - The loaded font
 * @param {number} size - Text size
 * @param {number} depth - Extrusion depth (3D thickness)
 * @param {number} curveSegments - Number of points on curves
 * @param {boolean} bevelEnabled - Enable beveling
 * @param {number} bevelThickness - Bevel depth
 * @param {number} bevelSize - Bevel extension distance
 * @param {number} bevelOffset - Bevel start offset
 * @param {number} bevelSegments - Number of bevel segments
 * @returns {THREE.BufferGeometry}
 */
export function createTextGeometry(
  text,
  font,
  size,
  depth,
  curveSegments,
  bevelEnabled,
  bevelThickness,
  bevelSize,
  bevelOffset,
  bevelSegments
) {
  return new TextGeometry(text, {
    font: font,
    size: size,
    depth: depth,
    curveSegments: curveSegments,
    bevelEnabled: bevelEnabled,
    bevelThickness: bevelThickness,
    bevelSize: bevelSize,
    bevelOffset: bevelOffset,
    bevelSegments: bevelSegments
  });
}

// ============================================================================
// MATERIALS
// ============================================================================

/**
 * Helper function to convert Gleam option to JavaScript value
 * @param {any} value - Potentially a Gleam option value
 * @returns {any|null} - The unwrapped value or null if it's option.None
 */
function optionToNull(value) {
  // Handle null/undefined
  if (!value) {
    return null;
  }

  // Check if it's a Gleam None by constructor name
  if (value.constructor?.name === "None") {
    return null;
  }

  // Check if it's a Gleam Some and extract the wrapped value
  if (value.constructor?.name === "Some" && Object.prototype.hasOwnProperty.call(value, '0')) {
    return value[0];
  }

  // Otherwise return as is (might be a raw texture object)
  return value;
}

/**
 * Create basic material
 * @param {number} color
 * @param {boolean} transparent
 * @param {number} opacity
 * @param {THREE.Texture|null} map
 * @returns {THREE.MeshBasicMaterial}
 */
export function createBasicMaterial(color, transparent, opacity, map) {
  return new THREE.MeshBasicMaterial({
    color,
    transparent,
    opacity,
    map: optionToNull(map)
  });
}

/**
 * Create standard material
 * @param {number} color
 * @param {number} metalness
 * @param {number} roughness
 * @param {THREE.Texture|null} map
 * @param {THREE.Texture|null} normalMap
 * @param {THREE.Texture|null} aoMap
 * @param {THREE.Texture|null} roughnessMap
 * @param {THREE.Texture|null} metalnessMap
 * @returns {THREE.MeshStandardMaterial}
 */
export function createStandardMaterial(color, metalness, roughness, transparent, opacity, map, normalMap, aoMap, roughnessMap, metalnessMap) {
  const validMap = optionToNull(map);
  const validNormalMap = optionToNull(normalMap);
  const validAoMap = optionToNull(aoMap);
  const validRoughnessMap = optionToNull(roughnessMap);
  const validMetalnessMap = optionToNull(metalnessMap);

  const material = new THREE.MeshStandardMaterial({
    color,
    metalness,
    roughness,
    transparent,
    opacity,
    map: validMap,
    normalMap: validNormalMap,
    aoMap: validAoMap,
    roughnessMap: validRoughnessMap,
    metalnessMap: validMetalnessMap
  });

  if (validNormalMap) {
    material.normalScale.set(1.0, 1.0);
  }

  return material;
}

/**
 * Create phong material
 * @param {number} color
 * @param {number} shininess
 * @param {THREE.Texture|null} map
 * @param {THREE.Texture|null} normalMap
 * @param {THREE.Texture|null} aoMap
 * @returns {THREE.MeshPhongMaterial}
 */
export function createPhongMaterial(color, shininess, map, normalMap, aoMap) {
  const validMap = optionToNull(map);
  const validNormalMap = optionToNull(normalMap);
  const validAoMap = optionToNull(aoMap);

  const material = new THREE.MeshPhongMaterial({
    color,
    shininess,
    map: validMap,
    normalMap: validNormalMap,
    aoMap: validAoMap
  });

  if (validNormalMap) {
    material.normalScale.set(1.0, 1.0);
  }

  return material;
}

/**
 * Create lambert material
 * @param {number} color
 * @param {THREE.Texture|null} map
 * @param {THREE.Texture|null} normalMap
 * @param {THREE.Texture|null} aoMap
 * @returns {THREE.MeshLambertMaterial}
 */
export function createLambertMaterial(color, map, normalMap, aoMap) {
  const validMap = optionToNull(map);
  const validNormalMap = optionToNull(normalMap);
  const validAoMap = optionToNull(aoMap);

  const material = new THREE.MeshLambertMaterial({
    color,
    map: validMap,
    normalMap: validNormalMap,
    aoMap: validAoMap
  });

  if (validNormalMap) {
    material.normalScale.set(1.0, 1.0);
  }

  return material;
}

/**
 * Create toon material
 * @param {number} color
 * @param {THREE.Texture|null} map
 * @param {THREE.Texture|null} normalMap
 * @param {THREE.Texture|null} aoMap
 * @returns {THREE.MeshToonMaterial}
 */
export function createToonMaterial(color, map, normalMap, aoMap) {
  return new THREE.MeshToonMaterial({
    color,
    map: optionToNull(map),
    normalMap: optionToNull(normalMap),
    aoMap: optionToNull(aoMap)
  });
}

/**
 * Create line material
 * @param {number} color
 * @param {number} linewidth
 * @returns {THREE.LineBasicMaterial}
 */
export function createLineMaterial(color, linewidth) {
  return new THREE.LineBasicMaterial({
    color,
    linewidth
  });
}

/**
 * Create sprite material
 * @param {number} color
 * @param {boolean} transparent
 * @param {number} opacity
 * @param {THREE.Texture|null} map
 * @returns {THREE.SpriteMaterial}
 */
export function createSpriteMaterial(color, transparent, opacity, map) {
  return new THREE.SpriteMaterial({
    color,
    transparent,
    opacity,
    map: optionToNull(map)
  });
}

/**
 * Create point material
 * @param {number} size
 * @param {boolean} vertexColors
 * @param {boolean} transparent
 * @param {number} opacity
 * @param {boolean} depthWrite
 * @param {number} blending
 * @param {boolean} sizeAttenuation
 * @returns {THREE.PointsMaterial}
 */
export function createPointsMaterial(size, vertexColors, transparent, opacity, depthWrite, blending, sizeAttenuation) {
  return new THREE.PointsMaterial({
    size,
    vertexColors,
    transparent,
    opacity,
    depthWrite,
    blending,
    sizeAttenuation
  });
}

/**
 * Dispose material
 * @param {THREE.Material} material
 */
export function disposeMaterial(material) {
  material.dispose();
}

// ============================================================================
// LIGHTS
// ============================================================================

/**
 * Create ambient light
 * @param {number} color
 * @param {number} intensity
 * @returns {THREE.AmbientLight}
 */
export function createAmbientLight(color, intensity) {
  return new THREE.AmbientLight(color, intensity);
}

/**
 * Create directional light
 * @param {number} color
 * @param {number} intensity
 * @param {boolean} castShadow
 * @param {number} shadowResolution
 * @param {number} shadowBias
 * @returns {THREE.DirectionalLight}
 */
export function createDirectionalLight(color, intensity, castShadow, shadowResolution, shadowBias) {
  const light = new THREE.DirectionalLight(color, intensity);
  light.castShadow = castShadow;

  if (castShadow) {
    light.shadow.mapSize.width = shadowResolution;
    light.shadow.mapSize.height = shadowResolution;
    light.shadow.bias = shadowBias;
    light.shadow.camera.updateProjectionMatrix();
  }

  return light;
}

/**
 * Create point light
 * @param {number} color
 * @param {number} intensity
 * @param {number} distance
 * @param {boolean} castShadow
 * @param {number} shadowResolution
 * @param {number} shadowBias
 * @returns {THREE.PointLight}
 */
export function createPointLight(color, intensity, distance, castShadow, shadowResolution, shadowBias) {
  const light = new THREE.PointLight(color, intensity, distance);
  light.castShadow = castShadow;

  if (castShadow) {
    light.shadow.mapSize.width = shadowResolution;
    light.shadow.mapSize.height = shadowResolution;
    light.shadow.bias = shadowBias;
    light.shadow.camera.updateProjectionMatrix();
  }

  return light;
}

/**
 * Create spot light
 * @param {number} color
 * @param {number} intensity
 * @param {number} distance
 * @param {number} angle
 * @param {number} penumbra
 * @param {boolean} castShadow
 * @param {number} shadowResolution
 * @param {number} shadowBias
 * @returns {THREE.SpotLight}
 */
export function createSpotLight(color, intensity, distance, angle, penumbra, castShadow, shadowResolution, shadowBias) {
  const light = new THREE.SpotLight(color, intensity, distance, angle, penumbra);
  light.castShadow = castShadow;

  if (castShadow) {
    light.shadow.mapSize.width = shadowResolution;
    light.shadow.mapSize.height = shadowResolution;
    light.shadow.bias = shadowBias;
    light.shadow.camera.updateProjectionMatrix();
  }

  return light;
}

/**
 * Create hemisphere light
 * @param {number} skyColor
 * @param {number} groundColor
 * @param {number} intensity
 * @returns {THREE.HemisphereLight}
 */
export function createHemisphereLight(skyColor, groundColor, intensity) {
  return new THREE.HemisphereLight(skyColor, groundColor, intensity);
}

// ============================================================================
// OBJECTS
// ============================================================================

/**
 * Create a mesh
 * @param {THREE.BufferGeometry} geometry
 * @param {THREE.Material} material
 * @returns {THREE.Mesh}
 */
export function createMesh(geometry, material) {
  const mesh = new THREE.Mesh(geometry, material);
  mesh.castShadow = true;
  mesh.receiveShadow = true;
  return mesh;
}

/**
 * Create an instanced mesh
 * @param {THREE.BufferGeometry} geometry
 * @param {THREE.Material} material
 * @param {number} count
 * @returns {THREE.InstancedMesh}
 */
export function createInstancedMesh(geometry, material, count) {
  return new THREE.InstancedMesh(geometry, material, count);
}

/**
 * Set instance matrix
 * @param {THREE.InstancedMesh} mesh
 * @param {number} index
 * @param {THREE.Matrix4} matrix
 */
export function setInstanceMatrix(mesh, index, matrix) {
  mesh.setMatrixAt(index, matrix);
}

/**
 * Mark instance matrix as needing update
 * @param {THREE.InstancedMesh} mesh
 */
export function updateInstanceMatrix(mesh) {
  mesh.instanceMatrix.needsUpdate = true;
}

/**
 * Create a group
 * @returns {THREE.Group}
 */
export function createGroup() {
  return new THREE.Group();
}

/**
 * Create an LOD object
 * @returns {THREE.LOD}
 */
export function createLOD() {
  return new THREE.LOD();
}

/**
 * Add LOD level
 * @param {THREE.LOD} lod
 * @param {THREE.Object3D} object
 * @param {number} distance
 */
export function addLODLevel(lod, object, distance) {
  lod.addLevel(object, distance);
}

/**
 * Remove LOD level
 * @param {THREE.LOD} lod
 * @param {number} distance
 */
export function removeLODLevel(lod, distance) {
  lod.removeLevel(distance);
}

/**
 * Create a sprite
 * @param {THREE.SpriteMaterial} material
 * @returns {THREE.Sprite}
 */
export function createSprite(material) {
  return new THREE.Sprite(material);
}

/**
 * Create points
 * @param {THREE.BufferGeometry} geometry
 * @param {THREE.PointsMaterial} material
 * @returns {THREE.Points}
 */
export function createPoints(geometry, material) {
  return new THREE.Points(geometry, material);
}

/**
 * Create line segments
 * @param {THREE.BufferGeometry} geometry
 * @param {THREE.LineBasicMaterial} material
 * @returns {THREE.LineSegments}
 */
export function createLineSegments(geometry, material) {
  return new THREE.LineSegments(geometry, material);
}

/**
 * Clone an object
 * @param {THREE.Object3D} object
 * @returns {THREE.Object3D}
 */
export function cloneObject(object) {
  return object.clone();
}

// ============================================================================
// SCENE GRAPH MANIPULATION
// ============================================================================

/**
 * Add child to parent
 * @param {THREE.Object3D} parent
 * @param {THREE.Object3D} child
 */
export function addChild(parent, child) {
  parent.add(child);
}

/**
 * Remove child from parent
 * @param {THREE.Object3D} parent
 * @param {THREE.Object3D} child
 */
export function removeChild(parent, child) {
  parent.remove(child);
}

/**
 * Set position
 * @param {THREE.Object3D} object
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setPosition(object, x, y, z) {
  object.position.set(x, y, z);
}

/**
 * Set rotation
 * @param {THREE.Object3D} object
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setRotation(object, x, y, z) {
  object.rotation.set(x, y, z);
}

/**
 * Set scale
 * @param {THREE.Object3D} object
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setScale(object, x, y, z) {
  object.scale.set(x, y, z);
}

/**
 * Update world matrix
 * @param {THREE.Object3D} object
 * @param {boolean} updateParents
 * @param {boolean} updateChildren
 */
export function updateMatrixWorld(object, updateParents, updateChildren) {
  object.updateMatrixWorld(updateParents, updateChildren);
}

/**
 * Copy position from another object
 * @param {THREE.Object3D} object
 * @param {THREE.Object3D} source
 */
export function copyPosition(object, source) {
  object.position.copy(source.position);
}

/**
 * Copy rotation from another object
 * @param {THREE.Object3D} object
 * @param {THREE.Object3D} source
 */
export function copyRotation(object, source) {
  object.rotation.copy(source.rotation);
}

/**
 * Copy scale from another object
 * @param {THREE.Object3D} object
 * @param {THREE.Object3D} source
 */
export function copyScale(object, source) {
  object.scale.copy(source.scale);
}

/**
 * Set object position from a dynamic position object
 * @param {THREE.Object3D} object
 * @param {Object} position - {x, y, z}
 */
export function setObjectPosition(object, position) {
  object.position.set(position.x, position.y, position.z);
}

/**
 * Set object rotation from a dynamic rotation object
 * @param {THREE.Object3D} object
 * @param {Object} rotation - {x, y, z}
 */
export function setObjectRotation(object, rotation) {
  object.rotation.set(rotation.x, rotation.y, rotation.z);
}

/**
 * Set object scale from a dynamic scale object
 * @param {THREE.Object3D} object
 * @param {Object} scale - {x, y, z}
 */
export function setObjectScale(object, scale) {
  object.scale.set(scale.x, scale.y, scale.z);
}

// ============================================================================
// ANIMATION
// ============================================================================

/**
 * Create animation mixer
 * @param {THREE.Object3D} root
 * @returns {THREE.AnimationMixer}
 */
export function createAnimationMixer(root) {
  return new THREE.AnimationMixer(root);
}

/**
 * Get clip action from mixer
 * @param {THREE.AnimationMixer} mixer
 * @param {THREE.AnimationClip} clip
 * @returns {THREE.AnimationAction}
 */
export function clipAction(mixer, clip) {
  return mixer.clipAction(clip);
}

/**
 * Update animation mixer
 * @param {THREE.AnimationMixer} mixer
 * @param {number} deltaTime
 */
export function updateMixer(mixer, deltaTime) {
  mixer.update(deltaTime);
}

/**
 * Play animation action
 * @param {THREE.AnimationAction} action
 */
export function playAction(action) {
  action.play();
}

/**
 * Stop animation action
 * @param {THREE.AnimationAction} action
 */
export function stopAction(action) {
  action.stop();
}

/**
 * Set action loop mode
 * @param {THREE.AnimationAction} action
 * @param {number} loopMode - THREE.LoopOnce, THREE.LoopRepeat, etc.
 */
export function setActionLoop(action, loopMode) {
  action.setLoop(loopMode);
}

/**
 * Set action time scale
 * @param {THREE.AnimationAction} action
 * @param {number} timeScale
 */
export function setActionTimeScale(action, timeScale) {
  action.timeScale = timeScale;
}

/**
 * Set action weight
 * @param {THREE.AnimationAction} action
 * @param {number} weight
 */
export function setActionWeight(action, weight) {
  action.weight = weight;
}

/**
 * Get animation clip name
 * @param {THREE.AnimationClip} clip
 * @returns {string}
 */
export function getClipName(clip) {
  return clip.name || 'unnamed';
}

/**
 * Get animation clip duration
 * @param {THREE.AnimationClip} clip
 * @returns {number}
 */
export function getClipDuration(clip) {
  return clip.duration;
}

// ============================================================================
// TEXTURES
// ============================================================================

/**
 * Load texture
 * @param {string} url
 * @returns {Promise<THREE.Texture>}
 */
export function loadTexture(url) {
  const loader = new THREE.TextureLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      url,
      (texture) => resolve(texture),
      undefined,
      (error) => reject(error)
    );
  });
}

/**
 * Load equirectangular texture (360° spherical texture)
 * @param {string} url
 * @returns {Promise<THREE.Texture>}
 */
export function loadEquirectangularTexture(url) {
  const loader = new THREE.TextureLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      url,
      (texture) => {
        texture.mapping = THREE.EquirectangularReflectionMapping;
        texture.colorSpace = THREE.SRGBColorSpace;
        resolve(texture);
      },
      undefined,
      (error) => reject(error)
    );
  });
}

/**
 * Load cube texture (skybox)
 * @param {string[]} urls - Array of 6 URLs [px, nx, py, ny, pz, nz]
 * @returns {Promise<THREE.CubeTexture>}
 */
export function loadCubeTexture(urls) {
  const loader = new THREE.CubeTextureLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      urls,
      (cubeTexture) => resolve(cubeTexture),
      undefined,
      (error) => reject(error)
    );
  });
}

/**
 * Dispose texture
 * @param {THREE.Texture} texture
 */
export function disposeTexture(texture) {
  texture.dispose();
}

// ============================================================================
// MODEL LOADERS
// ============================================================================

/**
 * Load GLTF model
 * @param {string} url
 * @returns {Promise<Object>} - Returns GLTF object with scene, animations, etc.
 */
export function loadGLTF(url) {
  const loader = new GLTFLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      url,
      (gltf) => resolve(gltf),
      undefined,
      (error) => reject(error)
    );
  });
}

/**
 * Load OBJ model
 * @param {string} url
 * @returns {Promise<THREE.Group>}
 */
export function loadOBJ(url) {
  const loader = new OBJLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      url,
      (obj) => resolve(obj),
      undefined,
      (error) => reject(error)
    );
  });
}

/**
 * Load STL model
 * @param {string} url
 * @returns {Promise<THREE.BufferGeometry>}
 */
export function loadSTL(url) {
  const loader = new STLLoader();
  return new Promise((resolve, reject) => {
    loader.load(
      url,
      (geometry) => resolve(geometry),
      undefined,
      (error) => reject(error)
    );
  });
}

// ============================================================================
// AUDIO
// ============================================================================

/**
 * Create audio listener
 * @returns {THREE.AudioListener}
 */
export function createAudioListener() {
  return new THREE.AudioListener();
}

/**
 * Create global audio (non-positional)
 * @param {THREE.AudioListener} listener
 * @returns {THREE.Audio}
 */
export function createAudio(listener) {
  return new THREE.Audio(listener);
}

/**
 * Create positional audio (3D audio)
 * @param {THREE.AudioListener} listener
 * @returns {THREE.PositionalAudio}
 */
export function createPositionalAudio(listener) {
  return new THREE.PositionalAudio(listener);
}

/**
 * Set audio buffer
 * @param {THREE.Audio} audio
 * @param {AudioBuffer} buffer
 */
export function setAudioBuffer(audio, buffer) {
  audio.setBuffer(buffer);
}

/**
 * Play audio
 * @param {THREE.Audio} audio
 */
export function playAudio(audio) {
  audio.play();
}

/**
 * Pause audio
 * @param {THREE.Audio} audio
 */
export function pauseAudio(audio) {
  audio.pause();
}

/**
 * Stop audio
 * @param {THREE.Audio} audio
 */
export function stopAudio(audio) {
  audio.stop();
}

/**
 * Set audio volume
 * @param {THREE.Audio} audio
 * @param {number} volume
 */
export function setAudioVolume(audio, volume) {
  audio.setVolume(volume);
}

/**
 * Set audio loop
 * @param {THREE.Audio} audio
 * @param {boolean} loop
 */
export function setAudioLoop(audio, loop) {
  audio.setLoop(loop);
}

/**
 * Set audio playback rate
 * @param {THREE.Audio} audio
 * @param {number} rate
 */
export function setAudioPlaybackRate(audio, rate) {
  audio.setPlaybackRate(rate);
}

/**
 * Check if audio is playing
 * @param {THREE.Audio} audio
 * @returns {boolean}
 */
export function isAudioPlaying(audio) {
  return audio.isPlaying;
}

/**
 * Set positional audio reference distance
 * @param {THREE.PositionalAudio} audio
 * @param {number} distance
 */
export function setRefDistance(audio, distance) {
  audio.setRefDistance(distance);
}

/**
 * Set positional audio rolloff factor
 * @param {THREE.PositionalAudio} audio
 * @param {number} factor
 */
export function setRolloffFactor(audio, factor) {
  audio.setRolloffFactor(factor);
}

/**
 * Set positional audio max distance
 * @param {THREE.PositionalAudio} audio
 * @param {number} distance
 */
export function setMaxDistance(audio, distance) {
  audio.setMaxDistance(distance);
}

/**
 * Check if audio has a buffer
 * @param {THREE.Audio} audio
 * @returns {boolean}
 */
export function hasAudioBuffer(audio) {
  return audio.buffer !== null && audio.buffer !== undefined;
}

/**
 * Get audio loop state
 * @param {THREE.Audio} audio
 * @returns {boolean}
 */
export function getAudioLoop(audio) {
  return audio.loop || false;
}

/**
 * Get AudioContext state
 * @returns {string} - 'suspended', 'running', or 'closed'
 */
export function getAudioContextState() {
  // Get audio listener from asset module
  const listener = new THREE.AudioListener();
  return listener.context.state;
}

/**
 * Resume AudioContext
 */
export function resumeAudioContext() {
  const listener = new THREE.AudioListener();
  if (listener.context.state === 'suspended') {
    listener.context.resume();
  }
}

/**
 * Play audio with fade in effect
 * @param {THREE.Audio} audio
 * @param {number} fadeDurationMs - Fade duration in milliseconds
 * @param {number} targetVolume - Target volume (0.0 to 1.0)
 */
export function playAudioWithFadeIn(audio, fadeDurationMs, targetVolume) {
  if (!audio.buffer) {
    console.warn('[Tiramisu] Cannot play audio without buffer');
    return;
  }

  audio.setVolume(0.0);
  if (!audio.isPlaying) {
    try {
      audio.play();
    } catch (error) {
      console.error('[Tiramisu] Failed to play audio:', error);
      return;
    }
  }

  const startTime = Date.now();
  const fadeInterval = setInterval(() => {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / fadeDurationMs, 1.0);
    const currentVolume = progress * targetVolume;
    audio.setVolume(currentVolume);

    if (progress >= 1.0) {
      clearInterval(fadeInterval);
    }
  }, 16); // ~60fps
}

/**
 * Stop audio with fade out effect
 * @param {THREE.Audio} audio
 * @param {number} fadeDurationMs - Fade duration in milliseconds
 * @param {boolean} pauseInsteadOfStop - If true, pause instead of stop
 */
export function stopAudioWithFadeOut(audio, fadeDurationMs, pauseInsteadOfStop) {
  if (!audio.isPlaying) return;

  const startVolume = audio.getVolume();
  const startTime = Date.now();

  const fadeInterval = setInterval(() => {
    const elapsed = Date.now() - startTime;
    const progress = Math.min(elapsed / fadeDurationMs, 1.0);
    const currentVolume = startVolume * (1.0 - progress);
    audio.setVolume(currentVolume);

    if (progress >= 1.0) {
      clearInterval(fadeInterval);
      if (pauseInsteadOfStop) {
        audio.pause();
      } else {
        audio.stop();
      }
      audio.setVolume(startVolume);
    }
  }, 16); // ~60fps
}

// ============================================================================
// DEBUG HELPERS
// ============================================================================

/**
 * Create axes helper
 * @param {number} size
 * @returns {THREE.AxesHelper}
 */
export function createAxesHelper(size) {
  return new THREE.AxesHelper(size);
}

/**
 * Create grid helper
 * @param {number} size
 * @param {number} divisions
 * @param {number} color
 * @returns {THREE.GridHelper}
 */
export function createGridHelper(size, divisions, color) {
  return new THREE.GridHelper(size, divisions, color, color);
}

/**
 * Create box helper
 * @param {THREE.Object3D} object
 * @param {number} color
 * @returns {THREE.BoxHelper}
 */
export function createBoxHelper(object, color) {
  return new THREE.BoxHelper(object, color);
}

// ============================================================================
// BUFFER GEOMETRY MANIPULATION
// ============================================================================

/**
 * Create buffer geometry
 * @returns {THREE.BufferGeometry}
 */
export function createBufferGeometry() {
  return new THREE.BufferGeometry();
}

/**
 * Set geometry attribute
 * @param {THREE.BufferGeometry} geometry
 * @param {string} name
 * @param {THREE.BufferAttribute} attribute
 */
export function setGeometryAttribute(geometry, name, attribute) {
  geometry.setAttribute(name, attribute);
}

/**
 * Create buffer attribute
 * @param {Float32Array|Uint16Array} array
 * @param {number} itemSize
 * @returns {THREE.BufferAttribute}
 */
export function createBufferAttribute(array, itemSize) {
  return new THREE.BufferAttribute(array, itemSize);
}

/**
 * Mark buffer attribute as needing update
 * @param {THREE.BufferAttribute} attribute
 */
export function markAttributeNeedsUpdate(attribute) {
  attribute.needsUpdate = true;
}

/**
 * Set geometry draw range
 * @param {THREE.BufferGeometry} geometry
 * @param {number} start
 * @param {number} count
 */
export function setDrawRange(geometry, start, count) {
  geometry.setDrawRange(start, count);
}

// ============================================================================
// MATH UTILITIES
// ============================================================================

/**
 * Create Matrix4
 * @returns {THREE.Matrix4}
 */
export function createMatrix4() {
  return new THREE.Matrix4();
}

/**
 * Compose matrix from position, quaternion, scale
 * @param {THREE.Matrix4} matrix
 * @param {THREE.Vector3} position
 * @param {THREE.Quaternion} quaternion
 * @param {THREE.Vector3} scale
 */
export function composeMatrix(matrix, position, quaternion, scale) {
  matrix.compose(position, quaternion, scale);
}

/**
 * Create Vector3
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @returns {THREE.Vector3}
 */
export function createVector3(x, y, z) {
  return new THREE.Vector3(x, y, z);
}

/**
 * Create Euler
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @returns {THREE.Euler}
 */
export function createEuler(x, y, z) {
  return new THREE.Euler(x, y, z);
}

/**
 * Create Quaternion
 * @returns {THREE.Quaternion}
 */
export function createQuaternion() {
  return new THREE.Quaternion();
}

/**
 * Set quaternion from Euler
 * @param {THREE.Quaternion} quaternion
 * @param {THREE.Euler} euler
 */
export function setQuaternionFromEuler(quaternion, euler) {
  quaternion.setFromEuler(euler);
}

/**
 * Create Color
 * @param {number} hex
 * @returns {THREE.Color}
 */
export function createColor(hex) {
  return new THREE.Color(hex);
}

/**
 * Lerp between two colors
 * @param {THREE.Color} color1
 * @param {THREE.Color} color2
 * @param {number} t
 * @returns {THREE.Color}
 */
export function lerpColor(color1, color2, t) {
  return color1.clone().lerp(color2, t);
}

// ============================================================================
// UI - CSS2DRenderer
// ============================================================================

/**
 * Create CSS2DRenderer
 * @returns {CSS2DRenderer}
 */
export function createCSS2DRenderer() {
  return new CSS2DRenderer();
}

/**
 * Set CSS2DRenderer size
 * @param {CSS2DRenderer} renderer
 * @param {number} width
 * @param {number} height
 */
export function setCSS2DRendererSize(renderer, width, height) {
  renderer.setSize(width, height);
}

/**
 * Get CSS2DRenderer DOM element
 * @param {CSS2DRenderer} renderer
 * @returns {HTMLElement}
 */
export function getCSS2DRendererDomElement(renderer) {
  return renderer.domElement;
}

/**
 * Render CSS2D
 * @param {CSS2DRenderer} renderer
 * @param {THREE.Scene} scene
 * @param {THREE.Camera} camera
 */
export function renderCSS2D(renderer, scene, camera) {
  renderer.render(scene, camera);
}

/**
 * Create CSS2DObject
 * @param {HTMLElement} element
 * @returns {CSS2DObject}
 */
export function createCSS2DObject(element) {
  return new CSS2DObject(element);
}

// ============================================================================
// CONSTANTS
// ============================================================================

/**
 * Get THREE.LoopOnce constant
 * @returns {number}
 */
export function getLoopOnce() {
  return THREE.LoopOnce;
}

/**
 * Get THREE.LoopRepeat constant
 * @returns {number}
 */
export function getLoopRepeat() {
  return THREE.LoopRepeat;
}

/**
 * Get THREE.LoopPingPong constant
 * @returns {number}
 */
export function getLoopPingPong() {
  return THREE.LoopPingPong;
}

/**
 * Get THREE.AdditiveBlending constant
 * @returns {number}
 */
export function getAdditiveBlending() {
  return THREE.AdditiveBlending;
}

/**
 * Get THREE.NormalBlending constant
 * @returns {number}
 */
export function getNormalBlending() {
  return THREE.NormalBlending;
}

// ============================================================================
// PARTICLE SYSTEMS
// ============================================================================

/**
 * Get red component from color
 * @param {THREE.Color} color
 * @returns {number}
 */
export function getColorR(color) {
  return color.r;
}

/**
 * Get green component from color
 * @param {THREE.Color} color
 * @returns {number}
 */
export function getColorG(color) {
  return color.g;
}

/**
 * Get blue component from color
 * @param {THREE.Color} color
 * @returns {number}
 */
export function getColorB(color) {
  return color.b;
}

/**
 * Create Float32Array
 * @param {number} size - Number of elements
 * @returns {Float32Array}
 */
export function createFloat32Array(size) {
  return new Float32Array(size);
}

/**
 * Set buffer attribute (combines array and attribute creation)
 * @param {THREE.BufferGeometry} geometry
 * @param {string} name
 * @param {Float32Array} array
 * @param {number} itemSize
 */
export function setBufferAttribute(geometry, name, array, itemSize) {
  const attribute = new THREE.BufferAttribute(array, itemSize);
  geometry.setAttribute(name, attribute);
}

/**
 * Get geometry from Points object
 * @param {THREE.Points} points
 * @returns {THREE.BufferGeometry}
 */
export function getGeometry(points) {
  return points.geometry;
}

/**
 * Get attribute from geometry
 * @param {THREE.BufferGeometry} geometry
 * @param {string} name
 * @returns {THREE.BufferAttribute}
 */
export function getAttribute(geometry, name) {
  return geometry.getAttribute(name);
}

/**
 * Set XYZ values in buffer attribute at index
 * @param {THREE.BufferAttribute} attribute
 * @param {number} index
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setBufferXYZ(attribute, index, x, y, z) {
  const i = index * 3;
  attribute.array[i] = x;
  attribute.array[i + 1] = y;
  attribute.array[i + 2] = z;
}

/**
 * Set single value in buffer attribute at index
 * @param {THREE.BufferAttribute} attribute
 * @param {number} index
 * @param {number} value
 */
export function setBufferX(attribute, index, value) {
  attribute.array[index] = value;
}

/**
 * Mark attribute as needing update (alias for markAttributeNeedsUpdate)
 * @param {THREE.BufferAttribute} attribute
 * @param {boolean} needsUpdate
 */
export function setAttributeNeedsUpdate(attribute, needsUpdate) {
  attribute.needsUpdate = needsUpdate;
}


// ============================================================================
// RENDERER PATCH APPLICATION - IMPERATIVE OPERATIONS
// ============================================================================

/**
 * Apply transform to Three.js object
 * @param {THREE.Object3D} object
 * @param {Object} transform - {position: {x, y, z}, rotation: {x, y, z, w} (quaternion), scale: {x, y, z}}
 */
export function applyTransform(object, transform) {
  object.position.set(transform.position.x, transform.position.y, transform.position.z);
  object.quaternion.set(transform.rotation.x, transform.rotation.y, transform.rotation.z, transform.rotation.w);
  object.scale.set(transform.scale.x, transform.scale.y, transform.scale.z);
}

/**
 * Apply transform to Three.js object using quaternion for rotation
 * This avoids Euler angle conversion issues when working with physics engines
 * @param {THREE.Object3D} object
 * @param {Object} position - {x, y, z}
 * @param {Object} quaternion - {x, y, z, w}
 * @param {Object} scale - {x, y, z}
 */
export function applyTransformWithQuaternion(object, position, quaternion, scale) {
  object.position.set(position.x, position.y, position.z);
  object.quaternion.set(quaternion.x, quaternion.y, quaternion.z, quaternion.w);
  object.scale.set(scale.x, scale.y, scale.z);
}

/**
 * Apply camera lookAt with proper world matrix handling
 * @param {THREE.Camera} camera
 * @param {Object} target - {x, y, z}
 */
export function applyCameraLookAt(camera, target) {
  camera.updateMatrixWorld(true);
  camera.lookAt(target.x, target.y, target.z);
}

/**
 * Apply camera lookAt with a dynamic target (for reapplying stored lookAt)
 * @param {THREE.Camera} camera
 * @param {any} target - Dynamic target (should have x, y, z properties)
 */
export function applyCameraLookAtDynamic(camera, target) {
  camera.updateMatrixWorld(true);
  camera.lookAt(target.x, target.y, target.z);
}

/**
 * Set shadow properties on an object
 * @param {THREE.Object3D} object
 * @param {boolean} castShadow
 * @param {boolean} receiveShadow
 */
export function setShadowProperties(object, castShadow, receiveShadow) {
  object.castShadow = castShadow;
  object.receiveShadow = receiveShadow;
}

/**
 * Add object to scene
 * @param {THREE.Scene} scene
 * @param {THREE.Object3D} object
 */
export function addToScene(scene, object) {
  scene.add(object);
}

/**
 * Remove object from scene
 * @param {THREE.Scene} scene
 * @param {THREE.Object3D} object
 */
export function removeFromScene(scene, object) {
  scene.remove(object);
}

/**
 * Set camera user data
 * @param {THREE.Camera} camera
 * @param {string} key
 * @param {any} value
 */
export function setCameraUserData(camera, key, value) {
  camera.userData[key] = value;
}

/**
 * Get camera user data
 * @param {THREE.Camera} camera
 * @param {string} key
 * @returns {any}
 */
export function getCameraUserData(camera, key) {
  return camera.userData[key];
}

/**
 * Delete camera user data
 * @param {THREE.Camera} camera
 * @param {string} key
 */
export function deleteCameraUserData(camera, key) {
  delete camera.userData[key];
}

/**
 * Check if camera has user data
 * @param {THREE.Camera} camera
 * @param {string} key
 * @returns {boolean}
 */
export function hasCameraUserData(camera, key) {
  return camera.userData && camera.userData[key] !== undefined;
}

/**
 * Update instanced mesh transforms
 * @param {THREE.InstancedMesh} mesh
 * @param {Array} instances - Array of {position: {x,y,z}, rotation: {x,y,z}, scale: {x,y,z}}
 */
export function updateInstancedMeshTransforms(mesh, instances) {
  const matrix = new THREE.Matrix4();
  const position = new THREE.Vector3();
  const rotation = new THREE.Euler();
  const quaternion = new THREE.Quaternion();
  const scale = new THREE.Vector3();

  let i = 0;
  for (const inst of instances) {
    position.set(inst.position.x, inst.position.y, inst.position.z);
    rotation.set(inst.rotation.x, inst.rotation.y, inst.rotation.z);
    scale.set(inst.scale.x, inst.scale.y, inst.scale.z);

    quaternion.setFromEuler(rotation);
    matrix.compose(position, quaternion, scale);

    mesh.setMatrixAt(i, matrix);
    i++;
  }

  mesh.instanceMatrix.needsUpdate = true;
}

/**
 * Check if object is a PerspectiveCamera
 * @param {any} object
 * @returns {boolean}
 */
export function isPerspectiveCamera(object) {
  return object instanceof THREE.PerspectiveCamera;
}

/**
 * Check if object is an OrthographicCamera
 * @param {any} object
 * @returns {boolean}
 */
export function isOrthographicCamera(object) {
  return object instanceof THREE.OrthographicCamera;
}

/**
 * Check if object is an InstancedMesh
 * @param {any} object
 * @returns {boolean}
 */
export function isInstancedMesh(object) {
  return object instanceof THREE.InstancedMesh;
}

/**
 * Check if object is an LOD
 * @param {any} object
 * @returns {boolean}
 */
export function isLOD(object) {
  return object instanceof THREE.LOD;
}

/**
 * Clear all LOD levels
 * @param {THREE.LOD} lod
 */
export function clearLODLevels(lod) {
  while (lod.levels.length > 0) {
    lod.removeLevel(lod.levels[0].distance);
  }
}

/**
 * Update camera projection matrix
 * @param {THREE.Camera} camera
 */
export function updateCameraProjectionMatrix(camera) {
  camera.updateProjectionMatrix();
}

/**
 * Set camera projection parameters for perspective camera
 * @param {THREE.PerspectiveCamera} camera
 * @param {number} fov
 * @param {number} aspect
 * @param {number} near
 * @param {number} far
 */
export function setPerspectiveCameraParams(camera, fov, aspect, near, far) {
  camera.fov = fov;
  camera.aspect = aspect;
  camera.near = near;
  camera.far = far;
}

/**
 * Set camera projection parameters for orthographic camera
 * @param {THREE.OrthographicCamera} camera
 * @param {number} left
 * @param {number} right
 * @param {number} top
 * @param {number} bottom
 * @param {number} near
 * @param {number} far
 */
export function setOrthographicCameraParams(camera, left, right, top, bottom, near, far) {
  camera.left = left;
  camera.right = right;
  camera.top = top;
  camera.bottom = bottom;
  camera.near = near;
  camera.far = far;
}

/**
 * Get object geometry (if it has one)
 * @param {THREE.Object3D} object
 * @returns {THREE.BufferGeometry|undefined}
 */
export function getObjectGeometry(object) {
  return object.geometry;
}

/**
 * Get object material (if it has one)
 * @param {THREE.Object3D} object
 * @returns {THREE.Material|THREE.Material[]|undefined}
 */
export function getObjectMaterial(object) {
  return object.material;
}

/**
 * Set object geometry
 * @param {THREE.Object3D} object
 * @param {THREE.BufferGeometry} geometry
 */
export function setObjectGeometry(object, geometry) {
  object.geometry = geometry;
}

/**
 * Set object material
 * @param {THREE.Object3D} object
 * @param {THREE.Material|THREE.Material[]} material
 */
export function setObjectMaterial(object, material) {
  object.material = material;
}

/**
 * Get object position
 * @param {THREE.Object3D} object
 * @returns {Object} - {x, y, z}
 */
export function getObjectPosition(object) {
  return {
    x: object.position.x,
    y: object.position.y,
    z: object.position.z
  };
}

/**
 * Get object rotation
 * @param {THREE.Object3D} object
 * @returns {Object} - {x, y, z}
 */
export function getObjectRotation(object) {
  return {
    x: object.rotation.x,
    y: object.rotation.y,
    z: object.rotation.z
  };
}

/**
 * Get object scale
 * @param {THREE.Object3D} object
 * @returns {Object} - {x, y, z}
 */
export function getObjectScale(object) {
  return {
    x: object.scale.x,
    y: object.scale.y,
    z: object.scale.z
  };
}

/**
 * Get object quaternion
 * @param {THREE.Object3D} object
 * @returns {Object} - {x, y, z, w}
 */
export function getObjectQuaternion(object) {
  return {
    x: object.quaternion.x,
    y: object.quaternion.y,
    z: object.quaternion.z,
    w: object.quaternion.w
  };
}

/**
 * Set object quaternion
 * @param {THREE.Object3D} object
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {number} w
 */
export function setObjectQuaternion(object, x, y, z, w) {
  object.quaternion.set(x, y, z, w);
}

// ============================================================================
// EULER <-> QUATERNION CONVERSION
// ============================================================================

/**
 * Convert Euler angles to Quaternion using Three.js's built-in conversion
 * @param {Vec3} euler - Euler angles as Vec3(x, y, z) in radians
 * @returns {Quaternion} Quaternion
 */
export function eulerToQuaternion(euler) {
  const threeEuler = new THREE.Euler(euler.x, euler.y, euler.z, 'XYZ');
  const threeQuaternion = new THREE.Quaternion().setFromEuler(threeEuler);
  return new Quaternion(threeQuaternion.x, threeQuaternion.y, threeQuaternion.z, threeQuaternion.w);
}

/**
 * Convert Quaternion to Euler angles using Three.js's built-in conversion
 * @param {Quaternion} q - Quaternion
 * @returns {Vec3} Euler angles as Vec3(x, y, z) in radians
 */
export function quaternionToEuler(q) {
  const threeQuaternion = new THREE.Quaternion(q.x, q.y, q.z, q.w);
  const threeEuler = new THREE.Euler().setFromQuaternion(threeQuaternion, 'XYZ');
  return new Vec3(threeEuler.x, threeEuler.y, threeEuler.z);
}

/**
 * Multiply two quaternions using Three.js
 * @param {Quaternion} q1 - First quaternion
 * @param {Quaternion} q2 - Second quaternion
 * @returns {Quaternion} Result of q1 * q2
 */
export function multiplyQuaternions(q1, q2) {
  const threeQ1 = new THREE.Quaternion(q1.x, q1.y, q1.z, q1.w);
  const threeQ2 = new THREE.Quaternion(q2.x, q2.y, q2.z, q2.w);
  threeQ1.multiply(threeQ2);
  return new Quaternion(threeQ1.x, threeQ1.y, threeQ1.z, threeQ1.w);
}
