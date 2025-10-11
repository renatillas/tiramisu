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
 * @param {boolean} antialias
 * @param {boolean} alpha
 * @returns {THREE.WebGLRenderer}
 */
export function createRenderer(antialias, alpha) {
  const renderer = new THREE.WebGLRenderer({ antialias, alpha });
  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;
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

// ============================================================================
// MATERIALS
// ============================================================================

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
    map
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
export function createStandardMaterial(color, metalness, roughness, map, normalMap, aoMap, roughnessMap, metalnessMap) {
  const material = new THREE.MeshStandardMaterial({
    color,
    metalness,
    roughness,
    map,
    normalMap,
    aoMap,
    roughnessMap,
    metalnessMap
  });

  if (normalMap) {
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
  const material = new THREE.MeshPhongMaterial({
    color,
    shininess,
    map,
    normalMap,
    aoMap
  });

  if (normalMap) {
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
  const material = new THREE.MeshLambertMaterial({
    color,
    map,
    normalMap,
    aoMap
  });

  if (normalMap) {
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
    map,
    normalMap,
    aoMap
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
    map
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
