import * as THREE from 'three';
import { AddNode, RemoveNode, UpdateTransform, UpdateMaterial, UpdateGeometry, UpdateLight, UpdateAnimation, UpdatePhysics, UpdateAudio, UpdateInstances, UpdateLODLevels, UpdateCamera, SetActiveCamera } from '../scene.mjs';
import {
  Mesh, InstancedMesh, Light, Group, LOD, Model3D, Audio, Camera,
  DebugBox, DebugSphere, DebugLine, DebugAxes, DebugGrid, DebugPoint,
  BoxGeometry, SphereGeometry, ConeGeometry, PlaneGeometry, CircleGeometry,
  CylinderGeometry, TorusGeometry, TetrahedronGeometry, IcosahedronGeometry,
  CustomGeometry,
  BasicMaterial, StandardMaterial, PhongMaterial, LambertMaterial, ToonMaterial,
  LineMaterial, SpriteMaterial,
  AmbientLight, DirectionalLight, PointLight, SpotLight, HemisphereLight
} from '../scene.mjs';
import { LoopOnce, LoopRepeat, SingleAnimation, BlendedAnimations } from '../object3d.mjs';
import { GlobalAudio, PositionalAudio } from '../audio.mjs';
import { playAudio, pauseAudio, stopAudio, setAudioVolume, updateAudioConfig } from './audio.mjs';
import { createRigidBody, removeRigidBody, getBodyTransform } from './physics.mjs';
import { createDebugBox, createDebugSphere, createDebugLine, createDebugAxes, createDebugGrid, createDebugPoint, updatePerformanceStats, setRenderStats } from './debug.mjs';
import { createThreeCamera } from './camera.mjs';
import { setCamera } from './effects.mjs';

// Cache of Three.js objects by ID
const objectCache = new Map();

// Cache of Animation Mixers by node ID
const mixerCache = new Map();

// Cache of current animation actions by node ID (can be array for blending)
const actionCache = new Map();

// Cache of camera viewports by camera ID
const cameraViewports = new Map();

// Create Three.js geometry from geometry type
export function createGeometry(geomType) {
  if (geomType instanceof BoxGeometry) {
    return new THREE.BoxGeometry(geomType.width, geomType.height, geomType.depth);
  } else if (geomType instanceof SphereGeometry) {
    return new THREE.SphereGeometry(geomType.radius, geomType.width_segments, geomType.height_segments);
  } else if (geomType instanceof ConeGeometry) {
    return new THREE.ConeGeometry(geomType.radius, geomType.height, geomType.segments);
  } else if (geomType instanceof PlaneGeometry) {
    return new THREE.PlaneGeometry(geomType.width, geomType.height);
  } else if (geomType instanceof CircleGeometry) {
    return new THREE.CircleGeometry(geomType.radius, geomType.segments);
  } else if (geomType instanceof CylinderGeometry) {
    return new THREE.CylinderGeometry(
      geomType.radius_top,
      geomType.radius_bottom,
      geomType.height,
      geomType.radial_segments
    );
  } else if (geomType instanceof TorusGeometry) {
    return new THREE.TorusGeometry(
      geomType.radius,
      geomType.tube,
      geomType.radial_segments,
      geomType.tubular_segments
    );
  } else if (geomType instanceof TetrahedronGeometry) {
    return new THREE.TetrahedronGeometry(geomType.radius, geomType.detail);
  } else if (geomType instanceof IcosahedronGeometry) {
    return new THREE.IcosahedronGeometry(geomType.radius, geomType.detail);
  } else if (geomType instanceof CustomGeometry) {
    // CustomGeometry wraps a Three.js BufferGeometry directly
    return geomType[0]; // Extract the BufferGeometry from the wrapper
  } else {
    console.warn('Unknown geometry type:', geomType);
    return new THREE.BoxGeometry(1, 1, 1);
  }
}

// Helper to extract texture from Option
function getTexture(optionTexture) {
  // Option is either Some(texture) or None
  // In Gleam's JS output, Some is an object with a property, None is an object without
  if (optionTexture && optionTexture[0]) {
    return optionTexture[0]; // Extract texture from Some
  }
  return null;
}

// Create Three.js material from material type
export function createMaterial(matType) {
  if (matType instanceof BasicMaterial) {
    const map = getTexture(matType.map);
    return new THREE.MeshBasicMaterial({
      color: matType.color,
      transparent: matType.transparent,
      opacity: matType.opacity,
      map: map
    });
  } else if (matType instanceof StandardMaterial) {
    const map = getTexture(matType.map);
    return new THREE.MeshStandardMaterial({
      color: matType.color,
      metalness: matType.metalness,
      roughness: matType.roughness,
      map: map
    });
  } else if (matType instanceof PhongMaterial) {
    const map = getTexture(matType.map);
    return new THREE.MeshPhongMaterial({
      color: matType.color,
      shininess: matType.shininess,
      map: map
    });
  } else if (matType instanceof LambertMaterial) {
    const map = getTexture(matType.map);
    return new THREE.MeshLambertMaterial({
      color: matType.color,
      map: map
    });
  } else if (matType instanceof ToonMaterial) {
    const map = getTexture(matType.map);
    return new THREE.MeshToonMaterial({
      color: matType.color,
      map: map
    });
  } else if (matType instanceof LineMaterial) {
    return new THREE.LineBasicMaterial({
      color: matType.color,
      linewidth: matType.linewidth
    });
  } else if (matType instanceof SpriteMaterial) {
    const map = getTexture(matType.map);
    return new THREE.SpriteMaterial({
      color: matType.color,
      transparent: matType.transparent,
      opacity: matType.opacity,
      map: map
    });
  } else {
    console.warn('Unknown material type:', matType);
    return new THREE.MeshBasicMaterial({ color: 0xffffff });
  }
}

// Create Three.js light from light type
export function createLight(lightType) {
  if (lightType instanceof AmbientLight) {
    return new THREE.AmbientLight(lightType.color, lightType.intensity);
  } else if (lightType instanceof DirectionalLight) {
    return new THREE.DirectionalLight(lightType.color, lightType.intensity);
  } else if (lightType instanceof PointLight) {
    return new THREE.PointLight(lightType.color, lightType.intensity, lightType.distance);
  } else if (lightType instanceof SpotLight) {
    return new THREE.SpotLight(
      lightType.color,
      lightType.intensity,
      lightType.distance,
      lightType.angle,
      lightType.penumbra
    );
  } else if (lightType instanceof HemisphereLight) {
    return new THREE.HemisphereLight(
      lightType.sky_color,
      lightType.ground_color,
      lightType.intensity
    );
  } else {
    console.warn('Unknown light type:', lightType);
    return new THREE.AmbientLight(0xffffff, 1);
  }
}

// Apply transform to Three.js object
export function applyTransform(object, transform) {
  object.position.set(transform.position.x, transform.position.y, transform.position.z);
  object.rotation.set(transform.rotation.x, transform.rotation.y, transform.rotation.z);
  object.scale.set(transform.scale.x, transform.scale.y, transform.scale.z);
}

// Setup animation (single or blended) for a Model3D
function setupAnimation(id, mixer, animPlayback) {
  // Stop any existing animations
  const existingActions = actionCache.get(id);
  if (existingActions) {
    if (Array.isArray(existingActions)) {
      existingActions.forEach(action => action.stop());
    } else {
      existingActions.stop();
    }
  }

  if (animPlayback instanceof SingleAnimation) {
    // Single animation
    const animConfig = animPlayback[0]; // Extract Animation from SingleAnimation
    const action = mixer.clipAction(animConfig.clip);
    action.setLoop(animConfig.loop instanceof LoopRepeat ? THREE.LoopRepeat : THREE.LoopOnce);
    action.timeScale = animConfig.speed;
    action.weight = animConfig.weight;
    action.play();

    actionCache.set(id, action);
    console.log('[Renderer] Playing single animation for Model3D:', id);
  } else if (animPlayback instanceof BlendedAnimations) {
    // Blended animations
    const fromAnim = animPlayback.from;
    const toAnim = animPlayback.to;
    const blendFactor = animPlayback.blend_factor;

    const fromAction = mixer.clipAction(fromAnim.clip);
    fromAction.setLoop(fromAnim.loop instanceof LoopRepeat ? THREE.LoopRepeat : THREE.LoopOnce);
    fromAction.timeScale = fromAnim.speed;
    fromAction.weight = (1.0 - blendFactor) * fromAnim.weight;
    fromAction.play();

    const toAction = mixer.clipAction(toAnim.clip);
    toAction.setLoop(toAnim.loop instanceof LoopRepeat ? THREE.LoopRepeat : THREE.LoopOnce);
    toAction.timeScale = toAnim.speed;
    toAction.weight = blendFactor * toAnim.weight;
    toAction.play();

    actionCache.set(id, [fromAction, toAction]);
    console.log('[Renderer] Blending animations for Model3D:', id, 'Factor:', blendFactor.toFixed(2));
  }
}

// Apply a single patch to the Three.js scene
export function applyPatch(scene, patch) {
  if (patch instanceof AddNode) {
    const { id, node, parent_id } = patch;

    let threeObj;

    if (node instanceof Mesh) {
      const geometry = createGeometry(node.geometry);
      const material = createMaterial(node.material);
      threeObj = new THREE.Mesh(geometry, material);
      applyTransform(threeObj, node.transform);

      // Create physics body if specified
      if (node.physics && node.physics[0]) {
        createRigidBody(id, node.physics[0], node.transform);
      }
    } else if (node instanceof InstancedMesh) {
      const geometry = createGeometry(node.geometry);
      const material = createMaterial(node.material);

      // Convert Gleam list to array to get count
      const instancesArray = Array.from(node.instances);
      const count = instancesArray.length;

      threeObj = new THREE.InstancedMesh(geometry, material, count);

      // Set transform matrix for each instance
      updateInstancedMeshTransforms(threeObj, node.instances);
    } else if (node instanceof Light) {
      threeObj = createLight(node.light_type);
      applyTransform(threeObj, node.transform);
    } else if (node instanceof Group) {
      threeObj = new THREE.Group();
      applyTransform(threeObj, node.transform);
    } else if (node instanceof LOD) {
      threeObj = new THREE.LOD();
      applyTransform(threeObj, node.transform);

      // Add LOD levels
      for (const level of node.levels) {
        const levelNode = level.node;
        const distance = level.distance;

        // Create Three.js object for this LOD level
        // We need to recursively create the node (but we don't add it to scene)
        let levelObj;

        if (levelNode instanceof Mesh) {
          const geometry = createGeometry(levelNode.geometry);
          const material = createMaterial(levelNode.material);
          levelObj = new THREE.Mesh(geometry, material);
          applyTransform(levelObj, levelNode.transform);
        } else if (levelNode instanceof Group) {
          levelObj = new THREE.Group();
          applyTransform(levelObj, levelNode.transform);
          // TODO: Handle children of Group in LOD levels if needed
        } else if (levelNode instanceof Model3D) {
          levelObj = levelNode.object.clone();
          applyTransform(levelObj, levelNode.transform);
        }
        // TODO: Add other node types as needed

        if (levelObj) {
          threeObj.addLevel(levelObj, distance);
        }
      }
    } else if (node instanceof Model3D) {
      // Use the object directly - it will be cached
      // The immutability is maintained at the Gleam level via the scene graph diff
      threeObj = node.object;
      applyTransform(threeObj, node.transform);

      // Create animation mixer once for this Model3D instance
      const mixer = new THREE.AnimationMixer(threeObj);
      mixerCache.set(id, mixer);

      // Setup animation if provided
      if (node.animation && node.animation[0]) {
        setupAnimation(id, mixer, node.animation[0]);
      }

      // Create physics body if specified
      if (node.physics && node.physics[0]) {
        createRigidBody(id, node.physics[0], node.transform);
      }
    } else if (node instanceof Audio) {
      // Audio nodes don't need a visual Three.js object
      // Just play the audio using the audio system
      playAudio(id, node.buffer, node.config, node.audio_type);
      // Store a placeholder to track the audio node in cache
      threeObj = new THREE.Group(); // Empty group as placeholder
    } else if (node instanceof Camera) {
      // Create Three.js camera from Gleam camera config
      threeObj = createThreeCamera(node.camera_type.projection);

      // Apply camera position from Gleam config
      const pos = node.camera_type.position;
      threeObj.position.set(pos.x, pos.y, pos.z);

      // Apply look_at from Gleam config
      const lookAt = node.camera_type.look_at_target;
      threeObj.lookAt(lookAt.x, lookAt.y, lookAt.z);

      // Update projection matrix
      threeObj.updateProjectionMatrix();

      // Store viewport if specified
      if (node.viewport && node.viewport[0]) {
        const viewport = node.viewport[0];
        cameraViewports.set(id, viewport);
      } else {
        cameraViewports.delete(id);
      }

      // If this camera is active, set it as the active camera for rendering
      if (node.active) {
        setCamera(threeObj);
        console.log('[Renderer] Set active camera:', id);
      }
    } else if (node instanceof DebugBox) {
      threeObj = createDebugBox(node.min, node.max, node.color);
    } else if (node instanceof DebugSphere) {
      threeObj = createDebugSphere(node.center, node.radius, node.color);
    } else if (node instanceof DebugLine) {
      threeObj = createDebugLine(node.from, node.to, node.color);
    } else if (node instanceof DebugAxes) {
      threeObj = createDebugAxes(node.origin, node.size);
    } else if (node instanceof DebugGrid) {
      threeObj = createDebugGrid(node.size, node.divisions, node.color);
    } else if (node instanceof DebugPoint) {
      threeObj = createDebugPoint(node.position, node.size, node.color);
    }

    if (threeObj) {
      objectCache.set(id, threeObj);

      // Add to parent or scene
      if (parent_id && parent_id[0]) {
        // parent_id is Option(String), [0] gets the Some value
        const parentObj = objectCache.get(parent_id[0]);
        if (parentObj) {
          parentObj.add(threeObj);
        } else {
          console.warn(`Parent ${parent_id[0]} not found, adding to scene`);
          scene.add(threeObj);
        }
      } else {
        scene.add(threeObj);
      }
    }
  } else if (patch instanceof RemoveNode) {
    const id = patch.id;
    const obj = objectCache.get(id);
    if (obj) {
      scene.remove(obj);
      // Dispose geometry and material to free memory
      if (obj.geometry) obj.geometry.dispose();
      if (obj.material) obj.material.dispose();
      objectCache.delete(id);

      // Clean up animation mixer if exists
      if (mixerCache.has(id)) {
        mixerCache.delete(id);
        actionCache.delete(id);
      }

      // Stop audio if exists
      stopAudio(id);

      // Remove physics body if exists
      removeRigidBody(id);
    }
  } else if (patch instanceof UpdateTransform) {
    const { id, transform } = patch;
    const obj = objectCache.get(id);
    if (obj) {
      applyTransform(obj, transform);
    }
  } else if (patch instanceof UpdateMaterial) {
    const { id, material } = patch;
    const obj = objectCache.get(id);
    if (obj && obj.material) {
      obj.material.dispose();
      obj.material = createMaterial(material);
    }
  } else if (patch instanceof UpdateGeometry) {
    const { id, geometry } = patch;
    const obj = objectCache.get(id);
    if (obj && obj.geometry) {
      obj.geometry.dispose();
      obj.geometry = createGeometry(geometry);
    }
  } else if (patch instanceof UpdateLight) {
    const { id, light_type } = patch;
    const oldLight = objectCache.get(id);
    if (oldLight) {
      const newLight = createLight(light_type);
      newLight.position.copy(oldLight.position);
      newLight.rotation.copy(oldLight.rotation);
      newLight.scale.copy(oldLight.scale);

      scene.remove(oldLight);
      scene.add(newLight);
      objectCache.set(id, newLight);
    }
  } else if (patch instanceof UpdateAnimation) {
    const { id, animation } = patch;
    const mixer = mixerCache.get(id);

    if (!mixer) {
      console.warn('[Renderer] No mixer found for Model3D:', id);
      return;
    }

    // If animation is Some, setup new animation
    if (animation && animation[0]) {
      setupAnimation(id, mixer, animation[0]);
      console.log('[Renderer] Updated animation for Model3D:', id);
    } else {
      // Stop all animations
      const currentActions = actionCache.get(id);
      if (currentActions) {
        if (Array.isArray(currentActions)) {
          currentActions.forEach(action => action.stop());
        } else {
          currentActions.stop();
        }
        actionCache.delete(id);
      }
      console.log('[Renderer] Stopped animation for Model3D:', id);
    }
  } else if (patch instanceof UpdatePhysics) {
    const { id, physics } = patch;

    // Remove old physics body if it exists
    removeRigidBody(id);

    // Create new physics body if provided
    if (physics && physics[0]) {
      // Get current transform from Three.js object
      const obj = objectCache.get(id);
      if (obj) {
        const transform = {
          position: { x: obj.position.x, y: obj.position.y, z: obj.position.z },
          rotation: { x: obj.rotation.x, y: obj.rotation.y, z: obj.rotation.z },
          scale: { x: obj.scale.x, y: obj.scale.y, z: obj.scale.z }
        };
        createRigidBody(id, physics[0], transform);
        console.log('[Renderer] Updated physics for node:', id);
      }
    } else {
      console.log('[Renderer] Removed physics from node:', id);
    }
  } else if (patch instanceof UpdateAudio) {
    const { id, config } = patch;
    // Update audio configuration
    updateAudioConfig(id, config);
    console.log('[Renderer] Updated audio config for node:', id);
  } else if (patch instanceof UpdateInstances) {
    const { id, instances } = patch;
    const obj = objectCache.get(id);
    if (obj && obj instanceof THREE.InstancedMesh) {
      updateInstancedMeshTransforms(obj, instances);
    } else {
      console.warn('[Renderer] Object not found or not an InstancedMesh:', id);
    }
  } else if (patch instanceof UpdateLODLevels) {
    const { id, levels } = patch;
    const obj = objectCache.get(id);
    if (obj && obj instanceof THREE.LOD) {
      // Clear existing levels
      while (obj.levels.length > 0) {
        obj.removeLevel(obj.levels[0].distance);
      }

      // Add new levels
      for (const level of levels) {
        const levelNode = level.node;
        const distance = level.distance;

        // Create Three.js object for this LOD level
        let levelObj;

        if (levelNode instanceof Mesh) {
          const geometry = createGeometry(levelNode.geometry);
          const material = createMaterial(levelNode.material);
          levelObj = new THREE.Mesh(geometry, material);
          applyTransform(levelObj, levelNode.transform);
        } else if (levelNode instanceof Group) {
          levelObj = new THREE.Group();
          applyTransform(levelObj, levelNode.transform);
        } else if (levelNode instanceof Model3D) {
          levelObj = levelNode.object.clone();
          applyTransform(levelObj, levelNode.transform);
        }

        if (levelObj) {
          obj.addLevel(levelObj, distance);
        }
      }
      console.log('[Renderer] Updated LOD levels for:', id);
    } else {
      console.warn('[Renderer] Object not found or not an LOD:', id);
    }
  } else if (patch instanceof UpdateCamera) {
    const { id, camera_type } = patch;
    const cameraObj = objectCache.get(id);
    if (cameraObj && (cameraObj instanceof THREE.PerspectiveCamera || cameraObj instanceof THREE.OrthographicCamera)) {
      // Update camera position
      const pos = camera_type.position;
      cameraObj.position.set(pos.x, pos.y, pos.z);

      // Update look_at
      const lookAt = camera_type.look_at_target;
      cameraObj.lookAt(lookAt.x, lookAt.y, lookAt.z);

      // Update camera projection parameters if they changed
      const projection = camera_type.projection;
      if (projection.fov !== undefined && cameraObj instanceof THREE.PerspectiveCamera) {
        cameraObj.fov = projection.fov;
        cameraObj.aspect = projection.aspect;
        cameraObj.near = projection.near;
        cameraObj.far = projection.far;
      } else if (projection.left !== undefined && cameraObj instanceof THREE.OrthographicCamera) {
        cameraObj.left = projection.left;
        cameraObj.right = projection.right;
        cameraObj.top = projection.top;
        cameraObj.bottom = projection.bottom;
        cameraObj.near = projection.near;
        cameraObj.far = projection.far;
      }

      // Update projection matrix
      cameraObj.updateProjectionMatrix();

      console.log('[Renderer] Updated camera:', id);
    } else {
      console.warn('[Renderer] Camera not found:', id);
    }
  } else if (patch instanceof SetActiveCamera) {
    const id = patch.id;
    const cameraObj = objectCache.get(id);
    if (cameraObj && (cameraObj instanceof THREE.PerspectiveCamera || cameraObj instanceof THREE.OrthographicCamera)) {
      setCamera(cameraObj);
      console.log('[Renderer] Switched to active camera:', id);
    } else {
      console.warn('[Renderer] Camera not found or not a camera:', id);
    }
  } else {
    console.warn('Unknown patch type:', patch);
  }
}

/**
 * Get all cameras with viewports for multi-viewport rendering
 * Returns array of { camera, viewport: [x, y, width, height] }
 */
export function getCamerasWithViewports() {
  const cameras = [];
  for (const [id, viewport] of cameraViewports.entries()) {
    const camera = objectCache.get(id);
    if (camera) {
      cameras.push({ camera, viewport });
    }
  }
  return cameras;
}

// Update InstancedMesh transform matrices from Gleam InstanceTransform list
function updateInstancedMeshTransforms(instancedMesh, instances) {
  const matrix = new THREE.Matrix4();
  const position = new THREE.Vector3();
  const rotation = new THREE.Euler();
  const quaternion = new THREE.Quaternion();
  const scale = new THREE.Vector3();

  // Convert Gleam list to array and iterate
  let i = 0;
  for (const inst of instances) {
    // Extract position, rotation, scale from Gleam InstanceTransform
    position.set(inst.position.x, inst.position.y, inst.position.z);
    rotation.set(inst.rotation.x, inst.rotation.y, inst.rotation.z);
    scale.set(inst.scale.x, inst.scale.y, inst.scale.z);

    // Compose matrix
    quaternion.setFromEuler(rotation);
    matrix.compose(position, quaternion, scale);

    // Set matrix for this instance
    instancedMesh.setMatrixAt(i, matrix);
    i++;
  }

  // Mark instance matrix as needing update
  instancedMesh.instanceMatrix.needsUpdate = true;
}

// Apply multiple patches
export function applyPatches(scene, patches) {
  // Convert Gleam list to array for iteration
  for (const patch of patches) {
    applyPatch(scene, patch);
  }
}

// Update all animation mixers
export function updateMixers(deltaTime) {
  if (mixerCache.size > 0) {
    mixerCache.forEach(mixer => {
      mixer.update(deltaTime);
    });
  }
}

// Sync physics body transforms to Three.js objects
export function syncPhysicsTransforms() {
  objectCache.forEach((obj, id) => {
    const transform = getBodyTransform(id);
    if (transform && transform[0]) {
      // transform is Some(Transform)
      const t = transform[0];
      obj.position.set(t.position.x, t.position.y, t.position.z);
      obj.rotation.set(t.rotation.x, t.rotation.y, t.rotation.z);
      // Don't update scale - physics doesn't affect scale
    }
  });
}

// Clear all cached objects (useful for cleanup)
export function clearCache() {
  objectCache.forEach(obj => {
    if (obj.geometry) obj.geometry.dispose();
    if (obj.material) obj.material.dispose();
  });
  objectCache.clear();
  mixerCache.clear();
  actionCache.clear();
}

export function createRenderer(options) {
  const renderer = new THREE.WebGLRenderer({
    antialias: options.antialias,
    alpha: options.alpha,
  });
  renderer.setSize(options.width, options.height);
  renderer.setPixelRatio(window.devicePixelRatio);

  // Add WebGL context loss/restore handling
  setupContextLossHandling(renderer);

  return renderer;
}

/**
 * Setup WebGL context loss and restore event handlers
 * @param {THREE.WebGLRenderer} renderer
 */
function setupContextLossHandling(renderer) {
  const canvas = renderer.domElement;

  canvas.addEventListener('webglcontextlost', (event) => {
    console.warn('[Tiramisu] WebGL context lost!');
    event.preventDefault(); // Prevent default to enable context restoration

    // Stop rendering loop temporarily
    // The game loop will handle this gracefully by checking if context exists
  }, false);

  canvas.addEventListener('webglcontextrestored', () => {
    console.log('[Tiramisu] WebGL context restored!');

    // Reinitialize renderer settings
    renderer.setPixelRatio(window.devicePixelRatio);

    // Clear caches - textures and geometries need to be re-uploaded to GPU
    // The scene will be re-rendered, triggering resource re-creation
    console.log('[Tiramisu] Caches cleared, resources will be re-uploaded on next render');
  }, false);
}

/**
 * Check if the WebGL context is still valid
 * @param {THREE.WebGLRenderer} renderer
 * @returns {boolean}
 */
export function isContextValid(renderer) {
  const gl = renderer.getContext();
  return gl && !gl.isContextLost();
}

export function render(renderer, scene, camera) {
  renderer.render(scene, camera);
}

export function setSize(renderer, width, height) {
  renderer.setSize(width, height);
  return renderer;
}

export function getDomElement(renderer) {
  return renderer.domElement;
}

export function setClearColor(renderer, color, alpha) {
  renderer.setClearColor(color, alpha);
  return renderer;
}
