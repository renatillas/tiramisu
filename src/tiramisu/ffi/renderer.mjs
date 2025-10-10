import * as THREE from 'three';
import * as SCENE_GLEAM from '../scene.mjs';
import * as GEOMETRY_GLEAM from '../geometry.mjs';
import * as MATERIAL_GLEAM from '../material.mjs';
import * as LIGHT_GLEAM from '../light.mjs';
import * as OBJECT3D_GLEAM from '../object3d.mjs';
import * as AUDIO_GLEAM from '../audio.mjs';
import * as AUDIO from './audio.mjs';
import * as ASSET from './asset.mjs';
import * as PHYSICS from './physics.mjs';
import * as DEBUG from './debug.mjs';
import * as CAMERA from './camera.mjs';

// Cache of Three.js objects by ID
const objectCache = new Map();

// Cache of Animation Mixers by node ID
const mixerCache = new Map();

// Cache of current animation actions by node ID (can be array for blending)
const actionCache = new Map();

// Cache of camera viewports by camera ID
const cameraViewports = new Map();

// Cache of particle systems by node ID
const particleSystemCache = new Map();

/**
 * Particle System - manages particle spawning, updating, and rendering
 */
class ParticleSystem {
  constructor(emitter, transform) {
    this.emitter = emitter;
    this.active = true;
    this.particles = [];
    this.timeSinceLastSpawn = 0;

    // Create Three.js Points object for rendering
    const maxParticles = emitter.max_particles;
    const geometry = new THREE.BufferGeometry();

    // Pre-allocate buffers for all particles
    this.positions = new Float32Array(maxParticles * 3);
    this.colors = new Float32Array(maxParticles * 3);
    this.sizes = new Float32Array(maxParticles);
    this.alphas = new Float32Array(maxParticles);

    geometry.setAttribute('position', new THREE.BufferAttribute(this.positions, 3));
    geometry.setAttribute('color', new THREE.BufferAttribute(this.colors, 3));
    geometry.setAttribute('size', new THREE.BufferAttribute(this.sizes, 1));
    geometry.setAttribute('alpha', new THREE.BufferAttribute(this.alphas, 1));

    // Create material
    const material = new THREE.PointsMaterial({
      size: emitter.size,
      vertexColors: true,
      transparent: true,
      opacity: 1.0,
      depthWrite: false,
      blending: THREE.AdditiveBlending, // Additive blending for glowy particles
      sizeAttenuation: true
    });

    // Create Points mesh
    this.points = new THREE.Points(geometry, material);
    this.points.position.set(transform.position.x, transform.position.y, transform.position.z);
    this.points.rotation.set(transform.rotation.x, transform.rotation.y, transform.rotation.z);
    this.points.scale.set(transform.scale.x, transform.scale.y, transform.scale.z);

    // Extract start and end colors
    this.startColor = new THREE.Color(emitter.color);
    this.endColor = emitter.color_end && emitter.color_end[0]
      ? new THREE.Color(emitter.color_end[0])
      : this.startColor.clone();
  }

  spawn() {
    if (this.particles.length >= this.emitter.max_particles) {
      // Recycle oldest particle
      const particle = this.particles.shift();
      this.initParticle(particle);
      this.particles.push(particle);
    } else {
      // Create new particle
      const particle = {};
      this.initParticle(particle);
      this.particles.push(particle);
    }
  }

  initParticle(particle) {
    const emitter = this.emitter;

    // Randomize velocity with variance
    const vx = emitter.velocity.x + (Math.random() * 2 - 1) * emitter.velocity_variance.x;
    const vy = emitter.velocity.y + (Math.random() * 2 - 1) * emitter.velocity_variance.y;
    const vz = emitter.velocity.z + (Math.random() * 2 - 1) * emitter.velocity_variance.z;

    // Randomize size with variance
    const size = Math.max(0.01, emitter.size + (Math.random() * 2 - 1) * emitter.size_variance);

    particle.position = { x: 0, y: 0, z: 0 }; // Start at emitter position
    particle.velocity = { x: vx, y: vy, z: vz };
    particle.life = 0; // Current age
    particle.lifetime = emitter.lifetime;
    particle.size = size;
  }

  update(deltaTime) {
    const emitter = this.emitter;

    // Spawn new particles
    if (this.active) {
      this.timeSinceLastSpawn += deltaTime;
      const spawnInterval = 1.0 / emitter.rate;

      while (this.timeSinceLastSpawn >= spawnInterval) {
        this.spawn();
        this.timeSinceLastSpawn -= spawnInterval;
      }
    }

    // Update existing particles
    const gravity = { x: 0, y: -9.81 * emitter.gravity_scale, z: 0 };

    for (let i = this.particles.length - 1; i >= 0; i--) {
      const particle = this.particles[i];

      // Update lifetime
      particle.life += deltaTime;

      // Remove dead particles
      if (particle.life >= particle.lifetime) {
        this.particles.splice(i, 1);
        continue;
      }

      // Apply gravity
      particle.velocity.x += gravity.x * deltaTime;
      particle.velocity.y += gravity.y * deltaTime;
      particle.velocity.z += gravity.z * deltaTime;

      // Update position
      particle.position.x += particle.velocity.x * deltaTime;
      particle.position.y += particle.velocity.y * deltaTime;
      particle.position.z += particle.velocity.z * deltaTime;
    }

    // Update buffers
    this.updateBuffers();
  }

  updateBuffers() {
    const count = this.particles.length;

    for (let i = 0; i < count; i++) {
      const particle = this.particles[i];
      const i3 = i * 3;

      // Position (relative to emitter)
      this.positions[i3] = particle.position.x;
      this.positions[i3 + 1] = particle.position.y;
      this.positions[i3 + 2] = particle.position.z;

      // Color interpolation based on lifetime
      const t = particle.life / particle.lifetime;
      const color = this.startColor.clone().lerp(this.endColor, t);
      this.colors[i3] = color.r;
      this.colors[i3 + 1] = color.g;
      this.colors[i3 + 2] = color.b;

      // Size
      this.sizes[i] = particle.size;

      // Alpha fade out at end of life
      const fadeStart = 0.7; // Start fading at 70% of lifetime
      this.alphas[i] = t > fadeStart ? (1.0 - (t - fadeStart) / (1.0 - fadeStart)) : 1.0;
    }

    // Clear unused particles
    for (let i = count; i < this.emitter.max_particles; i++) {
      const i3 = i * 3;
      this.positions[i3] = 0;
      this.positions[i3 + 1] = 0;
      this.positions[i3 + 2] = 0;
      this.sizes[i] = 0;
      this.alphas[i] = 0;
    }

    // Mark buffers for update
    this.points.geometry.attributes.position.needsUpdate = true;
    this.points.geometry.attributes.color.needsUpdate = true;
    this.points.geometry.attributes.size.needsUpdate = true;
    this.points.geometry.attributes.alpha.needsUpdate = true;

    // Update draw range to only render active particles
    this.points.geometry.setDrawRange(0, count);
  }

  updateEmitter(emitter) {
    this.emitter = emitter;
    this.startColor = new THREE.Color(emitter.color);
    this.endColor = emitter.color_end && emitter.color_end[0]
      ? new THREE.Color(emitter.color_end[0])
      : this.startColor.clone();

    // Update material size
    this.points.material.size = emitter.size;
  }

  setActive(active) {
    this.active = active;
  }

  dispose() {
    this.points.geometry.dispose();
    this.points.material.dispose();
  }
}

export function createBoxGeometry(width, height, depth) {
  return new THREE.BoxGeometry(width, height, depth);
}

export function createSphereGeometry(radius, widthSegments, heightSegments) {
  return new THREE.SphereGeometry(radius, widthSegments, heightSegments);
}

export function createConeGeometry(radius, height, segments) {
  return new THREE.ConeGeometry(radius, height, segments);
}

export function createPlaneGeometry(width, height) {
  return new THREE.PlaneGeometry(width, height);
}

export function createCircleGeometry(radius, segments) {
  return new THREE.CircleGeometry(radius, segments);
}

export function createCylinderGeometry(radiusTop, radiusBottom, height, radialSegments) {
  return new THREE.CylinderGeometry(radiusTop, radiusBottom, height, radialSegments);
}

export function createTorusGeometry(radius, tube, radialSegments, tubularSegments) {
  return new THREE.TorusGeometry(radius, tube, radialSegments, tubularSegments);
}

export function createTetrahedronGeometry(radius, detail) {
  return new THREE.TetrahedronGeometry(radius, detail);
}

export function createIcosahedronGeometry(radius, detail) {
  return new THREE.IcosahedronGeometry(radius, detail);
}

export function createCustomGeometry(buffer) {
  // buffer is already a Three.js BufferGeometry
  return buffer;
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

// Individual material creation functions (called from Gleam)
export function createBasicMaterial(color, transparent, opacity, map, normalMap) {
  return new THREE.MeshBasicMaterial({
    color,
    transparent,
    opacity,
    map: getTexture(map)
    // Note: BasicMaterial doesn't support normalMap
  });
}

export function createStandardMaterial(color, metalness, roughness, map, normalMap, aoMap, roughnessMap, metalnessMap) {
  const extractedNormalMap = getTexture(normalMap);

  const material = new THREE.MeshStandardMaterial({
    color,
    metalness,
    roughness,
    map: getTexture(map),
    normalMap: extractedNormalMap,
    aoMap: getTexture(aoMap),
    roughnessMap: getTexture(roughnessMap),
    metalnessMap: getTexture(metalnessMap)
  });

  // Set normal map scale to make it more pronounced
  if (extractedNormalMap) {
    material.normalScale.set(1.0, 1.0);
  }

  return material;
}

export function createPhongMaterial(color, shininess, map, normalMap, aoMap) {
  const extractedNormalMap = getTexture(normalMap);
  const material = new THREE.MeshPhongMaterial({
    color,
    shininess,
    map: getTexture(map),
    normalMap: extractedNormalMap,
    aoMap: getTexture(aoMap)
  });

  if (extractedNormalMap) {
    material.normalScale.set(1.0, 1.0);
  }

  return material;
}

export function createLambertMaterial(color, map, normalMap, aoMap) {
  const extractedNormalMap = getTexture(normalMap);
  const material = new THREE.MeshLambertMaterial({
    color,
    map: getTexture(map),
    normalMap: extractedNormalMap,
    aoMap: getTexture(aoMap)
  });

  // Set normal map scale
  if (extractedNormalMap) {
    material.normalScale.set(1.0, 1.0);
  }

  return material;
}

export function createToonMaterial(color, map, normalMap, aoMap) {
  return new THREE.MeshToonMaterial({
    color,
    map: getTexture(map),
    normalMap: getTexture(normalMap),
    aoMap: getTexture(aoMap)
  });
}

export function createLineMaterial(color, linewidth) {
  return new THREE.LineBasicMaterial({
    color,
    linewidth
  });
}

export function createSpriteMaterial(color, transparent, opacity, map, normalMap) {
  return new THREE.SpriteMaterial({
    color,
    transparent,
    opacity,
    map: getTexture(map)
  });
}

// Individual light creation functions (called from Gleam)
export function createAmbientLight(intensity, color) {
  return new THREE.AmbientLight(color, intensity);
}

export function createDirectionalLight(intensity, color) {
  return new THREE.DirectionalLight(color, intensity);
}

export function createPointLight(intensity, color, distance) {
  return new THREE.PointLight(color, intensity, distance);
}

export function createSpotLight(intensity, color, distance, angle, penumbra) {
  return new THREE.SpotLight(color, intensity, distance, angle, penumbra);
}

export function createHemisphereLight(intensity, skyColor, groundColor) {
  return new THREE.HemisphereLight(skyColor, groundColor, intensity);
}


// Apply transform to Three.js object
export function applyTransform(object, transform) {
  object.position.set(transform.position.x, transform.position.y, transform.position.z);
  object.rotation.set(transform.rotation.x, transform.rotation.y, transform.rotation.z);
  object.scale.set(transform.scale.x, transform.scale.y, transform.scale.z);
}

// Apply camera lookAt with proper handling of nested transforms
// The lookAt target is treated as a world-space position
function applyCameraLookAt(camera, lookAtTarget) {
  // Update world matrix to get proper world position
  camera.updateMatrixWorld(true);

  // The lookAt target is always in world space
  const lookAtWorld = new THREE.Vector3(lookAtTarget.x, lookAtTarget.y, lookAtTarget.z);

  // Three.js lookAt expects world coordinates, so we can use it directly
  camera.lookAt(lookAtWorld);
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

  if (animPlayback instanceof OBJECT3D_GLEAM.SingleAnimation) {
    // Single animation
    const animConfig = animPlayback[0]; // Extract Animation from SingleAnimation
    const action = mixer.clipAction(animConfig.clip);
    action.setLoop(animConfig.loop instanceof OBJECT3D_GLEAM.LoopRepeat ? THREE.LoopRepeat : THREE.LoopOnce);
    action.timeScale = animConfig.speed;
    action.weight = animConfig.weight;
    action.play();

    actionCache.set(id, action);
  } else if (animPlayback instanceof OBJECT3D_GLEAM.BlendedAnimations) {
    // Blended animations
    const fromAnim = animPlayback.from;
    const toAnim = animPlayback.to;
    const blendFactor = animPlayback.blend_factor;

    const fromAction = mixer.clipAction(fromAnim.clip);
    fromAction.setLoop(fromAnim.loop instanceof OBJECT3D_GLEAM.LoopRepeat ? THREE.LoopRepeat : THREE.LoopOnce);
    fromAction.timeScale = fromAnim.speed;
    fromAction.weight = (1.0 - blendFactor) * fromAnim.weight;
    fromAction.play();

    const toAction = mixer.clipAction(toAnim.clip);
    toAction.setLoop(toAnim.loop instanceof OBJECT3D_GLEAM.LoopRepeat ? THREE.LoopRepeat : THREE.LoopOnce);
    toAction.timeScale = toAnim.speed;
    toAction.weight = blendFactor * toAnim.weight;
    toAction.play();

    actionCache.set(id, [fromAction, toAction]);
  }
}

// Apply a single patch to the Three.js scene
export function applyPatch(scene, patch, physicsWorld) {
  if (patch instanceof SCENE_GLEAM.AddNode) {
    const { id, node, parent_id } = patch;

    let threeObj;

    if (node instanceof SCENE_GLEAM.Mesh) {
      const geometry = GEOMETRY_GLEAM.create_geometry(node.geometry);
      const material = MATERIAL_GLEAM.create_material(node.material);
      threeObj = new THREE.Mesh(geometry, material);
      applyTransform(threeObj, node.transform);

      // Create physics body if specified
      if (node.physics && node.physics[0] && physicsWorld) {
        PHYSICS.createRigidBody(physicsWorld, id, node.physics[0], node.transform);
      }
    } else if (node instanceof SCENE_GLEAM.InstancedMesh) {
      const geometry = GEOMETRY_GLEAM.create_geometry(node.geometry);
      const material = MATERIAL_GLEAM.create_material(node.material);

      // Convert Gleam list to array to get count
      const instancesArray = Array.from(node.instances);
      const count = instancesArray.length;

      threeObj = new THREE.InstancedMesh(geometry, material, count);

      // Set transform matrix for each instance
      updateInstancedMeshTransforms(threeObj, node.instances);
    } else if (node instanceof SCENE_GLEAM.Light) {
      threeObj = LIGHT_GLEAM.create_light(node.light);
      applyTransform(threeObj, node.transform);
    } else if (node instanceof SCENE_GLEAM.Group) {
      threeObj = new THREE.Group();
      applyTransform(threeObj, node.transform);
    } else if (node instanceof SCENE_GLEAM.LOD) {
      threeObj = new THREE.LOD();
      applyTransform(threeObj, node.transform);

      // Add LOD levels
      for (const level of node.levels) {
        const levelNode = level.node;
        const distance = level.distance;

        // Create Three.js object for this LOD level
        // We need to recursively create the node (but we don't add it to scene)
        let levelObj;

        if (levelNode instanceof SCENE_GLEAM.Mesh) {
          const geometry = GEOMETRY_GLEAM.create_geometry(levelNode.geometry);
          const material = MATERIAL_GLEAM.create_material(levelNode.material);
          levelObj = new THREE.Mesh(geometry, material);
          applyTransform(levelObj, levelNode.transform);
        } else if (levelNode instanceof SCENE_GLEAM.Group) {
          levelObj = new THREE.Group();
          applyTransform(levelObj, levelNode.transform);
          // TODO: Handle children of Group in LOD levels if needed
        } else if (levelNode instanceof SCENE_GLEAM.Model3D) {
          levelObj = levelNode.object.clone();
          applyTransform(levelObj, levelNode.transform);
        }
        // TODO: Add other node types as needed

        if (levelObj) {
          threeObj.addLevel(levelObj, distance);
        }
      }
    } else if (node instanceof SCENE_GLEAM.Model3D) {
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
      if (node.physics && node.physics[0] && physicsWorld) {
        PHYSICS.createRigidBody(physicsWorld, id, node.physics[0], node.transform);
      }
    } else if (node instanceof SCENE_GLEAM.Audio) {
      // Audio nodes don't need a visual Three.js object
      // Just play the audio using the audio system
      // Extract buffer and config from Audio type
      const audioData = node.audio;
      let buffer, config, audioType;

      if (audioData instanceof AUDIO_GLEAM.GlobalAudio) {
        buffer = audioData.buffer;
        config = audioData.config;
        audioType = audioData; // Pass the whole Audio type
      } else if (audioData instanceof AUDIO_GLEAM.PositionalAudio) {
        buffer = audioData.buffer;
        config = audioData.config;
        audioType = audioData; // Pass the whole Audio type
      }

      AUDIO.playAudio(id, buffer, config, audioType);
      // Store a placeholder to track the audio node in cache
      threeObj = new THREE.Group(); // Empty group as placeholder
    } else if (node instanceof SCENE_GLEAM.Camera) {
      // Create Three.js camera from Gleam camera config
      // Pass viewport if specified to calculate correct aspect ratio
      const viewport = node.viewport && node.viewport[0] ? node.viewport[0] : null;
      threeObj = CAMERA.createThreeCamera(node.camera.projection, viewport);

      // Add AudioListener to the camera (required for Three.js audio to work)
      // The listener must be a child of the camera in the scene graph
      const listener = ASSET.getAudioListener();
      if (!threeObj.children.includes(listener)) {
        threeObj.add(listener);
        console.log(`[Tiramisu] Added AudioListener to camera: ${id} (active: ${node.active})`);
      }

      // Apply node transform (position and rotation)
      applyTransform(threeObj, node.transform);

      // Store lookAt target to apply after adding to scene (if provided)
      // (we need to know the parent transform first)
      if (node.look_at && node.look_at[0]) {
        threeObj.userData.lookAtTarget = node.look_at[0];
        threeObj.userData.needsLookAtUpdate = true;
      }

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
        CAMERA.setCamera(threeObj);
      }
    } else if (node instanceof SCENE_GLEAM.DebugBox) {
      threeObj = DEBUG.createDebugBox(node.min, node.max, node.color);
    } else if (node instanceof SCENE_GLEAM.DebugSphere) {
      threeObj = DEBUG.createDebugSphere(node.center, node.radius, node.color);
    } else if (node instanceof SCENE_GLEAM.DebugLine) {
      threeObj = DEBUG.createDebugLine(node.from, node.to, node.color);
    } else if (node instanceof SCENE_GLEAM.DebugAxes) {
      threeObj = DEBUG.createDebugAxes(node.origin, node.size);
    } else if (node instanceof SCENE_GLEAM.DebugGrid) {
      threeObj = DEBUG.createDebugGrid(node.size, node.divisions, node.color);
    } else if (node instanceof SCENE_GLEAM.DebugPoint) {
      threeObj = DEBUG.createDebugPoint(node.position, node.size, node.color);
    } else if (node instanceof SCENE_GLEAM.Particles) {
      // Create particle system
      const particleSystem = new ParticleSystem(node.emitter, node.transform);
      particleSystem.setActive(node.active);
      particleSystemCache.set(id, particleSystem);
      threeObj = particleSystem.points;
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

      // Apply camera lookAt after adding to scene (for nested transforms)
      if (threeObj.userData && threeObj.userData.needsLookAtUpdate) {
        applyCameraLookAt(threeObj, threeObj.userData.lookAtTarget);
        delete threeObj.userData.needsLookAtUpdate;
      }
    }
  } else if (patch instanceof SCENE_GLEAM.RemoveNode) {
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
      AUDIO.stopAudio(id);

      // Remove physics body if exists
      if (physicsWorld) {
        PHYSICS.removeRigidBody(physicsWorld, id);
      }

      // Clean up particle system if exists
      if (particleSystemCache.has(id)) {
        const particleSystem = particleSystemCache.get(id);
        particleSystem.dispose();
        particleSystemCache.delete(id);
      }
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateTransform) {
    const { id, transform } = patch;
    const obj = objectCache.get(id);
    if (obj) {
      applyTransform(obj, transform);
      // Force update world matrix for this object and all children
      // This ensures cameras and other children move with their parents
      obj.updateMatrixWorld(true);

      // If this is a camera with a lookAt target, reapply it after moving
      if ((obj instanceof THREE.PerspectiveCamera || obj instanceof THREE.OrthographicCamera) &&
          obj.userData && obj.userData.lookAtTarget) {
        applyCameraLookAt(obj, obj.userData.lookAtTarget);
      }
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateMaterial) {
    const { id, material } = patch;
    const obj = objectCache.get(id);
    if (obj && obj.material) {
      obj.material.dispose();
      obj.material = MATERIAL_GLEAM.create_material(material);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateGeometry) {
    const { id, geometry } = patch;
    const obj = objectCache.get(id);
    if (obj && obj.geometry) {
      obj.geometry.dispose();
      obj.geometry = GEOMETRY_GLEAM.create_geometry(geometry);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateLight) {
    const { id, light } = patch;
    const oldLight = objectCache.get(id);
    if (oldLight) {
      const newLight = LIGHT_GLEAM.create_light(light);
      newLight.position.copy(oldLight.position);
      newLight.rotation.copy(oldLight.rotation);
      newLight.scale.copy(oldLight.scale);

      scene.remove(oldLight);
      scene.add(newLight);
      objectCache.set(id, newLight);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateAnimation) {
    const { id, animation } = patch;
    const mixer = mixerCache.get(id);

    if (!mixer) {
      console.warn('[Renderer] No mixer found for Model3D:', id);
      return;
    }

    // If animation is Some, setup new animation
    if (animation && animation[0]) {
      setupAnimation(id, mixer, animation[0]);
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
    }
  } else if (patch instanceof SCENE_GLEAM.UpdatePhysics) {
    const { id, physics } = patch;

    if (physicsWorld) {
      // Remove old physics body if it exists
      PHYSICS.removeRigidBody(physicsWorld, id);

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
          PHYSICS.createRigidBody(physicsWorld, id, physics[0], transform);
        }
      }
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateAudio) {
    const { id, audio } = patch;
    // Extract config from Audio type
    const config = audio instanceof AUDIO_GLEAM.GlobalAudio || audio instanceof AUDIO_GLEAM.PositionalAudio
      ? audio.config
      : null;

    if (config) {
      // Update audio configuration
      AUDIO.updateAudioConfig(id, config);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateInstances) {
    const { id, instances } = patch;
    const obj = objectCache.get(id);
    if (obj && obj instanceof THREE.InstancedMesh) {
      updateInstancedMeshTransforms(obj, instances);
    } else {
      console.warn('[Renderer] Object not found or not an InstancedMesh:', id);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateLODLevels) {
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

        if (levelNode instanceof SCENE_GLEAM.Mesh) {
          const geometry = GEOMETRY_GLEAM.create_geometry(levelNode.geometry);
          const material = MATERIAL_GLEAM.create_material(levelNode.material);
          levelObj = new THREE.Mesh(geometry, material);
          applyTransform(levelObj, levelNode.transform);
        } else if (levelNode instanceof SCENE_GLEAM.Group) {
          levelObj = new THREE.Group();
          applyTransform(levelObj, levelNode.transform);
        } else if (levelNode instanceof SCENE_GLEAM.Model3D) {
          levelObj = levelNode.object.clone();
          applyTransform(levelObj, levelNode.transform);
        }

        if (levelObj) {
          obj.addLevel(levelObj, distance);
        }
      }
    } else {
      console.warn('[Renderer] Object not found or not an LOD:', id);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateCamera) {
    const { id, camera_type, look_at } = patch;
    const cameraObj = objectCache.get(id);
    if (cameraObj && (cameraObj instanceof THREE.PerspectiveCamera || cameraObj instanceof THREE.OrthographicCamera)) {
      // Update look_at (handle nested transforms) if provided
      if (look_at && look_at[0]) {
        applyCameraLookAt(cameraObj, look_at[0]);
      }

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

    } else {
      console.warn('[Renderer] Camera not found:', id);
    }
  } else if (patch instanceof SCENE_GLEAM.SetActiveCamera) {
    const id = patch.id;
    const cameraObj = objectCache.get(id);
    if (cameraObj && (cameraObj instanceof THREE.PerspectiveCamera || cameraObj instanceof THREE.OrthographicCamera)) {
      CAMERA.setCamera(cameraObj);
    } else {
      console.warn('[Renderer] Camera not found or not a camera:', id);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateParticleEmitter) {
    const { id, emitter } = patch;
    const particleSystem = particleSystemCache.get(id);
    if (particleSystem) {
      particleSystem.updateEmitter(emitter);
    } else {
      console.warn('[Renderer] Particle system not found:', id);
    }
  } else if (patch instanceof SCENE_GLEAM.UpdateParticleActive) {
    const { id, active } = patch;
    const particleSystem = particleSystemCache.get(id);
    if (particleSystem) {
      particleSystem.setActive(active);
    } else {
      console.warn('[Renderer] Particle system not found:', id);
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
export function applyPatches(scene, patches, physicsWorld) {
  // Convert Gleam list to array for iteration
  for (const patch of patches) {
    applyPatch(scene, patch, physicsWorld);
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

// Update all particle systems
export function updateParticleSystems(deltaTime) {
  if (particleSystemCache.size > 0) {
    particleSystemCache.forEach(particleSystem => {
      particleSystem.update(deltaTime);
    });
  }
}

// Sync physics body transforms to Three.js objects
export function syncPhysicsTransforms(physicsWorld) {
  if (!physicsWorld) return; // No physics world, skip syncing

  objectCache.forEach((obj, id) => {
    // Get the rigid body by typed ID using PhysicsWorld's BiMap
    const body = PHYSICS.getBodyByTypedId(physicsWorld, id);

    if (body) {
      // Get position and rotation directly from physics body
      const translation = body.translation();
      const quaternion = body.rotation();

      // Update position
      obj.position.set(translation.x, translation.y, translation.z);

      // Update rotation using quaternion directly (avoids Euler conversion issues)
      obj.quaternion.set(quaternion.x, quaternion.y, quaternion.z, quaternion.w);

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

  // Clean up particle systems
  particleSystemCache.forEach(particleSystem => {
    particleSystem.dispose();
  });
  particleSystemCache.clear();
}

export function createRenderer(options) {
  const renderer = new THREE.WebGLRenderer({
    antialias: options.antialias,
    alpha: options.alpha,
  });

  // Check if dimensions is None (Gleam's None is an empty object/falsy)
  // or if dimensions[0] exists (Some(Dimensions))
  let width, height, isFullscreen;

  if (options.dimensions && options.dimensions[0]) {
    // Some(Dimensions) - use specified dimensions
    const dims = options.dimensions[0];
    width = dims.width;
    height = dims.height;
    isFullscreen = false;
  } else {
    // None - use fullscreen
    width = window.innerWidth;
    height = window.innerHeight;
    isFullscreen = true;
  }

  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio);

  // Add WebGL context loss/restore handling
  setupContextLossHandling(renderer);

  // If fullscreen mode, add resize listener
  if (isFullscreen) {
    window.addEventListener('resize', () => {
      renderer.setSize(window.innerWidth, window.innerHeight);

      // Update camera aspect ratio if it's a perspective camera
      const camera = CAMERA.getCamera();
      if (camera && camera instanceof THREE.PerspectiveCamera) {
        camera.aspect = window.innerWidth / window.innerHeight;
        camera.updateProjectionMatrix();
      }
    });
  }

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
    // Reinitialize renderer settings
    renderer.setPixelRatio(window.devicePixelRatio);
    // Clear caches - textures and geometries need to be re-uploaded to GPU
    // The scene will be re-rendered, triggering resource re-creation
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
