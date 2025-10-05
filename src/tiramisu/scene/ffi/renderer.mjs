import * as THREE from 'three';
import { AddNode, RemoveNode, UpdateTransform, UpdateMaterial, UpdateGeometry, UpdateLight } from '../diff.mjs';
import {
  Mesh, Light, Group,
  BoxGeometry, SphereGeometry, ConeGeometry, PlaneGeometry, CircleGeometry,
  CylinderGeometry, TorusGeometry, TetrahedronGeometry, IcosahedronGeometry,
  BasicMaterial, StandardMaterial, PhongMaterial, LambertMaterial, ToonMaterial,
  LineMaterial, SpriteMaterial,
  AmbientLight, DirectionalLight, PointLight, SpotLight, HemisphereLight
} from '../../scene.mjs';

// Cache of Three.js objects by ID
const objectCache = new Map();

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
    const light = new THREE.SpotLight(
      lightType.color,
      lightType.intensity,
      lightType.distance,
      lightType.angle,
      lightType.penumbra
    );
    return light;
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
    } else if (node instanceof Light) {
      threeObj = createLight(node.light_type);
      applyTransform(threeObj, node.transform);
    } else if (node instanceof Group) {
      threeObj = new THREE.Group();
      applyTransform(threeObj, node.transform);
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
  } else {
    console.warn('Unknown patch type:', patch);
  }
}

// Apply multiple patches
export function applyPatches(scene, patches) {
  // Convert Gleam list to array for iteration
  for (const patch of patches) {
    applyPatch(scene, patch);
  }
}

// Clear all cached objects (useful for cleanup)
export function clearCache() {
  objectCache.forEach(obj => {
    if (obj.geometry) obj.geometry.dispose();
    if (obj.material) obj.material.dispose();
  });
  objectCache.clear();
}
