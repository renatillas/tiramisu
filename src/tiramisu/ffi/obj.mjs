import * as THREE from 'three';
import { OBJLoader } from 'three/addons/loaders/OBJLoader.js';
import { MTLLoader } from 'three/addons/loaders/MTLLoader.js';
import * as GLEAM from '../../gleam.mjs';
import * as ASSETS_GLEAM from '../asset.mjs';

// Promise-based OBJ loading with MTL material support
export function loadOBJAsync(objUrl, mtlUrl) {
  return new Promise((resolve) => {
    if (!objUrl || objUrl.trim() === '') {
      resolve(new GLEAM.Error(new ASSETS_GLEAM.InvalidUrl(objUrl)));
      return;
    }

    const objLoader = new OBJLoader();

    // Helper function to load OBJ after materials are ready (or without materials)
    const loadOBJ = (materials) => {
      if (materials) {
        objLoader.setMaterials(materials);
      }

      objLoader.load(
        objUrl,
        // Success callback
        (object) => {
          // Center the model
          const box = new THREE.Box3().setFromObject(object);
          const center = box.getCenter(new THREE.Vector3());
          object.position.sub(center);

          // Ensure all meshes have proper normals
          object.traverse((child) => {
            if (child.isMesh && !child.geometry.attributes.normal) {
              child.geometry.computeVertexNormals();
            }
          });

          resolve(new GLEAM.Ok(object));
        },
        // Progress callback
        undefined,
        // Error callback
        (error) => {
          if (error.message && error.message.includes('404')) {
            resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError('File not found: ' + objUrl)));
          } else if (error.message && (error.message.includes('Invalid') || error.message.includes('parse'))) {
            resolve(new GLEAM.Error(new ASSETS_GLEAM.ParseError(error.message)));
          } else {
            resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError(error.message || 'Failed to load OBJ')));
          }
        }
      );
    };

    // If MTL file is provided, load materials first
    if (mtlUrl && mtlUrl.trim() !== '') {
      const loadingManager = new THREE.LoadingManager();
      const mtlLoader = new MTLLoader(loadingManager);

      // Extract base path for texture loading (assumes textures are in same directory as MTL)
      const basePath = mtlUrl.substring(0, mtlUrl.lastIndexOf('/') + 1);
      mtlLoader.setResourcePath(basePath);

      mtlLoader.load(
        mtlUrl,
        // Success callback
        (materials) => {
          // Preload textures specified in MTL
          materials.preload();

          // MTLLoader doesn't properly load all texture types, so we manually load them
          setTimeout(() => {
            const textureLoader = new THREE.TextureLoader(loadingManager);

            // Helper to extract filename from MTL texture paths
            // MTL files can have options like "map_bump -bm 1 texture.jpg"
            const extractFilename = (path) => {
              if (!path) return null;
              const parts = path.trim().split(/\s+/);
              return parts[parts.length - 1];
            };

            for (let matName in materials.materials) {
              const mat = materials.materials[matName];
              const info = materials.materialsInfo[matName];

              if (info) {
                // Load diffuse/color map (map_Kd)
                if (info.map_kd && !mat.map) {
                  mat.map = textureLoader.load(basePath + extractFilename(info.map_kd));
                }

                // Load normal map (map_bump) - convert to normalMap for better rendering
                if (info.map_bump && !mat.normalMap) {
                  mat.normalMap = textureLoader.load(basePath + extractFilename(info.map_bump));
                }

                // Load ambient occlusion map (map_Ka)
                if (info.map_ka && !mat.aoMap) {
                  mat.aoMap = textureLoader.load(basePath + extractFilename(info.map_ka));
                  mat.aoMapIntensity = 1.0;
                }

                mat.needsUpdate = true;
              }
            }

            // Wait for all textures to load
            const checkTexturesLoaded = () => {
              if (loadingManager.itemsLoaded === loadingManager.itemsTotal) {
                loadOBJ(materials);
              } else {
                setTimeout(checkTexturesLoaded, 100);
              }
            };

            setTimeout(checkTexturesLoaded, 100);
          }, 50);
        },
        // Progress callback
        undefined,
        // Error callback - continue without materials if MTL fails
        (error) => {
          loadOBJ(null);
        }
      );
    } else {
      // No MTL file, load OBJ directly
      loadOBJ(null);
    }
  });
}
