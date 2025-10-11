// Effects for game loop integration
import * as THREE from 'three';

// Global reference to the scene (set by game initialization)
let gameScene = null;

/**
 * Set the game scene reference for effects to use
 * @param {THREE.Scene} scene
 */
export function setGameScene(scene) {
  gameScene = scene;
}

/**
 * Request animation frame effect
 * Schedules a message to be dispatched on the next animation frame
 */
export function requestAnimationFrame(msg) {
  return {
    perform: (dispatch) => {
      window.requestAnimationFrame(() => {
        dispatch(msg);
      });
    }
  };
}

/**
 * Set background effect
 * Changes the scene background to a color, texture, or cube texture
 * @param {Object} background - Gleam Background type variant
 */
export function setBackground(background) {
  return {
    perform: (_dispatch) => {
      if (!gameScene) {
        console.error('[Tiramisu] Cannot set background: Scene not initialized');
        return;
      }

      // Handle Gleam ADT variants
      const variantName = background[0];

      switch (variantName) {
        case 'Color': {
          const colorValue = background[1];
          gameScene.background = new THREE.Color(colorValue);
          break;
        }

        case 'Texture': {
          const url = background[1];
          const textureLoader = new THREE.TextureLoader();
          textureLoader.load(
            url,
            (texture) => {
              gameScene.background = texture;
            },
            undefined,
            (error) => {
              console.error(`[Tiramisu] Failed to load background texture: ${url}`, error);
            }
          );
          break;
        }

        case 'CubeTexture': {
          const urls = background[1].toArray();
          if (urls.length !== 6) {
            console.error('[Tiramisu] CubeTexture requires exactly 6 URLs [px, nx, py, ny, pz, nz]');
            break;
          }

          const cubeTextureLoader = new THREE.CubeTextureLoader();
          cubeTextureLoader.load(
            urls,
            (cubeTexture) => {
              gameScene.background = cubeTexture;
            },
            undefined,
            (error) => {
              console.error('[Tiramisu] Failed to load cube texture background', error);
            }
          );
          break;
        }

        default:
          console.error(`[Tiramisu] Unknown background variant: ${variantName}`);
      }
    }
  };
}
