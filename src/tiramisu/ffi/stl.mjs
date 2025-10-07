import * as THREE from 'three';
import { STLLoader } from 'three/addons/loaders/STLLoader.js';
import * as GLEAM from '../../gleam.mjs';
import * as ASSETS_GLEAM from '../asset.mjs';

// Helper to validate STL file format
function validateSTLFile(data) {
  // Check if it's ASCII STL
  const decoder = new TextDecoder('utf-8');
  const headerText = decoder.decode(data.slice(0, 80));

  if (headerText.toLowerCase().includes('solid')) {
    console.log('[STL Loader] Detected ASCII STL format');
    return { format: 'ascii', valid: true };
  }

  if (data.byteLength < 84) {
    return { format: 'binary', valid: false, error: 'File too small to be valid binary STL' };
  }

  const view = new DataView(data);
  const triangleCount = view.getUint32(80, true); // little-endian


  if (triangleCount > 100000000 || triangleCount === 0) {
    return {
      format: 'binary',
      valid: false,
      error: `Invalid triangle count: ${triangleCount}. File may be corrupted or not a valid STL.`
    };
  }

  return { format: 'binary', valid: true, triangleCount };
}

// Promise-based STL loading
export function loadSTLAsync(url) {
  return new Promise((resolve) => {
    if (!url || url.trim() === '') {
      resolve(new GLEAM.Error(new ASSETS_GLEAM.InvalidUrl(url)));
      return;
    }

    fetch(url)
      .then(response => {
        if (!response.ok) {
          throw new Error(`HTTP ${response.status}: ${response.statusText}`);
        }
        return response.arrayBuffer();
      })
      .then(arrayBuffer => {

        // Validate the file
        const validation = validateSTLFile(arrayBuffer);

        if (!validation.valid) {
          throw new Error(validation.error || 'Invalid STL file');
        }


        // Parse the validated file
        const loader = new STLLoader();
        const geometry = loader.parse(arrayBuffer);

        // Center the geometry for better viewing
        geometry.computeBoundingBox();
        const center = geometry.boundingBox.getCenter(new THREE.Vector3());
        geometry.translate(-center.x, -center.y, -center.z);

        // Always recompute vertex normals for smooth shading
        // STL files often have flat normals, we want smooth vertex normals
        geometry.deleteAttribute('normal'); // Remove existing normals
        geometry.computeVertexNormals();

        // Log bounding box size for debugging
        const size = new THREE.Vector3();
        geometry.boundingBox.getSize(size);

        resolve(new GLEAM.Ok(geometry));
      })
      .catch(error => {

        if (error.message && error.message.includes('404')) {
          resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError('File not found: ' + url)));
        } else if (error.message && (error.message.includes('Invalid') || error.message.includes('corrupted'))) {
          resolve(new GLEAM.Error(new ASSETS_GLEAM.ParseError(error.message)));
        } else {
          resolve(new GLEAM.Error(new ASSETS_GLEAM.LoadError(error.message || 'Failed to load STL')));
        }
      });
  });
}
