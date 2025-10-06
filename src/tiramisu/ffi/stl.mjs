import * as THREE from 'three';
import { STLLoader } from 'three/addons/loaders/STLLoader.js';
import { Ok, Error } from '../../gleam.mjs';

// Define error constructors to match Gleam types
class LoadError {
  constructor(message) {
    this.message = message;
  }
}

class InvalidUrl {
  constructor(url) {
    this.url = url;
  }
}

class ParseError {
  constructor(message) {
    this.message = message;
  }
}

// Helper to validate STL file format
function validateSTLFile(data) {
  // Check if it's ASCII STL
  const decoder = new TextDecoder('utf-8');
  const headerText = decoder.decode(data.slice(0, 80));

  if (headerText.toLowerCase().includes('solid')) {
    console.log('[STL Loader] Detected ASCII STL format');
    return { format: 'ascii', valid: true };
  }

  // Check binary STL format
  if (data.byteLength < 84) {
    return { format: 'binary', valid: false, error: 'File too small to be valid binary STL' };
  }

  // Read triangle count from binary header (bytes 80-83)
  const view = new DataView(data);
  const triangleCount = view.getUint32(80, true); // little-endian

  console.log('[STL Loader] Binary STL header - Triangle count:', triangleCount);

  // Sanity check: typical STL files have < 10 million triangles
  if (triangleCount > 100000000 || triangleCount === 0) {
    return {
      format: 'binary',
      valid: false,
      error: `Invalid triangle count: ${triangleCount}. File may be corrupted or not a valid STL.`
    };
  }

  // Check expected file size
  const expectedSize = 84 + (triangleCount * 50);
  if (Math.abs(data.byteLength - expectedSize) > 100) {
    console.warn('[STL Loader] File size mismatch. Expected:', expectedSize, 'Got:', data.byteLength);
  }

  return { format: 'binary', valid: true, triangleCount };
}

// Promise-based STL loading
export function loadSTLAsync(url) {
  return new Promise((resolve) => {
    if (!url || url.trim() === '') {
      resolve(new Error(new InvalidUrl(url)));
      return;
    }

    console.log('[STL Loader] Loading from URL:', url);

    // First, fetch and validate the file
    fetch(url)
      .then(response => {
        if (!response.ok) {
          throw new Error(`HTTP ${response.status}: ${response.statusText}`);
        }
        return response.arrayBuffer();
      })
      .then(arrayBuffer => {
        console.log('[STL Loader] File downloaded, size:', arrayBuffer.byteLength, 'bytes');

        // Validate the file
        const validation = validateSTLFile(arrayBuffer);

        if (!validation.valid) {
          throw new Error(validation.error || 'Invalid STL file');
        }

        console.log('[STL Loader] File validation passed, format:', validation.format);

        // Parse the validated file
        const loader = new STLLoader();
        const geometry = loader.parse(arrayBuffer);

        console.log('[STL Loader] ✅ Successfully parsed geometry');
        console.log('[STL Loader] Vertices:', geometry.attributes.position.count);
        console.log('[STL Loader] Faces:', geometry.attributes.position.count / 3);

        // Center the geometry for better viewing
        geometry.computeBoundingBox();
        const center = geometry.boundingBox.getCenter(new THREE.Vector3());
        geometry.translate(-center.x, -center.y, -center.z);
        console.log('[STL Loader] Centered geometry at origin');

        // Always recompute vertex normals for smooth shading
        // STL files often have flat normals, we want smooth vertex normals
        geometry.deleteAttribute('normal'); // Remove existing normals
        geometry.computeVertexNormals();
        console.log('[STL Loader] Recomputed vertex normals for smooth shading');

        // Log bounding box size for debugging
        const size = new THREE.Vector3();
        geometry.boundingBox.getSize(size);
        console.log('[STL Loader] Model size (X,Y,Z):', size.x.toFixed(2), size.y.toFixed(2), size.z.toFixed(2));

        resolve(new Ok(geometry));
      })
      .catch(error => {
        console.error('[STL Loader] ❌ Error:', error);
        console.error('[STL Loader] Error details:', {
          message: error.message,
          type: error.constructor.name,
          stack: error.stack
        });

        if (error.message && error.message.includes('404')) {
          resolve(new Error(new LoadError('File not found: ' + url)));
        } else if (error.message && (error.message.includes('Invalid') || error.message.includes('corrupted'))) {
          resolve(new Error(new ParseError(error.message)));
        } else {
          resolve(new Error(new LoadError(error.message || 'Failed to load STL')));
        }
      });
  });
}
