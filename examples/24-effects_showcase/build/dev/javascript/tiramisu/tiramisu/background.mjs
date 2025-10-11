import { CustomType as $CustomType } from "../gleam.mjs";

/**
 * Solid color background (hex color, e.g., 0x111111)
 */
export class Color extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * 2D texture background loaded from URL or path
 */
export class Texture extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Cube texture (skybox) with 6 face images [px, nx, py, ny, pz, nz]
 */
export class CubeTexture extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
