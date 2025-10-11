import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import { Ok, Error, CustomType as $CustomType } from "../gleam.mjs";
import {
  identity as create_custom_geometry,
  createConeGeometry as create_cone_geometry,
  createCircleGeometry as create_circle_geometry,
  createBoxGeometry as create_box_geometry,
  createCylinderGeometry as create_cylinder_geometry,
  createIcosahedronGeometry as create_icosahedron_geometry,
  createPlaneGeometry as create_plane_geometry,
  createSphereGeometry as create_sphere_geometry,
  createTetrahedronGeometry as create_tetrahedron_geometry,
  createTorusGeometry as create_torus_geometry,
} from "../threejs.ffi.mjs";
import * as $asset from "../tiramisu/asset.mjs";

class BoxGeometry extends $CustomType {
  constructor(width, height, depth) {
    super();
    this.width = width;
    this.height = height;
    this.depth = depth;
  }
}

class SphereGeometry extends $CustomType {
  constructor(radius, width_segments, height_segments) {
    super();
    this.radius = radius;
    this.width_segments = width_segments;
    this.height_segments = height_segments;
  }
}

class ConeGeometry extends $CustomType {
  constructor(radius, height, segments) {
    super();
    this.radius = radius;
    this.height = height;
    this.segments = segments;
  }
}

class PlaneGeometry extends $CustomType {
  constructor(width, height) {
    super();
    this.width = width;
    this.height = height;
  }
}

class CircleGeometry extends $CustomType {
  constructor(radius, segments) {
    super();
    this.radius = radius;
    this.segments = segments;
  }
}

class CylinderGeometry extends $CustomType {
  constructor(radius_top, radius_bottom, height, radial_segments) {
    super();
    this.radius_top = radius_top;
    this.radius_bottom = radius_bottom;
    this.height = height;
    this.radial_segments = radial_segments;
  }
}

class TorusGeometry extends $CustomType {
  constructor(radius, tube, radial_segments, tubular_segments) {
    super();
    this.radius = radius;
    this.tube = tube;
    this.radial_segments = radial_segments;
    this.tubular_segments = tubular_segments;
  }
}

class TetrahedronGeometry extends $CustomType {
  constructor(radius, detail) {
    super();
    this.radius = radius;
    this.detail = detail;
  }
}

class IcosahedronGeometry extends $CustomType {
  constructor(radius, detail) {
    super();
    this.radius = radius;
    this.detail = detail;
  }
}

class CustomGeometry extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NonPositiveWidth extends $CustomType {
  constructor(width) {
    super();
    this.width = width;
  }
}

export class NonPositiveHeight extends $CustomType {
  constructor(height) {
    super();
    this.height = height;
  }
}

export class NonPositiveDepth extends $CustomType {
  constructor(depth) {
    super();
    this.depth = depth;
  }
}

export class NonPositiveRadius extends $CustomType {
  constructor(radius) {
    super();
    this.radius = radius;
  }
}

export class InvalidGeometryTube extends $CustomType {
  constructor(tube) {
    super();
    this.tube = tube;
  }
}

export class LessThanThreeSegmentCountWidth extends $CustomType {
  constructor(count) {
    super();
    this.count = count;
  }
}

export class LessThanTwoSegmentCountHeight extends $CustomType {
  constructor(count) {
    super();
    this.count = count;
  }
}

export class NegativeSegmentCount extends $CustomType {
  constructor(count) {
    super();
    this.count = count;
  }
}

/**
 * Create a validated box geometry.
 *
 * All dimensions must be positive (> 0).
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(cube) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
 * let assert Ok(wall) = scene.box(width: 10.0, height: 3.0, depth: 0.1)
 * ```
 */
export function box(width, height, depth) {
  return $bool.guard(
    width <= 0.0,
    new Error(new NonPositiveWidth(width)),
    () => {
      return $bool.guard(
        height <= 0.0,
        new Error(new NonPositiveHeight(height)),
        () => {
          return $bool.guard(
            depth <= 0.0,
            new Error(new NonPositiveDepth(depth)),
            () => { return new Ok(new BoxGeometry(width, height, depth)); },
          );
        },
      );
    },
  );
}

/**
 * Create a validated sphere geometry.
 *
 * Radius must be positive. Width segments >= 3, height segments >= 2.
 * More segments = smoother sphere but more triangles.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(ball) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 16)
 * let assert Ok(low_poly) = scene.sphere(radius: 1.0, width_segments: 8, height_segments: 6)
 * ```
 */
export function sphere(radius, width_segments, height_segments) {
  return $bool.guard(
    radius <= 0.0,
    new Error(new NonPositiveRadius(radius)),
    () => {
      return $bool.guard(
        width_segments < 3,
        new Error(new LessThanThreeSegmentCountWidth(width_segments)),
        () => {
          return $bool.guard(
            height_segments < 2,
            new Error(new LessThanTwoSegmentCountHeight(height_segments)),
            () => {
              return new Ok(
                new SphereGeometry(radius, width_segments, height_segments),
              );
            },
          );
        },
      );
    },
  );
}

export function cone(radius, height, segments) {
  return $bool.guard(
    radius <= 0.0,
    new Error(new NonPositiveRadius(radius)),
    () => {
      return $bool.guard(
        height <= 0.0,
        new Error(new NonPositiveHeight(height)),
        () => {
          return $bool.guard(
            segments < 3,
            new Error(new NegativeSegmentCount(segments)),
            () => { return new Ok(new ConeGeometry(radius, height, segments)); },
          );
        },
      );
    },
  );
}

export function plane(width, height) {
  return $bool.guard(
    width <= 0.0,
    new Error(new NonPositiveWidth(width)),
    () => {
      return $bool.guard(
        height <= 0.0,
        new Error(new NonPositiveHeight(height)),
        () => { return new Ok(new PlaneGeometry(width, height)); },
      );
    },
  );
}

export function circle(radius, segments) {
  return $bool.guard(
    radius <= 0.0,
    new Error(new NonPositiveRadius(radius)),
    () => {
      return $bool.guard(
        segments < 3,
        new Error(new NegativeSegmentCount(segments)),
        () => { return new Ok(new CircleGeometry(radius, segments)); },
      );
    },
  );
}

export function custom_geometry(geometry) {
  return new CustomGeometry(geometry);
}

/**
 * Create a validated cylinder geometry.
 *
 * Both radii must be non-negative, height positive, radial segments >= 3.
 * Set one radius to 0 to create a cone shape.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(cylinder) = scene.cylinder(radius_top: 1.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)
 * let assert Ok(cone) = scene.cylinder(radius_top: 0.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)
 * ```
 */
export function cylinder(radius_top, radius_bottom, height, radial_segments) {
  return $bool.guard(
    radius_top < 0.0,
    new Error(new NonPositiveRadius(radius_top)),
    () => {
      return $bool.guard(
        radius_bottom < 0.0,
        new Error(new NonPositiveRadius(radius_bottom)),
        () => {
          return $bool.guard(
            height <= 0.0,
            new Error(new NonPositiveHeight(height)),
            () => {
              return $bool.guard(
                radial_segments < 3,
                new Error(new NegativeSegmentCount(radial_segments)),
                () => {
                  return new Ok(
                    new CylinderGeometry(
                      radius_top,
                      radius_bottom,
                      height,
                      radial_segments,
                    ),
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Create a validated torus (donut) geometry.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(donut) = scene.torus(radius: 2.0, tube: 0.5, radial_segments: 16, tubular_segments: 100)
 * ```
 */
export function torus(radius, tube, radial_segments, tubular_segments) {
  return $bool.guard(
    radius <= 0.0,
    new Error(new NonPositiveRadius(radius)),
    () => {
      return $bool.guard(
        tube <= 0.0,
        new Error(new InvalidGeometryTube(tube)),
        () => {
          return $bool.guard(
            radial_segments < 3,
            new Error(new NegativeSegmentCount(radial_segments)),
            () => {
              return $bool.guard(
                tubular_segments < 3,
                new Error(new NegativeSegmentCount(tubular_segments)),
                () => {
                  return new Ok(
                    new TorusGeometry(
                      radius,
                      tube,
                      radial_segments,
                      tubular_segments,
                    ),
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Create a validated tetrahedron (4-sided polyhedron) geometry.
 *
 * Detail level controls subdivision (0 = no subdivision, higher = more triangles).
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(shape) = scene.tetrahedron(radius: 1.0, detail: 0)
 * ```
 */
export function tetrahedron(radius, detail) {
  return $bool.guard(
    radius <= 0.0,
    new Error(new NonPositiveRadius(radius)),
    () => {
      return $bool.guard(
        detail < 0,
        new Error(new NegativeSegmentCount(detail)),
        () => { return new Ok(new TetrahedronGeometry(radius, detail)); },
      );
    },
  );
}

/**
 * Create a validated icosahedron (20-sided polyhedron) geometry.
 *
 * Detail level controls subdivision. Good for creating spheres with flat faces.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(shape) = scene.icosahedron(radius: 1.0, detail: 2)
 * ```
 */
export function icosahedron(radius, detail) {
  return $bool.guard(
    radius <= 0.0,
    new Error(new NonPositiveRadius(radius)),
    () => {
      return $bool.guard(
        detail < 0,
        new Error(new NegativeSegmentCount(detail)),
        () => { return new Ok(new IcosahedronGeometry(radius, detail)); },
      );
    },
  );
}

export function create_geometry(geometry) {
  if (geometry instanceof BoxGeometry) {
    let width = geometry.width;
    let height = geometry.height;
    let depth = geometry.depth;
    return create_box_geometry(width, height, depth);
  } else if (geometry instanceof SphereGeometry) {
    let radius = geometry.radius;
    let width_segments = geometry.width_segments;
    let height_segments = geometry.height_segments;
    return create_sphere_geometry(radius, width_segments, height_segments);
  } else if (geometry instanceof ConeGeometry) {
    let radius = geometry.radius;
    let height = geometry.height;
    let segments = geometry.segments;
    return create_cone_geometry(radius, height, segments);
  } else if (geometry instanceof PlaneGeometry) {
    let width = geometry.width;
    let height = geometry.height;
    return create_plane_geometry(width, height);
  } else if (geometry instanceof CircleGeometry) {
    let radius = geometry.radius;
    let segments = geometry.segments;
    return create_circle_geometry(radius, segments);
  } else if (geometry instanceof CylinderGeometry) {
    let radius_top = geometry.radius_top;
    let radius_bottom = geometry.radius_bottom;
    let height = geometry.height;
    let radial_segments = geometry.radial_segments;
    return create_cylinder_geometry(
      radius_top,
      radius_bottom,
      height,
      radial_segments,
    );
  } else if (geometry instanceof TorusGeometry) {
    let radius = geometry.radius;
    let tube = geometry.tube;
    let radial_segments = geometry.radial_segments;
    let tubular_segments = geometry.tubular_segments;
    return create_torus_geometry(
      radius,
      tube,
      radial_segments,
      tubular_segments,
    );
  } else if (geometry instanceof TetrahedronGeometry) {
    let radius = geometry.radius;
    let detail = geometry.detail;
    return create_tetrahedron_geometry(radius, detail);
  } else if (geometry instanceof IcosahedronGeometry) {
    let radius = geometry.radius;
    let detail = geometry.detail;
    return create_icosahedron_geometry(radius, detail);
  } else {
    let buffer = geometry[0];
    return create_custom_geometry(buffer);
  }
}
