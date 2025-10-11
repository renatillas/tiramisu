import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import { Ok, Error, CustomType as $CustomType } from "../gleam.mjs";
import {
  createAmbientLight as create_ambient_light,
  createDirectionalLight as create_directional_light,
  createPointLight as create_point_light,
  createSpotLight as create_spot_light,
  createHemisphereLight as create_hemisphere_light,
} from "../threejs.ffi.mjs";
import * as $object3d from "../tiramisu/object3d.mjs";

/**
 * Global ambient light (affects all objects equally, no direction).
 */
class Ambient extends $CustomType {
  constructor(intensity, color) {
    super();
    this.intensity = intensity;
    this.color = color;
  }
}

/**
 * Directional light like the sun (parallel rays, infinite distance).
 */
class Directional extends $CustomType {
  constructor(intensity, color, cast_shadow, shadow_resolution, shadow_bias) {
    super();
    this.intensity = intensity;
    this.color = color;
    this.cast_shadow = cast_shadow;
    this.shadow_resolution = shadow_resolution;
    this.shadow_bias = shadow_bias;
  }
}

/**
 * Point light that radiates in all directions (like a light bulb).
 */
class Point extends $CustomType {
  constructor(intensity, color, distance, cast_shadow, shadow_resolution, shadow_bias) {
    super();
    this.intensity = intensity;
    this.color = color;
    this.distance = distance;
    this.cast_shadow = cast_shadow;
    this.shadow_resolution = shadow_resolution;
    this.shadow_bias = shadow_bias;
  }
}

/**
 * Cone-shaped spotlight (like a flashlight or stage light).
 */
class Spot extends $CustomType {
  constructor(intensity, color, distance, angle, penumbra, cast_shadow, shadow_resolution, shadow_bias) {
    super();
    this.intensity = intensity;
    this.color = color;
    this.distance = distance;
    this.angle = angle;
    this.penumbra = penumbra;
    this.cast_shadow = cast_shadow;
    this.shadow_resolution = shadow_resolution;
    this.shadow_bias = shadow_bias;
  }
}

/**
 * Hemisphere light with different colors for sky and ground (outdoor ambient).
 */
class Hemisphere extends $CustomType {
  constructor(intensity, sky_color, ground_color) {
    super();
    this.intensity = intensity;
    this.sky_color = sky_color;
    this.ground_color = ground_color;
  }
}

export class NegativeIntensity extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class OutOfBoundsColor extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NegativeDistance extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InvalidShadowResolution extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InvalidShadowBias extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export function ambient(intensity, color) {
  return $bool.guard(
    intensity < 0.0,
    new Error(new NegativeIntensity(intensity)),
    () => {
      return $bool.guard(
        (color < 0) && (color > 0xffffff),
        new Error(new OutOfBoundsColor(color)),
        () => { return new Ok(new Ambient(intensity, color)); },
      );
    },
  );
}

export function directional(intensity, color) {
  return $bool.guard(
    intensity < 0.0,
    new Error(new NegativeIntensity(intensity)),
    () => {
      return $bool.guard(
        (color < 0) && (color > 0xffffff),
        new Error(new OutOfBoundsColor(color)),
        () => {
          return new Ok(new Directional(intensity, color, false, 1024, 0.0001));
        },
      );
    },
  );
}

export function point(intensity, color, distance) {
  return $bool.guard(
    intensity < 0.0,
    new Error(new NegativeIntensity(intensity)),
    () => {
      return $bool.guard(
        (color < 0) && (color > 0xffffff),
        new Error(new OutOfBoundsColor(color)),
        () => {
          return $bool.guard(
            distance < 0.0,
            new Error(new NegativeDistance(distance)),
            () => {
              return new Ok(
                new Point(intensity, color, distance, false, 1024, 0.0001),
              );
            },
          );
        },
      );
    },
  );
}

export function spot(intensity, color, distance, angle, penumbra) {
  return $bool.guard(
    intensity < 0.0,
    new Error(new NegativeIntensity(intensity)),
    () => {
      return $bool.guard(
        (color < 0) && (color > 0xffffff),
        new Error(new OutOfBoundsColor(color)),
        () => {
          return $bool.guard(
            distance < 0.0,
            new Error(new NegativeDistance(distance)),
            () => {
              return new Ok(
                new Spot(
                  intensity,
                  color,
                  distance,
                  angle,
                  penumbra,
                  false,
                  1024,
                  0.0001,
                ),
              );
            },
          );
        },
      );
    },
  );
}

export function hemisphere(intensity, sky_color, ground_color) {
  return $bool.guard(
    intensity < 0.0,
    new Error(new NegativeIntensity(intensity)),
    () => {
      return $bool.guard(
        (sky_color < 0) && (sky_color > 0xffffff),
        new Error(new OutOfBoundsColor(sky_color)),
        () => {
          return $bool.guard(
            (ground_color < 0) && (ground_color > 0xffffff),
            new Error(new OutOfBoundsColor(sky_color)),
            () => {
              return new Ok(new Hemisphere(intensity, sky_color, ground_color));
            },
          );
        },
      );
    },
  );
}

/**
 * Enable shadow casting for a light.
 *
 * Only directional, point, and spot lights can cast shadows.
 * Ambient and hemisphere lights are ignored.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
 *   |> light.with_shadows(True)
 * ```
 */
export function with_shadows(light, cast_shadow) {
  if (light instanceof Directional) {
    let intensity = light.intensity;
    let color = light.color;
    let shadow_resolution = light.shadow_resolution;
    let shadow_bias = light.shadow_bias;
    return new Directional(
      intensity,
      color,
      cast_shadow,
      shadow_resolution,
      shadow_bias,
    );
  } else if (light instanceof Point) {
    let intensity = light.intensity;
    let color = light.color;
    let distance = light.distance;
    let shadow_resolution = light.shadow_resolution;
    let shadow_bias = light.shadow_bias;
    return new Point(
      intensity,
      color,
      distance,
      cast_shadow,
      shadow_resolution,
      shadow_bias,
    );
  } else if (light instanceof Spot) {
    let intensity = light.intensity;
    let color = light.color;
    let distance = light.distance;
    let angle = light.angle;
    let penumbra = light.penumbra;
    let shadow_resolution = light.shadow_resolution;
    let shadow_bias = light.shadow_bias;
    return new Spot(
      intensity,
      color,
      distance,
      angle,
      penumbra,
      cast_shadow,
      shadow_resolution,
      shadow_bias,
    );
  } else {
    return light;
  }
}

/**
 * Set shadow map resolution (in pixels).
 *
 * Higher values produce sharper shadows but use more memory.
 * Common values: 512, 1024 (default), 2048, 4096.
 * Must be a power of 2.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
 *   |> light.with_shadows(True)
 *   |> light.with_shadow_resolution(2048)
 * ```
 */
export function with_shadow_resolution(light, resolution) {
  return $bool.guard(
    (resolution <= 0) || ((resolution % 2) !== 0),
    new Error(new InvalidShadowResolution(resolution)),
    () => {
      if (light instanceof Directional) {
        let intensity = light.intensity;
        let color = light.color;
        let cast_shadow = light.cast_shadow;
        let shadow_bias = light.shadow_bias;
        return new Ok(
          new Directional(
            intensity,
            color,
            cast_shadow,
            resolution,
            shadow_bias,
          ),
        );
      } else if (light instanceof Point) {
        let intensity = light.intensity;
        let color = light.color;
        let distance = light.distance;
        let cast_shadow = light.cast_shadow;
        let shadow_bias = light.shadow_bias;
        return new Ok(
          new Point(
            intensity,
            color,
            distance,
            cast_shadow,
            resolution,
            shadow_bias,
          ),
        );
      } else if (light instanceof Spot) {
        let intensity = light.intensity;
        let color = light.color;
        let distance = light.distance;
        let angle = light.angle;
        let penumbra = light.penumbra;
        let cast_shadow = light.cast_shadow;
        let shadow_bias = light.shadow_bias;
        return new Ok(
          new Spot(
            intensity,
            color,
            distance,
            angle,
            penumbra,
            cast_shadow,
            resolution,
            shadow_bias,
          ),
        );
      } else {
        return new Ok(light);
      }
    },
  );
}

/**
 * Set shadow bias to reduce shadow acne artifacts.
 *
 * Typical values: 0.00001 to 0.001 (default: 0.0001).
 * Increase if you see shadow artifacts (shadow acne).
 * Decrease if shadows appear detached from objects.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)
 *   |> light.with_shadows(True)
 *   |> light.with_shadow_bias(0.0005)
 * ```
 */
export function with_shadow_bias(light, bias) {
  return $bool.guard(
    bias < 0.0,
    new Error(new InvalidShadowBias(bias)),
    () => {
      if (light instanceof Directional) {
        let intensity = light.intensity;
        let color = light.color;
        let cast_shadow = light.cast_shadow;
        let shadow_resolution = light.shadow_resolution;
        return new Ok(
          new Directional(
            intensity,
            color,
            cast_shadow,
            shadow_resolution,
            bias,
          ),
        );
      } else if (light instanceof Point) {
        let intensity = light.intensity;
        let color = light.color;
        let distance = light.distance;
        let cast_shadow = light.cast_shadow;
        let shadow_resolution = light.shadow_resolution;
        return new Ok(
          new Point(
            intensity,
            color,
            distance,
            cast_shadow,
            shadow_resolution,
            bias,
          ),
        );
      } else if (light instanceof Spot) {
        let intensity = light.intensity;
        let color = light.color;
        let distance = light.distance;
        let angle = light.angle;
        let penumbra = light.penumbra;
        let cast_shadow = light.cast_shadow;
        let shadow_resolution = light.shadow_resolution;
        return new Ok(
          new Spot(
            intensity,
            color,
            distance,
            angle,
            penumbra,
            cast_shadow,
            shadow_resolution,
            bias,
          ),
        );
      } else {
        return new Ok(light);
      }
    },
  );
}

export function create_light(light) {
  if (light instanceof Ambient) {
    let intensity = light.intensity;
    let color = light.color;
    return create_ambient_light(color, intensity);
  } else if (light instanceof Directional) {
    let intensity = light.intensity;
    let color = light.color;
    let cast_shadow = light.cast_shadow;
    let shadow_resolution = light.shadow_resolution;
    let shadow_bias = light.shadow_bias;
    return create_directional_light(
      color,
      intensity,
      cast_shadow,
      shadow_resolution,
      shadow_bias,
    );
  } else if (light instanceof Point) {
    let intensity = light.intensity;
    let color = light.color;
    let distance = light.distance;
    let cast_shadow = light.cast_shadow;
    let shadow_resolution = light.shadow_resolution;
    let shadow_bias = light.shadow_bias;
    return create_point_light(
      color,
      intensity,
      distance,
      cast_shadow,
      shadow_resolution,
      shadow_bias,
    );
  } else if (light instanceof Spot) {
    let intensity = light.intensity;
    let color = light.color;
    let distance = light.distance;
    let angle = light.angle;
    let penumbra = light.penumbra;
    let cast_shadow = light.cast_shadow;
    let shadow_resolution = light.shadow_resolution;
    let shadow_bias = light.shadow_bias;
    return create_spot_light(
      color,
      intensity,
      distance,
      angle,
      penumbra,
      cast_shadow,
      shadow_resolution,
      shadow_bias,
    );
  } else {
    let intensity = light.intensity;
    let sky_color = light.sky_color;
    let ground_color = light.ground_color;
    return create_hemisphere_light(sky_color, ground_color, intensity);
  }
}
