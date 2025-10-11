import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { Ok, Error, CustomType as $CustomType } from "../gleam.mjs";
import {
  createBasicMaterial as create_basic_material,
  createStandardMaterial as create_standard_material,
  createPhongMaterial as create_phong_material,
  createLambertMaterial as create_lambert_material,
  createToonMaterial as create_toon_material,
  createLineMaterial as create_line_material,
  createSpriteMaterial as create_sprite_material,
} from "../threejs.ffi.mjs";
import * as $asset from "../tiramisu/asset.mjs";

/**
 * Unlit material (no lighting calculations). Fast and useful for flat-shaded objects.
 */
class BasicMaterial extends $CustomType {
  constructor(color, map, transparent, opacity) {
    super();
    this.color = color;
    this.map = map;
    this.transparent = transparent;
    this.opacity = opacity;
  }
}

/**
 * Physically-based material with metalness/roughness workflow. Most realistic.
 */
class StandardMaterial extends $CustomType {
  constructor(color, map, normal_map, ambient_oclusion_map, roughness_map, metalness_map, metalness, roughness, transparent, opacity) {
    super();
    this.color = color;
    this.map = map;
    this.normal_map = normal_map;
    this.ambient_oclusion_map = ambient_oclusion_map;
    this.roughness_map = roughness_map;
    this.metalness_map = metalness_map;
    this.metalness = metalness;
    this.roughness = roughness;
    this.transparent = transparent;
    this.opacity = opacity;
  }
}

/**
 * Shiny material with specular highlights (like plastic or ceramic).
 */
class PhongMaterial extends $CustomType {
  constructor(color, map, normal_map, ambient_oclusion_map, shininess) {
    super();
    this.color = color;
    this.map = map;
    this.normal_map = normal_map;
    this.ambient_oclusion_map = ambient_oclusion_map;
    this.shininess = shininess;
  }
}

/**
 * Matte material (like cloth or wood). Non-shiny diffuse lighting.
 */
class LambertMaterial extends $CustomType {
  constructor(color, map, normal_map, ambient_oclusion_map) {
    super();
    this.color = color;
    this.map = map;
    this.normal_map = normal_map;
    this.ambient_oclusion_map = ambient_oclusion_map;
  }
}

/**
 * Cartoon-style material with banded shading.
 */
class ToonMaterial extends $CustomType {
  constructor(color, map, normal_map, ambient_oclusion_map) {
    super();
    this.color = color;
    this.map = map;
    this.normal_map = normal_map;
    this.ambient_oclusion_map = ambient_oclusion_map;
  }
}

/**
 * Material for rendering lines.
 */
class LineMaterial extends $CustomType {
  constructor(color, linewidth) {
    super();
    this.color = color;
    this.linewidth = linewidth;
  }
}

/**
 * Material for 2D sprites that always face the camera.
 */
class SpriteMaterial extends $CustomType {
  constructor(color, map, transparent, opacity) {
    super();
    this.color = color;
    this.map = map;
    this.transparent = transparent;
    this.opacity = opacity;
  }
}

export class OutOfBoundsColor extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class OutOfBoundsOpacity extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class OutOfBoundsRoughness extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class OutOfBoundsMetalness extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NonPositiveLinewidth extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NonPositiveShininess extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class StandardMaterialBuilder extends $CustomType {
  constructor(color, metalness, roughness, transparent, opacity, map, normal_map, ambient_oclusion_map, roughness_map, metalness_map) {
    super();
    this.color = color;
    this.metalness = metalness;
    this.roughness = roughness;
    this.transparent = transparent;
    this.opacity = opacity;
    this.map = map;
    this.normal_map = normal_map;
    this.ambient_oclusion_map = ambient_oclusion_map;
    this.roughness_map = roughness_map;
    this.metalness_map = metalness_map;
  }
}

/**
 * Create a validated basic (unlit) material.
 *
 * Basic materials don't react to lights, making them very fast to render.
 * Opacity must be between 0.0 (fully transparent) and 1.0 (fully opaque).
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(red) = material.basic(color: 0xff0000, transparent: False, opacity: 1.0, map: option.None)
 * let assert Ok(glass) = material.basic(color: 0x88ccff, transparent: True, opacity: 0.5, map: option.None)
 * ```
 */
export function basic(color, transparent, opacity, map) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return $bool.guard(
        (opacity < 0.0) || (opacity > 1.0),
        new Error(new OutOfBoundsOpacity(opacity)),
        () => {
          return new Ok(new BasicMaterial(color, map, transparent, opacity));
        },
      );
    },
  );
}

/**
 * Create a validated physically-based (PBR) standard material.
 *
 * Standard materials use metalness/roughness workflow for realistic rendering.
 * - Metalness: 0.0 = dielectric (plastic, wood), 1.0 = metal
 * - Roughness: 0.0 = mirror-smooth, 1.0 = completely rough
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(gold) = scene.standard_material(color: 0xffd700, metalness: 1.0, roughness: 0.3, transparent: False, opacity: 1.0, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
 * let assert Ok(plastic) = scene.standard_material(color: 0xff0000, metalness: 0.0, roughness: 0.5, transparent: False, opacity: 1.0, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
 * ```
 */
export function standard(
  color,
  metalness,
  roughness,
  transparent,
  opacity,
  map,
  normal_map,
  ambient_oclusion_map,
  roughness_map,
  metalness_map
) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return $bool.guard(
        (metalness < 0.0) || (metalness > 1.0),
        new Error(new OutOfBoundsMetalness(metalness)),
        () => {
          return $bool.guard(
            (roughness < 0.0) || (roughness > 1.0),
            new Error(new OutOfBoundsRoughness(roughness)),
            () => {
              return $bool.guard(
                (opacity < 0.0) || (opacity > 1.0),
                new Error(new OutOfBoundsOpacity(opacity)),
                () => {
                  return new Ok(
                    new StandardMaterial(
                      color,
                      map,
                      normal_map,
                      ambient_oclusion_map,
                      roughness_map,
                      metalness_map,
                      metalness,
                      roughness,
                      transparent,
                      opacity,
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
 * Create a validated line material for rendering lines.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(line_mat) = scene.line_material(color: 0xff0000, linewidth: 2.0)
 * ```
 */
export function line(color, linewidth) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return $bool.guard(
        linewidth <= 0.0,
        new Error(new NonPositiveLinewidth(linewidth)),
        () => { return new Ok(new LineMaterial(color, linewidth)); },
      );
    },
  );
}

/**
 * Create a validated sprite material for 2D billboards.
 *
 * Sprites always face the camera and are useful for particles, UI elements, etc.
 *
 * ## Example
 *
 * ```gleam
 * let assert Ok(sprite_mat) = material.sprite(color: 0xffffff, transparent: True, opacity: 0.8, map: option.None)
 * ```
 */
export function sprite(color, transparent, opacity, map) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return $bool.guard(
        (opacity < 0.0) || (opacity > 1.0),
        new Error(new OutOfBoundsOpacity(opacity)),
        () => {
          return new Ok(new SpriteMaterial(color, map, transparent, opacity));
        },
      );
    },
  );
}

export function lambert(color, map, normal_map, ambient_oclusion_map) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return new Ok(
        new LambertMaterial(color, map, normal_map, ambient_oclusion_map),
      );
    },
  );
}

export function phong(color, shininess, map, normal_map, ambient_oclusion_map) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return $bool.guard(
        shininess < 0.0,
        new Error(new NonPositiveShininess(shininess)),
        () => {
          return new Ok(
            new PhongMaterial(
              color,
              map,
              normal_map,
              ambient_oclusion_map,
              shininess,
            ),
          );
        },
      );
    },
  );
}

export function toon(color, map, normal_map, ambient_oclusion_map) {
  return $bool.guard(
    (color < 0x0) || (color > 0xffffff),
    new Error(new OutOfBoundsColor(color)),
    () => {
      return new Ok(
        new ToonMaterial(color, map, normal_map, ambient_oclusion_map),
      );
    },
  );
}

export function new$() {
  return new StandardMaterialBuilder(
    0x808080,
    0.5,
    0.5,
    false,
    1.0,
    new $option.None(),
    new $option.None(),
    new $option.None(),
    new $option.None(),
    new $option.None(),
  );
}

export function with_color(builder, color) {
  return new StandardMaterialBuilder(
    color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_metalness(builder, metalness) {
  return new StandardMaterialBuilder(
    builder.color,
    metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_roughness(builder, roughness) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_color_map(builder, map) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    new $option.Some(map),
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_normal_map(builder, normal_map) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    new $option.Some(normal_map),
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_ambient_oclusion_map(builder, ambient_oclusion_map) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    new $option.Some(ambient_oclusion_map),
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_roughness_map(builder, roughness_map) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    new $option.Some(roughness_map),
    builder.metalness_map,
  );
}

export function with_metalness_map(builder, metalness_map) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    new $option.Some(metalness_map),
  );
}

export function with_transparent(builder, transparent) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function with_opacity(builder, opacity) {
  return new StandardMaterialBuilder(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function build(builder) {
  return standard(
    builder.color,
    builder.metalness,
    builder.roughness,
    builder.transparent,
    builder.opacity,
    builder.map,
    builder.normal_map,
    builder.ambient_oclusion_map,
    builder.roughness_map,
    builder.metalness_map,
  );
}

export function create_material(material) {
  if (material instanceof BasicMaterial) {
    let color = material.color;
    let map = material.map;
    let transparent = material.transparent;
    let opacity = material.opacity;
    return create_basic_material(color, transparent, opacity, map);
  } else if (material instanceof StandardMaterial) {
    let color = material.color;
    let map = material.map;
    let normal_map = material.normal_map;
    let ambient_oclusion_map = material.ambient_oclusion_map;
    let roughness_map = material.roughness_map;
    let metalness_map = material.metalness_map;
    let metalness = material.metalness;
    let roughness = material.roughness;
    let transparent = material.transparent;
    let opacity = material.opacity;
    return create_standard_material(
      color,
      metalness,
      roughness,
      transparent,
      opacity,
      map,
      normal_map,
      ambient_oclusion_map,
      roughness_map,
      metalness_map,
    );
  } else if (material instanceof PhongMaterial) {
    let color = material.color;
    let map = material.map;
    let normal_map = material.normal_map;
    let ambient_oclusion_map = material.ambient_oclusion_map;
    let shininess = material.shininess;
    return create_phong_material(
      color,
      shininess,
      map,
      normal_map,
      ambient_oclusion_map,
    );
  } else if (material instanceof LambertMaterial) {
    let color = material.color;
    let map = material.map;
    let normal_map = material.normal_map;
    let ambient_oclusion_map = material.ambient_oclusion_map;
    return create_lambert_material(color, map, normal_map, ambient_oclusion_map);
  } else if (material instanceof ToonMaterial) {
    let color = material.color;
    let map = material.map;
    let normal_map = material.normal_map;
    let ambient_oclusion_map = material.ambient_oclusion_map;
    return create_toon_material(color, map, normal_map, ambient_oclusion_map);
  } else if (material instanceof LineMaterial) {
    let color = material.color;
    let linewidth = material.linewidth;
    return create_line_material(color, linewidth);
  } else {
    let color = material.color;
    let map = material.map;
    let transparent = material.transparent;
    let opacity = material.opacity;
    return create_sprite_material(color, transparent, opacity, map);
  }
}
