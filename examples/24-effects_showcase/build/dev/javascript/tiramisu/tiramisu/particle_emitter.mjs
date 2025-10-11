import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import { Ok, Error, CustomType as $CustomType } from "../gleam.mjs";

class ParticleEmitter extends $CustomType {
  constructor(rate, lifetime, velocity, velocity_variance, size, size_variance, color, color_end, gravity_scale, max_particles) {
    super();
    this.rate = rate;
    this.lifetime = lifetime;
    this.velocity = velocity;
    this.velocity_variance = velocity_variance;
    this.size = size;
    this.size_variance = size_variance;
    this.color = color;
    this.color_end = color_end;
    this.gravity_scale = gravity_scale;
    this.max_particles = max_particles;
  }
}

export class NegativeRate extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NegativeLifetime extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NegativeSize extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NegativeSizeVariance extends $CustomType {
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

export class OutOfBoundsColorEnd extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class NegativeMaxParticles extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class ParticleEmitterBuilder extends $CustomType {
  constructor(rate, lifetime, velocity, velocity_variance, size, size_variance, color, color_end, gravity_scale, max_particles) {
    super();
    this.rate = rate;
    this.lifetime = lifetime;
    this.velocity = velocity;
    this.velocity_variance = velocity_variance;
    this.size = size;
    this.size_variance = size_variance;
    this.color = color;
    this.color_end = color_end;
    this.gravity_scale = gravity_scale;
    this.max_particles = max_particles;
  }
}

function particle_emitter(
  rate,
  lifetime,
  velocity,
  velocity_variance,
  size,
  size_variance,
  color,
  color_end,
  gravity_scale,
  max_particles
) {
  return $bool.guard(
    rate <= 0.0,
    new Error(new NegativeRate(rate)),
    () => {
      return $bool.guard(
        lifetime <= 0.0,
        new Error(new NegativeLifetime(lifetime)),
        () => {
          return $bool.guard(
            size <= 0.0,
            new Error(new NegativeSize(size)),
            () => {
              return $bool.guard(
                size_variance < 0.0,
                new Error(new NegativeSizeVariance(size_variance)),
                () => {
                  return $bool.guard(
                    (color < 0x0) || (color > 0xffffff),
                    new Error(new OutOfBoundsColor(color)),
                    () => {
                      return $bool.guard(
                        (() => {
                          if (color_end instanceof $option.Some) {
                            let c = color_end[0];
                            return (c < 0x0) || (c > 0xffffff);
                          } else {
                            return false;
                          }
                        })(),
                        new Error(
                          new OutOfBoundsColorEnd(
                            (() => {
                              if (color_end instanceof $option.Some) {
                                let c = color_end[0];
                                return c;
                              } else {
                                return 0;
                              }
                            })(),
                          ),
                        ),
                        () => {
                          return $bool.guard(
                            max_particles <= 0,
                            new Error(new NegativeMaxParticles(max_particles)),
                            () => {
                              return new Ok(
                                new ParticleEmitter(
                                  rate,
                                  lifetime,
                                  velocity,
                                  velocity_variance,
                                  size,
                                  size_variance,
                                  color,
                                  color_end,
                                  gravity_scale,
                                  max_particles,
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
            },
          );
        },
      );
    },
  );
}

/**
 * Create a new particle emitter builder with default values.
 *
 * Defaults:
 * - rate: 50.0 particles/sec
 * - lifetime: 1.0 seconds
 * - velocity: upward (0, 2, 0)
 * - velocity_variance: (1, 1, 1)
 * - size: 0.1
 * - size_variance: 0.05
 * - color: white (0xffffff)
 * - color_end: None
 * - gravity_scale: 1.0
 * - max_particles: 1000
 */
export function new$() {
  return new ParticleEmitterBuilder(
    50.0,
    1.0,
    new $vec3.Vec3(0.0, 2.0, 0.0),
    new $vec3.Vec3(1.0, 1.0, 1.0),
    0.1,
    0.05,
    0xffffff,
    new $option.None(),
    1.0,
    1000,
  );
}

/**
 * Set the emission rate (particles per second).
 */
export function rate(builder, rate) {
  return new ParticleEmitterBuilder(
    rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set how long particles live (in seconds).
 */
export function lifetime(builder, lifetime) {
  return new ParticleEmitterBuilder(
    builder.rate,
    lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set the base velocity for new particles.
 */
export function velocity(builder, velocity) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set random variance added to velocity (per axis).
 */
export function velocity_variance(builder, variance) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set the base particle size.
 */
export function size(builder, size) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set random variance added to size.
 */
export function size_variance(builder, variance) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set the start color for particles.
 */
export function color(builder, color) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set the end color for particles (for fading effect).
 */
export function fade_to(builder, color) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    new $option.Some(color),
    builder.gravity_scale,
    builder.max_particles,
  );
}

/**
 * Set gravity multiplier (1.0 = normal, 0.0 = no gravity).
 */
export function gravity(builder, scale) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    scale,
    builder.max_particles,
  );
}

/**
 * Set maximum number of active particles.
 */
export function max_particles(builder, max) {
  return new ParticleEmitterBuilder(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    max,
  );
}

/**
 * Build the particle emitter from the builder (validates parameters).
 */
export function build(builder) {
  return particle_emitter(
    builder.rate,
    builder.lifetime,
    builder.velocity,
    builder.velocity_variance,
    builder.size,
    builder.size_variance,
    builder.color,
    builder.color_end,
    builder.gravity_scale,
    builder.max_particles,
  );
}

export function get_emit_rate(emitter) {
  let _pipe = emitter.rate;
  return $float.round(_pipe);
}

export function get_max_particles(emitter) {
  return emitter.max_particles;
}

export function get_velocity(emitter) {
  return emitter.velocity;
}

export function get_velocity_variance(emitter) {
  return emitter.velocity_variance;
}

export function get_size(emitter) {
  return emitter.size;
}

export function get_size_variance(emitter) {
  return emitter.size_variance;
}

export function get_lifetime(emitter) {
  return emitter.lifetime;
}

export function get_lifetime_variance(emitter) {
  return emitter.lifetime * 0.2;
}

export function get_start_color(emitter) {
  return emitter.color;
}

export function get_end_color(emitter) {
  let $ = emitter.color_end;
  if ($ instanceof $option.Some) {
    let c = $[0];
    return c;
  } else {
    return emitter.color;
  }
}

export function get_gravity_scale(emitter) {
  return emitter.gravity_scale;
}
