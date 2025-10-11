import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $vec3 from "../../../vec/vec/vec3.mjs";
import { toList, Empty as $Empty, CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import {
  identity as do_state_to_dynamic,
  identity as do_dynamic_to_state,
  createColor as create_color_ffi,
  lerpColor as lerp_color_ffi,
  getColorR as get_color_r_ffi,
  getColorG as get_color_g_ffi,
  getColorB as get_color_b_ffi,
  createFloat32Array as create_float32_array_ffi,
  createBufferGeometry as create_buffer_geometry_ffi,
  setBufferAttribute as set_buffer_attribute_ffi,
  getGeometry as get_geometry_ffi,
  getAttribute as get_attribute_ffi,
  setBufferXYZ as set_buffer_xyz_ffi,
  setBufferX as set_buffer_x_ffi,
  setAttributeNeedsUpdate as set_needs_update_ffi,
  createPointsMaterial as create_points_material_ffi,
  getAdditiveBlending as get_additive_blending_ffi,
  createPoints as create_points_ffi,
  setPosition as set_position_ffi,
} from "../../threejs.ffi.mjs";
import * as $object_cache from "../../tiramisu/internal/object_cache.mjs";
import * as $particle_emitter from "../../tiramisu/particle_emitter.mjs";
import * as $transform from "../../tiramisu/transform.mjs";

export class Particle extends $CustomType {
  constructor(position, velocity, life, lifetime, size) {
    super();
    this.position = position;
    this.velocity = velocity;
    this.life = life;
    this.lifetime = lifetime;
    this.size = size;
  }
}

export class ParticleSystemState extends $CustomType {
  constructor(emitter, transform, active, particles, time_since_last_spawn, points_object, start_color, end_color) {
    super();
    this.emitter = emitter;
    this.transform = transform;
    this.active = active;
    this.particles = particles;
    this.time_since_last_spawn = time_since_last_spawn;
    this.points_object = points_object;
    this.start_color = start_color;
    this.end_color = end_color;
  }
}

/**
 * Initialize a new particle with randomized properties
 * 
 * @ignore
 */
function init_particle(emitter, transform) {
  let pos = transform.position;
  let base_velocity = $particle_emitter.get_velocity(emitter);
  let velocity_variance = $particle_emitter.get_velocity_variance(emitter);
  let base_size = $particle_emitter.get_size(emitter);
  let size_variance = $particle_emitter.get_size_variance(emitter);
  let base_lifetime = $particle_emitter.get_lifetime(emitter);
  let lifetime_variance = $particle_emitter.get_lifetime_variance(emitter);
  let vx = base_velocity.x + ((($float.random() * 2.0) - 1.0) * velocity_variance.x);
  let vy = base_velocity.y + ((($float.random() * 2.0) - 1.0) * velocity_variance.y);
  let vz = base_velocity.z + ((($float.random() * 2.0) - 1.0) * velocity_variance.z);
  let size = base_size + ($float.random() * size_variance);
  let lifetime = base_lifetime + ($float.random() * lifetime_variance);
  return new Particle(pos, new $vec3.Vec3(vx, vy, vz), 0.0, lifetime, size);
}

/**
 * Spawn a new particle
 * 
 * @ignore
 */
function spawn_particle(state) {
  let emitter = state.emitter;
  let max_particles = $particle_emitter.get_max_particles(emitter);
  let particle_count = $list.length(state.particles);
  let $ = particle_count >= max_particles;
  if ($) {
    let $1 = state.particles;
    if ($1 instanceof $Empty) {
      return state;
    } else {
      let rest = $1.tail;
      let new_particle = init_particle(emitter, state.transform);
      return new ParticleSystemState(
        state.emitter,
        state.transform,
        state.active,
        $list.append(rest, toList([new_particle])),
        state.time_since_last_spawn,
        state.points_object,
        state.start_color,
        state.end_color,
      );
    }
  } else {
    let new_particle = init_particle(emitter, state.transform);
    return new ParticleSystemState(
      state.emitter,
      state.transform,
      state.active,
      $list.append(state.particles, toList([new_particle])),
      state.time_since_last_spawn,
      state.points_object,
      state.start_color,
      state.end_color,
    );
  }
}

/**
 * Handle particle spawning based on emit rate
 * 
 * @ignore
 */
function handle_spawning(state, delta_time) {
  let new_time = state.time_since_last_spawn + delta_time;
  let emit_rate = $particle_emitter.get_emit_rate(state.emitter);
  let spawn_interval = divideFloat(1.0, $int.to_float(emit_rate));
  let $ = new_time >= spawn_interval;
  if ($) {
    let spawned_state = spawn_particle(state);
    return new ParticleSystemState(
      spawned_state.emitter,
      spawned_state.transform,
      spawned_state.active,
      spawned_state.particles,
      0.0,
      spawned_state.points_object,
      spawned_state.start_color,
      spawned_state.end_color,
    );
  } else {
    return new ParticleSystemState(
      state.emitter,
      state.transform,
      state.active,
      state.particles,
      new_time,
      state.points_object,
      state.start_color,
      state.end_color,
    );
  }
}

/**
 * Update a single particle
 * 
 * @ignore
 */
function update_single_particle(particle, delta_time, gravity_scale) {
  let new_life = particle.life + delta_time;
  let gravity_force = -9.81 * gravity_scale;
  let new_vy = particle.velocity.y + (gravity_force * delta_time);
  let _block;
  let _record = particle.velocity;
  _block = new $vec3.Vec3(_record.x, new_vy, _record.z);
  let new_velocity = _block;
  let new_x = particle.position.x + (new_velocity.x * delta_time);
  let new_y = particle.position.y + (new_velocity.y * delta_time);
  let new_z = particle.position.z + (new_velocity.z * delta_time);
  let new_position = new $vec3.Vec3(new_x, new_y, new_z);
  return new Particle(
    new_position,
    new_velocity,
    new_life,
    particle.lifetime,
    particle.size,
  );
}

/**
 * Update all particles in the list
 * 
 * @ignore
 */
function update_particle_list(particles, delta_time, gravity_scale) {
  return $list.map(
    particles,
    (particle) => {
      return update_single_particle(particle, delta_time, gravity_scale);
    },
  );
}

/**
 * Remove particles that have exceeded their lifetime
 * 
 * @ignore
 */
function remove_dead_particles(particles) {
  return $list.filter(
    particles,
    (particle) => { return particle.life < particle.lifetime; },
  );
}

/**
 * Set particle system active state
 */
export function set_active(state, active) {
  return new ParticleSystemState(
    state.emitter,
    state.transform,
    active,
    state.particles,
    state.time_since_last_spawn,
    state.points_object,
    state.start_color,
    state.end_color,
  );
}

/**
 * Get the Three.js Points object for adding to scene
 */
export function get_points_object(state) {
  return state.points_object;
}

function state_to_dynamic(state) {
  return do_state_to_dynamic(state);
}

/**
 * Wrap ParticleSystemState as opaque ParticleSystem for cache
 */
export function wrap_as_cache_entry(state) {
  let state_dynamic = state_to_dynamic(state);
  return $object_cache.wrap_particle_system(state_dynamic);
}

function dynamic_to_state(dyn) {
  return do_dynamic_to_state(dyn);
}

/**
 * Unwrap opaque ParticleSystem from cache to ParticleSystemState
 */
export function unwrap_from_cache_entry(system) {
  let state_dynamic = $object_cache.unwrap_particle_system(system);
  return dynamic_to_state(state_dynamic);
}

/**
 * Update particle emitter configuration
 */
export function update_emitter(state, emitter) {
  let start_color = create_color_ffi($particle_emitter.get_start_color(emitter));
  let end_color = create_color_ffi($particle_emitter.get_end_color(emitter));
  return new ParticleSystemState(
    emitter,
    state.transform,
    state.active,
    state.particles,
    state.time_since_last_spawn,
    state.points_object,
    start_color,
    end_color,
  );
}

/**
 * Recursively update particle buffers
 * 
 * @ignore
 */
function update_particle_buffers_recursive(
  loop$particles,
  loop$index,
  loop$position_attr,
  loop$color_attr,
  loop$size_attr,
  loop$alpha_attr,
  loop$start_color,
  loop$end_color
) {
  while (true) {
    let particles = loop$particles;
    let index = loop$index;
    let position_attr = loop$position_attr;
    let color_attr = loop$color_attr;
    let size_attr = loop$size_attr;
    let alpha_attr = loop$alpha_attr;
    let start_color = loop$start_color;
    let end_color = loop$end_color;
    if (particles instanceof $Empty) {
      return undefined;
    } else {
      let particle = particles.head;
      let rest = particles.tail;
      set_buffer_xyz_ffi(
        position_attr,
        index,
        particle.position.x,
        particle.position.y,
        particle.position.z,
      );
      let t = divideFloat(particle.life, particle.lifetime);
      let color = lerp_color_ffi(start_color, end_color, t);
      let r = get_color_r_ffi(color);
      let g = get_color_g_ffi(color);
      let b = get_color_b_ffi(color);
      set_buffer_xyz_ffi(color_attr, index, r, g, b);
      set_buffer_x_ffi(size_attr, index, particle.size);
      let fade_start = 0.7;
      let _block;
      let $ = t > fade_start;
      if ($) {
        _block = 1.0 - (divideFloat((t - fade_start), (1.0 - fade_start)));
      } else {
        _block = 1.0;
      }
      let alpha = _block;
      set_buffer_x_ffi(alpha_attr, index, alpha);
      loop$particles = rest;
      loop$index = index + 1;
      loop$position_attr = position_attr;
      loop$color_attr = color_attr;
      loop$size_attr = size_attr;
      loop$alpha_attr = alpha_attr;
      loop$start_color = start_color;
      loop$end_color = end_color;
    }
  }
}

/**
 * Update Three.js buffers with current particle data
 * 
 * @ignore
 */
function update_buffers(state) {
  let points = state.points_object;
  let geometry = get_geometry_ffi(points);
  let position_attr = get_attribute_ffi(geometry, "position");
  let color_attr = get_attribute_ffi(geometry, "color");
  let size_attr = get_attribute_ffi(geometry, "size");
  let alpha_attr = get_attribute_ffi(geometry, "alpha");
  update_particle_buffers_recursive(
    state.particles,
    0,
    position_attr,
    color_attr,
    size_attr,
    alpha_attr,
    state.start_color,
    state.end_color,
  );
  set_needs_update_ffi(position_attr, true);
  set_needs_update_ffi(color_attr, true);
  set_needs_update_ffi(size_attr, true);
  set_needs_update_ffi(alpha_attr, true);
  return undefined;
}

/**
 * Update all particles in the system
 */
export function update_particles(state, delta_time) {
  let $ = state.active;
  if ($) {
    let state$1 = handle_spawning(state, delta_time);
    let gravity_scale = $particle_emitter.get_gravity_scale(state$1.emitter);
    let updated_particles = update_particle_list(
      state$1.particles,
      delta_time,
      gravity_scale,
    );
    let alive_particles = remove_dead_particles(updated_particles);
    let new_state = new ParticleSystemState(
      state$1.emitter,
      state$1.transform,
      state$1.active,
      alive_particles,
      state$1.time_since_last_spawn,
      state$1.points_object,
      state$1.start_color,
      state$1.end_color,
    );
    update_buffers(new_state);
    return new_state;
  } else {
    return state;
  }
}

/**
 * Create a new particle system
 */
export function create_particle_system(emitter, transform) {
  let max_particles = $particle_emitter.get_max_particles(emitter);
  let start_color_hex = $particle_emitter.get_start_color(emitter);
  let end_color_hex = $particle_emitter.get_end_color(emitter);
  let start_color = create_color_ffi(start_color_hex);
  let end_color = create_color_ffi(end_color_hex);
  let positions_buffer = create_float32_array_ffi(max_particles * 3);
  let colors_buffer = create_float32_array_ffi(max_particles * 3);
  let sizes_buffer = create_float32_array_ffi(max_particles);
  let alphas_buffer = create_float32_array_ffi(max_particles);
  let geometry = create_buffer_geometry_ffi();
  set_buffer_attribute_ffi(geometry, "position", positions_buffer, 3);
  set_buffer_attribute_ffi(geometry, "color", colors_buffer, 3);
  set_buffer_attribute_ffi(geometry, "size", sizes_buffer, 1);
  set_buffer_attribute_ffi(geometry, "alpha", alphas_buffer, 1);
  let base_size = $particle_emitter.get_size(emitter);
  let material = create_points_material_ffi(
    base_size,
    true,
    true,
    1.0,
    false,
    get_additive_blending_ffi(),
    true,
  );
  let points = create_points_ffi(geometry, material);
  let pos = transform.position;
  set_position_ffi(points, pos.x, pos.y, pos.z);
  return new ParticleSystemState(
    emitter,
    transform,
    true,
    toList([]),
    0.0,
    points,
    start_color,
    end_color,
  );
}
