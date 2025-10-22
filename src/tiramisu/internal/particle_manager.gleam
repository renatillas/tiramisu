/// Internal particle system management
///
/// This module manages particle systems, including spawning, updating,
/// and lifecycle management. All particle simulation logic lives in Gleam,
/// with only buffer updates calling into Three.js FFI.
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/list
import tiramisu/internal/object_cache.{type ParticleSystem}
import tiramisu/particle_emitter.{type ParticleEmitter}
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}

/// Individual particle data
pub type Particle {
  Particle(
    position: Vec3(Float),
    velocity: Vec3(Float),
    life: Float,
    lifetime: Float,
    size: Float,
  )
}

/// Internal particle system state
pub type ParticleSystemState {
  ParticleSystemState(
    emitter: ParticleEmitter,
    transform: Transform,
    active: Bool,
    particles: List(Particle),
    time_since_last_spawn: Float,
    // Three.js objects (opaque)
    points_object: Dynamic,
    start_color: Dynamic,
    end_color: Dynamic,
  )
}

// ============================================================================
// CREATION
// ============================================================================

/// Create a new particle system
pub fn create_particle_system(
  emitter: ParticleEmitter,
  transform: Transform,
) -> ParticleSystemState {
  let max_particles = particle_emitter.get_max_particles(emitter)
  let start_color_hex = particle_emitter.get_start_color(emitter)
  let end_color_hex = particle_emitter.get_end_color(emitter)

  // Create Three.js Color objects
  let start_color = create_color_ffi(start_color_hex)
  let end_color = create_color_ffi(end_color_hex)

  // Pre-allocate buffers for all particles
  let positions_buffer = create_float32_array_ffi(max_particles * 3)
  let colors_buffer = create_float32_array_ffi(max_particles * 3)
  let sizes_buffer = create_float32_array_ffi(max_particles)
  let alphas_buffer = create_float32_array_ffi(max_particles)

  // Create Three.js BufferGeometry
  let geometry = create_buffer_geometry_ffi()
  set_buffer_attribute_ffi(geometry, "position", positions_buffer, 3)
  set_buffer_attribute_ffi(geometry, "color", colors_buffer, 3)
  set_buffer_attribute_ffi(geometry, "size", sizes_buffer, 1)
  set_buffer_attribute_ffi(geometry, "alpha", alphas_buffer, 1)

  // Create Three.js PointsMaterial with proper configuration
  let base_size = particle_emitter.get_size(emitter)
  let material =
    create_points_material_ffi(
      base_size,
      // size
      True,
      // vertexColors - use per-particle colors
      True,
      // transparent - enable alpha blending
      1.0,
      // opacity
      False,
      // depthWrite - particles shouldn't write depth
      get_additive_blending_ffi(),
      // blending - additive for glow effect
      True,
      // sizeAttenuation - particles get smaller with distance
    )

  // Create Three.js Points object
  let points = create_points_ffi(geometry, material)

  // Apply transform
  let pos = transform.position(transform)
  set_position_ffi(points, pos.x, pos.y, pos.z)

  ParticleSystemState(
    emitter: emitter,
    transform: transform,
    active: True,
    particles: [],
    time_since_last_spawn: 0.0,
    points_object: points,
    start_color: start_color,
    end_color: end_color,
  )
}

// ============================================================================
// SPAWNING
// ============================================================================

/// Spawn a new particle
fn spawn_particle(state: ParticleSystemState) -> ParticleSystemState {
  let emitter = state.emitter
  let max_particles = particle_emitter.get_max_particles(emitter)

  // Check if we're at capacity
  let particle_count = list.length(state.particles)
  case particle_count >= max_particles {
    True -> {
      // Recycle oldest particle (remove first, add new at end)
      case state.particles {
        [] -> state
        [_oldest, ..rest] -> {
          let new_particle = init_particle(emitter, state.transform)
          ParticleSystemState(
            ..state,
            particles: list.append(rest, [
              new_particle,
            ]),
          )
        }
      }
    }
    False -> {
      // Add new particle
      let new_particle = init_particle(emitter, state.transform)
      ParticleSystemState(
        ..state,
        particles: list.append(state.particles, [new_particle]),
      )
    }
  }
}

/// Initialize a new particle with randomized properties
fn init_particle(emitter: ParticleEmitter, transform: Transform) -> Particle {
  // Get emitter position from transform
  let pos = transform.position(transform)

  // Get emitter properties
  let base_velocity = particle_emitter.get_velocity(emitter)
  let velocity_variance = particle_emitter.get_velocity_variance(emitter)
  let base_size = particle_emitter.get_size(emitter)
  let size_variance = particle_emitter.get_size_variance(emitter)
  let base_lifetime = particle_emitter.get_lifetime(emitter)
  let lifetime_variance = particle_emitter.get_lifetime_variance(emitter)

  // Randomize velocity with variance
  let vx =
    base_velocity.x +. { float.random() *. 2.0 -. 1.0 } *. velocity_variance.x
  let vy =
    base_velocity.y +. { float.random() *. 2.0 -. 1.0 } *. velocity_variance.y
  let vz =
    base_velocity.z +. { float.random() *. 2.0 -. 1.0 } *. velocity_variance.z

  // Randomize size
  let size = base_size +. float.random() *. size_variance

  // Randomize lifetime
  let lifetime = base_lifetime +. float.random() *. lifetime_variance

  Particle(
    position: pos,
    velocity: vec3.Vec3(x: vx, y: vy, z: vz),
    life: 0.0,
    lifetime: lifetime,
    size: size,
  )
}

// ============================================================================
// UPDATE
// ============================================================================

/// Update all particles in the system
///
/// **Note**: delta_time should be in milliseconds (e.g., 16.0 for 60 FPS).
/// The function converts it to seconds internally for physics calculations.
pub fn update_particles(
  state: ParticleSystemState,
  delta_time: Float,
) -> ParticleSystemState {
  // Convert delta_time from milliseconds to seconds for physics calculations
  // (spawn rates, lifetimes, velocities are all specified in seconds)
  let delta_seconds = delta_time /. 1000.0

  case state.active {
    False -> state
    True -> {
      // Spawn new particles if needed
      let state = handle_spawning(state, delta_seconds)

      // Update existing particles
      let gravity_scale = particle_emitter.get_gravity_scale(state.emitter)
      let updated_particles =
        update_particle_list(state.particles, delta_seconds, gravity_scale)

      // Remove dead particles
      let alive_particles = remove_dead_particles(updated_particles)

      let new_state = ParticleSystemState(..state, particles: alive_particles)

      // Update Three.js buffers
      update_buffers(new_state)

      new_state
    }
  }
}

/// Handle particle spawning based on emit rate
fn handle_spawning(
  state: ParticleSystemState,
  delta_time: Float,
) -> ParticleSystemState {
  let new_time = state.time_since_last_spawn +. delta_time
  let emit_rate = particle_emitter.get_emit_rate(state.emitter)
  let spawn_interval = 1.0 /. int.to_float(emit_rate)

  case new_time >=. spawn_interval {
    True -> {
      // Spawn particle and reset timer
      let spawned_state = spawn_particle(state)
      ParticleSystemState(..spawned_state, time_since_last_spawn: 0.0)
    }
    False -> {
      // Update timer
      ParticleSystemState(..state, time_since_last_spawn: new_time)
    }
  }
}

/// Update all particles in the list
fn update_particle_list(
  particles: List(Particle),
  delta_time: Float,
  gravity_scale: Float,
) -> List(Particle) {
  list.map(particles, fn(particle) {
    update_single_particle(particle, delta_time, gravity_scale)
  })
}

/// Update a single particle
fn update_single_particle(
  particle: Particle,
  delta_time: Float,
  gravity_scale: Float,
) -> Particle {
  // Update lifetime
  let new_life = particle.life +. delta_time

  // Apply gravity to velocity (gravity is -9.81 m/s²)
  let gravity_force = -9.81 *. gravity_scale
  let new_vy = particle.velocity.y +. gravity_force *. delta_time

  let new_velocity = vec3.Vec3(..particle.velocity, y: new_vy)

  // Update position
  let new_x = particle.position.x +. new_velocity.x *. delta_time
  let new_y = particle.position.y +. new_velocity.y *. delta_time
  let new_z = particle.position.z +. new_velocity.z *. delta_time

  let new_position = vec3.Vec3(x: new_x, y: new_y, z: new_z)

  Particle(
    ..particle,
    position: new_position,
    velocity: new_velocity,
    life: new_life,
  )
}

/// Remove particles that have exceeded their lifetime
fn remove_dead_particles(particles: List(Particle)) -> List(Particle) {
  list.filter(particles, fn(particle) { particle.life <. particle.lifetime })
}

// ============================================================================
// BUFFER UPDATES
// ============================================================================

/// Update Three.js buffers with current particle data
fn update_buffers(state: ParticleSystemState) -> Nil {
  let points = state.points_object

  // Get geometry and attributes
  let geometry = get_geometry_ffi(points)
  let position_attr = get_attribute_ffi(geometry, "position")
  let color_attr = get_attribute_ffi(geometry, "color")
  let size_attr = get_attribute_ffi(geometry, "size")
  let alpha_attr = get_attribute_ffi(geometry, "alpha")

  // Update each particle's buffer data
  update_particle_buffers_recursive(
    state.particles,
    0,
    position_attr,
    color_attr,
    size_attr,
    alpha_attr,
    state.start_color,
    state.end_color,
  )

  // Mark attributes as needing update
  set_needs_update_ffi(position_attr, True)
  set_needs_update_ffi(color_attr, True)
  set_needs_update_ffi(size_attr, True)
  set_needs_update_ffi(alpha_attr, True)

  Nil
}

/// Recursively update particle buffers
fn update_particle_buffers_recursive(
  particles: List(Particle),
  index: Int,
  position_attr: Dynamic,
  color_attr: Dynamic,
  size_attr: Dynamic,
  alpha_attr: Dynamic,
  start_color: Dynamic,
  end_color: Dynamic,
) -> Nil {
  case particles {
    [] -> Nil
    [particle, ..rest] -> {
      // Update position buffer
      set_buffer_xyz_ffi(
        position_attr,
        index,
        particle.position.x,
        particle.position.y,
        particle.position.z,
      )

      // Calculate color interpolation (0.0 = start, 1.0 = end)
      let t = particle.life /. particle.lifetime

      // Interpolate color
      let color = lerp_color_ffi(start_color, end_color, t)
      let r = get_color_r_ffi(color)
      let g = get_color_g_ffi(color)
      let b = get_color_b_ffi(color)

      set_buffer_xyz_ffi(color_attr, index, r, g, b)

      // Update size buffer
      set_buffer_x_ffi(size_attr, index, particle.size)

      // Calculate alpha (fade out in last 30% of lifetime)
      let fade_start = 0.7
      let alpha = case t >. fade_start {
        True -> 1.0 -. { t -. fade_start } /. { 1.0 -. fade_start }
        False -> 1.0
      }

      set_buffer_x_ffi(alpha_attr, index, alpha)

      // Continue with next particle
      update_particle_buffers_recursive(
        rest,
        index + 1,
        position_attr,
        color_attr,
        size_attr,
        alpha_attr,
        start_color,
        end_color,
      )
    }
  }
}

// ============================================================================
// STATE UPDATES
// ============================================================================

/// Set particle system active state
pub fn set_active(
  state: ParticleSystemState,
  active: Bool,
) -> ParticleSystemState {
  ParticleSystemState(..state, active: active)
}

/// Update particle emitter configuration
pub fn update_emitter(
  state: ParticleSystemState,
  emitter: ParticleEmitter,
) -> ParticleSystemState {
  // Update colors if they changed
  let start_color = create_color_ffi(particle_emitter.get_start_color(emitter))
  let end_color = create_color_ffi(particle_emitter.get_end_color(emitter))

  ParticleSystemState(
    ..state,
    emitter: emitter,
    start_color: start_color,
    end_color: end_color,
  )
}

/// Get the Three.js Points object for adding to scene
pub fn get_points_object(state: ParticleSystemState) -> Dynamic {
  state.points_object
}

// ============================================================================
// WRAPPING/UNWRAPPING
// ============================================================================

/// Wrap ParticleSystemState as opaque ParticleSystem for cache
pub fn wrap_as_cache_entry(state: ParticleSystemState) -> ParticleSystem {
  let state_dynamic = state_to_dynamic(state)
  object_cache.wrap_particle_system(state_dynamic)
}

/// Unwrap opaque ParticleSystem from cache to ParticleSystemState
pub fn unwrap_from_cache_entry(system: ParticleSystem) -> ParticleSystemState {
  let state_dynamic = object_cache.unwrap_particle_system(system)
  dynamic_to_state(state_dynamic)
}

fn state_to_dynamic(state: ParticleSystemState) -> Dynamic {
  do_state_to_dynamic(state)
}

@external(javascript, "../../threejs.ffi.mjs", "identity")
fn do_state_to_dynamic(state: ParticleSystemState) -> Dynamic

fn dynamic_to_state(dyn: Dynamic) -> ParticleSystemState {
  do_dynamic_to_state(dyn)
}

@external(javascript, "../../threejs.ffi.mjs", "identity")
fn do_dynamic_to_state(dyn: Dynamic) -> ParticleSystemState

// ============================================================================
// FFI FUNCTIONS
// ============================================================================

// Color operations
@external(javascript, "../../threejs.ffi.mjs", "createColor")
fn create_color_ffi(hex: Int) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "lerpColor")
fn lerp_color_ffi(color1: Dynamic, color2: Dynamic, t: Float) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "getColorR")
fn get_color_r_ffi(color: Dynamic) -> Float

@external(javascript, "../../threejs.ffi.mjs", "getColorG")
fn get_color_g_ffi(color: Dynamic) -> Float

@external(javascript, "../../threejs.ffi.mjs", "getColorB")
fn get_color_b_ffi(color: Dynamic) -> Float

// Buffer operations
@external(javascript, "../../threejs.ffi.mjs", "createFloat32Array")
fn create_float32_array_ffi(size: Int) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "createBufferGeometry")
fn create_buffer_geometry_ffi() -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "setBufferAttribute")
fn set_buffer_attribute_ffi(
  geometry: Dynamic,
  name: String,
  array: Dynamic,
  item_size: Int,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "getGeometry")
fn get_geometry_ffi(points: Dynamic) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "getAttribute")
fn get_attribute_ffi(geometry: Dynamic, name: String) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "setBufferXYZ")
fn set_buffer_xyz_ffi(
  attribute: Dynamic,
  index: Int,
  x: Float,
  y: Float,
  z: Float,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setBufferX")
fn set_buffer_x_ffi(attribute: Dynamic, index: Int, value: Float) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setAttributeNeedsUpdate")
fn set_needs_update_ffi(attribute: Dynamic, needs_update: Bool) -> Nil

// Points/Material operations
@external(javascript, "../../threejs.ffi.mjs", "createPointsMaterial")
fn create_points_material_ffi(
  size: Float,
  vertex_colors: Bool,
  transparent: Bool,
  opacity: Float,
  depth_write: Bool,
  blending: Dynamic,
  size_attenuation: Bool,
) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "getAdditiveBlending")
fn get_additive_blending_ffi() -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "createPoints")
fn create_points_ffi(geometry: Dynamic, material: Dynamic) -> Dynamic

@external(javascript, "../../threejs.ffi.mjs", "setPosition")
fn set_position_ffi(object: Dynamic, x: Float, y: Float, z: Float) -> Nil
