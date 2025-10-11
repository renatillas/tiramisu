import gleam/bool
import gleam/float
import gleam/option.{type Option}
import vec/vec3.{type Vec3}

/// Particle emitter configuration for creating particle effects.
///
/// Particle emitters spawn particles over time with configurable properties.
/// Particles are rendered efficiently using Three.js Points with automatic
/// lifetime management, velocity integration, and color fading.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(emitter) = scene.particle_emitter(
///   rate: 50.0,
///   lifetime: 2.0,
///   velocity: vec3.Vec3(0.0, 5.0, 0.0),
///   velocity_variance: vec3.Vec3(2.0, 1.0, 2.0),
///   size: 0.1,
///   size_variance: 0.05,
///   color: 0xffff00,
///   color_end: option.Some(0xff0000),
///   gravity_scale: 1.0,
///   max_particles: 1000,
/// )
/// ```
pub opaque type ParticleEmitter {
  ParticleEmitter(
    rate: Float,
    lifetime: Float,
    velocity: Vec3(Float),
    velocity_variance: Vec3(Float),
    size: Float,
    size_variance: Float,
    color: Int,
    color_end: Option(Int),
    gravity_scale: Float,
    max_particles: Int,
  )
}

pub type ParticleError {
  NegativeRate(Float)
  NegativeLifetime(Float)
  NegativeSize(Float)
  NegativeSizeVariance(Float)
  OutOfBoundsColor(Int)
  OutOfBoundsColorEnd(Int)
  NegativeMaxParticles(Int)
}

fn particle_emitter(
  rate rate: Float,
  lifetime lifetime: Float,
  velocity velocity: Vec3(Float),
  velocity_variance velocity_variance: Vec3(Float),
  size size: Float,
  size_variance size_variance: Float,
  color color: Int,
  color_end color_end: Option(Int),
  gravity_scale gravity_scale: Float,
  max_particles max_particles: Int,
) -> Result(ParticleEmitter, ParticleError) {
  use <- bool.guard(rate <=. 0.0, Error(NegativeRate(rate)))
  use <- bool.guard(lifetime <=. 0.0, Error(NegativeLifetime(lifetime)))
  use <- bool.guard(size <=. 0.0, Error(NegativeSize(size)))
  use <- bool.guard(
    size_variance <. 0.0,
    Error(NegativeSizeVariance(size_variance)),
  )
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(OutOfBoundsColor(color)),
  )
  use <- bool.guard(
    case color_end {
      option.Some(c) -> c < 0x000000 || c > 0xffffff
      option.None -> False
    },
    Error(
      OutOfBoundsColorEnd(case color_end {
        option.Some(c) -> c
        option.None -> 0
      }),
    ),
  )
  use <- bool.guard(
    max_particles <= 0,
    Error(NegativeMaxParticles(max_particles)),
  )

  Ok(ParticleEmitter(
    rate:,
    lifetime:,
    velocity:,
    velocity_variance:,
    size:,
    size_variance:,
    color:,
    color_end:,
    gravity_scale:,
    max_particles:,
  ))
}

// --- Particle Emitter Builder Pattern ---

/// Builder for particle emitter with sensible defaults.
///
/// Start with `new_particle_emitter()`, chain setter methods, then call `build_emitter()`.
///
/// ## Example
///
/// ```gleam
/// let emitter = scene.new_particle_emitter()
///   |> scene.emitter_rate(100.0)
///   |> scene.emitter_lifetime(2.0)
///   |> scene.emitter_velocity(vec3.Vec3(0.0, 5.0, 0.0))
///   |> scene.emitter_color(0xffff00)
///   |> scene.emitter_fade_to(0xff0000)
///   |> scene.build_emitter()
/// ```
pub opaque type ParticleEmitterBuilder {
  ParticleEmitterBuilder(
    rate: Float,
    lifetime: Float,
    velocity: Vec3(Float),
    velocity_variance: Vec3(Float),
    size: Float,
    size_variance: Float,
    color: Int,
    color_end: Option(Int),
    gravity_scale: Float,
    max_particles: Int,
  )
}

/// Create a new particle emitter builder with default values.
///
/// Defaults:
/// - rate: 50.0 particles/sec
/// - lifetime: 1.0 seconds
/// - velocity: upward (0, 2, 0)
/// - velocity_variance: (1, 1, 1)
/// - size: 0.1
/// - size_variance: 0.05
/// - color: white (0xffffff)
/// - color_end: None
/// - gravity_scale: 1.0
/// - max_particles: 1000
pub fn new() -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(
    rate: 50.0,
    lifetime: 1.0,
    velocity: vec3.Vec3(0.0, 2.0, 0.0),
    velocity_variance: vec3.Vec3(1.0, 1.0, 1.0),
    size: 0.1,
    size_variance: 0.05,
    color: 0xffffff,
    color_end: option.None,
    gravity_scale: 1.0,
    max_particles: 1000,
  )
}

/// Set the emission rate (particles per second).
pub fn rate(
  builder: ParticleEmitterBuilder,
  rate: Float,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, rate: rate)
}

/// Set how long particles live (in seconds).
pub fn lifetime(
  builder: ParticleEmitterBuilder,
  lifetime: Float,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, lifetime: lifetime)
}

/// Set the base velocity for new particles.
pub fn velocity(
  builder: ParticleEmitterBuilder,
  velocity: Vec3(Float),
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, velocity: velocity)
}

/// Set random variance added to velocity (per axis).
pub fn velocity_variance(
  builder: ParticleEmitterBuilder,
  variance: Vec3(Float),
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, velocity_variance: variance)
}

/// Set the base particle size.
pub fn size(
  builder: ParticleEmitterBuilder,
  size: Float,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, size: size)
}

/// Set random variance added to size.
pub fn size_variance(
  builder: ParticleEmitterBuilder,
  variance: Float,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, size_variance: variance)
}

/// Set the start color for particles.
pub fn color(
  builder: ParticleEmitterBuilder,
  color: Int,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, color: color)
}

/// Set the end color for particles (for fading effect).
pub fn fade_to(
  builder: ParticleEmitterBuilder,
  color: Int,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, color_end: option.Some(color))
}

/// Set gravity multiplier (1.0 = normal, 0.0 = no gravity).
pub fn gravity(
  builder: ParticleEmitterBuilder,
  scale: Float,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, gravity_scale: scale)
}

/// Set maximum number of active particles.
pub fn max_particles(
  builder: ParticleEmitterBuilder,
  max: Int,
) -> ParticleEmitterBuilder {
  ParticleEmitterBuilder(..builder, max_particles: max)
}

/// Build the particle emitter from the builder (validates parameters).
pub fn build(
  builder: ParticleEmitterBuilder,
) -> Result(ParticleEmitter, ParticleError) {
  particle_emitter(
    rate: builder.rate,
    lifetime: builder.lifetime,
    velocity: builder.velocity,
    velocity_variance: builder.velocity_variance,
    size: builder.size,
    size_variance: builder.size_variance,
    color: builder.color,
    color_end: builder.color_end,
    gravity_scale: builder.gravity_scale,
    max_particles: builder.max_particles,
  )
}

// --- Internal Accessors (for particle_manager) ---

@internal
pub fn get_emit_rate(emitter: ParticleEmitter) -> Int {
  emitter.rate |> float.round
}

@internal
pub fn get_max_particles(emitter: ParticleEmitter) -> Int {
  emitter.max_particles
}

@internal
pub fn get_velocity(emitter: ParticleEmitter) -> Vec3(Float) {
  emitter.velocity
}

@internal
pub fn get_velocity_variance(emitter: ParticleEmitter) -> Vec3(Float) {
  emitter.velocity_variance
}

@internal
pub fn get_size(emitter: ParticleEmitter) -> Float {
  emitter.size
}

@internal
pub fn get_size_variance(emitter: ParticleEmitter) -> Float {
  emitter.size_variance
}

@internal
pub fn get_lifetime(emitter: ParticleEmitter) -> Float {
  emitter.lifetime
}

@internal
pub fn get_lifetime_variance(emitter: ParticleEmitter) -> Float {
  emitter.lifetime *. 0.2
}

@internal
pub fn get_start_color(emitter: ParticleEmitter) -> Int {
  emitter.color
}

@internal
pub fn get_end_color(emitter: ParticleEmitter) -> Int {
  case emitter.color_end {
    option.Some(c) -> c
    option.None -> emitter.color
  }
}

@internal
pub fn get_gravity_scale(emitter: ParticleEmitter) -> Float {
  emitter.gravity_scale
}
