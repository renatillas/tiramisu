import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/time/duration.{type Duration}
import quaternion
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}

type RapierWorld

type RapierEventQueue

type RapierRigidBody

type RapierCharacterController

/// Opaque handle to the Rapier physics world
/// This is part of your Model and gets updated each frame
pub opaque type PhysicsWorld {
  PhysicsWorld(
    world: RapierWorld,
    queue: RapierEventQueue,
    /// Declared rigid body configurations (from scene)
    bodies: Dict(String, RigidBody),
    /// Actual Rapier rigid body handles
    rapier_bodies: Dict(String, RapierRigidBody),
    /// Commands to be applied during the next physics step
    pending_commands: List(PhysicsCommand),
    /// Mapping from collider handles to body string IDs
    collider_to_body: Dict(Int, String),
    /// Collision events from the last physics step
    collision_events: List(CollisionEvent),
    /// Character controllers for kinematic character movement (one per body)
    character_controllers: Dict(String, RapierCharacterController),
  )
}

/// Physics commands that can be queued and applied during step
type PhysicsCommand {
  ApplyForce(id: String, force: Vec3(Float))
  ApplyImpulse(id: String, impulse: Vec3(Float))
  SetVelocity(id: String, velocity: Vec3(Float))
  SetAngularVelocity(id: String, velocity: Vec3(Float))
  ApplyTorque(id: String, torque: Vec3(Float))
  ApplyTorqueImpulse(id: String, impulse: Vec3(Float))
  SetKinematicTranslation(id: String, position: Vec3(Float))
}

// --- Public Types ---

/// Physics body type
pub type Body {
  /// Dynamic bodies are affected by forces and gravity
  Dynamic
  /// Kinematic bodies can be moved programmatically but don't respond to forces
  Kinematic
  /// Fixed (static) bodies don't move
  Fixed
}

/// Collider shape
pub type ColliderShape {
  /// Box collider with half-extents
  Box(offset: transform.Transform, width: Float, height: Float, depth: Float)
  /// Sphere collider with radius
  Sphere(offset: transform.Transform, radius: Float)
  /// Capsule collider (cylinder with rounded caps)
  Capsule(offset: transform.Transform, half_height: Float, radius: Float)
  /// Cylinder collider
  Cylinder(offset: transform.Transform, half_height: Float, radius: Float)
}

/// Axis locks for restricting body movement/rotation
pub type AxisLock {
  AxisLock(
    /// Lock translation on X axis
    lock_translation_x: Bool,
    /// Lock translation on Y axis
    lock_translation_y: Bool,
    /// Lock translation on Z axis
    lock_translation_z: Bool,
    /// Lock rotation on X axis
    lock_rotation_x: Bool,
    /// Lock rotation on Y axis
    lock_rotation_y: Bool,
    /// Lock rotation on Z axis
    lock_rotation_z: Bool,
  )
}

/// Collision groups for filtering which objects can collide with each other.
///
/// Uses Rapier's collision groups system based on 32-bit bitmasks:
/// - `membership`: What collision layers this body belongs to (0-15)
/// - `filter`: What collision layers this body can interact with (0-15)
///
/// Two bodies can collide only if:
/// - Body A's membership overlaps with Body B's filter, AND
/// - Body B's membership overlaps with Body A's filter
///
/// ## Example
///
/// ```gleam
/// // Player belongs to layer 0, collides with layers 1 (enemies) and 2 (ground)
/// let player_groups = physics.CollisionGroups(
///   membership: [0],
///   filter: [1, 2]
/// )
///
/// // Enemy belongs to layer 1, collides with layers 0 (player) and 3 (projectiles)
/// let enemy_groups = physics.CollisionGroups(
///   membership: [1],
///   filter: [0, 3]
/// )
///
/// // Ground belongs to layer 2, collides with everything
/// let ground_groups = physics.CollisionGroups(
///   membership: [2],
///   filter: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
/// )
/// ```
pub type CollisionGroups {
  CollisionGroups(
    /// List of collision layers this body belongs to (0-15)
    membership: List(Int),
    /// List of collision layers this body can collide with (0-15)
    filter: List(Int),
  )
}

/// Character controller configuration for kinematic character movement.
///
/// Character controllers provide collision-aware movement for kinematic bodies,
/// perfect for player characters, NPCs, and moving platforms.
///
/// ## Example
///
/// ```gleam
/// physics.CharacterController(
///   offset: 0.01,
///   up_vector: vec3.Vec3(0.0, 1.0, 0.0),
///   slide_enabled: True,
/// )
/// ```
pub type CharacterController {
  CharacterController(
    /// Offset from surfaces (default: 0.01)
    offset: Float,
    /// Up vector for the character (default: positive Y)
    up_vector: Vec3(Float),
    /// Enable sliding along surfaces (default: True)
    slide_enabled: Bool,
  )
}

/// Physics body configuration
pub opaque type RigidBody {
  RigidBody(
    /// Type of rigid body
    kind: Body,
    /// Mass (only for Dynamic bodies)
    mass: option.Option(Float),
    /// Restitution (bounciness) 0.0 = no bounce, 1.0 = perfect bounce
    restitution: Float,
    /// Friction coefficient
    friction: Float,
    /// Linear damping (resistance to movement)
    linear_damping: Float,
    /// Angular damping (resistance to rotation)
    angular_damping: Float,
    /// Collider shape
    collider: ColliderShape,
    /// Enable continuous collision detection
    ccd_enabled: Bool,
    /// Axis locks for restricting movement/rotation
    axis_locks: AxisLock,
    /// Collision groups for filtering interactions
    collision_groups: option.Option(CollisionGroups),
    /// Character controller configuration (for kinematic bodies)
    character_controller: option.Option(CharacterController),
    /// Enable collision event tracking (default: False)
    track_collision_events: Bool,
  )
}

/// Physics world configuration
pub type WorldConfig {
  WorldConfig(
    /// Gravity vector (typically Vec3(0.0, -9.81, 0.0))
    gravity: Vec3(Float),
  )
}

/// Result of a raycast hit
pub type RaycastHit {
  RaycastHit(
    /// ID of the body that was hit
    id: String,
    /// Point where the ray intersected the body
    point: Vec3(Float),
    /// Normal vector at the hit point
    normal: Vec3(Float),
    /// Distance from ray origin to hit point
    distance: Float,
  )
}

/// Collision events that occurred during the physics step
pub type CollisionEvent {
  /// Two bodies started colliding
  CollisionStarted(body_a: String, body_b: String)
  /// Two bodies stopped colliding
  CollisionEnded(body_a: String, body_b: String)
}

// --- Constructor Functions ---

/// Create a new physics world.
///
/// Call this in your `init()` function and store the world in your Model.
/// Return it as the third element of the init triple so Tiramisu can manage it.
///
/// **Gravity**: Typical Earth gravity is `Vec3(0.0, -9.81, 0.0)`.
/// Use `Vec3(0.0, 0.0, 0.0)` for zero-gravity space games.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/physics
/// import vec/vec3
/// import gleam/option
///
/// type Model {
///   Model(physics_world: physics.PhysicsWorld(String))
/// }
///
/// fn init(ctx) {
///   let world = physics.new_world(physics.WorldConfig(
///     gravity: vec3.Vec3(0.0, -9.81, 0.0),  // Earth gravity
///   ))
///
///   #(
///     Model(physics_world: world),
///     effect.none(),
///     option.Some(world),  // Return world for Tiramisu to manage
///   )
/// }
/// ```
pub fn new_world(config: WorldConfig) -> PhysicsWorld {
  // Initialize the Rapier world via physics_manager
  // Convert config to Dynamic for physics_manager
  let #(world, queue) = create_world(config)

  PhysicsWorld(
    world,
    queue,
    bodies: dict.new(),
    rapier_bodies: dict.new(),
    pending_commands: [],
    collider_to_body: dict.new(),
    collision_events: [],
    character_controllers: dict.new(),
  )
}

/// Builder for creating rigid bodies with a fluent API
pub opaque type RigidBodyBuilder(a) {
  RigidBodyBuilder(
    kind: Body,
    collider: option.Option(ColliderShape),
    mass: option.Option(Float),
    restitution: Float,
    friction: Float,
    linear_damping: Float,
    angular_damping: Float,
    ccd_enabled: Bool,
    axis_locks: AxisLock,
    collision_groups: option.Option(CollisionGroups),
    character_controller: option.Option(CharacterController),
    track_collision_events: Bool,
  )
}

/// Create a new rigid body builder.
///
/// Start here to build a physics body using the fluent builder pattern.
/// You must call `with_collider()` before `build()`.
///
/// **Body Types:**
/// - `Dynamic`: Moves and responds to forces (balls, characters, props)
/// - `Kinematic`: Programmatically controlled, doesn't respond to forces (elevators, doors)
/// - `Fixed`: Static, immovable (walls, floors, terrain)
///
/// ## Example
///
/// ```gleam
/// import tiramisu/physics
/// import tiramisu/transform
///
/// // Dynamic ball
/// let ball = physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_collider(physics.Sphere(
///     offset: transform.identity,
///     radius: 1.0,
///   ))
///   |> physics.with_mass(5.0)
///   |> physics.with_restitution(0.8)
///   |> physics.build()
///
/// // Static ground
/// let ground = physics.new_rigid_body(physics.Fixed)
///   |> physics.with_collider(physics.Box(
///     offset: transform.identity,
///     width: 50.0,
///     height: 1.0,
///     depth: 50.0,
///   ))
///   |> physics.build()
/// ```
pub fn new_rigid_body(body_type: Body) -> RigidBodyBuilder(WithoutCollider) {
  RigidBodyBuilder(
    kind: body_type,
    collider: option.None,
    mass: option.None,
    restitution: 0.3,
    friction: 0.5,
    linear_damping: 0.0,
    angular_damping: 0.0,
    ccd_enabled: False,
    axis_locks: AxisLock(
      lock_translation_x: False,
      lock_translation_y: False,
      lock_translation_z: False,
      lock_rotation_x: False,
      lock_rotation_y: False,
      lock_rotation_z: False,
    ),
    collision_groups: option.None,
    character_controller: option.None,
    track_collision_events: False,
  )
}

/// Set the collider shape (required).
///
/// **Shapes:** Box, Sphere, Capsule, Cylinder
/// **Offset**: Position relative to object center (usually `transform.identity`)
///
/// ## Example
///
/// ```gleam
/// // Character capsule (best for characters)
/// physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_collider(physics.Capsule(
///     offset: transform.identity,
///     half_height: 0.9,  // Total height = 1.8
///     radius: 0.3,
///   ))
/// ```
pub fn with_collider(
  builder: RigidBodyBuilder(_),
  collider: ColliderShape,
) -> RigidBodyBuilder(WithCollider) {
  RigidBodyBuilder(..builder, collider: option.Some(collider))
}

/// Set the mass in kilograms (for Dynamic bodies).
///
/// **Mass** affects how forces and collisions influence the body.
/// Default: Calculated from volume and density if not specified.
///
/// ## Example
///
/// ```gleam
/// physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_mass(70.0)  // Average human = 70kg
/// ```
pub fn with_mass(
  builder: RigidBodyBuilder(_),
  mass: Float,
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, mass: option.Some(mass))
}

/// Set restitution (bounciness).
///
/// **Restitution**: 0.0 = no bounce, 1.0 = perfect bounce (energy conserved)
/// Default: 0.3
///
/// ## Example
///
/// ```gleam
/// // Bouncy ball
/// physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_restitution(0.9)  // Very bouncy
///
/// // Non-bouncy box
/// physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_restitution(0.1)  // Barely bounces
/// ```
pub fn with_restitution(
  builder: RigidBodyBuilder(_),
  restitution: Float,
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, restitution: restitution)
}

/// Set friction coefficient.
///
/// **Friction**: 0.0 = ice (no friction), 1.0+ = very grippy
/// Default: 0.5
///
/// ## Example
///
/// ```gleam
/// // Slippery ice
/// physics.new_rigid_body(physics.Fixed)
///   |> physics.with_friction(0.05)
///
/// // Grippy rubber
/// physics.new_rigid_body(physics.Fixed)
///   |> physics.with_friction(0.9)
/// ```
pub fn with_friction(
  builder: RigidBodyBuilder(_),
  friction: Float,
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, friction: friction)
}

/// Set linear damping (air resistance for translation).
///
/// **Damping**: 0.0 = no resistance, higher = more drag
/// Useful for simulating air/water resistance. Default: 0.0
///
/// ## Example
///
/// ```gleam
/// // Underwater physics
/// physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_linear_damping(2.0)  // Heavy water resistance
/// ```
pub fn with_linear_damping(
  builder: RigidBodyBuilder(_),
  damping: Float,
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, linear_damping: damping)
}

/// Set angular damping (air resistance for rotation).
///
/// **Damping**: 0.0 = no resistance, higher = more drag
/// Prevents bodies from spinning forever. Default: 0.0
pub fn with_angular_damping(
  builder: RigidBodyBuilder(_),
  damping: Float,
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, angular_damping: damping)
}

/// Enable Continuous Collision Detection (CCD).
///
/// CCD prevents fast-moving objects from tunneling through thin obstacles.
/// Use for bullets, fast-moving balls, or high-velocity objects.
///
/// ## Example
///
/// ```gleam
/// // Bullet that shouldn't pass through walls
/// physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_collider(physics.Sphere(transform.identity, 0.1))
///   |> physics.with_body_ccd_enabled()
/// ```
pub fn with_body_ccd_enabled(
  builder: RigidBodyBuilder(_),
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, ccd_enabled: True)
}

/// Lock translation on the X axis
pub fn with_lock_translation_x(
  builder: RigidBodyBuilder(_),
) -> RigidBodyBuilder(_) {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_translation_x: True),
  )
}

/// Lock translation on the Y axis
pub fn with_lock_translation_y(
  builder: RigidBodyBuilder(_),
) -> RigidBodyBuilder(_) {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_translation_y: True),
  )
}

/// Lock translation on the Z axis
pub fn with_lock_translation_z(
  builder: RigidBodyBuilder(_),
) -> RigidBodyBuilder(_) {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_translation_z: True),
  )
}

/// Lock rotation on the X axis (pitch)
pub fn with_lock_rotation_x(builder: RigidBodyBuilder(_)) -> RigidBodyBuilder(_) {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_rotation_x: True),
  )
}

/// Lock rotation on the Y axis (yaw)
pub fn with_lock_rotation_y(builder: RigidBodyBuilder(_)) -> RigidBodyBuilder(_) {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_rotation_y: True),
  )
}

/// Lock rotation on the Z axis (roll)
pub fn with_lock_rotation_z(builder: RigidBodyBuilder(_)) -> RigidBodyBuilder(_) {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_rotation_z: True),
  )
}

/// Set collision groups for filtering which objects can collide
///
/// ## Example
///
/// ```gleam
/// // Player belongs to layer 0, collides with enemies (1) and ground (2)
/// let body = physics.new_rigid_body(physics.Dynamic)
///   |> physics.body_collider(physics.Capsule(1.0, 0.5))
///   |> physics.body_collision_groups(
///     membership: [0],
///     filter: [1, 2]
///   )
///   |> physics.build_body()
/// ```
pub fn with_collision_groups(
  builder: RigidBodyBuilder(_),
  membership membership: List(Int),
  can_collide_with filter: List(Int),
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(
    ..builder,
    collision_groups: option.Some(CollisionGroups(
      membership: membership,
      filter: filter,
    )),
  )
}

/// Add a character controller for collision-aware kinematic movement.
///
/// Character controllers are perfect for player characters and NPCs, providing:
/// - Automatic collision detection and response
/// - Sliding along surfaces
/// - Configurable offset from obstacles
///
/// **Note**: Character controllers are only useful for Kinematic bodies.
///
/// ## Example
///
/// ```gleam
/// // Player character with character controller
/// let player = physics.new_rigid_body(physics.Kinematic)
///   |> physics.with_collider(physics.Capsule(
///     offset: transform.identity,
///     half_height: 0.9,
///     radius: 0.3,
///   ))
///   |> physics.with_character_controller(
///     offset: 0.01,
///     up_vector: vec3.Vec3(0.0, 1.0, 0.0),
///     slide_enabled: True,
///   )
///   |> physics.build()
/// ```
pub fn with_character_controller(
  builder: RigidBodyBuilder(_),
  offset offset: Float,
  up_vector up_vector: Vec3(Float),
  slide_enabled slide_enabled: Bool,
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(
    ..builder,
    character_controller: option.Some(CharacterController(
      offset: offset,
      up_vector: up_vector,
      slide_enabled: slide_enabled,
    )),
  )
}

/// Enable collision event tracking for this body.
///
/// By default, collision events are not tracked to minimize performance overhead.
/// Call this method if you need to receive collision events via `get_collision_events()`.
///
/// **Performance Note:** Only enable collision events for bodies where you actually
/// need to detect collisions (e.g., player, enemies, collectibles). Static decorations
/// and particle effects typically don't need event tracking.
///
/// ## Example
///
/// ```gleam
/// // Player needs collision events (e.g., for damage detection)
/// let player = physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_collider(physics.Capsule(
///     offset: transform.identity,
///     half_height: 0.9,
///     radius: 0.3,
///   ))
///   |> physics.with_collision_events()  // Enable events
///   |> physics.build()
///
/// // Static ground doesn't need events
/// let ground = physics.new_rigid_body(physics.Fixed)
///   |> physics.with_collider(physics.Box(
///     offset: transform.identity,
///     width: 50.0,
///     height: 1.0,
///     depth: 50.0,
///   ))
///   |> physics.build()  // No events, saves performance
/// ```
pub fn with_collision_events(
  builder: RigidBodyBuilder(_),
) -> RigidBodyBuilder(_) {
  RigidBodyBuilder(..builder, track_collision_events: True)
}

pub type WithCollider

pub type WithoutCollider

/// Build the final rigid body from the builder.
///
/// This function is type-safe - you cannot call it without first calling `with_collider()`.
///
/// ## Example
///
/// ```gleam
/// let body = physics.new_rigid_body(physics.Dynamic)
///   |> physics.with_collider(physics.Sphere(transform.identity, 1.0))
///   |> physics.with_mass(5.0)
///   |> physics.build()  // Returns RigidBody ready to use
/// ```
pub fn build(builder: RigidBodyBuilder(WithCollider)) -> RigidBody {
  let assert option.Some(collider) = builder.collider
  RigidBody(
    kind: builder.kind,
    mass: builder.mass,
    restitution: builder.restitution,
    friction: builder.friction,
    linear_damping: builder.linear_damping,
    angular_damping: builder.angular_damping,
    collider: collider,
    ccd_enabled: builder.ccd_enabled,
    axis_locks: builder.axis_locks,
    collision_groups: builder.collision_groups,
    character_controller: builder.character_controller,
    track_collision_events: builder.track_collision_events,
  )
}

/// Apply a single physics command via FFI
fn apply_command(
  command: PhysicsCommand,
  rapier_bodies: Dict(String, RapierRigidBody),
) -> Result(Nil, Nil) {
  // Extract the ID and convert to string using BiMap, then get Rapier body
  case command {
    ApplyForce(id, force) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      add_body_force_ffi(rapier_body, force.x, force.y, force.z, True)
      Ok(Nil)
    }
    ApplyImpulse(id, impulse) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      apply_body_impulse_ffi(rapier_body, impulse.x, impulse.y, impulse.z, True)
      Ok(Nil)
    }
    SetVelocity(id, velocity) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      set_body_linvel_ffi(rapier_body, velocity.x, velocity.y, velocity.z, True)
      Ok(Nil)
    }
    SetAngularVelocity(id, velocity) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      set_body_angvel_ffi(rapier_body, velocity.x, velocity.y, velocity.z, True)
      Ok(Nil)
    }
    ApplyTorque(id, torque) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      add_body_torque_ffi(rapier_body, torque.x, torque.y, torque.z, True)
      Ok(Nil)
    }
    ApplyTorqueImpulse(id, impulse) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      apply_body_torque_impulse_ffi(
        rapier_body,
        impulse.x,
        impulse.y,
        impulse.z,
        True,
      )
      Ok(Nil)
    }
    SetKinematicTranslation(id, position) -> {
      use rapier_body <- result.try(dict.get(rapier_bodies, id))
      set_body_next_kinematic_translation_ffi(
        rapier_body,
        position.x,
        position.y,
        position.z,
      )
      Ok(Nil)
    }
  }
}

/// Step the physics simulation forward with variable timestep
/// This should be called in your update function each frame
///
/// **IMPORTANT**: Pass `ctx.delta_time` for frame-rate independent physics!
///
/// ## Example
/// ```gleam
/// fn update(model, msg, ctx) {
///   let world = physics.step(model.physics_world, ctx.delta_time)
///   #(Model(..model, physics_world: world), effect.none(), option.None)
/// }
/// ```
///
/// Returns updated world with new transforms for all bodies
pub fn step(world: PhysicsWorld, delta_time: Duration) -> PhysicsWorld {
  // Apply all pending commands in the correct order
  // Commands are prepended (O(1)), so reverse once here before processing
  world.pending_commands
  |> list.reverse
  |> list.each(fn(command) {
    let _ = apply_command(command, world.rapier_bodies)
    Nil
  })

  // Convert delta time to seconds for Rapier (it expects Float seconds)
  let delta_time_seconds = duration.to_seconds(delta_time)

  // Step the Rapier world with actual frame time (frame-rate independent!)
  step_world_ffi(world.world, world.queue, delta_time_seconds)

  // Drain collision events from the queue
  let collision_events =
    world.collider_to_body
    |> drain_collision_events(world, _)

  // Return world with cleared commands and updated events
  PhysicsWorld(
    ..world,
    pending_commands: [],
    collision_events: collision_events,
  )
}

/// Drain collision events from the Rapier event queue
/// Converts collider handles to body IDs using the collider_to_body mapping
fn drain_collision_events(
  world: PhysicsWorld,
  collider_to_body: Dict(Int, String),
) -> List(CollisionEvent) {
  // Call FFI to drain events and return them as a list
  let raw_events = drain_collision_events_ffi(world.queue)

  // Convert raw events (with collider handles) to CollisionEvents (with body IDs)
  list.filter_map(raw_events, fn(raw_event) {
    let #(handle1, handle2, started) = raw_event

    // Look up body IDs for both collider handles
    case
      dict.get(collider_to_body, handle1),
      dict.get(collider_to_body, handle2)
    {
      Ok(body_id1), Ok(body_id2) ->
        case started {
          True -> Ok(CollisionStarted(body_id1, body_id2))
          False -> Ok(CollisionEnded(body_id1, body_id2))
        }
      _, _ -> Error(Nil)
    }
  })
}

/// Get the current transform of a rigid body.
///
/// Queries the physics simulation directly, so it always returns the latest position
/// even for bodies that were just created in the current frame.
///
/// ## Example
///
/// ```gleam
/// let cube_transform = case physics.get_transform(physics_world, Cube1) {
///   Ok(t) -> t
///   Error(_) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
/// }
/// ```
pub fn get_transform(
  physics_world: PhysicsWorld,
  id: String,
) -> Result(Transform, Nil) {
  use rapier_body <- result.try(dict.get(physics_world.rapier_bodies, id))
  let translation = get_body_translation_ffi(rapier_body)
  let rotation_quat = get_body_rotation_ffi(rapier_body)

  Ok(
    transform.identity
    |> transform.with_position(vec3.Vec3(
      translation.x,
      translation.y,
      translation.z,
    ))
    |> transform.with_quaternion_rotation(rotation_quat),
  )
}

/// Get the raw position and quaternion rotation from a rigid body.
///
/// This returns the rotation as a quaternion directly from Rapier,
/// avoiding conversion to Euler angles which can cause rotation errors.
///
/// This is used internally by the renderer for physics synchronization.
///
/// ## Example
///
/// ```gleam
/// case physics.get_body_transform_raw(physics_world, Cube1) {
///   Ok(#(position, quaternion)) -> {
///     // Use position and quaternion directly
///   }
///   Error(_) -> // Handle missing body
/// }
/// ```
@internal
pub fn get_body_transform_raw(
  physics_world: PhysicsWorld,
  id: String,
) -> Result(#(Vec3(Float), quaternion.Quaternion), Nil) {
  use rapier_body <- result.try(dict.get(physics_world.rapier_bodies, id))

  // Get translation and rotation directly from Rapier body
  let translation = get_body_translation_ffi(rapier_body)
  let quaternion = get_body_rotation_ffi(rapier_body)

  Ok(#(translation, quaternion))
}

// --- Forces and Impulses (Functional API) ---

/// Queue a force to be applied to a rigid body during the next physics step.
/// Returns updated world with the command queued.
///
/// ## Example
///
/// ```gleam
/// let world = physics.apply_force(world, "player", vec3.Vec3(0.0, 100.0, 0.0))
/// let world = physics.step(world, ctx.delta_time)  // Force is applied here
/// ```
pub fn apply_force(
  world: PhysicsWorld,
  id: String,
  force: Vec3(Float),
) -> PhysicsWorld {
  let command = ApplyForce(id:, force:)
  // Prepend for O(1) insertion - reversed before execution in step()
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Queue an impulse to be applied to a rigid body during the next physics step.
/// Returns updated world with the command queued.
///
/// ## Example
///
/// ```gleam
/// // Jump
/// let world = physics.apply_impulse(world, "player", vec3.Vec3(0.0, 10.0, 0.0))
/// ```
pub fn apply_impulse(
  world: PhysicsWorld,
  id: String,
  impulse: Vec3(Float),
) -> PhysicsWorld {
  let command = ApplyImpulse(id:, impulse:)
  // Prepend for O(1) insertion - reversed before execution in step()
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Queue a velocity change for a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn set_velocity(
  world: PhysicsWorld,
  id: String,
  velocity: Vec3(Float),
) -> PhysicsWorld {
  let command = SetVelocity(id:, velocity:)
  // Prepend for O(1) insertion - reversed before execution in step()
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Get the current velocity of a rigid body
pub fn get_velocity(world: PhysicsWorld, id: String) -> Result(Vec3(Float), Nil) {
  use rapier_body <- result.try(dict.get(world.rapier_bodies, id))
  let vel = get_body_linvel_ffi(rapier_body)
  Ok(vec3.Vec3(vel.x, vel.y, vel.z))
}

/// Queue a kinematic translation change for a kinematic rigid body during the next physics step.
/// This is the proper way to move kinematic bodies in Rapier.
/// Returns updated world with the command queued.
pub fn set_kinematic_translation(
  world: PhysicsWorld,
  id: String,
  position: Vec3(Float),
) -> PhysicsWorld {
  let command = SetKinematicTranslation(id:, position:)
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Compute collision-aware movement for a kinematic character.
/// Returns the actual movement that can be safely applied without penetrating colliders.
/// Must have created a character controller for this body first.
pub fn compute_character_movement(
  world: PhysicsWorld,
  id: String,
  desired_translation: Vec3(Float),
) -> Result(Vec3(Float), Nil) {
  // Get controller for this specific body
  case dict.get(world.character_controllers, id) {
    Error(_) -> Error(Nil)
    Ok(controller) -> {
      // Get the first collider for this body
      case dict.get(world.rapier_bodies, id) {
        Error(_) -> Error(Nil)
        Ok(rapier_body) -> {
          let num_colliders = get_body_num_colliders_ffi(rapier_body)

          case num_colliders > 0 {
            True -> {
              case get_body_collider_ffi(rapier_body, 0) {
                Error(_) -> Error(Nil)
                Ok(collider) -> {
                  // Compute the collision-aware movement
                  let desired_translation_obj =
                    create_vec3_object(
                      desired_translation.x,
                      desired_translation.y,
                      desired_translation.z,
                    )

                  compute_character_movement_ffi(
                    world.world,
                    controller,
                    collider,
                    desired_translation_obj,
                  )

                  // Get the computed safe movement
                  let safe_movement =
                    get_character_computed_movement_ffi(controller)
                  Ok(safe_movement)
                }
              }
            }
            False -> Error(Nil)
          }
        }
      }
    }
  }
}

/// Check if a character is grounded (on the ground).
/// This uses the character controller's computed grounded state.
/// Must have called compute_character_movement for this body first in this frame.
pub fn is_character_grounded(
  world: PhysicsWorld,
  id: String,
) -> Result(Bool, Nil) {
  case dict.get(world.character_controllers, id) {
    Error(_) -> Error(Nil)
    Ok(controller) -> {
      let grounded = get_character_computed_grounded_ffi(controller)
      Ok(grounded)
    }
  }
}

/// Queue an angular velocity change for a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn set_angular_velocity(
  world: PhysicsWorld,
  id: String,
  velocity: Vec3(Float),
) -> PhysicsWorld {
  let command = SetAngularVelocity(id:, velocity:)
  // Prepend for O(1) insertion - reversed before execution in step()
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Get the current angular velocity of a rigid body
pub fn get_angular_velocity(
  world: PhysicsWorld,
  id: String,
) -> Result(Vec3(Float), Nil) {
  use rapier_body <- result.try(dict.get(world.rapier_bodies, id))
  let vel = get_body_angvel_ffi(rapier_body)
  Ok(vec3.Vec3(vel.x, vel.y, vel.z))
}

/// Queue a torque to be applied to a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn apply_torque(
  world: PhysicsWorld,
  id: String,
  torque: Vec3(Float),
) -> PhysicsWorld {
  let command = ApplyTorque(id:, torque:)
  // Prepend for O(1) insertion - reversed before execution in step()
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Queue a torque impulse to be applied to a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn apply_torque_impulse(
  world: PhysicsWorld,
  id: String,
  impulse: Vec3(Float),
) -> PhysicsWorld {
  let command = ApplyTorqueImpulse(id:, impulse:)
  // Prepend for O(1) insertion - reversed before execution in step()
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

// --- Raycasting ---

/// Cast a ray and return the first hit
///
/// Useful for shooting mechanics, line-of-sight checks, and ground detection.
///
/// ## Example
///
/// ```gleam
/// // Cast ray downward from player position
/// let origin = player_position
/// let direction = vec3.Vec3(0.0, -1.0, 0.0)
///
/// case physics.raycast(world, origin, direction, max_distance: 10.0) {
///   Ok(hit) -> {
///     // Found ground at hit.distance units below player
///     io.println("Hit body with ID")
///   }
///   Error(Nil) -> {
///     // No ground found within 10 units
///   }
/// }
/// ```
pub fn raycast(
  world: PhysicsWorld,
  origin origin: Vec3(Float),
  direction direction: Vec3(Float),
  max_distance max_distance: Float,
) -> Result(RaycastHit, Nil) {
  // Create a ray using Rapier FFI
  let ray =
    create_ray_ffi(
      origin.x,
      origin.y,
      origin.z,
      direction.x,
      direction.y,
      direction.z,
    )

  // Cast the ray and get hit info with normal
  case cast_ray_and_get_normal_ffi(world.world, ray, max_distance, True) {
    Ok(hit_info) -> {
      // Extract collider handle from hit info
      let collider_handle = get_hit_collider_handle_ffi(hit_info)

      // Look up the body ID from collider handle
      case dict.get(world.collider_to_body, collider_handle) {
        Ok(id) -> {
          // Convert string ID to typed ID
          // Extract hit point and normal from hit info
          let toi = get_hit_toi_ffi(hit_info)
          let point = ray_point_at_ffi(ray, toi)
          let normal = get_hit_normal_ffi(hit_info)

          Ok(RaycastHit(
            id:,
            point: vec3.Vec3(point.x, point.y, point.z),
            normal: vec3.Vec3(normal.x, normal.y, normal.z),
            distance: toi,
          ))
        }
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Get all collision events that occurred during the last physics step.
///
/// Events are automatically collected when `step()` is called and stored in the world.
///
/// ## Example
///
/// ```gleam
/// let physics_world = physics.step(physics_world)
/// let collision_events = physics.get_collision_events(physics_world)
///
/// list.each(collision_events, fn(event) {
///   case event {
///     physics.CollisionStarted(a, b) ->
///       io.println(a <> " started colliding with " <> b)
///     physics.CollisionEnded(a, b) ->
///       io.println(a <> " ended colliding with " <> b)
///   }
/// })
/// ```
pub fn get_collision_events(world: PhysicsWorld) -> List(CollisionEvent) {
  world.collision_events
}

/// Iterate over all physics bodies and call a function for each
/// This keeps all internal field access within the physics module
@internal
pub fn for_each_body(
  world: PhysicsWorld,
  callback: fn(String, Transform) -> Nil,
) -> Nil {
  // Access rapier_bodies directly - this ensures we're within the module where the opaque type is defined
  for_each_body_internal(world.rapier_bodies, world, callback)
}

// Internal helper that does the actual iteration
fn for_each_body_internal(
  rapier_bodies: dict.Dict(String, RapierRigidBody),
  world: PhysicsWorld,
  callback: fn(String, Transform) -> Nil,
) -> Nil {
  // Get all string IDs from rapier_bodies
  let ids = dict.keys(rapier_bodies)

  // For each string ID, get transform and call callback
  list.each(ids, fn(id) {
    // Get the transform for this body
    case get_transform(world, id) {
      Ok(transform) -> callback(id, transform)
      Error(_) -> Nil
    }
  })
}

/// Iterate over all physics bodies with raw quaternion data and body type.
///
/// This is used by the renderer for physics synchronization, avoiding
/// quaternion-to-Euler conversion which can cause rotation errors.
@internal
pub fn for_each_body_raw(
  world: PhysicsWorld,
  callback: fn(String, Vec3(Float), quaternion.Quaternion, Body) -> Nil,
) -> Nil {
  let ids = dict.keys(world.rapier_bodies)

  list.each(ids, fn(id) {
    case
      dict.get(world.rapier_bodies, id),
      get_body_transform_raw(world, id),
      dict.get(world.bodies, id)
    {
      Ok(rapier_body), Ok(#(position, quaternion)), Ok(body_config) -> {
        // Performance: Skip sleeping/inactive bodies (they haven't moved)
        // Only Dynamic bodies can sleep; Kinematic/Fixed are always considered "awake"
        case body_config.kind {
          Dynamic ->
            case is_body_sleeping_ffi(rapier_body) {
              True -> Nil
              False -> callback(id, position, quaternion, body_config.kind)
            }
          // Kinematic and Fixed bodies don't sleep, always process them
          Kinematic | Fixed ->
            callback(id, position, quaternion, body_config.kind)
        }
      }
      _, _, _ -> Nil
    }
  })
}

// --- Internal Helper Functions ---

/// Convert list of collision layer indices (0-15) to a 16-bit bitmask
///
/// For example, [0, 2, 3] becomes 0b0000000000001101 = 0x000D
fn layers_to_bitmask(layers: List(Int)) -> Int {
  list.fold(layers, 0, fn(mask, layer) {
    case layer >= 0 && layer <= 15 {
      True -> {
        // Set the bit at position 'layer'
        let bit = int.bitwise_shift_left(1, layer)
        int.bitwise_or(mask, bit)
      }
      False -> mask
      // Ignore invalid layer indices
    }
  })
}

/// Pack membership and filter bitmasks into a single 32-bit value
///
/// Rapier format: 16 upper bits = membership, 16 lower bits = filter
/// For example: membership=0x000D, filter=0x0004 -> 0x000D0004
fn pack_collision_groups(membership: Int, filter: Int) -> Int {
  let membership_shifted = int.bitwise_shift_left(membership, 16)
  int.bitwise_or(membership_shifted, filter)
}

/// Convert CollisionGroups to Rapier's 32-bit packed format
@internal
pub fn collision_groups_to_bitmask(groups: CollisionGroups) -> Int {
  let membership_mask = layers_to_bitmask(groups.membership)
  let filter_mask = layers_to_bitmask(groups.filter)
  pack_collision_groups(membership_mask, filter_mask)
}

// --- FFI Functions ---
// These go directly to Rapier FFI

// Rigid body operations
@external(javascript, "../rapier.ffi.mjs", "addBodyForce")
fn add_body_force_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "applyBodyImpulse")
fn apply_body_impulse_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setBodyLinvel")
fn set_body_linvel_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "getBodyLinvel")
fn get_body_linvel_ffi(body: RapierRigidBody) -> vec3.Vec3(Float)

@external(javascript, "../rapier.ffi.mjs", "setBodyAngvel")
fn set_body_angvel_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "getBodyAngvel")
fn get_body_angvel_ffi(body: RapierRigidBody) -> vec3.Vec3(Float)

@external(javascript, "../rapier.ffi.mjs", "addBodyTorque")
fn add_body_torque_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "applyBodyTorqueImpulse")
fn apply_body_torque_impulse_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "getBodyTranslation")
fn get_body_translation_ffi(body: RapierRigidBody) -> vec3.Vec3(Float)

@external(javascript, "../rapier.ffi.mjs", "getBodyRotation")
fn get_body_rotation_ffi(body: RapierRigidBody) -> quaternion.Quaternion

// Raycasting FFI using Rapier
type RapierRay

type RapierRayHit

@external(javascript, "../rapier.ffi.mjs", "createRay")
fn create_ray_ffi(
  origin_x: Float,
  origin_y: Float,
  origin_z: Float,
  dir_x: Float,
  dir_y: Float,
  dir_z: Float,
) -> RapierRay

@external(javascript, "../rapier.ffi.mjs", "castRayAndGetNormal")
fn cast_ray_and_get_normal_ffi(
  world: RapierWorld,
  ray: RapierRay,
  max_distance: Float,
  solid: Bool,
) -> Result(RapierRayHit, Nil)

@external(javascript, "../rapier.ffi.mjs", "rayPointAt")
fn ray_point_at_ffi(ray: RapierRay, toi: Float) -> vec3.Vec3(Float)

// Extract info from RapierRayHit
@external(javascript, "../rapier.ffi.mjs", "getHitColliderHandle")
fn get_hit_collider_handle_ffi(hit: RapierRayHit) -> Int

@external(javascript, "../rapier.ffi.mjs", "getHitToi")
fn get_hit_toi_ffi(hit: RapierRayHit) -> Float

@external(javascript, "../rapier.ffi.mjs", "getHitNormal")
fn get_hit_normal_ffi(hit: RapierRayHit) -> vec3.Vec3(Float)

// ============================================================================
// WORLD INITIALIZATION
// ============================================================================

/// Initialize the global physics world
/// Takes a WorldConfig as Dynamic and extracts gravity
fn create_world(config: WorldConfig) -> #(RapierWorld, RapierEventQueue) {
  // Create Rapier world
  let world =
    create_world_ffi(config.gravity.x, config.gravity.y, config.gravity.z)
  let queue = create_event_queue_ffi(True)

  #(world, queue)
}

// ============================================================================
// FFI FUNCTIONS - WORLD MANAGEMENT
// ============================================================================

@external(javascript, "../rapier.ffi.mjs", "createWorld")
fn create_world_ffi(
  gravity_x: Float,
  gravity_y: Float,
  gravity_z: Float,
) -> RapierWorld

@external(javascript, "../rapier.ffi.mjs", "createEventQueue")
fn create_event_queue_ffi(auto_drain: Bool) -> RapierEventQueue

@external(javascript, "../rapier.ffi.mjs", "stepWorld")
fn step_world_ffi(
  world: RapierWorld,
  queue: RapierEventQueue,
  delta_time_seconds: Float,
) -> Nil

// ============================================================================
// BODY CREATION/REMOVAL (Called by renderer)
// ============================================================================

/// Create a rigid body in the physics world
/// This is called by the renderer when a scene node with physics is added
@internal
pub fn create_body(
  world: PhysicsWorld,
  id: String,
  config: RigidBody,
  transform: Transform,
) -> PhysicsWorld {
  // Create rigid body descriptor
  let body_desc = case config.kind {
    Dynamic -> create_dynamic_body_desc_ffi()
    Kinematic -> create_kinematic_body_desc_ffi()
    Fixed -> create_fixed_body_desc_ffi()
  }

  // Set transform
  let pos = transform.position(transform)
  set_body_desc_translation_ffi(body_desc, pos.x, pos.y, pos.z)

  // Get quaternion rotation directly from transform
  let quat = transform.rotation_quaternion(transform)
  set_body_desc_rotation_ffi(body_desc, quat.x, quat.y, quat.z, quat.w)

  // Set damping
  set_linear_damping_ffi(body_desc, config.linear_damping)
  set_angular_damping_ffi(body_desc, config.angular_damping)

  // Set CCD
  case config.ccd_enabled {
    True -> set_ccd_enabled_ffi(body_desc, True)
    False -> Nil
  }

  // Set axis locks
  set_enabled_translations_ffi(
    body_desc,
    !config.axis_locks.lock_translation_x,
    !config.axis_locks.lock_translation_y,
    !config.axis_locks.lock_translation_z,
    True,
  )
  set_enabled_rotations_ffi(
    body_desc,
    !config.axis_locks.lock_rotation_x,
    !config.axis_locks.lock_rotation_y,
    !config.axis_locks.lock_rotation_z,
    True,
  )

  // Create rigid body
  let rapier_body = create_rigid_body_ffi(world.world, body_desc)

  // Create collider descriptor and extract offset
  let #(collider_desc, offset) = case config.collider {
    Box(offset, width, height, depth) -> #(
      create_cuboid_collider_desc_ffi(width /. 2.0, height /. 2.0, depth /. 2.0),
      offset,
    )
    Sphere(offset, radius) -> #(create_ball_collider_desc_ffi(radius), offset)
    Capsule(offset, half_height, radius) -> #(
      create_capsule_collider_desc_ffi(half_height, radius),
      offset,
    )
    Cylinder(offset, half_height, radius) -> #(
      create_cylinder_collider_desc_ffi(half_height, radius),
      offset,
    )
  }

  // Set collider offset (position)
  let pos = transform.position(offset)
  set_collider_translation_ffi(collider_desc, pos.x, pos.y, pos.z)

  // Set collider offset (rotation) - get quaternion directly from transform
  let quat = transform.rotation_quaternion(offset)
  set_collider_rotation_ffi(collider_desc, quat.x, quat.y, quat.z, quat.w)

  // Set collider properties
  set_collider_restitution_ffi(collider_desc, config.restitution)
  set_collider_friction_ffi(collider_desc, config.friction)

  // Set mass if provided
  case config.mass {
    option.Some(mass) -> set_collider_mass_ffi(collider_desc, mass)
    option.None -> Nil
  }

  // Set collision groups if provided
  case config.collision_groups {
    option.Some(groups) -> {
      let bitmask = collision_groups_to_bitmask(groups)
      set_collider_collision_groups_ffi(collider_desc, bitmask)
    }
    option.None -> Nil
  }

  // Enable collision events if configured
  case config.track_collision_events {
    True ->
      set_collider_active_events_ffi(collider_desc, get_active_events_ffi())
    False -> Nil
  }

  // Create collider attached to body
  let collider = create_collider_ffi(world.world, collider_desc, rapier_body)

  // Register collider handle for collision event tracking
  let collider_handle = get_collider_handle_ffi(collider)

  // Create character controller if configured
  let character_controllers = case config.character_controller {
    option.Some(controller_config) -> {
      let controller =
        create_character_controller_ffi(world.world, controller_config.offset)
      // Set up vector
      set_character_up_vector_ffi(
        controller,
        controller_config.up_vector.x,
        controller_config.up_vector.y,
        controller_config.up_vector.z,
      )
      // Set slide mode
      set_character_slide_ffi(controller, controller_config.slide_enabled)
      dict.insert(world.character_controllers, id, controller)
    }
    option.None -> world.character_controllers
  }

  // Store body and collider mapping in world
  PhysicsWorld(
    ..world,
    bodies: dict.insert(world.bodies, id, config),
    rapier_bodies: dict.insert(world.rapier_bodies, id, rapier_body),
    collider_to_body: dict.insert(world.collider_to_body, collider_handle, id),
    character_controllers: character_controllers,
  )
}

/// Update a rigid body's transform in the physics world
/// This is called by the renderer when a scene node's transform is updated
/// Primarily useful for Kinematic bodies that are controlled programmatically
@internal
pub fn update_body_transform(
  world: PhysicsWorld,
  id: String,
  transform: Transform,
) -> PhysicsWorld {
  case dict.get(world.rapier_bodies, id) {
    Ok(rapier_body) -> {
      // Get position and rotation from transform
      let position = transform.position(transform)
      let quaternion = transform.rotation_quaternion(transform)

      // Update the rigid body's position and rotation in Rapier
      // wake_up = True to ensure kinematic bodies apply their new position
      set_body_translation_ffi(
        rapier_body,
        position.x,
        position.y,
        position.z,
        True,
      )
      set_body_rotation_ffi(
        rapier_body,
        quaternion.x,
        quaternion.y,
        quaternion.z,
        quaternion.w,
        True,
      )

      world
    }
    Error(_) -> world
  }
}

/// Check if a rigid body exists in the physics world
/// This is used by the renderer to determine if a body needs to be created or updated
@internal
pub fn has_body(world: PhysicsWorld, id: String) -> Bool {
  dict.has_key(world.rapier_bodies, id)
}

/// Remove a rigid body from the physics world
/// This is called by the renderer when a scene node with physics is removed
@internal
pub fn remove_body(world: PhysicsWorld, id: String) -> PhysicsWorld {
  case dict.get(world.rapier_bodies, id) {
    Ok(rapier_body) -> {
      // Get all collider handles for this body and remove them from mapping
      let num_colliders = get_body_num_colliders_ffi(rapier_body)
      let collider_handles =
        get_body_collider_handles(rapier_body, num_colliders)

      let updated_collider_map =
        list.fold(collider_handles, world.collider_to_body, fn(map, handle) {
          dict.delete(map, handle)
        })

      // Remove the rigid body
      remove_rigid_body_ffi(world.world, rapier_body)
      PhysicsWorld(
        ..world,
        bodies: dict.delete(world.bodies, id),
        rapier_bodies: dict.delete(world.rapier_bodies, id),
        collider_to_body: updated_collider_map,
      )
    }
    Error(_) -> world
  }
}

/// Get all collider handles for a rigid body
fn get_body_collider_handles(
  body: RapierRigidBody,
  num_colliders: Int,
) -> List(Int) {
  list.range(0, num_colliders - 1)
  |> list.filter_map(fn(i) {
    case get_body_collider_ffi(body, i) {
      Ok(collider) -> Ok(get_collider_handle_ffi(collider))
      Error(_) -> Error(Nil)
    }
  })
}

// ============================================================================
// FFI DECLARATIONS - BODY/COLLIDER CREATION
// ============================================================================

@external(javascript, "../rapier.ffi.mjs", "createDynamicBodyDesc")
fn create_dynamic_body_desc_ffi() -> RapierBodyDesc

@external(javascript, "../rapier.ffi.mjs", "createKinematicBodyDesc")
fn create_kinematic_body_desc_ffi() -> RapierBodyDesc

@external(javascript, "../rapier.ffi.mjs", "createFixedBodyDesc")
fn create_fixed_body_desc_ffi() -> RapierBodyDesc

@external(javascript, "../rapier.ffi.mjs", "setBodyTranslation")
fn set_body_desc_translation_ffi(
  desc: RapierBodyDesc,
  x: Float,
  y: Float,
  z: Float,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setBodyRotation")
fn set_body_desc_rotation_ffi(
  desc: RapierBodyDesc,
  x: Float,
  y: Float,
  z: Float,
  w: Float,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setBodyTranslation2")
fn set_body_translation_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setBodyRotation2")
fn set_body_rotation_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
  w: Float,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "isBodySleeping")
fn is_body_sleeping_ffi(body: RapierRigidBody) -> Bool

@external(javascript, "../rapier.ffi.mjs", "setLinearDamping")
fn set_linear_damping_ffi(desc: RapierBodyDesc, damping: Float) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setAngularDamping")
fn set_angular_damping_ffi(desc: RapierBodyDesc, damping: Float) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setCCDEnabled")
fn set_ccd_enabled_ffi(desc: RapierBodyDesc, enabled: Bool) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setEnabledTranslations")
fn set_enabled_translations_ffi(
  desc: RapierBodyDesc,
  enable_x: Bool,
  enable_y: Bool,
  enable_z: Bool,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setEnabledRotations")
fn set_enabled_rotations_ffi(
  desc: RapierBodyDesc,
  enable_x: Bool,
  enable_y: Bool,
  enable_z: Bool,
  wake_up: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "createRigidBody")
fn create_rigid_body_ffi(
  world: RapierWorld,
  desc: RapierBodyDesc,
) -> RapierRigidBody

@external(javascript, "../rapier.ffi.mjs", "removeRigidBody")
fn remove_rigid_body_ffi(world: RapierWorld, body: RapierRigidBody) -> Nil

@external(javascript, "../rapier.ffi.mjs", "createCuboidColliderDesc")
fn create_cuboid_collider_desc_ffi(
  hx: Float,
  hy: Float,
  hz: Float,
) -> RapierColliderDesc

@external(javascript, "../rapier.ffi.mjs", "createBallColliderDesc")
fn create_ball_collider_desc_ffi(radius: Float) -> RapierColliderDesc

@external(javascript, "../rapier.ffi.mjs", "createCapsuleColliderDesc")
fn create_capsule_collider_desc_ffi(
  half_height: Float,
  radius: Float,
) -> RapierColliderDesc

@external(javascript, "../rapier.ffi.mjs", "createCylinderColliderDesc")
fn create_cylinder_collider_desc_ffi(
  half_height: Float,
  radius: Float,
) -> RapierColliderDesc

@external(javascript, "../rapier.ffi.mjs", "setColliderRestitution")
fn set_collider_restitution_ffi(
  desc: RapierColliderDesc,
  restitution: Float,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setColliderFriction")
fn set_collider_friction_ffi(desc: RapierColliderDesc, friction: Float) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setColliderMass")
fn set_collider_mass_ffi(desc: RapierColliderDesc, mass: Float) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setColliderCollisionGroups")
fn set_collider_collision_groups_ffi(
  desc: RapierColliderDesc,
  groups: Int,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setColliderActiveEvents")
fn set_collider_active_events_ffi(desc: RapierColliderDesc, events: Int) -> Nil

@external(javascript, "../rapier.ffi.mjs", "getActiveEvents")
fn get_active_events_ffi() -> Int

@external(javascript, "../rapier.ffi.mjs", "setColliderTranslation")
fn set_collider_translation_ffi(
  desc: RapierColliderDesc,
  x: Float,
  y: Float,
  z: Float,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setColliderRotation")
fn set_collider_rotation_ffi(
  desc: RapierColliderDesc,
  x: Float,
  y: Float,
  z: Float,
  w: Float,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "createCollider")
fn create_collider_ffi(
  world: RapierWorld,
  desc: RapierColliderDesc,
  body: RapierRigidBody,
) -> RapierCollider

@external(javascript, "../rapier.ffi.mjs", "getColliderHandle")
fn get_collider_handle_ffi(collider: RapierCollider) -> Int

@external(javascript, "../rapier.ffi.mjs", "getBodyNumColliders")
fn get_body_num_colliders_ffi(body: RapierRigidBody) -> Int

@external(javascript, "../rapier.ffi.mjs", "getBodyCollider")
fn get_body_collider_ffi(
  body: RapierRigidBody,
  index: Int,
) -> Result(RapierCollider, Nil)

// Collision event draining FFI
// Returns list of tuples: (collider_handle1, collider_handle2, started)
@external(javascript, "../rapier.ffi.mjs", "drainCollisionEventsToList")
fn drain_collision_events_ffi(
  queue: RapierEventQueue,
) -> List(#(Int, Int, Bool))

type RapierBodyDesc

type RapierColliderDesc

type RapierCollider

// Character Controller FFI
@external(javascript, "../rapier.ffi.mjs", "createCharacterController")
fn create_character_controller_ffi(
  world: RapierWorld,
  offset: Float,
) -> RapierCharacterController

@external(javascript, "../rapier.ffi.mjs", "setCharacterUpVector")
fn set_character_up_vector_ffi(
  controller: RapierCharacterController,
  x: Float,
  y: Float,
  z: Float,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "setCharacterSlide")
fn set_character_slide_ffi(
  controller: RapierCharacterController,
  enabled: Bool,
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "computeCharacterMovement")
fn compute_character_movement_ffi(
  world: RapierWorld,
  controller: RapierCharacterController,
  collider: RapierCollider,
  desired_translation: Vec3(Float),
) -> Nil

@external(javascript, "../rapier.ffi.mjs", "getCharacterComputedMovement")
fn get_character_computed_movement_ffi(
  controller: RapierCharacterController,
) -> Vec3(Float)

@external(javascript, "../rapier.ffi.mjs", "getCharacterComputedGrounded")
fn get_character_computed_grounded_ffi(
  controller: RapierCharacterController,
) -> Bool

@external(javascript, "../rapier.ffi.mjs", "setBodyNextKinematicTranslation")
fn set_body_next_kinematic_translation_ffi(
  body: RapierRigidBody,
  x: Float,
  y: Float,
  z: Float,
) -> Nil

// Helper to create Vec3 object for FFI
fn create_vec3_object(x: Float, y: Float, z: Float) -> Vec3(Float) {
  vec3.Vec3(x, y, z)
}
