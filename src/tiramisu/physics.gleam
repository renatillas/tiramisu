//// Physics module using Rapier physics engine
////
//// Provides declarative physics simulation following the same immutable,
//// diff/patch pattern as the rest of Tiramisu.
////
//// Physics bodies are declared alongside scene nodes, and the physics world
//// is managed as part of the game's Model state.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import structures/bimap
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}

// --- Opaque Types ---

/// Opaque handle to the Rapier physics world
/// This is part of your Model and gets updated each frame
pub opaque type PhysicsWorld(body) {
  PhysicsWorld(
    bodies: Dict(body, PhysicsBodyState),
    /// Commands to be applied during the next physics step
    pending_commands: List(PhysicsCommand(body)),
    bimap: bimap.BiMap(body, String),
  )
}

/// Internal state tracking for a physics body
/// Stores only the body configuration metadata
/// Transforms are always queried directly from Rapier
type PhysicsBodyState {
  PhysicsBodyState(body: RigidBody)
}

/// Physics commands that can be queued and applied during step
type PhysicsCommand(body) {
  ApplyForce(id: body, force: Vec3(Float))
  ApplyImpulse(id: body, impulse: Vec3(Float))
  SetVelocity(id: body, velocity: Vec3(Float))
  SetAngularVelocity(id: body, velocity: Vec3(Float))
  ApplyTorque(id: body, torque: Vec3(Float))
  ApplyTorqueImpulse(id: body, impulse: Vec3(Float))
}

// --- Public Types ---

/// Physics body type
pub type BodyType {
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
  Box(width: Float, height: Float, depth: Float)
  /// Sphere collider with radius
  Sphere(radius: Float)
  /// Capsule collider (cylinder with rounded caps)
  Capsule(half_height: Float, radius: Float)
  /// Cylinder collider
  Cylinder(half_height: Float, radius: Float)
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

/// Physics body configuration (immutable, declarative)
pub type RigidBody {
  RigidBody(
    /// Type of rigid body
    body_type: BodyType,
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
  )
}

/// Physics world configuration
pub type WorldConfig(body) {
  WorldConfig(
    /// Gravity vector (typically Vec3(0.0, -9.81, 0.0))
    gravity: Vec3(Float),
    correspondances: List(#(body, String)),
  )
}

/// Result of a raycast hit
pub type RaycastHit(id) {
  RaycastHit(
    /// ID of the body that was hit
    id: id,
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

/// Create a new physics world (call this in your init function)
pub fn new_world(config: WorldConfig(body)) -> PhysicsWorld(body) {
  // Initialize the Rapier world via FFI
  init_world_ffi(config)
  let bimap = create_bimap(config.correspondances)
  PhysicsWorld(bodies: dict.new(), pending_commands: [], bimap:)
}

fn create_bimap(correspondances: List(#(body, String))) {
  list.fold(over: correspondances, from: bimap.new(), with: fn(acc, item) {
    let #(body, string) = item
    bimap.insert(acc, body, string)
  })
}

/// Create a new rigid body with default settings
pub fn rigid_body(body_type: BodyType, collider: ColliderShape) -> RigidBody {
  RigidBody(
    body_type: body_type,
    mass: option.None,
    restitution: 0.3,
    friction: 0.5,
    linear_damping: 0.0,
    angular_damping: 0.0,
    collider: collider,
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
  )
}

/// Set mass for a rigid body
pub fn set_mass(body: RigidBody, mass: Float) -> RigidBody {
  RigidBody(..body, mass: option.Some(mass))
}

/// Set restitution (bounciness)
pub fn set_restitution(body: RigidBody, restitution: Float) -> RigidBody {
  RigidBody(..body, restitution: restitution)
}

/// Set friction
pub fn set_friction(body: RigidBody, friction: Float) -> RigidBody {
  RigidBody(..body, friction: friction)
}

/// Set linear damping
pub fn set_linear_damping(body: RigidBody, damping: Float) -> RigidBody {
  RigidBody(..body, linear_damping: damping)
}

/// Set angular damping
pub fn set_angular_damping(body: RigidBody, damping: Float) -> RigidBody {
  RigidBody(..body, angular_damping: damping)
}

/// Enable continuous collision detection
pub fn enable_ccd(body: RigidBody) -> RigidBody {
  RigidBody(..body, ccd_enabled: True)
}

// --- Builder Pattern for Rigid Bodies ---

/// Builder for creating rigid bodies with a fluent API
pub opaque type RigidBodyBuilder {
  RigidBodyBuilder(
    body_type: BodyType,
    collider: option.Option(ColliderShape),
    mass: option.Option(Float),
    restitution: Float,
    friction: Float,
    linear_damping: Float,
    angular_damping: Float,
    ccd_enabled: Bool,
    axis_locks: AxisLock,
    collision_groups: option.Option(CollisionGroups),
  )
}

/// Create a new rigid body builder
///
/// ## Example
///
/// ```gleam
/// let body = physics.new_rigid_body(physics.Dynamic)
///   |> physics.body_collider(physics.Box(2.0, 2.0, 2.0))
///   |> physics.body_mass(5.0)
///   |> physics.build_body()
/// ```
pub fn new_rigid_body(body_type: BodyType) -> RigidBodyBuilder {
  RigidBodyBuilder(
    body_type: body_type,
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
  )
}

/// Set the collider shape for the rigid body
pub fn body_collider(
  builder: RigidBodyBuilder,
  collider: ColliderShape,
) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, collider: option.Some(collider))
}

/// Set the mass for the rigid body
pub fn body_mass(builder: RigidBodyBuilder, mass: Float) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, mass: option.Some(mass))
}

/// Set the restitution (bounciness) for the rigid body
pub fn body_restitution(
  builder: RigidBodyBuilder,
  restitution: Float,
) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, restitution: restitution)
}

/// Set the friction for the rigid body
pub fn body_friction(
  builder: RigidBodyBuilder,
  friction: Float,
) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, friction: friction)
}

/// Set the linear damping for the rigid body
pub fn body_linear_damping(
  builder: RigidBodyBuilder,
  damping: Float,
) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, linear_damping: damping)
}

/// Set the angular damping for the rigid body
pub fn body_angular_damping(
  builder: RigidBodyBuilder,
  damping: Float,
) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, angular_damping: damping)
}

/// Enable continuous collision detection for the rigid body
pub fn enable_body_ccd(builder: RigidBodyBuilder) -> RigidBodyBuilder {
  RigidBodyBuilder(..builder, ccd_enabled: True)
}

/// Lock translation on the X axis
pub fn lock_translation_x(builder: RigidBodyBuilder) -> RigidBodyBuilder {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_translation_x: True),
  )
}

/// Lock translation on the Y axis
pub fn lock_translation_y(builder: RigidBodyBuilder) -> RigidBodyBuilder {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_translation_y: True),
  )
}

/// Lock translation on the Z axis
pub fn lock_translation_z(builder: RigidBodyBuilder) -> RigidBodyBuilder {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_translation_z: True),
  )
}

/// Lock rotation on the X axis (pitch)
pub fn lock_rotation_x(builder: RigidBodyBuilder) -> RigidBodyBuilder {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_rotation_x: True),
  )
}

/// Lock rotation on the Y axis (yaw)
pub fn lock_rotation_y(builder: RigidBodyBuilder) -> RigidBodyBuilder {
  let locks = builder.axis_locks
  RigidBodyBuilder(
    ..builder,
    axis_locks: AxisLock(..locks, lock_rotation_y: True),
  )
}

/// Lock rotation on the Z axis (roll)
pub fn lock_rotation_z(builder: RigidBodyBuilder) -> RigidBodyBuilder {
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
pub fn body_collision_groups(
  builder: RigidBodyBuilder,
  membership membership: List(Int),
  filter filter: List(Int),
) -> RigidBodyBuilder {
  RigidBodyBuilder(
    ..builder,
    collision_groups: option.Some(CollisionGroups(
      membership: membership,
      filter: filter,
    )),
  )
}

/// Build the final rigid body from the builder
///
/// Returns an error if no collider was set.
pub fn build(builder: RigidBodyBuilder) -> Result(RigidBody, String) {
  case builder.collider {
    option.Some(collider) ->
      Ok(RigidBody(
        body_type: builder.body_type,
        mass: builder.mass,
        restitution: builder.restitution,
        friction: builder.friction,
        linear_damping: builder.linear_damping,
        angular_damping: builder.angular_damping,
        collider: collider,
        ccd_enabled: builder.ccd_enabled,
        axis_locks: builder.axis_locks,
        collision_groups: builder.collision_groups,
      ))
    option.None -> Error("Collider shape is required for rigid body")
  }
}

// --- Physics Simulation (called each frame in update) ---

/// Sync a single body ID with the body registry
/// Returns updated dict with body added if needed
fn sync_body_id(
  bodies_dict: Dict(body, PhysicsBodyState),
  id: body,
) -> Dict(body, PhysicsBodyState) {
  case dict.has_key(bodies_dict, id) {
    True -> bodies_dict
    False -> {
      // Body exists in Rapier but not tracked yet
      // This happens on first frame after patch creates it
      // Use a placeholder config - the actual config is in Rapier
      let placeholder_body = rigid_body(Dynamic, Box(1.0, 1.0, 1.0))
      let state = PhysicsBodyState(body: placeholder_body)
      dict.insert(bodies_dict, id, state)
    }
  }
}

/// Apply a single physics command via FFI
fn apply_command(
  command: PhysicsCommand(body),
  bimap: bimap.BiMap(body, String),
) -> Result(Nil, Nil) {
  // Extract the ID and convert to string using BiMap
  case command {
    ApplyForce(id, force) -> {
      case bimap.get(bimap, id) {
        Ok(string_id) -> {
          apply_force_ffi(string_id, force)
          Ok(Nil)
        }
        Error(_) -> Error(Nil)
      }
    }
    ApplyImpulse(id, impulse) -> {
      case bimap.get(bimap, id) {
        Ok(string_id) -> {
          apply_impulse_ffi(string_id, impulse)
          Ok(Nil)
        }
        Error(_) -> Error(Nil)
      }
    }
    SetVelocity(id, velocity) -> {
      case bimap.get(bimap, id) {
        Ok(string_id) -> {
          set_velocity_ffi(string_id, velocity)
          Ok(Nil)
        }
        Error(_) -> Error(Nil)
      }
    }
    SetAngularVelocity(id, velocity) -> {
      case bimap.get(bimap, id) {
        Ok(string_id) -> {
          set_angular_velocity_ffi(string_id, velocity)
          Ok(Nil)
        }
        Error(_) -> Error(Nil)
      }
    }
    ApplyTorque(id, torque) -> {
      case bimap.get(bimap, id) {
        Ok(string_id) -> {
          apply_torque_ffi(string_id, torque)
          Ok(Nil)
        }
        Error(_) -> Error(Nil)
      }
    }
    ApplyTorqueImpulse(id, impulse) -> {
      case bimap.get(bimap, id) {
        Ok(string_id) -> {
          apply_torque_impulse_ffi(string_id, impulse)
          Ok(Nil)
        }
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Step the physics simulation forward
/// This should be called in your update function each frame
/// Returns updated world with new transforms for all bodies
pub fn step(world: PhysicsWorld(body), delta_time: Float) -> PhysicsWorld(body) {
  // Apply all pending commands before stepping (reverse to apply in FIFO order)
  list.each(list.reverse(world.pending_commands), fn(command) {
    let _ = apply_command(command, world.bimap)
    Nil
  })

  // Step the physics simulation via FFI
  step_world_ffi(delta_time)

  // Get all body IDs that exist in Rapier
  let all_body_ids =
    list.map(get_all_body_ids_ffi(), fn(item) {
      bimap.get_val(world.bimap, item)
    })
    |> result.values
  // Sync body registry - add any bodies that exist in Rapier but not in our dict
  // This handles bodies created via scene patches
  let updated_bodies = list.fold(all_body_ids, world.bodies, sync_body_id)

  // Return world with cleared commands
  PhysicsWorld(..world, bodies: updated_bodies, pending_commands: [])
}

/// Get the current transform of a rigid body.
///
/// Queries the physics simulation directly, so it always returns the latest position
/// even for bodies that were just created in the current frame.
///
/// ## Example
///
/// ```gleam
/// let cube_transform = case physics.get_transform("cube1") {
///   Ok(t) -> t
///   Error(_) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
/// }
/// ```
pub fn get_transform(
  physics_world: PhysicsWorld(id),
  id: id,
) -> Result(Transform, Nil) {
  bimap.get(physics_world.bimap, id)
  |> result.try(get_body_transform_ffi)
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
  world: PhysicsWorld(body),
  id: body,
  force: Vec3(Float),
) -> PhysicsWorld(body) {
  let command = ApplyForce(id:, force:)
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
  world: PhysicsWorld(body),
  id: body,
  impulse: Vec3(Float),
) -> PhysicsWorld(body) {
  let command = ApplyImpulse(id:, impulse:)
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Queue a velocity change for a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn set_velocity(
  world: PhysicsWorld(body),
  id: body,
  velocity: Vec3(Float),
) -> PhysicsWorld(body) {
  let command = SetVelocity(id:, velocity:)
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Get the current velocity of a rigid body (queries immediately)
pub fn get_velocity(id: String) -> Result(Vec3(Float), Nil) {
  get_velocity_ffi(id)
}

/// Queue an angular velocity change for a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn set_angular_velocity(
  world: PhysicsWorld(body),
  id: body,
  velocity: Vec3(Float),
) -> PhysicsWorld(body) {
  let command = SetAngularVelocity(id:, velocity:)
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Get the current angular velocity of a rigid body (queries immediately)
pub fn get_angular_velocity(
  world: PhysicsWorld(body),
  id: body,
) -> Result(Vec3(Float), Nil) {
  bimap.get(world.bimap, id)
  |> result.try(get_angular_velocity_ffi)
}

/// Queue a torque to be applied to a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn apply_torque(
  world: PhysicsWorld(body),
  id: body,
  torque: Vec3(Float),
) -> PhysicsWorld(body) {
  let command = ApplyTorque(id:, torque:)
  PhysicsWorld(..world, bodies: world.bodies, pending_commands: [
    command,
    ..world.pending_commands
  ])
}

/// Queue a torque impulse to be applied to a rigid body during the next physics step.
/// Returns updated world with the command queued.
pub fn apply_torque_impulse(
  world: PhysicsWorld(body),
  id: body,
  impulse: Vec3(Float),
) -> PhysicsWorld(body) {
  let command = ApplyTorqueImpulse(id:, impulse:)
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
  world: PhysicsWorld(id),
  origin origin: Vec3(Float),
  direction direction: Vec3(Float),
  max_distance max_distance: Float,
) -> Result(RaycastHit(id), Nil) {
  // Call the FFI which returns RaycastHit with string ID
  case raycast_ffi(origin, direction, max_distance) {
    Ok(hit) -> {
      // Convert string ID back to typed ID using BiMap
      case bimap.get_val(world.bimap, hit.id) {
        Ok(typed_id) ->
          Ok(RaycastHit(
            id: typed_id,
            point: hit.point,
            normal: hit.normal,
            distance: hit.distance,
          ))
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Cast a ray and return all hits along the ray
///
/// Returns hits sorted by distance (closest first).
///
/// ## Example
///
/// ```gleam
/// // Check what's in front of the player
/// let hits = physics.raycast_all(origin, direction, max_distance: 100.0)
///
/// // Find first enemy hit
/// let enemy_hit = list.find(hits, fn(hit) {
///   string.starts_with(hit.body_id, "enemy_")
/// })
/// ```
pub fn raycast_all(
  origin origin: Vec3(Float),
  direction direction: Vec3(Float),
  max_distance max_distance: Float,
) -> List(RaycastHit(id)) {
  raycast_all_ffi(origin, direction, max_distance)
}

// --- Collision Events ---

/// Get collision events that occurred during the last physics step
///
/// Call this after `physics.step()` to get all collisions that occurred.
///
/// ## Example
///
/// ```gleam
/// // In your update function
/// let world = physics.step(world, ctx.delta_time)
/// let events = physics.get_collision_events()
///
/// // Process collision events
/// list.each(events, fn(event) {
///   case event {
///     physics.CollisionStarted("player", enemy_id) -> {
///       // Player hit an enemy, apply damage
///     }
///     physics.CollisionStarted("bullet", enemy_id) -> {
///       // Bullet hit an enemy
///     }
///     _ -> Nil
///   }
/// })
/// ```
pub fn get_collision_events() -> List(CollisionEvent) {
  get_collision_events_ffi()
}

/// Check if two specific bodies are currently colliding
///
/// ## Example
///
/// ```gleam
/// if physics.are_colliding("player", "ground") {
///   // Player is on the ground, allow jumping
/// }
/// ```
pub fn are_colliding(body_a: String, body_b: String) -> Bool {
  are_colliding_ffi(body_a, body_b)
}

// --- Helper Functions for FFI ---

/// Convert a typed ID to a string ID using the BiMap
/// This is called from JavaScript when creating/removing physics bodies
@internal
pub fn id_to_string(world: PhysicsWorld(id), id: id) -> Result(String, Nil) {
  bimap.get(world.bimap, id)
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

@external(javascript, "./ffi/physics.mjs", "initWorld")
fn init_world_ffi(config: WorldConfig(body)) -> Nil

@external(javascript, "./ffi/physics.mjs", "stepWorld")
fn step_world_ffi(delta_time: Float) -> Nil

@external(javascript, "./ffi/physics.mjs", "getBodyTransform")
fn get_body_transform_ffi(id: String) -> Result(Transform, Nil)

@external(javascript, "./ffi/physics.mjs", "applyForce")
fn apply_force_ffi(id: String, force: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "applyImpulse")
fn apply_impulse_ffi(id: String, impulse: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "setVelocity")
fn set_velocity_ffi(id: String, velocity: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "getVelocity")
fn get_velocity_ffi(id: String) -> Result(Vec3(Float), Nil)

@external(javascript, "./ffi/physics.mjs", "setAngularVelocity")
fn set_angular_velocity_ffi(id: String, velocity: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "getAngularVelocity")
fn get_angular_velocity_ffi(id: String) -> Result(Vec3(Float), Nil)

@external(javascript, "./ffi/physics.mjs", "applyTorque")
fn apply_torque_ffi(id: String, torque: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "applyTorqueImpulse")
fn apply_torque_impulse_ffi(id: String, impulse: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "raycast")
fn raycast_ffi(
  origin: Vec3(Float),
  direction: Vec3(Float),
  max_distance: Float,
) -> Result(RaycastHit(id), Nil)

@external(javascript, "./ffi/physics.mjs", "raycastAll")
fn raycast_all_ffi(
  origin: Vec3(Float),
  direction: Vec3(Float),
  max_distance: Float,
) -> List(RaycastHit(id))

@external(javascript, "./ffi/physics.mjs", "getCollisionEvents")
fn get_collision_events_ffi() -> List(CollisionEvent)

@external(javascript, "./ffi/physics.mjs", "areColliding")
fn are_colliding_ffi(body_a: String, body_b: String) -> Bool

@external(javascript, "./ffi/physics.mjs", "getAllBodyIds")
fn get_all_body_ids_ffi() -> List(String)
