/// Physics module using Rapier physics engine
///
/// Provides declarative physics simulation following the same immutable,
/// diff/patch pattern as the rest of Tiramisu.
///
/// Physics bodies are declared alongside scene nodes, and the physics world
/// is managed as part of the game's Model state.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import tiramisu/transform.{type Transform}
import vec/vec3.{type Vec3}

// --- Opaque Types ---

/// Opaque handle to the Rapier physics world
/// This is part of your Model and gets updated each frame
pub opaque type PhysicsWorld {
  PhysicsWorld(bodies: Dict(String, PhysicsBodyState))
}

/// Internal state tracking for a physics body
/// Stores only the body configuration metadata
/// Transforms are always queried directly from Rapier
type PhysicsBodyState {
  PhysicsBodyState(body: RigidBody)
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

/// Physics body configuration (immutable, declarative)
pub type RigidBody {
  RigidBody(
    /// Type of rigid body
    body_type: BodyType,
    /// Mass (only for Dynamic bodies)
    mass: Option(Float),
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
  )
}

/// Physics world configuration
pub type WorldConfig {
  WorldConfig(
    /// Gravity vector (typically Vec3(0.0, -9.81, 0.0))
    gravity: Vec3(Float),
  )
}

// --- Constructor Functions ---

/// Create a new physics world (call this in your init function)
pub fn new_world(config: WorldConfig) -> PhysicsWorld {
  // Initialize the Rapier world via FFI
  init_world_ffi(config)
  PhysicsWorld(bodies: dict.new())
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

// --- Physics Simulation (called each frame in update) ---

/// Step the physics simulation forward
/// This should be called in your update function each frame
/// Returns updated world with new transforms for all bodies
pub fn step(world: PhysicsWorld, delta_time: Float) -> PhysicsWorld {
  // Step the physics simulation via FFI
  step_world_ffi(delta_time)

  // Get all body IDs that exist in Rapier
  let all_body_ids = get_all_body_ids_ffi()

  // Sync body registry - add any bodies that exist in Rapier but not in our dict
  // This handles bodies created via scene patches
  let updated_bodies =
    list.fold(all_body_ids, world.bodies, fn(bodies_dict, id) {
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
    })

  PhysicsWorld(bodies: updated_bodies)
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
pub fn get_transform(id: String) -> Result(Transform, Nil) {
  case get_body_transform_ffi(id) {
    option.Some(transform) -> Ok(transform)
    option.None -> Error(Nil)
  }
}

/// Register a physics body in the world.
///
/// **Note:** You typically don't need to call this manually! Bodies are automatically
/// created when you add scene nodes with physics. This function is mainly for advanced
/// use cases where you need to manually create physics bodies outside of the scene graph.
pub fn register_body(
  world: PhysicsWorld,
  id: String,
  body: RigidBody,
  initial_transform: Transform,
) -> PhysicsWorld {
  // Create body in Rapier via FFI
  create_rigid_body_ffi(id, body, initial_transform)

  // Track it in our state (stores body config metadata)
  let state = PhysicsBodyState(body: body)
  PhysicsWorld(bodies: dict.insert(world.bodies, id, state))
}

/// Unregister a physics body from the world
pub fn unregister_body(world: PhysicsWorld, id: String) -> PhysicsWorld {
  remove_body(world, id)
}

@internal
pub fn remove_body(world: PhysicsWorld, id: String) -> PhysicsWorld {
  // Remove from Rapier via FFI
  remove_rigid_body_ffi(id)
  // Remove from our tracking
  PhysicsWorld(bodies: dict.delete(world.bodies, id))
}

@internal
pub fn update_body(
  world: PhysicsWorld,
  id: String,
  body: RigidBody,
) -> PhysicsWorld {
  case dict.get(world.bodies, id) {
    Ok(_state) -> {
      // Query current transform from Rapier before removing
      let current_transform = case get_body_transform_ffi(id) {
        option.Some(transform) -> transform
        option.None -> transform.identity
      }
      let world = remove_body(world, id)
      register_body(world, id, body, current_transform)
    }
    Error(_) -> world
  }
}

// --- Forces and Impulses ---

/// Apply a force to a rigid body (use in update function)
pub fn apply_force(id: String, force: Vec3(Float)) -> Nil {
  apply_force_ffi(id, force)
}

/// Apply an impulse to a rigid body (use in update function)
pub fn apply_impulse(id: String, impulse: Vec3(Float)) -> Nil {
  apply_impulse_ffi(id, impulse)
}

/// Set the velocity of a rigid body (use in update function)
pub fn set_velocity(id: String, velocity: Vec3(Float)) -> Nil {
  set_velocity_ffi(id, velocity)
}

/// Get the velocity of a rigid body
pub fn get_velocity(id: String) -> Option(Vec3(Float)) {
  get_velocity_ffi(id)
}

// --- FFI Functions ---

@external(javascript, "./ffi/physics.mjs", "initWorld")
fn init_world_ffi(config: WorldConfig) -> Nil

@external(javascript, "./ffi/physics.mjs", "stepWorld")
fn step_world_ffi(delta_time: Float) -> Nil

@external(javascript, "./ffi/physics.mjs", "createRigidBody")
fn create_rigid_body_ffi(
  id: String,
  body: RigidBody,
  transform: Transform,
) -> Nil

@external(javascript, "./ffi/physics.mjs", "removeRigidBody")
fn remove_rigid_body_ffi(id: String) -> Nil

@external(javascript, "./ffi/physics.mjs", "getBodyTransform")
fn get_body_transform_ffi(id: String) -> Option(Transform)

@external(javascript, "./ffi/physics.mjs", "applyForce")
fn apply_force_ffi(id: String, force: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "applyImpulse")
fn apply_impulse_ffi(id: String, impulse: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "setVelocity")
fn set_velocity_ffi(id: String, velocity: Vec3(Float)) -> Nil

@external(javascript, "./ffi/physics.mjs", "getVelocity")
fn get_velocity_ffi(id: String) -> Option(Vec3(Float))

@external(javascript, "./ffi/physics.mjs", "getAllBodyIds")
fn get_all_body_ids_ffi() -> List(String)
