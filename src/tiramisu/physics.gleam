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
import tiramisu/vec3.{type Vec3}

// --- Opaque Types ---

/// Opaque handle to the Rapier physics world
/// This is part of your Model and gets updated each frame
pub opaque type PhysicsWorld {
  PhysicsWorld(bodies: Dict(String, PhysicsBodyState))
}

/// Internal state tracking for a physics body
type PhysicsBodyState {
  PhysicsBodyState(
    body: RigidBody,
    // Current transform from physics simulation
    current_transform: Transform,
  )
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
    gravity: Vec3,
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

  // Sync all bodies from Rapier to PhysicsWorld
  let updated_bodies =
    list.fold(all_body_ids, world.bodies, fn(bodies_dict, id) {
      case get_body_transform_ffi(id) {
        option.Some(transform) -> {
          // Get existing body config or create placeholder
          let body_config = case dict.get(bodies_dict, id) {
            Ok(state) -> state.body
            Error(_) -> {
              // Body exists in Rapier but not tracked yet
              // This happens on first frame after patch creates it
              // Use a placeholder - the actual config is in Rapier
              rigid_body(Dynamic, Box(1.0, 1.0, 1.0))
            }
          }
          let new_state =
            PhysicsBodyState(body: body_config, current_transform: transform)
          dict.insert(bodies_dict, id, new_state)
        }
        option.None -> {
          // Body not found in Rapier
          bodies_dict
        }
      }
    })

  PhysicsWorld(bodies: updated_bodies)
}

/// Get the current transform of a physics body
/// Use this to sync physics transforms back to your scene graph
pub fn get_transform(world: PhysicsWorld, id: String) -> Option(Transform) {
  case dict.get(world.bodies, id) {
    Ok(state) -> option.Some(state.current_transform)
    Error(_) -> option.None
  }
}

/// Register a physics body in the world
/// Call this when you create a scene node with physics
pub fn register_body(
  world: PhysicsWorld,
  id: String,
  body: RigidBody,
  initial_transform: Transform,
) -> PhysicsWorld {
  let state = PhysicsBodyState(body: body, current_transform: initial_transform)
  PhysicsWorld(bodies: dict.insert(world.bodies, id, state))
}

/// Unregister a physics body from the world
pub fn unregister_body(world: PhysicsWorld, id: String) -> PhysicsWorld {
  PhysicsWorld(bodies: dict.delete(world.bodies, id))
}

// --- Body Management (internal, used by diff/patch) ---

/// Add a body to the physics world (internal use)
pub fn add_body(
  world: PhysicsWorld,
  id: String,
  body: RigidBody,
  initial_transform: Transform,
) -> PhysicsWorld {
  // Create body in Rapier via FFI
  create_rigid_body_ffi(id, body, initial_transform)

  // Track it in our state
  let state = PhysicsBodyState(body: body, current_transform: initial_transform)
  PhysicsWorld(bodies: dict.insert(world.bodies, id, state))
}

/// Remove a body from the physics world (internal use)
pub fn remove_body(world: PhysicsWorld, id: String) -> PhysicsWorld {
  // Remove from Rapier via FFI
  remove_rigid_body_ffi(id)

  // Remove from our tracking
  PhysicsWorld(bodies: dict.delete(world.bodies, id))
}

/// Update a body's properties (internal use)
pub fn update_body(
  world: PhysicsWorld,
  id: String,
  body: RigidBody,
) -> PhysicsWorld {
  // For now, remove and recreate
  // TODO: More efficient updates for specific properties
  case dict.get(world.bodies, id) {
    Ok(state) -> {
      let world = remove_body(world, id)
      add_body(world, id, body, state.current_transform)
    }
    Error(_) -> world
  }
}

// --- Forces and Impulses ---

/// Apply a force to a rigid body (use in update function)
pub fn apply_force(id: String, force: Vec3) -> Nil {
  apply_force_ffi(id, force)
}

/// Apply an impulse to a rigid body (use in update function)
pub fn apply_impulse(id: String, impulse: Vec3) -> Nil {
  apply_impulse_ffi(id, impulse)
}

/// Set the velocity of a rigid body (use in update function)
pub fn set_velocity(id: String, velocity: Vec3) -> Nil {
  set_velocity_ffi(id, velocity)
}

/// Get the velocity of a rigid body
pub fn get_velocity(id: String) -> Option(Vec3) {
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
fn apply_force_ffi(id: String, force: Vec3) -> Nil

@external(javascript, "./ffi/physics.mjs", "applyImpulse")
fn apply_impulse_ffi(id: String, impulse: Vec3) -> Nil

@external(javascript, "./ffi/physics.mjs", "setVelocity")
fn set_velocity_ffi(id: String, velocity: Vec3) -> Nil

@external(javascript, "./ffi/physics.mjs", "getVelocity")
fn get_velocity_ffi(id: String) -> Option(Vec3)

@external(javascript, "./ffi/physics.mjs", "getAllBodyIds")
fn get_all_body_ids_ffi() -> List(String)
