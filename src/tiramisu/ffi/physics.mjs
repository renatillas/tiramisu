/// Physics FFI - Rapier integration
///
/// This module manages a single global physics world and provides
/// functions for creating/removing bodies and stepping simulation.

import RAPIER from '@dimforge/rapier3d-compat';
import { toList, Ok, Error } from '../../../gleam_stdlib/gleam.mjs';
import { Transform } from '../transform.mjs';
import { Vec3 } from '../../../vec/vec/vec3.mjs';
import { RaycastHit, CollisionStarted, CollisionEnded, collision_groups_to_bitmask, id_to_string } from '../physics.mjs';

// Initialize Rapier (must be called before creating world)
await RAPIER.init();

// Global physics world instance
let physicsWorld = null;

// Map of body IDs to Rapier rigid bodies
const bodyMap = new Map();

// Event queue for collision events
let collisionEvents = [];

// Reverse map: Rapier body handle to ID
const handleToId = new Map();

/**
 * Initialize a physics world with configuration
 * @param {WorldConfig} config - World configuration from Gleam
 */
export function initWorld(config) {
  const gravity = {
    x: config.gravity.x,
    y: config.gravity.y,
    z: config.gravity.z
  };

  physicsWorld = new RAPIER.World(gravity);

  // Set up event queue for collision detection
  const eventQueue = new RAPIER.EventQueue(true);
  physicsWorld.eventQueue = eventQueue;
}

/**
 * Step the physics simulation
 * @param {number} deltaTime - Time step in seconds
 */
export function stepWorld(deltaTime) {
  if (physicsWorld) {
    // Clear previous collision events
    collisionEvents = [];

    // Step the simulation
    physicsWorld.step(physicsWorld.eventQueue);

    // Process collision events
    physicsWorld.eventQueue.drainCollisionEvents((handle1, handle2, started) => {
      const id1 = handleToId.get(handle1);
      const id2 = handleToId.get(handle2);

      if (id1 && id2) {
        if (started) {
          collisionEvents.push(new CollisionStarted(id1, id2));
        } else {
          collisionEvents.push(new CollisionEnded(id1, id2));
        }
      }
    });
  }
}

/**
 * Create a rigid body in the physics world
 * @param {PhysicsWorld} physicsWorldGleam - Gleam PhysicsWorld containing BiMap
 * @param {any} id - Unique identifier for this body (typed or string)
 * @param {RigidBody} bodyConfig - Body configuration from Gleam
 * @param {Transform} transform - Initial transform
 */
export function createRigidBody(physicsWorldGleam, id, bodyConfig, transform) {
  if (!physicsWorld) {
    console.warn('[Physics] createRigidBody called but physicsWorld is null:', id);
    return;
  }

  // Convert typed ID to string using Gleam's id_to_string function
  const stringIdResult = id_to_string(physicsWorldGleam, id);
  if (!stringIdResult || !stringIdResult[0]) {
    console.warn('[Physics] No string ID mapping found for:', id);
    return;
  }
  const actualId = stringIdResult[0]; // Extract from Result.Ok


  // Create rigid body descriptor based on type
  let bodyDesc;
  switch (bodyConfig.body_type.constructor.name) {
    case 'Dynamic':
      bodyDesc = RAPIER.RigidBodyDesc.dynamic();
      break;
    case 'Kinematic':
      bodyDesc = RAPIER.RigidBodyDesc.kinematicPositionBased();
      break;
    case 'Fixed':
      bodyDesc = RAPIER.RigidBodyDesc.fixed();
      break;
    default:
      console.warn('[Physics] Unknown body type, defaulting to Dynamic');
      bodyDesc = RAPIER.RigidBodyDesc.dynamic();
  }

  // Set initial position from transform
  bodyDesc.setTranslation(
    transform.position.x,
    transform.position.y,
    transform.position.z
  );

  // TODO: Set rotation from transform (convert Euler to quaternion)
  // For now, rotation is not set

  // Set damping
  bodyDesc.setLinearDamping(bodyConfig.linear_damping);
  bodyDesc.setAngularDamping(bodyConfig.angular_damping);

  // Enable CCD if requested
  if (bodyConfig.ccd_enabled) {
    bodyDesc.setCcdEnabled(true);
  }

  // Apply axis locks
  const locks = bodyConfig.axis_locks;
  bodyDesc.enabledTranslations(
    !locks.lock_translation_x,
    !locks.lock_translation_y,
    !locks.lock_translation_z,
    true // wake_up
  );
  bodyDesc.enabledRotations(
    !locks.lock_rotation_x,
    !locks.lock_rotation_y,
    !locks.lock_rotation_z,
    true // wake_up
  );

  // Create the rigid body
  const rigidBody = physicsWorld.createRigidBody(bodyDesc);

  // Create collider descriptor based on shape
  let colliderDesc;
  const shape = bodyConfig.collider;

  switch (shape.constructor.name) {
    case 'Box':
      // Rapier uses half-extents, and the user provides half-extents
      colliderDesc = RAPIER.ColliderDesc.cuboid(
        shape.width,
        shape.height,
        shape.depth
      );
      break;
    case 'Sphere':
      colliderDesc = RAPIER.ColliderDesc.ball(shape.radius);
      break;
    case 'Capsule':
      colliderDesc = RAPIER.ColliderDesc.capsule(shape.half_height, shape.radius);
      break;
    case 'Cylinder':
      colliderDesc = RAPIER.ColliderDesc.cylinder(shape.half_height, shape.radius);
      break;
    default:
      console.error('[Physics] Unknown collider shape:', shape);
      colliderDesc = RAPIER.ColliderDesc.cuboid(0.5, 0.5, 0.5);
  }

  // Set physical properties
  colliderDesc.setRestitution(bodyConfig.restitution);
  colliderDesc.setFriction(bodyConfig.friction);

  // Set mass/density if provided
  if (bodyConfig.mass && bodyConfig.mass[0] !== undefined) {
    colliderDesc.setDensity(bodyConfig.mass[0]);
  }

  // Set collision groups if provided
  if (bodyConfig.collision_groups && bodyConfig.collision_groups[0]) {
    const groups = bodyConfig.collision_groups[0];  // unwrap Option
    // Use Gleam's collision_groups_to_bitmask function for correct packing
    const packedGroups = collision_groups_to_bitmask(groups);
    colliderDesc.setCollisionGroups(packedGroups);
    // Also set solver groups to apply the same filtering to collision resolution
    colliderDesc.setSolverGroups(packedGroups);
  }

  // Create and attach collider to rigid body
  physicsWorld.createCollider(colliderDesc, rigidBody);

  // Store body in maps using string ID
  bodyMap.set(actualId, rigidBody);
  handleToId.set(rigidBody.handle, actualId);
}

/**
 * Remove a rigid body from the physics world
 * @param {PhysicsWorld} physicsWorldGleam - Gleam PhysicsWorld containing BiMap
 * @param {any} id - Body identifier (typed or string)
 */
export function removeRigidBody(physicsWorldGleam, id) {
  // Convert typed ID to string using Gleam's id_to_string function
  const stringIdResult = id_to_string(physicsWorldGleam, id);
  if (!stringIdResult || !stringIdResult[0]) {
    console.warn('[Physics] No string ID mapping found for removal:', id);
    return;
  }
  const actualId = stringIdResult[0];

  const body = bodyMap.get(actualId);
  if (body && physicsWorld) {
    const handle = body.handle;
    physicsWorld.removeRigidBody(body);
    bodyMap.delete(actualId);
    handleToId.delete(handle);
  }
}

/**
 * Get the current transform of a rigid body
 * @param {string} id - Body identifier
 * @returns {Transform|null} - Transform or Error
 */
export function getBodyTransform(id) {
  const body = bodyMap.get(id);
  if (!body) {
    return new Error();
  }

  const translation = body.translation();
  const quaternion = body.rotation();

  // Convert quaternion to Euler angles (XYZ order)
  // Using standard conversion formulas
  const q = quaternion;

  // Calculate Euler angles from quaternion
  const sinr_cosp = 2 * (q.w * q.x + q.y * q.z);
  const cosr_cosp = 1 - 2 * (q.x * q.x + q.y * q.y);
  const eulerX = Math.atan2(sinr_cosp, cosr_cosp);

  const sinp = 2 * (q.w * q.y - q.z * q.x);
  const eulerY = Math.abs(sinp) >= 1
    ? Math.sign(sinp) * Math.PI / 2  // Use 90 degrees if out of range
    : Math.asin(sinp);

  const siny_cosp = 2 * (q.w * q.z + q.x * q.y);
  const cosy_cosp = 1 - 2 * (q.y * q.y + q.z * q.z);
  const eulerZ = Math.atan2(siny_cosp, cosy_cosp);

  // Return Ok(Transform) - properly constructed Gleam types
  const position = new Vec3(translation.x, translation.y, translation.z);
  const rotation = new Vec3(eulerX, eulerY, eulerZ);
  const scale = new Vec3(1.0, 1.0, 1.0);
  const transform = new Transform(position, rotation, scale);

  return new Ok(transform);
}

/**
 * Apply a force to a rigid body
 * @param {string} id - Body identifier
 * @param {Vec3} force - Force vector
 */
export function applyForce(id, force) {
  const body = bodyMap.get(id);
  if (body) {
    body.addForce({ x: force.x, y: force.y, z: force.z }, true);
  }
}

/**
 * Apply an impulse to a rigid body
 * @param {string} id - Body identifier
 * @param {Vec3} impulse - Impulse vector
 */
export function applyImpulse(id, impulse) {
  const body = bodyMap.get(id);
  if (body) {
    body.applyImpulse({ x: impulse.x, y: impulse.y, z: impulse.z }, true);
  }
}

/**
 * Set the velocity of a rigid body
 * @param {string} id - Body identifier
 * @param {Vec3} velocity - Velocity vector
 */
export function setVelocity(id, velocity) {
  const body = bodyMap.get(id);
  if (body) {
    body.setLinvel({ x: velocity.x, y: velocity.y, z: velocity.z }, true);
  }
}

/**
 * Get the velocity of a rigid body
 * @param {string} id - Body identifier
 * @returns {Vec3|null} - Velocity vector or Error
 */
export function getVelocity(id) {
  const body = bodyMap.get(id);
  if (!body) return new Error();

  const vel = body.linvel();
  return new Ok(new Vec3(vel.x, vel.y, vel.z));
}

/**
 * Set the angular velocity of a rigid body
 * @param {string} id - Body identifier
 * @param {Vec3} velocity - Angular velocity vector
 */
export function setAngularVelocity(id, velocity) {
  const body = bodyMap.get(id);
  if (body) {
    body.setAngvel({ x: velocity.x, y: velocity.y, z: velocity.z }, true);
  }
}

/**
 * Get the angular velocity of a rigid body
 * @param {string} id - Body identifier
 * @returns {Vec3|null} - Angular velocity vector or Error
 */
export function getAngularVelocity(id) {
  const body = bodyMap.get(id);
  if (!body) return new Error();

  const angVel = body.angvel();
  return new Ok(new Vec3(angVel.x, angVel.y, angVel.z));
}

/**
 * Apply a torque to a rigid body
 * @param {string} id - Body identifier
 * @param {Vec3} torque - Torque vector
 */
export function applyTorque(id, torque) {
  const body = bodyMap.get(id);
  if (body) {
    body.addTorque({ x: torque.x, y: torque.y, z: torque.z }, true);
  }
}

/**
 * Apply a torque impulse to a rigid body
 * @param {string} id - Body identifier
 * @param {Vec3} impulse - Torque impulse vector
 */
export function applyTorqueImpulse(id, impulse) {
  const body = bodyMap.get(id);
  if (body) {
    body.applyTorqueImpulse({ x: impulse.x, y: impulse.y, z: impulse.z }, true);
  }
}

/**
 * Cast a ray and return the first hit
 * @param {Vec3} origin - Ray origin point
 * @param {Vec3} direction - Ray direction (should be normalized)
 * @param {number} maxDistance - Maximum distance to cast
 * @returns {Option<RaycastHit>} - First hit or Error
 */
export function raycast(origin, direction, maxDistance) {
  if (!physicsWorld) return new Error();

  const ray = new RAPIER.Ray(
    { x: origin.x, y: origin.y, z: origin.z },
    { x: direction.x, y: direction.y, z: direction.z }
  );

  // Use castRayAndGetNormal to get both hit point and surface normal
  const hit = physicsWorld.castRayAndGetNormal(ray, maxDistance, true);

  if (hit) {
    const collider = hit.collider;
    const rigidBody = collider.parent();

    // Find body ID from our map
    let bodyId = null;
    for (const [id, body] of bodyMap.entries()) {
      if (body === rigidBody) {
        bodyId = id;
        break;
      }
    }

    if (bodyId) {
      const hitPoint = ray.pointAt(hit.toi);
      const normal = hit.normal;

      return new Ok(new RaycastHit(
        bodyId,
        new Vec3(hitPoint.x, hitPoint.y, hitPoint.z),
        new Vec3(normal.x, normal.y, normal.z),
        hit.toi
      ));
    }
  }

  return new Error();
}

/**
 * Cast a ray and return all hits
 * @param {Vec3} origin - Ray origin point
 * @param {Vec3} direction - Ray direction (should be normalized)
 * @param {number} maxDistance - Maximum distance to cast
 * @returns {List<RaycastHit>} - List of all hits, sorted by distance
 */
export function raycastAll(origin, direction, maxDistance) {
  if (!physicsWorld) return toList([]);

  const ray = new RAPIER.Ray(
    { x: origin.x, y: origin.y, z: origin.z },
    { x: direction.x, y: direction.y, z: direction.z }
  );

  const hits = [];

  physicsWorld.intersectionsWithRay(ray, maxDistance, true, (hit) => {
    const collider = hit.collider;
    const rigidBody = collider.parent();

    // Find body ID from our map
    let bodyId = null;
    for (const [id, body] of bodyMap.entries()) {
      if (body === rigidBody) {
        bodyId = id;
        break;
      }
    }

    if (bodyId) {
      const hitPoint = ray.pointAt(hit.toi);
      const normal = hit.normal;

      hits.push(new RaycastHit(
        bodyId,
        new Vec3(hitPoint.x, hitPoint.y, hitPoint.z),
        new Vec3(normal.x, normal.y, normal.z),
        hit.toi
      ));
    }

    return true; // Continue checking for more hits
  });

  // Sort by distance (closest first)
  hits.sort((a, b) => a.distance - b.distance);

  return toList(hits);
}

/**
 * Get collision events from the last physics step
 * @returns {List<CollisionEvent>} - List of collision events
 */
export function getCollisionEvents() {
  return toList(collisionEvents);
}

/**
 * Check if two bodies are currently colliding
 * @param {string} bodyA - First body ID
 * @param {string} bodyB - Second body ID
 * @returns {boolean} - True if bodies are colliding
 */
export function areColliding(bodyA, bodyB) {
  const body1 = bodyMap.get(bodyA);
  const body2 = bodyMap.get(bodyB);

  if (!body1 || !body2 || !physicsWorld) return false;

  // Get colliders for both bodies
  const numColliders1 = body1.numColliders();
  const numColliders2 = body2.numColliders();

  // Check all collider pairs for contacts
  for (let i = 0; i < numColliders1; i++) {
    const collider1 = body1.collider(i);

    // Use contactsWith to find all colliders in contact with collider1
    let found = false;
    physicsWorld.contactsWith(collider1, (collider2) => {
      // Check if any of body2's colliders match
      for (let j = 0; j < numColliders2; j++) {
        if (body2.collider(j).handle === collider2.handle) {
          found = true;
          return false; // Stop iteration
        }
      }
      return true; // Continue iteration
    });

    if (found) {
      return true;
    }
  }

  return false;
}

/**
 * Get all rigid body IDs
 * @returns {List<string>} - Gleam list of body IDs
 */
export function getAllBodyIds() {
  return toList(Array.from(bodyMap.keys()));
}

/**
 * Get the physics world instance (for debug visualization)
 * @returns {RAPIER.World|null}
 */
export function getPhysicsWorld() {
  return physicsWorld;
}

/**
 * Get the body map (for debug visualization)
 * @returns {Map<string, RAPIER.RigidBody>}
 */
export function getBodyMap() {
  return bodyMap;
}

/**
 * Get a rigid body by typed ID using the PhysicsWorld's BiMap
 * @param {PhysicsWorld} physicsWorldGleam - Gleam PhysicsWorld containing BiMap
 * @param {any} id - Typed ID
 * @returns {RAPIER.RigidBody|null}
 */
export function getBodyByTypedId(physicsWorldGleam, id) {
  // Convert typed ID to string using Gleam's id_to_string function
  const stringIdResult = id_to_string(physicsWorldGleam, id);
  if (!stringIdResult || !stringIdResult[0]) {
    return null;
  }
  const actualId = stringIdResult[0];
  return bodyMap.get(actualId) || null;
}
