/// Physics FFI - Rapier integration
///
/// This module manages a single global physics world and provides
/// functions for creating/removing bodies and stepping simulation.

import RAPIER from '@dimforge/rapier3d-compat';
import { toList } from '../../../gleam_stdlib/gleam.mjs';
import { Some, None } from '../../../gleam_stdlib/gleam/option.mjs';
import { Transform } from '../transform.mjs';
import { Vec3 } from '../../../vec/vec/vec3.mjs';

// Initialize Rapier (must be called before creating world)
await RAPIER.init();

// Global physics world instance
let physicsWorld = null;

// Map of body IDs to Rapier rigid bodies
const bodyMap = new Map();

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
  console.log('[Physics] Physics world initialized with gravity:', gravity);
}

/**
 * Step the physics simulation
 * @param {number} deltaTime - Time step in seconds
 */
export function stepWorld(deltaTime) {
  if (physicsWorld) {
    physicsWorld.step();
  }
}

/**
 * Create a rigid body in the physics world
 * @param {string} id - Unique identifier for this body
 * @param {RigidBody} bodyConfig - Body configuration from Gleam
 * @param {Transform} transform - Initial transform
 */
export function createRigidBody(id, bodyConfig, transform) {
  if (!physicsWorld) {
    console.warn('[Physics] createRigidBody called but physicsWorld is null:', id);
    return;
  }

  console.log('[Physics] Creating rigid body:', id, 'at position:', transform.position);

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

  // Create the rigid body
  const rigidBody = physicsWorld.createRigidBody(bodyDesc);

  // Create collider descriptor based on shape
  let colliderDesc;
  const shape = bodyConfig.collider;

  switch (shape.constructor.name) {
    case 'Box':
      // Rapier uses half-extents
      colliderDesc = RAPIER.ColliderDesc.cuboid(
        shape.width / 2,
        shape.height / 2,
        shape.depth / 2
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

  // Create and attach collider to rigid body
  physicsWorld.createCollider(colliderDesc, rigidBody);

  // Store body in map
  bodyMap.set(id, rigidBody);
}

/**
 * Remove a rigid body from the physics world
 * @param {string} id - Body identifier
 */
export function removeRigidBody(id) {
  const body = bodyMap.get(id);
  if (body && physicsWorld) {
    physicsWorld.removeRigidBody(body);
    bodyMap.delete(id);
  }
}

/**
 * Get the current transform of a rigid body
 * @param {string} id - Body identifier
 * @returns {Transform|null} - Transform or None
 */
export function getBodyTransform(id) {
  const body = bodyMap.get(id);
  if (!body) {
    return new None();
  }

  const translation = body.translation();
  const quaternion = body.rotation();

  // Convert quaternion to Euler angles (simplified, may have gimbal lock issues)
  // For now, just use quaternion as-is
  // TODO: Proper quaternion to Euler conversion
  const eulerX = 0; // Placeholder
  const eulerY = 0; // Placeholder
  const eulerZ = 0; // Placeholder

  // Return Some(Transform) - properly constructed Gleam types
  const position = new Vec3(translation.x, translation.y, translation.z);
  const rotation = new Vec3(eulerX, eulerY, eulerZ);
  const scale = new Vec3(1.0, 1.0, 1.0);
  const transform = new Transform(position, rotation, scale);

  return new Some(transform);
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
 * @returns {Vec3|null} - Velocity vector or None
 */
export function getVelocity(id) {
  const body = bodyMap.get(id);
  if (!body) return new None();

  const vel = body.linvel();
  return new Some(new Vec3(vel.x, vel.y, vel.z));
}

/**
 * Get all rigid body IDs
 * @returns {List<string>} - Gleam list of body IDs
 */
export function getAllBodyIds() {
  return toList(Array.from(bodyMap.keys()));
}
