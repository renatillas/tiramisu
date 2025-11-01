/**
 * Rapier Physics FFI - Pure 1:1 bindings to Rapier physics library
 *
 * This module provides direct, minimal wrappers around Rapier3D API calls.
 * No game logic should exist here - this is purely a binding layer.
 *
 * All higher-level logic should be in the Gleam declarative layer.
 */

import RAPIER from '@dimforge/rapier3d-compat';
import { toList, Ok, Error } from '../prelude.mjs';

// Initialize Rapier (must be called before creating world)
await RAPIER.init();

// ============================================================================
// WORLD MANAGEMENT
// ============================================================================

/**
 * Create a physics world
 * @param {number} gravityX
 * @param {number} gravityY
 * @param {number} gravityZ
 * @returns {RAPIER.World}
 */
export function createWorld(gravityX, gravityY, gravityZ) {
  const gravity = { x: gravityX, y: gravityY, z: gravityZ };
  return new RAPIER.World(gravity);
}

/**
 * Step the physics simulation
 * @param {RAPIER.World} world
 * @param {RAPIER.EventQueue} eventQueue
 */
export function stepWorld(world, eventQueue) {
  world.step(eventQueue);
}

/**
 * Set world gravity
 * @param {RAPIER.World} world
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setGravity(world, x, y, z) {
  world.gravity.x = x;
  world.gravity.y = y;
  world.gravity.z = z;
}

// ============================================================================
// EVENT QUEUE
// ============================================================================

/**
 * Create an event queue
 * @param {boolean} autoDrain
 * @returns {RAPIER.EventQueue}
 */
export function createEventQueue(autoDrain) {
  return new RAPIER.EventQueue(autoDrain);
}

/**
 * Drain collision events from queue
 * @param {RAPIER.EventQueue} eventQueue
 * @param {Function} callback - (handle1, handle2, started) => void
 */
export function drainCollisionEvents(eventQueue, callback) {
  eventQueue.drainCollisionEvents(callback);
}

/**
 * Drain collision events from queue and return as Gleam list
 * @param {RAPIER.EventQueue} eventQueue
 * @returns {List} - Gleam List of [handle1, handle2, started] tuples
 */
export function drainCollisionEventsToList(eventQueue) {
  const events = [];
  eventQueue.drainCollisionEvents((handle1, handle2, started) => {
    // Create a proper tuple for Gleam (JavaScript arrays work as tuples)
    events.push([handle1, handle2, started]);
  });

  // Convert JavaScript array to Gleam List using the imported toList from prelude
  return toList(events);
}

/**
 * Drain contact force events from queue
 * @param {RAPIER.EventQueue} eventQueue
 * @param {Function} callback - (event) => void
 */
export function drainContactForceEvents(eventQueue, callback) {
  eventQueue.drainContactForceEvents(callback);
}

// ============================================================================
// RIGID BODY DESCRIPTORS
// ============================================================================

/**
 * Create a dynamic rigid body descriptor
 * @returns {RAPIER.RigidBodyDesc}
 */
export function createDynamicBodyDesc() {
  return RAPIER.RigidBodyDesc.dynamic();
}

/**
 * Create a kinematic (position-based) rigid body descriptor
 * @returns {RAPIER.RigidBodyDesc}
 */
export function createKinematicBodyDesc() {
  return RAPIER.RigidBodyDesc.kinematicPositionBased();
}

/**
 * Create a fixed (static) rigid body descriptor
 * @returns {RAPIER.RigidBodyDesc}
 */
export function createFixedBodyDesc() {
  return RAPIER.RigidBodyDesc.fixed();
}

/**
 * Set body translation
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setBodyTranslation(desc, x, y, z) {
  desc.setTranslation(x, y, z);
}

/**
 * Set body rotation from quaternion
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {number} w
 */
export function setBodyRotation(desc, x, y, z, w) {
  desc.setRotation({ x, y, z, w });
}

/**
 * Set linear damping
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {number} damping
 */
export function setLinearDamping(desc, damping) {
  desc.setLinearDamping(damping);
}

/**
 * Set angular damping
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {number} damping
 */
export function setAngularDamping(desc, damping) {
  desc.setAngularDamping(damping);
}

/**
 * Enable CCD (continuous collision detection)
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {boolean} enabled
 */
export function setCCDEnabled(desc, enabled) {
  desc.setCcdEnabled(enabled);
}

/**
 * Enable/disable translations on axes
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {boolean} enableX
 * @param {boolean} enableY
 * @param {boolean} enableZ
 * @param {boolean} wakeUp
 */
export function setEnabledTranslations(desc, enableX, enableY, enableZ, wakeUp) {
  desc.enabledTranslations(enableX, enableY, enableZ, wakeUp);
}

/**
 * Enable/disable rotations on axes
 * @param {RAPIER.RigidBodyDesc} desc
 * @param {boolean} enableX
 * @param {boolean} enableY
 * @param {boolean} enableZ
 * @param {boolean} wakeUp
 */
export function setEnabledRotations(desc, enableX, enableY, enableZ, wakeUp) {
  desc.enabledRotations(enableX, enableY, enableZ, wakeUp);
}

// ============================================================================
// RIGID BODIES
// ============================================================================

/**
 * Create rigid body from descriptor
 * @param {RAPIER.World} world
 * @param {RAPIER.RigidBodyDesc} desc
 * @returns {RAPIER.RigidBody}
 */
export function createRigidBody(world, desc) {
  return world.createRigidBody(desc);
}

/**
 * Remove rigid body from world
 * @param {RAPIER.World} world
 * @param {RAPIER.RigidBody} body
 */
export function removeRigidBody(world, body) {
  world.removeRigidBody(body);
}

/**
 * Get body translation
 * @param {RAPIER.RigidBody} body
 * @returns {Object} - {x, y, z}
 */
export function getBodyTranslation(body) {
  return body.translation();
}

/**
 * Get body rotation (quaternion)
 * @param {RAPIER.RigidBody} body
 * @returns {Object} - {x, y, z, w}
 */
export function getBodyRotation(body) {
  return body.rotation();
}

/**
 * Get body linear velocity
 * @param {RAPIER.RigidBody} body
 * @returns {Object} - {x, y, z}
 */
export function getBodyLinvel(body) {
  return body.linvel();
}

/**
 * Set body linear velocity
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function setBodyLinvel(body, x, y, z, wakeUp) {
  body.setLinvel({ x, y, z }, wakeUp);
}

/**
 * Get body angular velocity
 * @param {RAPIER.RigidBody} body
 * @returns {Object} - {x, y, z}
 */
export function getBodyAngvel(body) {
  return body.angvel();
}

/**
 * Set body angular velocity
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function setBodyAngvel(body, x, y, z, wakeUp) {
  body.setAngvel({ x, y, z }, wakeUp);
}

/**
 * Apply force to body
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function addBodyForce(body, x, y, z, wakeUp) {
  body.addForce({ x, y, z }, wakeUp);
}

/**
 * Apply impulse to body
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function applyBodyImpulse(body, x, y, z, wakeUp) {
  body.applyImpulse({ x, y, z }, wakeUp);
}

/**
 * Apply torque to body
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function addBodyTorque(body, x, y, z, wakeUp) {
  body.addTorque({ x, y, z }, wakeUp);
}

/**
 * Apply torque impulse to body
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function applyBodyTorqueImpulse(body, x, y, z, wakeUp) {
  body.applyTorqueImpulse({ x, y, z }, wakeUp);
}

/**
 * Get body handle
 * @param {RAPIER.RigidBody} body
 * @returns {number}
 */
export function getBodyHandle(body) {
  return body.handle;
}

/**
 * Get number of colliders attached to body
 * @param {RAPIER.RigidBody} body
 * @returns {number}
 */
export function getBodyNumColliders(body) {
  return body.numColliders();
}

/**
 * Get collider at index
 * @param {RAPIER.RigidBody} body
 * @param {number} index
 * @returns {RAPIER.Collider}
 */
export function getBodyCollider(body, index) {
  return body.collider(index);
}

/**
 * Set body translation
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {boolean} wakeUp
 */
export function setBodyTranslation2(body, x, y, z, wakeUp) {
  body.setTranslation({ x, y, z }, wakeUp);
}

/**
 * Set body rotation
 * @param {RAPIER.RigidBody} body
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {number} w
 * @param {boolean} wakeUp
 */
export function setBodyRotation2(body, x, y, z, w, wakeUp) {
  body.setRotation({ x, y, z, w }, wakeUp);
}

/**
 * Check if a rigid body is currently sleeping (inactive)
 * Sleeping bodies don't need transform updates since they haven't moved
 * @param {RAPIER.RigidBody} body - The rigid body handle
 * @returns {boolean} True if body is sleeping, false if awake/active
 */
export function isBodySleeping(body) {
  return body.isSleeping();
}

// ============================================================================
// COLLIDER DESCRIPTORS
// ============================================================================

/**
 * Create cuboid (box) collider descriptor
 * @param {number} hx - Half-width
 * @param {number} hy - Half-height
 * @param {number} hz - Half-depth
 * @returns {RAPIER.ColliderDesc}
 */
export function createCuboidColliderDesc(hx, hy, hz) {
  return RAPIER.ColliderDesc.cuboid(hx, hy, hz);
}

/**
 * Create ball (sphere) collider descriptor
 * @param {number} radius
 * @returns {RAPIER.ColliderDesc}
 */
export function createBallColliderDesc(radius) {
  return RAPIER.ColliderDesc.ball(radius);
}

/**
 * Create capsule collider descriptor
 * @param {number} halfHeight
 * @param {number} radius
 * @returns {RAPIER.ColliderDesc}
 */
export function createCapsuleColliderDesc(halfHeight, radius) {
  return RAPIER.ColliderDesc.capsule(halfHeight, radius);
}

/**
 * Create cylinder collider descriptor
 * @param {number} halfHeight
 * @param {number} radius
 * @returns {RAPIER.ColliderDesc}
 */
export function createCylinderColliderDesc(halfHeight, radius) {
  return RAPIER.ColliderDesc.cylinder(halfHeight, radius);
}

/**
 * Create cone collider descriptor
 * @param {number} halfHeight
 * @param {number} radius
 * @returns {RAPIER.ColliderDesc}
 */
export function createConeColliderDesc(halfHeight, radius) {
  return RAPIER.ColliderDesc.cone(halfHeight, radius);
}

/**
 * Create triangle mesh collider descriptor
 * @param {Float32Array} vertices
 * @param {Uint32Array} indices
 * @returns {RAPIER.ColliderDesc}
 */
export function createTrimeshColliderDesc(vertices, indices) {
  return RAPIER.ColliderDesc.trimesh(vertices, indices);
}

/**
 * Create convex hull collider descriptor
 * @param {Float32Array} points
 * @returns {RAPIER.ColliderDesc}
 */
export function createConvexHullColliderDesc(points) {
  return RAPIER.ColliderDesc.convexHull(points);
}

/**
 * Set collider restitution (bounciness)
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} restitution
 */
export function setColliderRestitution(desc, restitution) {
  desc.setRestitution(restitution);
}

/**
 * Set collider friction
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} friction
 */
export function setColliderFriction(desc, friction) {
  desc.setFriction(friction);
}

/**
 * Set collider density
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} density
 */
export function setColliderDensity(desc, density) {
  desc.setDensity(density);
}

/**
 * Set collider mass
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} mass
 */
export function setColliderMass(desc, mass) {
  desc.setMass(mass);
}

/**
 * Set collider as sensor
 * @param {RAPIER.ColliderDesc} desc
 * @param {boolean} isSensor
 */
export function setColliderSensor(desc, isSensor) {
  desc.setSensor(isSensor);
}

/**
 * Set collider collision groups
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} groups - Packed collision groups bitmask
 */
export function setColliderCollisionGroups(desc, groups) {
  desc.setCollisionGroups(groups);
}

/**
 * Set collider solver groups
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} groups - Packed solver groups bitmask
 */
export function setColliderSolverGroups(desc, groups) {
  desc.setSolverGroups(groups);
}

/**
 * Set collider translation (position offset relative to rigid body)
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setColliderTranslation(desc, x, y, z) {
  desc.setTranslation(x, y, z);
}

/**
 * Set collider rotation (rotation offset relative to rigid body)
 * @param {RAPIER.ColliderDesc} desc
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {number} w
 */
export function setColliderRotation(desc, x, y, z, w) {
  desc.setRotation({ x, y, z, w });
}

// ============================================================================
// COLLIDERS
// ============================================================================

/**
 * Create collider attached to rigid body
 * @param {RAPIER.World} world
 * @param {RAPIER.ColliderDesc} desc
 * @param {RAPIER.RigidBody} body
 * @returns {RAPIER.Collider}
 */
export function createCollider(world, desc, body) {
  return world.createCollider(desc, body);
}

/**
 * Remove collider from world
 * @param {RAPIER.World} world
 * @param {RAPIER.Collider} collider
 */
export function removeCollider(world, collider) {
  world.removeCollider(collider);
}

/**
 * Get collider parent body
 * @param {RAPIER.Collider} collider
 * @returns {RAPIER.RigidBody|null}
 */
export function getColliderParent(collider) {
  return collider.parent();
}

/**
 * Get collider handle
 * @param {RAPIER.Collider} collider
 * @returns {number}
 */
export function getColliderHandle(collider) {
  return collider.handle;
}

// ============================================================================
// RAYCASTING
// ============================================================================

/**
 * Create a ray
 * @param {number} originX
 * @param {number} originY
 * @param {number} originZ
 * @param {number} dirX
 * @param {number} dirY
 * @param {number} dirZ
 * @returns {RAPIER.Ray}
 */
export function createRay(originX, originY, originZ, dirX, dirY, dirZ) {
  return new RAPIER.Ray(
    { x: originX, y: originY, z: originZ },
    { x: dirX, y: dirY, z: dirZ }
  );
}

/**
 * Cast ray and get first hit
 * @param {RAPIER.World} world
 * @param {RAPIER.Ray} ray
 * @param {number} maxDistance
 * @param {boolean} solid
 * @returns {Object|null} - Hit info or null
 */
export function castRay(world, ray, maxDistance, solid) {
  return world.castRay(ray, maxDistance, solid);
}

/**
 * Cast ray and get first hit with normal
 * @param {RAPIER.World} world
 * @param {RAPIER.Ray} ray
 * @param {number} maxDistance
 * @param {boolean} solid
 * @returns {Result} - Gleam Result with hit info or Nil
 */
export function castRayAndGetNormal(world, ray, maxDistance, solid) {
  const hit = world.castRayAndGetNormal(ray, maxDistance, solid);
  if (hit) {
    return new Ok(hit);
  } else {
    return new Error(undefined);
  }
}

/**
 * Get all ray intersections
 * @param {RAPIER.World} world
 * @param {RAPIER.Ray} ray
 * @param {number} maxDistance
 * @param {boolean} solid
 * @param {Function} callback - (hit) => boolean (return true to continue)
 */
export function intersectionsWithRay(world, ray, maxDistance, solid, callback) {
  world.intersectionsWithRay(ray, maxDistance, solid, callback);
}

/**
 * Get point on ray at distance
 * @param {RAPIER.Ray} ray
 * @param {number} toi - Time of impact
 * @returns {Object} - {x, y, z}
 */
export function rayPointAt(ray, toi) {
  return ray.pointAt(toi);
}

/**
 * Get collider handle from ray hit
 * @param {Object} hit - Ray collision hit
 * @returns {number} - Collider handle
 */
export function getHitColliderHandle(hit) {
  return hit.collider.handle;
}

/**
 * Get time of impact (distance) from ray hit
 * @param {Object} hit - Ray collision hit
 * @returns {number} - Distance
 */
export function getHitToi(hit) {
  return hit.toi;
}

/**
 * Get normal vector from ray hit
 * @param {Object} hit - Ray collision hit
 * @returns {Object} - {x, y, z}
 */
export function getHitNormal(hit) {
  return hit.normal;
}

// ============================================================================
// COLLISION QUERIES
// ============================================================================

/**
 * Check if two colliders are in contact
 * @param {RAPIER.World} world
 * @param {RAPIER.Collider} collider
 * @param {Function} callback - (collider2) => boolean (return false to stop)
 */
export function contactsWith(world, collider, callback) {
  world.contactsWith(collider, callback);
}

/**
 * Check if two colliders intersect
 * @param {RAPIER.World} world
 * @param {RAPIER.Collider} collider1
 * @param {RAPIER.Collider} collider2
 * @returns {boolean}
 */
export function intersectionPair(world, collider1, collider2) {
  return world.intersectionPair(collider1, collider2);
}

/**
 * Get contact pair information
 * @param {RAPIER.World} world
 * @param {RAPIER.Collider} collider1
 * @param {RAPIER.Collider} collider2
 * @returns {Object|null} - Contact info or null
 */
export function contactPair(world, collider1, collider2) {
  return world.contactPair(collider1, collider2);
}

// ============================================================================
// SHAPE CASTING
// ============================================================================

/**
 * Cast a shape and get first hit
 * @param {RAPIER.World} world
 * @param {Object} shapePos - {x, y, z}
 * @param {Object} shapeRot - {x, y, z, w}
 * @param {Object} shapeVel - {x, y, z}
 * @param {RAPIER.Shape} shape
 * @param {number} maxToi
 * @param {boolean} stopAtPenetration
 * @returns {Object|null} - Hit info or null
 */
export function castShape(world, shapePos, shapeRot, shapeVel, shape, maxToi, stopAtPenetration) {
  return world.castShape(shapePos, shapeRot, shapeVel, shape, maxToi, stopAtPenetration);
}

/**
 * Check if shape intersects any colliders
 * @param {RAPIER.World} world
 * @param {Object} shapePos - {x, y, z}
 * @param {Object} shapeRot - {x, y, z, w}
 * @param {RAPIER.Shape} shape
 * @param {Function} callback - (handle) => boolean (return false to stop)
 */
export function intersectionsWithShape(world, shapePos, shapeRot, shape, callback) {
  world.intersectionsWithShape(shapePos, shapeRot, shape, callback);
}

// ============================================================================
// CONSTRAINTS (JOINTS)
// ============================================================================

/**
 * Create fixed joint descriptor
 * @param {Object} anchorPos1 - {x, y, z}
 * @param {Object} anchorRot1 - {x, y, z, w}
 * @param {Object} anchorPos2 - {x, y, z}
 * @param {Object} anchorRot2 - {x, y, z, w}
 * @returns {RAPIER.JointData}
 */
export function createFixedJoint(anchorPos1, anchorRot1, anchorPos2, anchorRot2) {
  return RAPIER.JointData.fixed(anchorPos1, anchorRot1, anchorPos2, anchorRot2);
}

/**
 * Create spherical (ball) joint descriptor
 * @param {Object} anchorPos1 - {x, y, z}
 * @param {Object} anchorPos2 - {x, y, z}
 * @returns {RAPIER.JointData}
 */
export function createSphericalJoint(anchorPos1, anchorPos2) {
  return RAPIER.JointData.spherical(anchorPos1, anchorPos2);
}

/**
 * Create revolute (hinge) joint descriptor
 * @param {Object} anchorPos1 - {x, y, z}
 * @param {Object} axis1 - {x, y, z}
 * @param {Object} anchorPos2 - {x, y, z}
 * @param {Object} axis2 - {x, y, z}
 * @returns {RAPIER.JointData}
 */
export function createRevoluteJoint(anchorPos1, axis1, anchorPos2, axis2) {
  return RAPIER.JointData.revolute(anchorPos1, axis1, anchorPos2, axis2);
}

/**
 * Create prismatic (sliding) joint descriptor
 * @param {Object} anchorPos1 - {x, y, z}
 * @param {Object} axis1 - {x, y, z}
 * @param {Object} anchorPos2 - {x, y, z}
 * @param {Object} axis2 - {x, y, z}
 * @returns {RAPIER.JointData}
 */
export function createPrismaticJoint(anchorPos1, axis1, anchorPos2, axis2) {
  return RAPIER.JointData.prismatic(anchorPos1, axis1, anchorPos2, axis2);
}

/**
 * Create joint between two bodies
 * @param {RAPIER.World} world
 * @param {RAPIER.JointData} jointData
 * @param {RAPIER.RigidBody} body1
 * @param {RAPIER.RigidBody} body2
 * @param {boolean} wakeUp
 * @returns {RAPIER.ImpulseJoint}
 */
export function createImpulseJoint(world, jointData, body1, body2, wakeUp) {
  return world.createImpulseJoint(jointData, body1, body2, wakeUp);
}

/**
 * Remove joint from world
 * @param {RAPIER.World} world
 * @param {RAPIER.ImpulseJoint} joint
 * @param {boolean} wakeUp
 */
export function removeImpulseJoint(world, joint, wakeUp) {
  world.removeImpulseJoint(joint, wakeUp);
}

// ============================================================================
// CHARACTER CONTROLLER
// ============================================================================

/**
 * Create character controller
 * @param {RAPIER.World} world
 * @param {number} offset
 * @returns {RAPIER.KinematicCharacterController}
 */
export function createCharacterController(world, offset) {
  return world.createCharacterController(offset);
}

/**
 * Remove character controller
 * @param {RAPIER.World} world
 * @param {RAPIER.KinematicCharacterController} controller
 */
export function removeCharacterController(world, controller) {
  world.removeCharacterController(controller);
}

/**
 * Compute character movement
 * @param {RAPIER.KinematicCharacterController} controller
 * @param {RAPIER.Collider} collider
 * @param {Object} desiredTranslation - {x, y, z}
 * @param {Function} filterFunc - Optional collision filter
 */
export function computeCharacterMovement(controller, collider, desiredTranslation, filterFunc) {
  if (filterFunc) {
    controller.computeColliderMovement(collider, desiredTranslation, filterFunc);
  } else {
    controller.computeColliderMovement(collider, desiredTranslation);
  }
}

/**
 * Get computed character movement
 * @param {RAPIER.KinematicCharacterController} controller
 * @returns {Object} - {x, y, z}
 */
export function getCharacterComputedMovement(controller) {
  return controller.computedMovement();
}

/**
 * Check if character is grounded
 * @param {RAPIER.KinematicCharacterController} controller
 * @returns {boolean}
 */
export function isCharacterGrounded(controller) {
  return controller.computedGrounded();
}

/**
 * Set character up vector
 * @param {RAPIER.KinematicCharacterController} controller
 * @param {number} x
 * @param {number} y
 * @param {number} z
 */
export function setCharacterUpVector(controller, x, y, z) {
  controller.setUp({ x, y, z });
}

/**
 * Set character slide enabled
 * @param {RAPIER.KinematicCharacterController} controller
 * @param {boolean} enabled
 */
export function setCharacterSlide(controller, enabled) {
  controller.setSlideEnabled(enabled);
}

/**
 * Set character autostep
 * @param {RAPIER.KinematicCharacterController} controller
 * @param {number} maxHeight
 * @param {number} minWidth
 * @param {boolean} includeDynamicBodies
 */
export function setCharacterAutostep(controller, maxHeight, minWidth, includeDynamicBodies) {
  controller.setAutostepMaxHeight(maxHeight);
  controller.setAutostepMinWidth(minWidth);
  controller.setAutostepIncludeDynamicBodies(includeDynamicBodies);
}

// ============================================================================
// DEBUG RENDERING
// ============================================================================

/**
 * Get debug render buffers
 * @param {RAPIER.World} world
 * @returns {Object} - Debug render buffers
 */
export function getDebugRenderBuffers(world) {
  return world.debugRender();
}

// ============================================================================
// CONSTANTS
// ============================================================================

/**
 * Get active collision types constant
 * @returns {number}
 */
export function getActiveCollisionTypes() {
  return RAPIER.ActiveCollisionTypes.DEFAULT;
}

/**
 * Get active events constant
 * @returns {number}
 */
export function getActiveEvents() {
  return RAPIER.ActiveEvents.COLLISION_EVENTS;
}

/**
 * Get active hooks constant
 * @returns {number}
 */
export function getActiveHooks() {
  return RAPIER.ActiveHooks.FILTER_CONTACT_PAIRS;
}

// ============================================================================
// QUATERNION/EULER CONVERSIONS
// ============================================================================

/**
 * Convert Euler angles (radians) to quaternion
 * @param {number} x - Pitch (rotation around X)
 * @param {number} y - Yaw (rotation around Y)
 * @param {number} z - Roll (rotation around Z)
 * @returns {Object} - {x, y, z, w}
 */
export function eulerToQuaternion(x, y, z) {
  // Using standard Euler angle to quaternion conversion (ZYX order)
  const c1 = Math.cos(y / 2); // yaw/2
  const s1 = Math.sin(y / 2);
  const c2 = Math.cos(x / 2); // pitch/2
  const s2 = Math.sin(x / 2);
  const c3 = Math.cos(z / 2); // roll/2
  const s3 = Math.sin(z / 2);

  return {
    x: s2 * c1 * c3 - c2 * s1 * s3,
    y: c2 * s1 * c3 + s2 * c1 * s3,
    z: c2 * c1 * s3 - s2 * s1 * c3,
    w: c2 * c1 * c3 + s2 * s1 * s3,
  };
}

/**
 * Convert quaternion to Euler angles (radians)
 * @param {number} x
 * @param {number} y
 * @param {number} z
 * @param {number} w
 * @returns {Object} - {x, y, z} in radians
 */
export function quaternionToEuler(x, y, z, w) {
  // Using standard quaternion to Euler angle conversion (ZYX order)
  const sinr_cosp = 2 * (w * x + y * z);
  const cosr_cosp = 1 - 2 * (x * x + y * y);
  const roll = Math.atan2(sinr_cosp, cosr_cosp);

  const sinp = 2 * (w * y - z * x);
  let pitch;
  if (Math.abs(sinp) >= 1) {
    pitch = Math.sign(sinp) * Math.PI / 2;
  } else {
    pitch = Math.asin(sinp);
  }

  const siny_cosp = 2 * (w * z + x * y);
  const cosy_cosp = 1 - 2 * (y * y + z * z);
  const yaw = Math.atan2(siny_cosp, cosy_cosp);

  return { x: pitch, y: yaw, z: roll };
}
