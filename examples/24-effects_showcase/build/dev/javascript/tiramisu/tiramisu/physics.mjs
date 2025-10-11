import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $bimap from "../../structures/structures/bimap.mjs";
import * as $vec3 from "../../vec/vec/vec3.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../gleam.mjs";
import {
  addBodyForce as add_body_force_ffi,
  applyBodyImpulse as apply_body_impulse_ffi,
  setBodyLinvel as set_body_linvel_ffi,
  getBodyLinvel as get_body_linvel_ffi,
  setBodyAngvel as set_body_angvel_ffi,
  getBodyAngvel as get_body_angvel_ffi,
  addBodyTorque as add_body_torque_ffi,
  applyBodyTorqueImpulse as apply_body_torque_impulse_ffi,
  getBodyTranslation as get_body_translation_ffi,
  getBodyRotation as get_body_rotation_ffi,
  quaternionToEuler as quat_to_euler_ffi,
  eulerToQuaternion as euler_to_quat_ffi,
  createRay as create_ray_ffi,
  castRayAndGetNormal as cast_ray_and_get_normal_ffi,
  rayPointAt as ray_point_at_ffi,
  getHitColliderHandle as get_hit_collider_handle_ffi,
  getHitToi as get_hit_toi_ffi,
  getHitNormal as get_hit_normal_ffi,
  createWorld as create_world_ffi,
  createEventQueue as create_event_queue_ffi,
  stepWorld as step_world_ffi,
  createDynamicBodyDesc as create_dynamic_body_desc_ffi,
  createKinematicBodyDesc as create_kinematic_body_desc_ffi,
  createFixedBodyDesc as create_fixed_body_desc_ffi,
  setBodyTranslation as set_body_translation_ffi,
  setBodyRotation as set_body_rotation_ffi,
  setLinearDamping as set_linear_damping_ffi,
  setAngularDamping as set_angular_damping_ffi,
  setCCDEnabled as set_ccd_enabled_ffi,
  setEnabledTranslations as set_enabled_translations_ffi,
  setEnabledRotations as set_enabled_rotations_ffi,
  createRigidBody as create_rigid_body_ffi,
  removeRigidBody as remove_rigid_body_ffi,
  createCuboidColliderDesc as create_cuboid_collider_desc_ffi,
  createBallColliderDesc as create_ball_collider_desc_ffi,
  createCapsuleColliderDesc as create_capsule_collider_desc_ffi,
  createCylinderColliderDesc as create_cylinder_collider_desc_ffi,
  setColliderRestitution as set_collider_restitution_ffi,
  setColliderFriction as set_collider_friction_ffi,
  setColliderMass as set_collider_mass_ffi,
  setColliderCollisionGroups as set_collider_collision_groups_ffi,
  createCollider as create_collider_ffi,
  getColliderHandle as get_collider_handle_ffi,
  getBodyNumColliders as get_body_num_colliders_ffi,
  getBodyCollider as get_body_collider_ffi,
  drainCollisionEventsToList as drain_collision_events_ffi,
} from "../rapier.ffi.mjs";
import * as $transform from "../tiramisu/transform.mjs";

const FILEPATH = "src/tiramisu/physics.gleam";

export class Quaternion extends $CustomType {
  constructor(x, y, z, w) {
    super();
    this.x = x;
    this.y = y;
    this.z = z;
    this.w = w;
  }
}

class PhysicsWorld extends $CustomType {
  constructor(world, queue, bodies, rapier_bodies, pending_commands, collider_to_body, collision_events, bimap) {
    super();
    this.world = world;
    this.queue = queue;
    this.bodies = bodies;
    this.rapier_bodies = rapier_bodies;
    this.pending_commands = pending_commands;
    this.collider_to_body = collider_to_body;
    this.collision_events = collision_events;
    this.bimap = bimap;
  }
}

class ApplyForce extends $CustomType {
  constructor(id, force) {
    super();
    this.id = id;
    this.force = force;
  }
}

class ApplyImpulse extends $CustomType {
  constructor(id, impulse) {
    super();
    this.id = id;
    this.impulse = impulse;
  }
}

class SetVelocity extends $CustomType {
  constructor(id, velocity) {
    super();
    this.id = id;
    this.velocity = velocity;
  }
}

class SetAngularVelocity extends $CustomType {
  constructor(id, velocity) {
    super();
    this.id = id;
    this.velocity = velocity;
  }
}

class ApplyTorque extends $CustomType {
  constructor(id, torque) {
    super();
    this.id = id;
    this.torque = torque;
  }
}

class ApplyTorqueImpulse extends $CustomType {
  constructor(id, impulse) {
    super();
    this.id = id;
    this.impulse = impulse;
  }
}

export class Dynamic extends $CustomType {}

export class Kinematic extends $CustomType {}

export class Fixed extends $CustomType {}

/**
 * Box collider with half-extents
 */
export class Box extends $CustomType {
  constructor(width, height, depth) {
    super();
    this.width = width;
    this.height = height;
    this.depth = depth;
  }
}

/**
 * Sphere collider with radius
 */
export class Sphere extends $CustomType {
  constructor(radius) {
    super();
    this.radius = radius;
  }
}

/**
 * Capsule collider (cylinder with rounded caps)
 */
export class Capsule extends $CustomType {
  constructor(half_height, radius) {
    super();
    this.half_height = half_height;
    this.radius = radius;
  }
}

/**
 * Cylinder collider
 */
export class Cylinder extends $CustomType {
  constructor(half_height, radius) {
    super();
    this.half_height = half_height;
    this.radius = radius;
  }
}

export class AxisLock extends $CustomType {
  constructor(lock_translation_x, lock_translation_y, lock_translation_z, lock_rotation_x, lock_rotation_y, lock_rotation_z) {
    super();
    this.lock_translation_x = lock_translation_x;
    this.lock_translation_y = lock_translation_y;
    this.lock_translation_z = lock_translation_z;
    this.lock_rotation_x = lock_rotation_x;
    this.lock_rotation_y = lock_rotation_y;
    this.lock_rotation_z = lock_rotation_z;
  }
}

export class CollisionGroups extends $CustomType {
  constructor(membership, filter) {
    super();
    this.membership = membership;
    this.filter = filter;
  }
}

export class RigidBody extends $CustomType {
  constructor(kind, mass, restitution, friction, linear_damping, angular_damping, collider, ccd_enabled, axis_locks, collision_groups) {
    super();
    this.kind = kind;
    this.mass = mass;
    this.restitution = restitution;
    this.friction = friction;
    this.linear_damping = linear_damping;
    this.angular_damping = angular_damping;
    this.collider = collider;
    this.ccd_enabled = ccd_enabled;
    this.axis_locks = axis_locks;
    this.collision_groups = collision_groups;
  }
}

export class WorldConfig extends $CustomType {
  constructor(gravity, correspondances) {
    super();
    this.gravity = gravity;
    this.correspondances = correspondances;
  }
}

export class RaycastHit extends $CustomType {
  constructor(id, point, normal, distance) {
    super();
    this.id = id;
    this.point = point;
    this.normal = normal;
    this.distance = distance;
  }
}

/**
 * Two bodies started colliding
 */
export class CollisionStarted extends $CustomType {
  constructor(body_a, body_b) {
    super();
    this.body_a = body_a;
    this.body_b = body_b;
  }
}

/**
 * Two bodies stopped colliding
 */
export class CollisionEnded extends $CustomType {
  constructor(body_a, body_b) {
    super();
    this.body_a = body_a;
    this.body_b = body_b;
  }
}

class RigidBodyBuilder extends $CustomType {
  constructor(kind, collider, mass, restitution, friction, linear_damping, angular_damping, ccd_enabled, axis_locks, collision_groups) {
    super();
    this.kind = kind;
    this.collider = collider;
    this.mass = mass;
    this.restitution = restitution;
    this.friction = friction;
    this.linear_damping = linear_damping;
    this.angular_damping = angular_damping;
    this.ccd_enabled = ccd_enabled;
    this.axis_locks = axis_locks;
    this.collision_groups = collision_groups;
  }
}

function create_bimap(correspondances) {
  return $list.fold(
    correspondances,
    $bimap.new$(),
    (acc, item) => {
      let body;
      let string;
      body = item[0];
      string = item[1];
      return $bimap.insert(acc, body, string);
    },
  );
}

/**
 * Create a new rigid body builder
 *
 * ## Example
 *
 * ```gleam
 * let body = physics.new_rigid_body(physics.Dynamic)
 *   |> physics.body_collider(physics.Box(2.0, 2.0, 2.0))
 *   |> physics.body_mass(5.0)
 *   |> physics.build_body()
 * ```
 */
export function new_rigid_body(body_type) {
  return new RigidBodyBuilder(
    body_type,
    new $option.None(),
    new $option.None(),
    0.3,
    0.5,
    0.0,
    0.0,
    false,
    new AxisLock(false, false, false, false, false, false),
    new $option.None(),
  );
}

/**
 * Set the collider shape for the rigid body
 */
export function with_collider(builder, collider) {
  return new RigidBodyBuilder(
    builder.kind,
    new $option.Some(collider),
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Set the mass for the rigid body
 */
export function with_mass(builder, mass) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    new $option.Some(mass),
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Set the restitution (bounciness) for the rigid body
 */
export function with_restitution(builder, restitution) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Set the friction for the rigid body
 */
export function with_friction(builder, friction) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Set the linear damping for the rigid body
 */
export function with_linear_damping(builder, damping) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    damping,
    builder.angular_damping,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Set the angular damping for the rigid body
 */
export function with_angular_damping(builder, damping) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    damping,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Enable continuous collision detection for the rigid body
 */
export function with_body_ccd_enabled(builder) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    true,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Lock translation on the X axis
 */
export function with_lock_translation_x(builder) {
  let locks = builder.axis_locks;
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    new AxisLock(
      true,
      locks.lock_translation_y,
      locks.lock_translation_z,
      locks.lock_rotation_x,
      locks.lock_rotation_y,
      locks.lock_rotation_z,
    ),
    builder.collision_groups,
  );
}

/**
 * Lock translation on the Y axis
 */
export function with_lock_translation_y(builder) {
  let locks = builder.axis_locks;
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    new AxisLock(
      locks.lock_translation_x,
      true,
      locks.lock_translation_z,
      locks.lock_rotation_x,
      locks.lock_rotation_y,
      locks.lock_rotation_z,
    ),
    builder.collision_groups,
  );
}

/**
 * Lock translation on the Z axis
 */
export function with_lock_translation_z(builder) {
  let locks = builder.axis_locks;
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    new AxisLock(
      locks.lock_translation_x,
      locks.lock_translation_y,
      true,
      locks.lock_rotation_x,
      locks.lock_rotation_y,
      locks.lock_rotation_z,
    ),
    builder.collision_groups,
  );
}

/**
 * Lock rotation on the X axis (pitch)
 */
export function with_lock_rotation_x(builder) {
  let locks = builder.axis_locks;
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    new AxisLock(
      locks.lock_translation_x,
      locks.lock_translation_y,
      locks.lock_translation_z,
      true,
      locks.lock_rotation_y,
      locks.lock_rotation_z,
    ),
    builder.collision_groups,
  );
}

/**
 * Lock rotation on the Y axis (yaw)
 */
export function with_lock_rotation_y(builder) {
  let locks = builder.axis_locks;
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    new AxisLock(
      locks.lock_translation_x,
      locks.lock_translation_y,
      locks.lock_translation_z,
      locks.lock_rotation_x,
      true,
      locks.lock_rotation_z,
    ),
    builder.collision_groups,
  );
}

/**
 * Lock rotation on the Z axis (roll)
 */
export function with_lock_rotation_z(builder) {
  let locks = builder.axis_locks;
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    new AxisLock(
      locks.lock_translation_x,
      locks.lock_translation_y,
      locks.lock_translation_z,
      locks.lock_rotation_x,
      locks.lock_rotation_y,
      true,
    ),
    builder.collision_groups,
  );
}

/**
 * Set collision groups for filtering which objects can collide
 *
 * ## Example
 *
 * ```gleam
 * // Player belongs to layer 0, collides with enemies (1) and ground (2)
 * let body = physics.new_rigid_body(physics.Dynamic)
 *   |> physics.body_collider(physics.Capsule(1.0, 0.5))
 *   |> physics.body_collision_groups(
 *     membership: [0],
 *     filter: [1, 2]
 *   )
 *   |> physics.build_body()
 * ```
 */
export function with_collision_groups(builder, membership, filter) {
  return new RigidBodyBuilder(
    builder.kind,
    builder.collider,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    builder.ccd_enabled,
    builder.axis_locks,
    new $option.Some(new CollisionGroups(membership, filter)),
  );
}

/**
 * Build the final rigid body from the builder
 *
 * Returns an error if no collider was set.
 */
export function build(builder) {
  let $ = builder.collider;
  let collider;
  if ($ instanceof $option.Some) {
    collider = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "tiramisu/physics",
      424,
      "build",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 11719,
        end: 11770,
        pattern_start: 11730,
        pattern_end: 11751
      }
    )
  }
  return new RigidBody(
    builder.kind,
    builder.mass,
    builder.restitution,
    builder.friction,
    builder.linear_damping,
    builder.angular_damping,
    collider,
    builder.ccd_enabled,
    builder.axis_locks,
    builder.collision_groups,
  );
}

/**
 * Queue a force to be applied to a rigid body during the next physics step.
 * Returns updated world with the command queued.
 *
 * ## Example
 *
 * ```gleam
 * let world = physics.apply_force(world, "player", vec3.Vec3(0.0, 100.0, 0.0))
 * let world = physics.step(world, ctx.delta_time)  // Force is applied here
 * ```
 */
export function apply_force(world, id, force) {
  let command = new ApplyForce(id, force);
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    listPrepend(command, world.pending_commands),
    world.collider_to_body,
    world.collision_events,
    world.bimap,
  );
}

/**
 * Queue an impulse to be applied to a rigid body during the next physics step.
 * Returns updated world with the command queued.
 *
 * ## Example
 *
 * ```gleam
 * // Jump
 * let world = physics.apply_impulse(world, "player", vec3.Vec3(0.0, 10.0, 0.0))
 * ```
 */
export function apply_impulse(world, id, impulse) {
  let command = new ApplyImpulse(id, impulse);
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    listPrepend(command, world.pending_commands),
    world.collider_to_body,
    world.collision_events,
    world.bimap,
  );
}

/**
 * Queue a velocity change for a rigid body during the next physics step.
 * Returns updated world with the command queued.
 */
export function set_velocity(world, id, velocity) {
  let command = new SetVelocity(id, velocity);
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    listPrepend(command, world.pending_commands),
    world.collider_to_body,
    world.collision_events,
    world.bimap,
  );
}

/**
 * Queue an angular velocity change for a rigid body during the next physics step.
 * Returns updated world with the command queued.
 */
export function set_angular_velocity(world, id, velocity) {
  let command = new SetAngularVelocity(id, velocity);
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    listPrepend(command, world.pending_commands),
    world.collider_to_body,
    world.collision_events,
    world.bimap,
  );
}

/**
 * Queue a torque to be applied to a rigid body during the next physics step.
 * Returns updated world with the command queued.
 */
export function apply_torque(world, id, torque) {
  let command = new ApplyTorque(id, torque);
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    listPrepend(command, world.pending_commands),
    world.collider_to_body,
    world.collision_events,
    world.bimap,
  );
}

/**
 * Queue a torque impulse to be applied to a rigid body during the next physics step.
 * Returns updated world with the command queued.
 */
export function apply_torque_impulse(world, id, impulse) {
  let command = new ApplyTorqueImpulse(id, impulse);
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    listPrepend(command, world.pending_commands),
    world.collider_to_body,
    world.collision_events,
    world.bimap,
  );
}

/**
 * Cast a ray and return all hits along the ray
 *
 * Returns hits sorted by distance (closest first).
 *
 * ## Example
 *
 * ```gleam
 * // Check what's in front of the player
 * let hits = physics.raycast_all(world, origin, direction, max_distance: 100.0)
 *
 * // Find first enemy hit
 * let enemy_hit = list.find(hits, fn(hit) {
 *   string.starts_with(hit.body_id, "enemy_")
 * })
 * ```
 */
export function raycast_all(_, _1, _2, _3) {
  return toList([]);
}

/**
 * Get all collision events that occurred during the last physics step.
 *
 * Events are automatically collected when `step()` is called and stored in the world.
 *
 * ## Example
 *
 * ```gleam
 * let physics_world = physics.step(physics_world)
 * let collision_events = physics.get_collision_events(physics_world)
 *
 * list.each(collision_events, fn(event) {
 *   case event {
 *     physics.CollisionStarted(a, b) ->
 *       io.println(a <> " started colliding with " <> b)
 *     physics.CollisionEnded(a, b) ->
 *       io.println(a <> " ended colliding with " <> b)
 *   }
 * })
 * ```
 */
export function get_collision_events(world) {
  return world.collision_events;
}

/**
 * Convert a typed ID to a string ID using the BiMap
 * This is called from JavaScript when creating/removing physics bodies
 * 
 * @ignore
 */
export function id_to_string(world, id) {
  return $bimap.get(world.bimap, id);
}

/**
 * Convert list of collision layer indices (0-15) to a 16-bit bitmask
 *
 * For example, [0, 2, 3] becomes 0b0000000000001101 = 0x000D
 * 
 * @ignore
 */
function layers_to_bitmask(layers) {
  return $list.fold(
    layers,
    0,
    (mask, layer) => {
      let $ = (layer >= 0) && (layer <= 15);
      if ($) {
        let bit = $int.bitwise_shift_left(1, layer);
        return $int.bitwise_or(mask, bit);
      } else {
        return mask;
      }
    },
  );
}

/**
 * Pack membership and filter bitmasks into a single 32-bit value
 *
 * Rapier format: 16 upper bits = membership, 16 lower bits = filter
 * For example: membership=0x000D, filter=0x0004 -> 0x000D0004
 * 
 * @ignore
 */
function pack_collision_groups(membership, filter) {
  let membership_shifted = $int.bitwise_shift_left(membership, 16);
  return $int.bitwise_or(membership_shifted, filter);
}

/**
 * Convert CollisionGroups to Rapier's 32-bit packed format
 * 
 * @ignore
 */
export function collision_groups_to_bitmask(groups) {
  let membership_mask = layers_to_bitmask(groups.membership);
  let filter_mask = layers_to_bitmask(groups.filter);
  return pack_collision_groups(membership_mask, filter_mask);
}

/**
 * Get the current velocity of a rigid body
 */
export function get_velocity(world, id) {
  return $result.try$(
    $bimap.get(world.bimap, id),
    (string_id) => {
      return $result.try$(
        $dict.get(world.rapier_bodies, string_id),
        (rapier_body) => {
          let vel = get_body_linvel_ffi(rapier_body);
          return new Ok(new $vec3.Vec3(vel.x, vel.y, vel.z));
        },
      );
    },
  );
}

/**
 * Get the current angular velocity of a rigid body
 */
export function get_angular_velocity(world, id) {
  return $result.try$(
    $bimap.get(world.bimap, id),
    (string_id) => {
      return $result.try$(
        $dict.get(world.rapier_bodies, string_id),
        (rapier_body) => {
          let vel = get_body_angvel_ffi(rapier_body);
          return new Ok(new $vec3.Vec3(vel.x, vel.y, vel.z));
        },
      );
    },
  );
}

/**
 * Apply a single physics command via FFI
 * 
 * @ignore
 */
function apply_command(command, bimap, rapier_bodies) {
  if (command instanceof ApplyForce) {
    let id = command.id;
    let force = command.force;
    return $result.try$(
      $bimap.get(bimap, id),
      (string_id) => {
        return $result.try$(
          $dict.get(rapier_bodies, string_id),
          (rapier_body) => {
            add_body_force_ffi(rapier_body, force.x, force.y, force.z, true);
            return new Ok(undefined);
          },
        );
      },
    );
  } else if (command instanceof ApplyImpulse) {
    let id = command.id;
    let impulse = command.impulse;
    return $result.try$(
      $bimap.get(bimap, id),
      (string_id) => {
        return $result.try$(
          $dict.get(rapier_bodies, string_id),
          (rapier_body) => {
            apply_body_impulse_ffi(
              rapier_body,
              impulse.x,
              impulse.y,
              impulse.z,
              true,
            );
            return new Ok(undefined);
          },
        );
      },
    );
  } else if (command instanceof SetVelocity) {
    let id = command.id;
    let velocity = command.velocity;
    return $result.try$(
      $bimap.get(bimap, id),
      (string_id) => {
        return $result.try$(
          $dict.get(rapier_bodies, string_id),
          (rapier_body) => {
            set_body_linvel_ffi(
              rapier_body,
              velocity.x,
              velocity.y,
              velocity.z,
              true,
            );
            return new Ok(undefined);
          },
        );
      },
    );
  } else if (command instanceof SetAngularVelocity) {
    let id = command.id;
    let velocity = command.velocity;
    return $result.try$(
      $bimap.get(bimap, id),
      (string_id) => {
        return $result.try$(
          $dict.get(rapier_bodies, string_id),
          (rapier_body) => {
            set_body_angvel_ffi(
              rapier_body,
              velocity.x,
              velocity.y,
              velocity.z,
              true,
            );
            return new Ok(undefined);
          },
        );
      },
    );
  } else if (command instanceof ApplyTorque) {
    let id = command.id;
    let torque = command.torque;
    return $result.try$(
      $bimap.get(bimap, id),
      (string_id) => {
        return $result.try$(
          $dict.get(rapier_bodies, string_id),
          (rapier_body) => {
            add_body_torque_ffi(rapier_body, torque.x, torque.y, torque.z, true);
            return new Ok(undefined);
          },
        );
      },
    );
  } else {
    let id = command.id;
    let impulse = command.impulse;
    return $result.try$(
      $bimap.get(bimap, id),
      (string_id) => {
        return $result.try$(
          $dict.get(rapier_bodies, string_id),
          (rapier_body) => {
            apply_body_torque_impulse_ffi(
              rapier_body,
              impulse.x,
              impulse.y,
              impulse.z,
              true,
            );
            return new Ok(undefined);
          },
        );
      },
    );
  }
}

/**
 * Get the raw position and quaternion rotation from a rigid body.
 *
 * This returns the rotation as a quaternion directly from Rapier,
 * avoiding conversion to Euler angles which can cause rotation errors.
 *
 * This is used internally by the renderer for physics synchronization.
 *
 * ## Example
 *
 * ```gleam
 * case physics.get_body_transform_raw(physics_world, Cube1) {
 *   Ok(#(position, quaternion)) -> {
 *     // Use position and quaternion directly
 *   }
 *   Error(_) -> // Handle missing body
 * }
 * ```
 * 
 * @ignore
 */
export function get_body_transform_raw(physics_world, id) {
  return $result.try$(
    $bimap.get(physics_world.bimap, id),
    (string_id) => {
      return $result.try$(
        $dict.get(physics_world.rapier_bodies, string_id),
        (rapier_body) => {
          let translation = get_body_translation_ffi(rapier_body);
          let quaternion = get_body_rotation_ffi(rapier_body);
          return new Ok([translation, quaternion]);
        },
      );
    },
  );
}

/**
 * Iterate over all physics bodies with raw quaternion data.
 *
 * This is used by the renderer for physics synchronization, avoiding
 * quaternion-to-Euler conversion which can cause rotation errors.
 * 
 * @ignore
 */
export function for_each_body_raw(world, callback) {
  let string_ids = $dict.keys(world.rapier_bodies);
  return $list.each(
    string_ids,
    (string_id) => {
      let $ = $bimap.get_val(world.bimap, string_id);
      if ($ instanceof Ok) {
        let typed_id = $[0];
        let $1 = get_body_transform_raw(world, typed_id);
        if ($1 instanceof Ok) {
          let position = $1[0][0];
          let quaternion = $1[0][1];
          return callback(typed_id, position, quaternion);
        } else {
          return undefined;
        }
      } else {
        return undefined;
      }
    },
  );
}

/**
 * Get the current transform of a rigid body.
 *
 * Queries the physics simulation directly, so it always returns the latest position
 * even for bodies that were just created in the current frame.
 *
 * ## Example
 *
 * ```gleam
 * let cube_transform = case physics.get_transform(physics_world, Cube1) {
 *   Ok(t) -> t
 *   Error(_) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
 * }
 * ```
 */
export function get_transform(physics_world, id) {
  return $result.try$(
    $bimap.get(physics_world.bimap, id),
    (string_id) => {
      return $result.try$(
        $dict.get(physics_world.rapier_bodies, string_id),
        (rapier_body) => {
          let translation = get_body_translation_ffi(rapier_body);
          let rotation_quat = get_body_rotation_ffi(rapier_body);
          let euler = quat_to_euler_ffi(
            rotation_quat.x,
            rotation_quat.y,
            rotation_quat.z,
            rotation_quat.w,
          );
          return new Ok(
            (() => {
              let _pipe = $transform.identity;
              let _pipe$1 = $transform.with_position(
                _pipe,
                new $vec3.Vec3(translation.x, translation.y, translation.z),
              );
              return $transform.with_rotation(_pipe$1, euler);
            })(),
          );
        },
      );
    },
  );
}

function for_each_body_internal(rapier_bodies, bimap, world, callback) {
  let string_ids = $dict.keys(rapier_bodies);
  return $list.each(
    string_ids,
    (string_id) => {
      let $ = $bimap.get_val(bimap, string_id);
      if ($ instanceof Ok) {
        let typed_id = $[0];
        let $1 = get_transform(world, typed_id);
        if ($1 instanceof Ok) {
          let transform = $1[0];
          return callback(typed_id, transform);
        } else {
          return undefined;
        }
      } else {
        return undefined;
      }
    },
  );
}

/**
 * Iterate over all physics bodies and call a function for each
 * This keeps all internal field access within the physics module
 * 
 * @ignore
 */
export function for_each_body(world, callback) {
  return for_each_body_internal(
    world.rapier_bodies,
    world.bimap,
    world,
    callback,
  );
}

/**
 * Cast a ray and return the first hit
 *
 * Useful for shooting mechanics, line-of-sight checks, and ground detection.
 *
 * ## Example
 *
 * ```gleam
 * // Cast ray downward from player position
 * let origin = player_position
 * let direction = vec3.Vec3(0.0, -1.0, 0.0)
 *
 * case physics.raycast(world, origin, direction, max_distance: 10.0) {
 *   Ok(hit) -> {
 *     // Found ground at hit.distance units below player
 *     io.println("Hit body with ID")
 *   }
 *   Error(Nil) -> {
 *     // No ground found within 10 units
 *   }
 * }
 * ```
 */
export function raycast(world, origin, direction, max_distance) {
  let ray = create_ray_ffi(
    origin.x,
    origin.y,
    origin.z,
    direction.x,
    direction.y,
    direction.z,
  );
  let $ = cast_ray_and_get_normal_ffi(world.world, ray, max_distance, true);
  if ($ instanceof Ok) {
    let hit_info = $[0];
    let collider_handle = get_hit_collider_handle_ffi(hit_info);
    let $1 = $dict.get(world.collider_to_body, collider_handle);
    if ($1 instanceof Ok) {
      let string_id = $1[0];
      let $2 = $bimap.get_val(world.bimap, string_id);
      if ($2 instanceof Ok) {
        let typed_id = $2[0];
        let toi = get_hit_toi_ffi(hit_info);
        let point = ray_point_at_ffi(ray, toi);
        let normal = get_hit_normal_ffi(hit_info);
        return new Ok(
          new RaycastHit(
            typed_id,
            new $vec3.Vec3(point.x, point.y, point.z),
            new $vec3.Vec3(normal.x, normal.y, normal.z),
            toi,
          ),
        );
      } else {
        return new Error(undefined);
      }
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

/**
 * Initialize the global physics world
 * Takes a WorldConfig as Dynamic and extracts gravity
 */
export function create_world(config) {
  let world = create_world_ffi(
    config.gravity.x,
    config.gravity.y,
    config.gravity.z,
  );
  let queue = create_event_queue_ffi(true);
  return [world, queue];
}

/**
 * Create a new physics world (call this in your init function)
 */
export function new_world(config) {
  let $ = create_world(config);
  let world;
  let queue;
  world = $[0];
  queue = $[1];
  let bimap = create_bimap(config.correspondances);
  return new PhysicsWorld(
    world,
    queue,
    $dict.new$(),
    $dict.new$(),
    toList([]),
    $dict.new$(),
    toList([]),
    bimap,
  );
}

/**
 * Step the physics world forward by delta_time
 */
export function step_world(physics_world) {
  step_world_ffi(physics_world.world, physics_world.queue);
  return undefined;
}

/**
 * Create a rigid body in the physics world
 * This is called by the renderer when a scene node with physics is added
 * 
 * @ignore
 */
export function create_body(world, id, config, transform) {
  let $ = $bimap.get(world.bimap, id);
  if ($ instanceof Ok) {
    let string_id = $[0];
    let _block;
    let $1 = config.kind;
    if ($1 instanceof Dynamic) {
      _block = create_dynamic_body_desc_ffi();
    } else if ($1 instanceof Kinematic) {
      _block = create_kinematic_body_desc_ffi();
    } else {
      _block = create_fixed_body_desc_ffi();
    }
    let body_desc = _block;
    let pos = transform.position;
    set_body_translation_ffi(body_desc, pos.x, pos.y, pos.z);
    let quat = euler_to_quat_ffi(
      transform.rotation.x,
      transform.rotation.y,
      transform.rotation.z,
    );
    set_body_rotation_ffi(body_desc, quat.x, quat.y, quat.z, quat.w);
    set_linear_damping_ffi(body_desc, config.linear_damping);
    set_angular_damping_ffi(body_desc, config.angular_damping);
    let $2 = config.ccd_enabled;
    if ($2) {
      set_ccd_enabled_ffi(body_desc, true)
    } else {
      undefined
    }
    set_enabled_translations_ffi(
      body_desc,
      !config.axis_locks.lock_translation_x,
      !config.axis_locks.lock_translation_y,
      !config.axis_locks.lock_translation_z,
      true,
    );
    set_enabled_rotations_ffi(
      body_desc,
      !config.axis_locks.lock_rotation_x,
      !config.axis_locks.lock_rotation_y,
      !config.axis_locks.lock_rotation_z,
      true,
    );
    let rapier_body = create_rigid_body_ffi(world.world, body_desc);
    let _block$1;
    let $3 = config.collider;
    if ($3 instanceof Box) {
      let width = $3.width;
      let height = $3.height;
      let depth = $3.depth;
      _block$1 = create_cuboid_collider_desc_ffi(
        width / 2.0,
        height / 2.0,
        depth / 2.0,
      );
    } else if ($3 instanceof Sphere) {
      let radius = $3.radius;
      _block$1 = create_ball_collider_desc_ffi(radius);
    } else if ($3 instanceof Capsule) {
      let half_height = $3.half_height;
      let radius = $3.radius;
      _block$1 = create_capsule_collider_desc_ffi(half_height, radius);
    } else {
      let half_height = $3.half_height;
      let radius = $3.radius;
      _block$1 = create_cylinder_collider_desc_ffi(half_height, radius);
    }
    let collider_desc = _block$1;
    set_collider_restitution_ffi(collider_desc, config.restitution);
    set_collider_friction_ffi(collider_desc, config.friction);
    let $4 = config.mass;
    if ($4 instanceof $option.Some) {
      let mass = $4[0];
      set_collider_mass_ffi(collider_desc, mass)
    } else {
      undefined
    }
    let $5 = config.collision_groups;
    if ($5 instanceof $option.Some) {
      let groups = $5[0];
      let bitmask = collision_groups_to_bitmask(groups);
      set_collider_collision_groups_ffi(collider_desc, bitmask)
    } else {
      undefined
    }
    let collider = create_collider_ffi(world.world, collider_desc, rapier_body);
    let collider_handle = get_collider_handle_ffi(collider);
    return new PhysicsWorld(
      world.world,
      world.queue,
      $dict.insert(world.bodies, id, config),
      $dict.insert(world.rapier_bodies, string_id, rapier_body),
      world.pending_commands,
      $dict.insert(world.collider_to_body, collider_handle, string_id),
      world.collision_events,
      world.bimap,
    );
  } else {
    return world;
  }
}

/**
 * Get all collider handles for a rigid body
 * 
 * @ignore
 */
function get_body_collider_handles(body, num_colliders) {
  let _pipe = $list.range(0, num_colliders - 1);
  return $list.filter_map(
    _pipe,
    (i) => {
      let $ = get_body_collider_ffi(body, i);
      if ($ instanceof Ok) {
        let collider = $[0];
        return new Ok(get_collider_handle_ffi(collider));
      } else {
        return new Error(undefined);
      }
    },
  );
}

/**
 * Remove a rigid body from the physics world
 * This is called by the renderer when a scene node with physics is removed
 * 
 * @ignore
 */
export function remove_body(world, id) {
  let $ = $bimap.get(world.bimap, id);
  if ($ instanceof Ok) {
    let string_id = $[0];
    let $1 = $dict.get(world.rapier_bodies, string_id);
    if ($1 instanceof Ok) {
      let rapier_body = $1[0];
      let num_colliders = get_body_num_colliders_ffi(rapier_body);
      let collider_handles = get_body_collider_handles(
        rapier_body,
        num_colliders,
      );
      let updated_collider_map = $list.fold(
        collider_handles,
        world.collider_to_body,
        (map, handle) => { return $dict.delete$(map, handle); },
      );
      remove_rigid_body_ffi(world.world, rapier_body);
      return new PhysicsWorld(
        world.world,
        world.queue,
        $dict.delete$(world.bodies, id),
        $dict.delete$(world.rapier_bodies, string_id),
        world.pending_commands,
        updated_collider_map,
        world.collision_events,
        world.bimap,
      );
    } else {
      return world;
    }
  } else {
    return world;
  }
}

/**
 * Drain collision events from the Rapier event queue
 * Converts collider handles to body IDs using the collider_to_body mapping
 * 
 * @ignore
 */
function drain_collision_events(queue, collider_to_body) {
  let raw_events = drain_collision_events_ffi(queue);
  return $list.filter_map(
    raw_events,
    (raw_event) => {
      let handle1;
      let handle2;
      let started;
      handle1 = raw_event[0];
      handle2 = raw_event[1];
      started = raw_event[2];
      let $ = $dict.get(collider_to_body, handle1);
      let $1 = $dict.get(collider_to_body, handle2);
      if ($1 instanceof Ok && $ instanceof Ok) {
        let body_id2 = $1[0];
        let body_id1 = $[0];
        if (started) {
          return new Ok(new CollisionStarted(body_id1, body_id2));
        } else {
          return new Ok(new CollisionEnded(body_id1, body_id2));
        }
      } else {
        return new Error(undefined);
      }
    },
  );
}

/**
 * Step the physics simulation forward
 * This should be called in your update function each frame
 * Returns updated world with new transforms for all bodies
 */
export function step(world) {
  $list.each(
    world.pending_commands,
    (command) => {
      let $ = apply_command(command, world.bimap, world.rapier_bodies);
      
      return undefined;
    },
  );
  step_world_ffi(world.world, world.queue);
  let collision_events = drain_collision_events(
    world.queue,
    world.collider_to_body,
  );
  return new PhysicsWorld(
    world.world,
    world.queue,
    world.bodies,
    world.rapier_bodies,
    toList([]),
    world.collider_to_body,
    collision_events,
    world.bimap,
  );
}
