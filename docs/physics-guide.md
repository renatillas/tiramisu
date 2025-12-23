# Physics Guide

Tiramisu integrates the [Rapier physics engine](https://rapier.rs/) to provide realistic physics simulation for your games. This guide covers everything from basic setup to advanced collision handling.

## Overview

### What is Rapier?

Rapier is a fast, cross-platform physics engine with:
- **Rigid body dynamics** - Gravity, forces, collisions
- **Collision detection** - Broad-phase and narrow-phase
- **Collision shapes** - Boxes, spheres, cylinders, capsules
- **Constraints** - Joints, springs (coming soon)
- **Performance** - Written in Rust, compiled to WebAssembly

### Tiramisu's Approach

Physics in Tiramisu follows the same **declarative, immutable** pattern as the rest of the engine:

1. **Declare physics bodies** alongside your scene nodes
2. **Initialize physics world** in your `init()` function
3. **Step simulation** in your `update()` function
4. **Query transforms** in your `view()` function

## Quick Start

### Basic Physics Scene

```gleam
import gleam/option
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Id {
  Ground
  Ball
}

pub type Model {
  Model
}

pub type Msg {
  Tick
}

pub fn main() {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  // Create physics world with Earth gravity
  let physics_world = physics.new_world(
    physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0))
  )

  #(Model, effect.tick(Tick), option.Some(physics_world))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let assert option.Some(physics_world) = ctx.physics_world

  case msg {
    Tick -> {
      // Step the physics simulation (requires delta_time for fixed timestep)
      let new_physics_world = physics.step(physics_world, ctx.delta_time)
      #(model, effect.tick(Tick), option.Some(new_physics_world))
    }
  }
}

fn view(model: Model, ctx: tiramisu.Context) -> scene.Node(Id) {
  let assert option.Some(physics_world) = ctx.physics_world

  let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(ground_geo) = geometry.box(width: 20.0, height: 0.5, depth: 20.0)
  let assert Ok(ground_mat) = material.new()
    |> material.with_color(0x808080)
    |> material.build()

  let assert Ok(ball_geo) = geometry.sphere(radius: 1.0, width_segments: 32, height_segments: 16)
  let assert Ok(ball_mat) = material.new()
    |> material.with_color(0xff4444)
    |> material.build()

  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [
      // Camera
      scene.camera(
        id: "camera",
        camera: cam,
        transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
        look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
        active: True,
        viewport: option.None,
        children: [],
        postprocessing: option.None,
      ),

      // Static ground
      scene.mesh(
        id: Ground,
        geometry: ground_geo,
        material: ground_mat,
        transform: transform.identity,
        physics: option.Some(
          physics.new_rigid_body(physics.Fixed)
          |> physics.with_collider(physics.Box(transform.identity, 20.0, 0.5, 20.0))
          |> physics.build()
        ),
      ),

      // Bouncing ball
      scene.mesh(
        id: Ball,
        geometry: ball_geo,
        material: ball_mat,
        // Get transform from physics simulation, or use initial position
        transform: case physics.get_transform(physics_world, Ball) {
          Ok(t) -> t
          Error(Nil) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
        },
        physics: option.Some(
          physics.new_rigid_body(physics.Dynamic)
          |> physics.with_collider(physics.Sphere(transform.identity, 1.0))
          |> physics.with_mass(1.0)
          |> physics.with_restitution(0.8)  // Very bouncy!
          |> physics.build()
        ),
      ),
    ],
  )
}
```

## Physics World

### Creating a World

The physics world is initialized in your `init()` function and returned as the third element of the tuple:

```gleam
fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let physics_world = physics.new_world(
    physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0)  // Earth gravity (m/s²)
    )
  )

  #(Model(...), effect.tick(Tick), option.Some(physics_world))
}
```

**Common gravity values:**
- Earth: `vec3.Vec3(0.0, -9.81, 0.0)`
- Moon: `vec3.Vec3(0.0, -1.62, 0.0)`
- Zero gravity: `vec3.Vec3(0.0, 0.0, 0.0)`
- Custom (platformer): `vec3.Vec3(0.0, -20.0, 0.0)` (faster falling)

### Stepping the Simulation

Call `physics.step()` every frame in your `update()` function. **Important**: `physics.step()` now requires the `delta_time` parameter (in milliseconds) to implement a fixed timestep physics loop.

```gleam
fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let assert option.Some(physics_world) = ctx.physics_world

  case msg {
    Tick -> {
      // Pass delta_time for fixed timestep (prevents game slowdown)
      let new_physics_world = physics.step(physics_world, ctx.delta_time)
      #(model, effect.tick(Tick), option.Some(new_physics_world))
    }
  }
}
```

The step function:
- Uses fixed timestep (60 Hz) with accumulator pattern
- Prevents game slowdown when FPS drops
- Applies forces and gravity
- Detects and resolves collisions
- Updates body positions and velocities
- Generates collision events

### Accessing the World in View

```gleam
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node(Id) {
  let assert option.Some(physics_world) = ctx.physics_world

  // Get physics transforms for dynamic bodies
  let player_transform = case physics.get_transform(physics_world, PlayerId) {
    Ok(t) -> t
    Error(Nil) -> transform.identity  // Fallback for first frame
  }

  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [
      // ... scene nodes
    ],
  )
}
```

## Rigid Bodies

### Body Types

**Dynamic** - Affected by forces, gravity, and collisions:
```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_mass(5.0)
  |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
  |> physics.build()
```

Use for: Moveable objects, projectiles, character physics

**Kinematic** - Moved programmatically, affects dynamic bodies but not affected by forces:
```gleam
physics.new_rigid_body(physics.Kinematic)
  |> physics.with_collider(physics.Box(transform.identity, 5.0, 5.0, 5.0))
  |> physics.build()
```

Use for: Moving platforms, doors, elevators

**Fixed** - Static, immovable objects:
```gleam
physics.new_rigid_body(physics.Fixed)
  |> physics.with_collider(physics.Box(transform.identity, 100.0, 1.0, 100.0))
  |> physics.build()
```

Use for: Terrain, walls, floors, static obstacles

### Builder Pattern

All physics bodies use the builder pattern:

```gleam
let body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(collider_shape)
  |> physics.with_mass(10.0)
  |> physics.with_restitution(0.5)
  |> physics.with_friction(0.8)
  |> physics.with_linear_damping(0.1)
  |> physics.with_angular_damping(0.1)
  |> physics.with_collision_groups(groups)
  |> physics.with_axis_locks(locks)
  |> physics.build()
```

## Collider Shapes

### Box Collider

Box-shaped collision volume:

```gleam
physics.Box(
  offset: transform.identity,  // Offset from body center
  width: 2.0,                   // Full width (not half-extents)
  height: 1.0,                  // Full height
  depth: 2.0,                   // Full depth
)
```

**Use for:** Crates, buildings, walls, platforms

### Sphere Collider

Spherical collision volume:

```gleam
physics.Sphere(
  offset: transform.identity,
  radius: 1.0,
)
```

**Use for:** Balls, planets, round objects, character approximation

### Capsule Collider

Cylinder with hemispherical caps (best for characters):

```gleam
physics.Capsule(
  offset: transform.identity,
  half_height: 1.0,  // Half-height of cylindrical section
  radius: 0.5,       // Radius of cylinder and caps
)
```

**Use for:** Characters, pills, rounded objects

### Cylinder Collider

Cylindrical collision volume:

```gleam
physics.Cylinder(
  offset: transform.identity,
  half_height: 2.0,  // Half of total height
  radius: 1.0,       // Radius of cylinder
)
```

**Use for:** Barrels, columns, trees

### Collider Offset

All colliders can be offset from the body's center:

```gleam
// Collider at body center
physics.Box(transform.identity, 1.0, 1.0, 1.0)

// Collider offset 0.5 units up
physics.Box(
  transform.at(position: vec3.Vec3(0.0, 0.5, 0.0)),
  1.0, 1.0, 1.0
)
```

## Physics Properties

### Mass

Mass affects how forces influence dynamic bodies. Higher mass = harder to move.

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_mass(5.0)  // 5 kg
  |> physics.build()
```

**Guidelines:**
- Small objects: 0.1 - 1.0 kg
- Medium objects: 1.0 - 10.0 kg
- Large objects: 10.0 - 100.0 kg
- Vehicles: 1000.0+ kg

### Restitution (Bounciness)

Controls how much energy is preserved when bouncing:

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_restitution(0.8)  // 0.0 = no bounce, 1.0 = perfect bounce
  |> physics.build()
```

**Common values:**
- Rubber ball: 0.8 - 0.95
- Basketball: 0.6 - 0.8
- Wood: 0.3 - 0.5
- Metal: 0.2 - 0.4
- No bounce: 0.0

### Friction

Resistance when surfaces slide against each other:

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_friction(0.5)  // 0.0 = ice, 1.0+ = very sticky
  |> physics.build()
```

**Common values:**
- Ice: 0.05 - 0.1
- Smooth surfaces: 0.2 - 0.4
- Default: 0.5
- Rough surfaces: 0.7 - 0.9
- Very sticky: 1.0+

### Damping

Reduces velocity over time (simulates air resistance):

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_linear_damping(0.1)   // Slows down movement
  |> physics.with_angular_damping(0.1)  // Slows down rotation
  |> physics.build()
```

**Guidelines:**
- No damping: 0.0
- Light damping: 0.01 - 0.1
- Medium damping: 0.1 - 0.5
- Heavy damping: 0.5 - 2.0

## Forces and Motion

### Applying Forces

Forces are applied over time (acceleration):

```gleam
// Queue a force to be applied on next physics step
let physics_world = physics.apply_force(
  physics_world,
  body_id: PlayerId,
  force: vec3.Vec3(100.0, 0.0, 0.0),  // Push 100 N to the right
)
```

**Use for:** Continuous acceleration, wind, thrusters

### Applying Impulses

Impulses are instant velocity changes:

```gleam
// Instant velocity change (like a jump or explosion)
let physics_world = physics.apply_impulse(
  physics_world,
  body_id: PlayerId,
  impulse: vec3.Vec3(0.0, 500.0, 0.0),  // Instant upward velocity
)
```

**Use for:** Jumps, explosions, instant hits

### Setting Velocity Directly

```gleam
// Set exact velocity
let physics_world = physics.set_velocity(
  physics_world,
  body_id: PlayerId,
  velocity: vec3.Vec3(5.0, 0.0, 0.0),  // Move at 5 m/s right
)
```

**Use for:** Character controllers, vehicles, special movement

### Angular Forces and Torques

```gleam
// Apply rotational force
let physics_world = physics.apply_torque(
  physics_world,
  body_id: ObjectId,
  torque: vec3.Vec3(0.0, 10.0, 0.0),  // Rotate around Y axis
)

// Apply instant rotational impulse
let physics_world = physics.apply_torque_impulse(
  physics_world,
  body_id: ObjectId,
  impulse: vec3.Vec3(0.0, 50.0, 0.0),
)

// Set angular velocity directly
let physics_world = physics.set_angular_velocity(
  physics_world,
  body_id: ObjectId,
  velocity: vec3.Vec3(0.0, 3.14, 0.0),  // Rotate 180°/second around Y
)
```

### Example: Character Controller

```gleam
fn handle_player_input(
  physics_world: physics.PhysicsWorld(Id),
  input: input.InputState,
) -> physics.PhysicsWorld(Id) {
  let move_force = 500.0
  let jump_impulse = 300.0

  let physics_world = case input.is_key_pressed(input, input.KeyW) {
    True -> physics.apply_force(physics_world, PlayerId, vec3.Vec3(0.0, 0.0, -move_force))
    False -> physics_world
  }

  let physics_world = case input.is_key_pressed(input, input.KeyS) {
    True -> physics.apply_force(physics_world, PlayerId, vec3.Vec3(0.0, 0.0, move_force))
    False -> physics_world
  }

  let physics_world = case input.is_key_just_pressed(input, input.KeySpace) {
    True -> physics.apply_impulse(physics_world, PlayerId, vec3.Vec3(0.0, jump_impulse, 0.0))
    False -> physics_world
  }

  physics_world
}
```

## Collision Detection

### Collision Events

Get collision events from the physics world after stepping:

```gleam
fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  let assert option.Some(physics_world) = ctx.physics_world

  case msg {
    Tick -> {
      let new_physics_world = physics.step(physics_world, ctx.delta_time)

      // Check for collisions
      let events = physics.get_collision_events(new_physics_world)

      // Process collision events
      let model = list.fold(events, model, fn(acc_model, event) {
        case event {
          physics.CollisionStarted(body1, body2) -> {
            // Handle collision started
            io.println("Collision between " <> debug.inspect(body1) <> " and " <> debug.inspect(body2))
            acc_model
          }
          physics.CollisionEnded(body1, body2) -> {
            // Handle collision ended
            acc_model
          }
        }
      })

      #(model, effect.tick(Tick), option.Some(new_physics_world))
    }
  }
}
```

### Collision Groups

Control which objects can collide using collision groups:

```gleam
// Player: belongs to layer 0, collides with layers 1 (enemies) and 2 (ground)
let player_groups = physics.CollisionGroups(
  membership: [0],
  filter: [1, 2],
)

let player_body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Capsule(transform.identity, 1.0, 0.5))
  |> physics.with_collision_groups(player_groups)
  |> physics.build()

// Enemy: belongs to layer 1, collides with layers 0 (player) and 2 (ground)
let enemy_groups = physics.CollisionGroups(
  membership: [1],
  filter: [0, 2],
)

let enemy_body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
  |> physics.with_collision_groups(enemy_groups)
  |> physics.build()

// Ground: belongs to layer 2, collides with everything
let ground_groups = physics.CollisionGroups(
  membership: [2],
  filter: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
)
```

**Layer system:**
- Layers 0-15 available (16 total)
- Use meaningful layer assignments:
  - 0: Player
  - 1: Enemies
  - 2: Ground/Walls
  - 3: Projectiles
  - 4: Triggers
  - 5: Pickups
  - etc.

### Example: Layer-Based Damage System

```gleam
pub type Id {
  Player
  Enemy(Int)
  Projectile(Int)
  Ground
}

fn setup_collision_layers() {
  let player_groups = physics.CollisionGroups(
    membership: [0],           // Player layer
    filter: [1, 2, 4, 5],     // Collides with enemies, ground, triggers, pickups
  )

  let enemy_groups = physics.CollisionGroups(
    membership: [1],           // Enemy layer
    filter: [0, 2, 3],        // Collides with player, ground, projectiles
  )

  let projectile_groups = physics.CollisionGroups(
    membership: [3],           // Projectile layer
    filter: [1, 2],           // Only collides with enemies and ground
  )

  let ground_groups = physics.CollisionGroups(
    membership: [2],           // Ground layer
    filter: [0, 1, 3],        // Collides with player, enemies, projectiles
  )

  // Apply to bodies...
}

fn process_collisions(events: List(physics.CollisionEvent), model: Model) -> Model {
  list.fold(events, model, fn(acc_model, event) {
    case event {
      physics.CollisionStarted(Projectile(id), Enemy(enemy_id)) -> {
        // Projectile hit enemy - apply damage
        damage_enemy(acc_model, enemy_id, 10)
      }
      physics.CollisionStarted(Player, Enemy(_)) -> {
        // Player touched enemy - take damage
        damage_player(acc_model, 5)
      }
      _ -> acc_model
    }
  })
}
```

## Axis Locks

Restrict movement and rotation on specific axes:

```gleam
// Lock all rotation (useful for top-down games)
let locks = physics.AxisLock(
  lock_translation_x: False,
  lock_translation_y: False,
  lock_translation_z: False,
  lock_rotation_x: True,   // Can't pitch
  lock_rotation_y: True,   // Can't yaw
  lock_rotation_z: True,   // Can't roll
)

let body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Capsule(transform.identity, 1.0, 0.5))
  |> physics.with_axis_locks(locks)
  |> physics.build()
```

**Common patterns:**

**Top-down game (2D movement, no rotation):**
```gleam
physics.AxisLock(
  lock_translation_x: False,
  lock_translation_y: True,   // Lock vertical movement
  lock_translation_z: False,
  lock_rotation_x: True,      // Lock all rotation
  lock_rotation_y: True,
  lock_rotation_z: True,
)
```

**Platformer (2D side-scroller):**
```gleam
physics.AxisLock(
  lock_translation_x: False,
  lock_translation_y: False,
  lock_translation_z: True,   // Lock depth
  lock_rotation_x: True,      // Lock all rotation
  lock_rotation_y: True,
  lock_rotation_z: True,
)
```

**Standing character (upright, can rotate on Y only):**
```gleam
physics.AxisLock(
  lock_translation_x: False,
  lock_translation_y: False,
  lock_translation_z: False,
  lock_rotation_x: True,      // Can't tip over
  lock_rotation_y: False,     // Can turn left/right
  lock_rotation_z: True,      // Can't tip over
)
```

## Physics Queries

### Raycasting

Cast a ray and find the first object hit:

```gleam
let result = physics.raycast(
  physics_world,
  origin: vec3.Vec3(0.0, 10.0, 0.0),
  direction: vec3.Vec3(0.0, -1.0, 0.0),  // Shoot downward
  max_distance: 100.0,
  solid: True,  // Stop at first hit
)

case result {
  option.Some(#(body_id, hit_point, hit_normal, distance)) -> {
    io.println("Hit " <> debug.inspect(body_id) <> " at distance " <> float.to_string(distance))
  }
  option.None -> {
    io.println("No hit")
  }
}
```

**Use for:** Ground detection, shooting, line of sight, mouse picking

## Debug Visualization

Enable collider wireframes for debugging:

```gleam
import tiramisu/debug

fn view(model: Model, ctx: tiramisu.Context) -> List(scene.Node(Id)) {
  let assert option.Some(physics_world) = ctx.physics_world

  // Show/hide debug wireframes with a key press
  case model.debug_mode {
    True -> debug.show_collider_wireframes(physics_world, True)
    False -> debug.show_collider_wireframes(physics_world, False)
  }

  // ... rest of scene
}
```

Or manually visualize specific colliders:

```gleam
import tiramisu/debug

// Visualize a collider shape
let collider_shape = physics.Box(transform.identity, 2.0, 1.0, 2.0)

let debug_vis = debug.collider(
  id: "player-collider-debug",
  shape: collider_shape,
  transform: player_transform,
  color: debug.color_green,
)
```

## Summary

**Key concepts:**
- Physics world is initialized in `init()` and stepped in `update()`
- Three body types: Dynamic, Kinematic, Fixed
- Four collider shapes: Box, Sphere, Capsule, Cylinder
- Forces are continuous, impulses are instant
- Use collision events for game logic (damage, triggers, etc.)
- Use collision groups to control what collides with what
- Use axis locks for 2D games or to prevent tipping
- Query physics with raycasts and shape casts
- Debug with wireframe visualization

**Performance tips:**
- Use Fixed bodies for static geometry
- Use simple collider shapes
- Use collision groups to reduce collision checks
- Batch force applications before stepping
- Consider spatial partitioning for large numbers of bodies

**Next steps:**
- Try the physics examples: `examples/17-physics_demo` and `examples/23-physics_advanced`
- Experiment with different body types and properties
- Build a character controller or projectile system
- Implement collision-based gameplay mechanics
