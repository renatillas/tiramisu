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
import gleam/option.{None, Some}
import gleam/time/duration
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
import vec/vec2
import vec/vec3

pub type Model {
  Model
}

pub type Msg {
  Tick
  BackgroundSet
  BackgroundFailed
}

pub fn main() {
  let assert Ok(_) = tiramisu.run(
    selector: "#app",
    dimensions: None,
    bridge: None,
    init: init,
    update: update,
    view: view,
  )
  Nil
}

fn init(ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  // Create physics world with Earth gravity
  let physics_world = physics.new_world(
    physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0))
  )

  // Set background color
  let set_bg = background.set(
    ctx.scene,
    background.Color(0x1a1a2e),
    BackgroundSet,
    BackgroundFailed,
  )

  #(Model, effect.batch([effect.tick(Tick), set_bg]), Some(physics_world))
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  let assert Some(physics_world) = ctx.physics_world

  case msg {
    BackgroundSet | BackgroundFailed -> #(model, effect.none(), None)
    Tick -> {
      // Step the physics simulation (requires delta_time as Duration)
      let new_physics_world = physics.step(physics_world, ctx.delta_time)
      #(model, effect.tick(Tick), Some(new_physics_world))
    }
  }
}

fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  let assert Some(physics_world) = ctx.physics_world

  let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let assert Ok(ground_geo) = geometry.box(size: vec3.Vec3(20.0, 0.5, 20.0))
  let assert Ok(ground_mat) = material.new()
    |> material.with_color(0x808080)
    |> material.build()

  let assert Ok(ball_geo) = geometry.sphere(radius: 1.0, segments: vec2.Vec2(32, 16))
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
        look_at: Some(vec3.Vec3(0.0, 0.0, 0.0)),
        active: True,
        viewport: None,
        postprocessing: None,
      ),

      // Static ground
      scene.mesh(
        id: "ground",
        geometry: ground_geo,
        material: ground_mat,
        transform: transform.identity,
        physics: Some(
          physics.new_rigid_body(physics.Fixed)
          |> physics.with_collider(physics.Box(
            offset: transform.identity,
            size: vec3.Vec3(20.0, 0.5, 20.0),
          ))
          |> physics.build()
        ),
      ),

      // Bouncing ball
      scene.mesh(
        id: "ball",
        geometry: ball_geo,
        material: ball_mat,
        // Get transform from physics simulation, or use initial position
        transform: case physics.get_transform(physics_world, "ball") {
          Ok(t) -> t
          Error(Nil) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
        },
        physics: Some(
          physics.new_rigid_body(physics.Dynamic)
          |> physics.with_collider(physics.Sphere(
            offset: transform.identity,
            radius: 1.0,
          ))
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
fn init(ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  let physics_world = physics.new_world(
    physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0)  // Earth gravity (m/s^2)
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

Call `physics.step()` every frame in your `update()` function. **Important**: `physics.step()` requires the `delta_time` parameter (as a `Duration` type) to implement frame-rate independent physics.

```gleam
import gleam/time/duration

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(a)) {
  let assert option.Some(physics_world) = ctx.physics_world

  case msg {
    Tick -> {
      // Pass delta_time (Duration type) directly
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
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  let assert option.Some(physics_world) = ctx.physics_world

  // Get physics transforms for dynamic bodies
  let player_transform = case physics.get_transform(physics_world, "player") {
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
  |> physics.with_collider(physics.Box(
    offset: transform.identity,
    size: vec3.Vec3(1.0, 1.0, 1.0),
  ))
  |> physics.build()
```

Use for: Moveable objects, projectiles, character physics

**Kinematic** - Moved programmatically, affects dynamic bodies but not affected by forces:
```gleam
physics.new_rigid_body(physics.Kinematic)
  |> physics.with_collider(physics.Box(
    offset: transform.identity,
    size: vec3.Vec3(5.0, 5.0, 5.0),
  ))
  |> physics.build()
```

Use for: Moving platforms, doors, elevators

**Fixed** - Static, immovable objects:
```gleam
physics.new_rigid_body(physics.Fixed)
  |> physics.with_collider(physics.Box(
    offset: transform.identity,
    size: vec3.Vec3(100.0, 1.0, 100.0),
  ))
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
  |> physics.with_collision_groups(membership: [0], can_collide_with: [1, 2])
  |> physics.with_lock_rotation_x()
  |> physics.with_lock_rotation_z()
  |> physics.build()
```

## Collider Shapes

### Box Collider

Box-shaped collision volume:

```gleam
physics.Box(
  offset: transform.identity,  // Offset from body center
  size: vec3.Vec3(2.0, 1.0, 2.0),  // Full dimensions (width, height, depth)
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
physics.Box(offset: transform.identity, size: vec3.Vec3(1.0, 1.0, 1.0))

// Collider offset 0.5 units up
physics.Box(
  offset: transform.at(position: vec3.Vec3(0.0, 0.5, 0.0)),
  size: vec3.Vec3(1.0, 1.0, 1.0),
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
  "player",
  vec3.Vec3(100.0, 0.0, 0.0),  // Push 100 N to the right
)
```

**Use for:** Continuous acceleration, wind, thrusters

### Applying Impulses

Impulses are instant velocity changes:

```gleam
// Instant velocity change (like a jump or explosion)
let physics_world = physics.apply_impulse(
  physics_world,
  "player",
  vec3.Vec3(0.0, 500.0, 0.0),  // Instant upward velocity
)
```

**Use for:** Jumps, explosions, instant hits

### Setting Velocity Directly

```gleam
// Set exact velocity
let physics_world = physics.set_velocity(
  physics_world,
  "player",
  vec3.Vec3(5.0, 0.0, 0.0),  // Move at 5 m/s right
)
```

**Use for:** Character controllers, vehicles, special movement

### Angular Forces and Torques

```gleam
// Apply rotational force
let physics_world = physics.apply_torque(
  physics_world,
  "object",
  vec3.Vec3(0.0, 10.0, 0.0),  // Rotate around Y axis
)

// Apply instant rotational impulse
let physics_world = physics.apply_torque_impulse(
  physics_world,
  "object",
  vec3.Vec3(0.0, 50.0, 0.0),
)

// Set angular velocity directly
let physics_world = physics.set_angular_velocity(
  physics_world,
  "object",
  vec3.Vec3(0.0, 3.14, 0.0),  // Rotate 180 degrees/second around Y
)
```

### Example: Character Controller

```gleam
import gleam/time/duration

fn handle_player_input(
  physics_world: physics.PhysicsWorld,
  input: input.InputState,
  delta_time: duration.Duration,
) -> physics.PhysicsWorld {
  let move_force = 500.0
  let jump_impulse = 300.0

  let physics_world = case input.is_key_pressed(input, input.KeyW) {
    True -> physics.apply_force(physics_world, "player", vec3.Vec3(0.0, 0.0, -move_force))
    False -> physics_world
  }

  let physics_world = case input.is_key_pressed(input, input.KeyS) {
    True -> physics.apply_force(physics_world, "player", vec3.Vec3(0.0, 0.0, move_force))
    False -> physics_world
  }

  let physics_world = case input.is_key_just_pressed(input, input.KeySpace) {
    True -> physics.apply_impulse(physics_world, "player", vec3.Vec3(0.0, jump_impulse, 0.0))
    False -> physics_world
  }

  physics_world
}
```

## Character Controller

For kinematic character movement with collision detection:

```gleam
// Create a kinematic body with character controller
let player = physics.new_rigid_body(physics.Kinematic)
  |> physics.with_collider(physics.Capsule(
    offset: transform.identity,
    half_height: 0.9,
    radius: 0.3,
  ))
  |> physics.with_character_controller(
    offset: 0.01,
    up_vector: vec3.Vec3(0.0, 1.0, 0.0),
    slide_enabled: True,
  )
  |> physics.build()

// In update, compute collision-aware movement
let desired_movement = vec3.Vec3(move_x, 0.0, move_z)
case physics.compute_character_movement(physics_world, "player", desired_movement) {
  Ok(safe_movement) -> {
    // Apply the safe movement that doesn't penetrate walls
    physics.set_kinematic_translation(physics_world, "player", safe_movement)
  }
  Error(_) -> physics_world
}

// Check if character is on the ground
case physics.is_character_grounded(physics_world, "player") {
  Ok(True) -> // Can jump
  Ok(False) -> // In the air
  Error(_) -> // No character controller
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
) -> #(Model, effect.Effect(Msg), option.Option(a)) {
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
            io.println("Collision between " <> body1 <> " and " <> body2)
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

**Important:** Enable collision events on bodies that need them:

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Capsule(
    offset: transform.identity,
    half_height: 0.9,
    radius: 0.3,
  ))
  |> physics.with_collision_events()  // Enable event tracking
  |> physics.build()
```

### Collision Groups

Control which objects can collide using collision groups:

```gleam
// Player: belongs to layer 0, collides with layers 1 (enemies) and 2 (ground)
let player_body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Capsule(
    offset: transform.identity,
    half_height: 1.0,
    radius: 0.5,
  ))
  |> physics.with_collision_groups(membership: [0], can_collide_with: [1, 2])
  |> physics.build()

// Enemy: belongs to layer 1, collides with layers 0 (player) and 2 (ground)
let enemy_body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Box(
    offset: transform.identity,
    size: vec3.Vec3(1.0, 1.0, 1.0),
  ))
  |> physics.with_collision_groups(membership: [1], can_collide_with: [0, 2])
  |> physics.build()

// Ground: belongs to layer 2, collides with everything
let ground_body = physics.new_rigid_body(physics.Fixed)
  |> physics.with_collider(physics.Box(
    offset: transform.identity,
    size: vec3.Vec3(100.0, 1.0, 100.0),
  ))
  |> physics.with_collision_groups(
    membership: [2],
    can_collide_with: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
  )
  |> physics.build()
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
fn process_collisions(events: List(physics.CollisionEvent), model: Model) -> Model {
  list.fold(events, model, fn(acc_model, event) {
    case event {
      physics.CollisionStarted(body_a, body_b) -> {
        // Check if projectile hit enemy
        case string.starts_with(body_a, "projectile-"), string.starts_with(body_b, "enemy-") {
          True, True -> damage_enemy(acc_model, body_b, 10)
          _, _ -> acc_model
        }
      }
      physics.CollisionEnded(_, _) -> acc_model
    }
  })
}
```

## Axis Locks

Restrict movement and rotation on specific axes:

```gleam
// Lock all rotation (useful for top-down games)
let body = physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Capsule(
    offset: transform.identity,
    half_height: 1.0,
    radius: 0.5,
  ))
  |> physics.with_lock_rotation_x()
  |> physics.with_lock_rotation_y()
  |> physics.with_lock_rotation_z()
  |> physics.build()
```

**Common patterns:**

**Top-down game (2D movement, no rotation):**
```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_lock_translation_y()  // Lock vertical movement
  |> physics.with_lock_rotation_x()     // Lock all rotation
  |> physics.with_lock_rotation_y()
  |> physics.with_lock_rotation_z()
```

**Platformer (2D side-scroller):**
```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_lock_translation_z()  // Lock depth
  |> physics.with_lock_rotation_x()     // Lock all rotation
  |> physics.with_lock_rotation_y()
  |> physics.with_lock_rotation_z()
```

**Standing character (upright, can rotate on Y only):**
```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_lock_rotation_x()  // Can't tip over
  |> physics.with_lock_rotation_z()  // Can't tip over
  // Y rotation allowed for turning
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
)

case result {
  Ok(hit) -> {
    io.println("Hit " <> hit.id <> " at distance " <> float.to_string(hit.distance))
    // hit.point - where the ray hit
    // hit.normal - surface normal at hit point
  }
  Error(Nil) -> {
    io.println("No hit")
  }
}
```

**Use for:** Ground detection, shooting, line of sight, mouse picking

## Summary

**Key concepts:**
- Physics world is initialized in `init()` and stepped in `update()`
- Three body types: Dynamic, Kinematic, Fixed
- Four collider shapes: Box, Sphere, Capsule, Cylinder
- Forces are continuous, impulses are instant
- Use collision events for game logic (damage, triggers, etc.)
- Use collision groups to control what collides with what
- Use axis locks for 2D games or to prevent tipping
- Query physics with raycasts
- All IDs are plain `String` values

**Performance tips:**
- Use Fixed bodies for static geometry
- Use simple collider shapes
- Use collision groups to reduce collision checks
- Only enable collision events on bodies that need them
- Consider using character controllers for player movement

**Next steps:**
- Try the physics examples: `examples/physics_demo`
- Experiment with different body types and properties
- Build a character controller or projectile system
- Implement collision-based gameplay mechanics
