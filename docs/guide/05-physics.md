# Physics

Adding physics to your game transforms static scenes into dynamic worlds. Objects fall, bounce, collide, and respond to forces. Tiramisu integrates [Rapier](https://rapier.rs/), a high-performance physics engine, while maintaining the declarative style you've learned.

This guide walks you through physics from the ground up: creating a world, adding bodies, detecting collisions, and building game mechanics on top.

## Why Rapier?

Rapier is a modern physics engine written in Rust and compiled to WebAssembly. It offers:

- **Fast simulation** - 60 FPS with hundreds of dynamic objects
- **Stable stacking** - Objects stack reliably without jittering
- **Accurate collisions** - Continuous collision detection prevents tunneling
- **Cross-platform** - Same behavior in browser and native

Tiramisu wraps Rapier in a declarative API that fits the MVU architecture. You describe physics bodies alongside your scene nodes, and the engine handles simulation.

## The physics flow

Physics in Tiramisu follows a specific pattern:

```
+-------------------------------------------------------------------+
|                                                                   |
|   init() ---> Create physics world ---> Return Some(world)        |
|                                                                   |
|   update(Tick) ---> Step simulation ---> Return Some(new_world)   |
|                                                                   |
|   view() ---> Query transforms from world ---> Render scene       |
|                                                                   |
+-------------------------------------------------------------------+
```

The physics world flows through your functions:

1. **init** creates the world with gravity and returns it
2. **update** steps the simulation each frame and returns the updated world
3. **view** queries body positions to render meshes at the right locations

## Your first physics scene

Let's create a ball bouncing on a floor:

```gleam
import gleam/option.{None, Some}
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
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
}

pub fn main() {
  let assert Ok(_) =
    tiramisu.application(init, update, view)
    |> tiramisu.start("#app", tiramisu.FullScreen, None)
  Nil
}

fn init(ctx: tiramisu.Context) {
  // Create physics world with Earth gravity
  let world = physics.new_world(
    physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0))
  )

  #(Model, effect.dispatch(Tick), Some(world))
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let assert Some(world) = ctx.physics_world

      // Step simulation forward
      let new_world = physics.step(world, ctx.delta_time)

      #(model, effect.dispatch(Tick), Some(new_world))
    }
  }
}

fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  let assert Some(world) = ctx.physics_world

  let assert Ok(cam) = camera.perspective(
    field_of_view: 75.0,
    near: 0.1,
    far: 1000.0,
  )

  let assert Ok(ground_geo) = geometry.box(size: vec3.Vec3(20.0, 0.5, 20.0))
  let assert Ok(ground_mat) =
    material.new()
    |> material.with_color(0x808080)
    |> material.build()

  let assert Ok(ball_geo) = geometry.sphere(radius: 1.0, segments: vec2.Vec2(32, 16))
  let assert Ok(ball_mat) =
    material.new()
    |> material.with_color(0xff4444)
    |> material.build()

  // Get ball position from physics simulation
  let ball_transform = case physics.get_transform(world, "ball") {
    Ok(t) -> t
    Error(Nil) -> transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
  }

  scene.empty(id: "root", transform: transform.identity, children: [])
  |> scene.with_children([
    scene.camera(
      id: "camera",
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 5.0, 15.0)),
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
      transform: ball_transform,
      physics: Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Sphere(
          offset: transform.identity,
          radius: 1.0,
        ))
        |> physics.with_restitution(0.8)
        |> physics.build()
      ),
    ),
  ])
}
```

Run this and you'll see a red ball drop and bounce on the gray floor. Let's break down what's happening.

## Creating the physics world

The physics world contains all bodies and handles simulation:

```gleam
let world = physics.new_world(
  physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0))
)
```

Gravity is a vector in units per second squared. Earth gravity is approximately -9.81 m/s² on the Y axis. You can change this for different effects:

```gleam
// Moon gravity (1/6 of Earth)
vec3.Vec3(0.0, -1.62, 0.0)

// No gravity (space)
vec3.Vec3(0.0, 0.0, 0.0)

// Sideways gravity (weird puzzle game?)
vec3.Vec3(-5.0, 0.0, 0.0)
```

## Stepping the simulation

Each frame, call `physics.step` to advance the simulation:

```gleam
let new_world = physics.step(world, ctx.delta_time)
```

The step function:

- Applies gravity to all dynamic bodies
- Integrates velocities to compute new positions
- Detects collisions between bodies
- Resolves penetrations and applies impulses
- Generates collision events

The `delta_time` parameter (a `Duration` type) ensures physics runs at the same speed regardless of frame rate. Internally, Rapier uses a fixed timestep with an accumulator for deterministic simulation.

## Rigid body types

Every physics object is a rigid body. Tiramisu supports three types:

### Dynamic

Affected by gravity and forces. Moves when pushed or hit:

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(...)
  |> physics.with_mass(5.0)
  |> physics.build()
```

Use for: Player characters, enemies, projectiles, physics props

### Fixed

Never moves. Other bodies collide with it but can't push it:

```gleam
physics.new_rigid_body(physics.Fixed)
  |> physics.with_collider(...)
  |> physics.build()
```

Use for: Ground, walls, platforms, level geometry

### Kinematic

Moved by code, not physics. Pushes dynamic bodies but isn't affected by them:

```gleam
physics.new_rigid_body(physics.Kinematic)
  |> physics.with_collider(...)
  |> physics.build()
```

Use for: Moving platforms, doors, elevators, scripted objects

## Collider shapes

Colliders define the shape used for collision detection. Choose shapes that approximate your geometry:

### Box

```gleam
physics.Box(
  offset: transform.identity,
  size: vec3.Vec3(2.0, 1.0, 3.0),  // Full dimensions
)
```

Use for: Crates, buildings, rectangular objects

### Sphere

```gleam
physics.Sphere(
  offset: transform.identity,
  radius: 1.0,
)
```

Use for: Balls, planets, character approximations

### Capsule

A cylinder with hemispherical caps. The most popular choice for characters:

```gleam
physics.Capsule(
  offset: transform.identity,
  half_height: 0.9,  // Half the cylinder height
  radius: 0.4,       // Radius of cylinder and caps
)
```

Use for: Characters, humanoid shapes

### Cylinder

```gleam
physics.Cylinder(
  offset: transform.identity,
  half_height: 1.0,
  radius: 0.5,
)
```

Use for: Barrels, columns, tree trunks

### Offsets

You can offset a collider from the body's origin:

```gleam
physics.Box(
  offset: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
  size: vec3.Vec3(1.0, 1.0, 1.0),
)
```

This places the collider 1 unit above the body's center. Useful when the visual mesh and collision shape need different centers.

## Physics properties

The builder pattern lets you configure body behavior:

### Mass

How heavy the object is. Affects how forces accelerate it:

```gleam
physics.with_mass(10.0)  // 10 kg
```

Heavier objects are harder to push and have more momentum.

### Restitution (bounciness)

How much energy is preserved on collision:

```gleam
physics.with_restitution(0.8)  // Very bouncy
physics.with_restitution(0.0)  // No bounce at all
```

Values range from 0 (no bounce) to 1 (perfect bounce). Values above 1 add energy on each bounce.

### Friction

Resistance when surfaces slide:

```gleam
physics.with_friction(0.5)  // Normal friction
physics.with_friction(0.0)  // Ice-like
physics.with_friction(1.0)  // Sticky
```

### Damping

Reduces velocity over time (air resistance):

```gleam
physics.with_linear_damping(0.1)   // Slows movement
physics.with_angular_damping(0.1)  // Slows rotation
```

## Applying forces

You can push objects around in your `update` function:

### Forces (continuous)

Apply over time, like a thruster:

```gleam
let world = physics.apply_force(
  world,
  "player",
  vec3.Vec3(100.0, 0.0, 0.0),  // Push right with 100 Newtons
)
```

### Impulses (instant)

Apply instantly, like a jump or explosion:

```gleam
let world = physics.apply_impulse(
  world,
  "player",
  vec3.Vec3(0.0, 300.0, 0.0),  // Instant upward velocity
)
```

### Set velocity directly

For precise control:

```gleam
let world = physics.set_velocity(
  world,
  "player",
  vec3.Vec3(5.0, 0.0, 0.0),  // Move at exactly 5 m/s right
)
```

### Example: WASD movement

```gleam
fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let assert Some(world) = ctx.physics_world

      let move_speed = 500.0

      // Apply forces based on input
      let world = case input.is_key_pressed(ctx.input, input.KeyW) {
        True -> physics.apply_force(world, "player", vec3.Vec3(0.0, 0.0, -move_speed))
        False -> world
      }

      let world = case input.is_key_pressed(ctx.input, input.KeyS) {
        True -> physics.apply_force(world, "player", vec3.Vec3(0.0, 0.0, move_speed))
        False -> world
      }

      let world = case input.is_key_pressed(ctx.input, input.KeyA) {
        True -> physics.apply_force(world, "player", vec3.Vec3(-move_speed, 0.0, 0.0))
        False -> world
      }

      let world = case input.is_key_pressed(ctx.input, input.KeyD) {
        True -> physics.apply_force(world, "player", vec3.Vec3(move_speed, 0.0, 0.0))
        False -> world
      }

      // Jump on space (just pressed, not held)
      let world = case input.is_key_just_pressed(ctx.input, input.KeySpace) {
        True -> physics.apply_impulse(world, "player", vec3.Vec3(0.0, 300.0, 0.0))
        False -> world
      }

      let new_world = physics.step(world, ctx.delta_time)

      #(model, effect.dispatch(Tick), Some(new_world))
    }
  }
}
```

## Collision detection

Rapier detects when bodies touch. You can query these events after stepping:

### Enabling collision events

First, mark bodies that should generate events:

```gleam
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(physics.Sphere(offset: transform.identity, radius: 0.5))
  |> physics.with_collision_events()  // Enable event generation
  |> physics.build()
```

### Querying events

After stepping, get the collision events:

```gleam
fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let assert Some(world) = ctx.physics_world
      let new_world = physics.step(world, ctx.delta_time)

      // Get collision events from this step
      let events = physics.get_collision_events(new_world)

      // Process them
      let new_model = list.fold(events, model, fn(acc, event) {
        case event {
          physics.CollisionStarted(body_a, body_b) -> {
            // Two bodies just started touching
            handle_collision_start(acc, body_a, body_b)
          }
          physics.CollisionEnded(body_a, body_b) -> {
            // Two bodies stopped touching
            handle_collision_end(acc, body_a, body_b)
          }
        }
      })

      #(new_model, effect.dispatch(Tick), Some(new_world))
    }
  }
}

fn handle_collision_start(model: Model, body_a: String, body_b: String) -> Model {
  // Check if player hit a coin
  case body_a == "player" && string.starts_with(body_b, "coin-") {
    True -> Model(..model, score: model.score + 1)
    False -> model
  }
}
```

## Collision groups

Not everything should collide with everything. Use collision groups to control this:

```gleam
// Player: layer 0, collides with enemies (1), ground (2), pickups (3)
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(...)
  |> physics.with_collision_groups(
    membership: [0],
    can_collide_with: [1, 2, 3],
  )
  |> physics.build()

// Enemy: layer 1, collides with player (0) and ground (2), but not other enemies
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_collider(...)
  |> physics.with_collision_groups(
    membership: [1],
    can_collide_with: [0, 2],
  )
  |> physics.build()

// Ground: layer 2, collides with everything
physics.new_rigid_body(physics.Fixed)
  |> physics.with_collider(...)
  |> physics.with_collision_groups(
    membership: [2],
    can_collide_with: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
  )
  |> physics.build()
```

Layers 0-15 are available. Plan your layer usage:

- 0: Player
- 1: Enemies
- 2: Ground/Walls
- 3: Pickups
- 4: Projectiles
- 5: Triggers (collision events only, no physics response)

## Axis locks

Restrict movement or rotation on specific axes:

```gleam
// Lock all rotation (useful for top-down or platformer characters)
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_lock_rotation_x()
  |> physics.with_lock_rotation_y()
  |> physics.with_lock_rotation_z()
  |> physics.build()

// Lock vertical movement (top-down game)
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_lock_translation_y()
  |> physics.build()

// Lock depth movement (2D side-scroller)
physics.new_rigid_body(physics.Dynamic)
  |> physics.with_lock_translation_z()
  |> physics.build()
```

## Raycasting

Cast a ray and find what it hits:

```gleam
let result = physics.raycast(
  world,
  origin: vec3.Vec3(0.0, 10.0, 0.0),
  direction: vec3.Vec3(0.0, -1.0, 0.0),  // Down
  max_distance: 100.0,
)

case result {
  Ok(hit) -> {
    // hit.id - which body was hit
    // hit.point - world position of hit
    // hit.normal - surface normal at hit point
    // hit.distance - distance from origin to hit
  }
  Error(Nil) -> {
    // Ray didn't hit anything
  }
}
```

Use for:

- Ground detection ("is the player standing on something?")
- Shooting ("did the bullet hit an enemy?")
- Line of sight ("can the enemy see the player?")
- Mouse picking ("what did the player click on?")

## Character controllers

For precise character movement, use a kinematic body with a character controller:

```gleam
physics.new_rigid_body(physics.Kinematic)
  |> physics.with_collider(physics.Capsule(
    offset: transform.identity,
    half_height: 0.9,
    radius: 0.3,
  ))
  |> physics.with_character_controller(
    offset: 0.01,  // Skin width for smooth sliding
    up_vector: vec3.Vec3(0.0, 1.0, 0.0),
    slide_enabled: True,
  )
  |> physics.build()
```

Then in update, compute safe movement:

```gleam
let desired_movement = vec3.Vec3(move_x, 0.0, move_z)

case physics.compute_character_movement(world, "player", desired_movement) {
  Ok(safe_movement) -> {
    // safe_movement is adjusted to not penetrate walls
    physics.set_kinematic_translation(world, "player", safe_movement)
  }
  Error(_) -> world
}
```

Check if grounded:

```gleam
case physics.is_character_grounded(world, "player") {
  Ok(True) -> // Can jump
  Ok(False) -> // In the air
  Error(_) -> // No character controller
}
```

## Common patterns

### Player with health

```gleam
pub type Model {
  Model(health: Float)
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
  case msg {
    Tick -> {
      let assert Some(world) = ctx.physics_world
      let new_world = physics.step(world, ctx.delta_time)

      let events = physics.get_collision_events(new_world)

      let new_health = list.fold(events, model.health, fn(hp, event) {
        case event {
          physics.CollisionStarted(a, b) -> {
            case a == "player" && string.starts_with(b, "enemy-"),
                 b == "player" && string.starts_with(a, "enemy-") {
              True, _ | _, True -> hp -. 10.0
              _, _ -> hp
            }
          }
          _ -> hp
        }
      })

      #(Model(health: new_health), effect.dispatch(Tick), Some(new_world))
    }
  }
}
```

### Projectiles

```gleam
fn spawn_bullet(world: physics.PhysicsWorld, position: Vec3, direction: Vec3) {
  // Bullets are dynamic but very fast
  // Use high velocity instead of forces for instant travel
  let bullet_body =
    physics.new_rigid_body(physics.Dynamic)
    |> physics.with_collider(physics.Sphere(offset: transform.identity, radius: 0.1))
    |> physics.with_collision_events()
    |> physics.with_collision_groups(membership: [4], can_collide_with: [1, 2])  // Hit enemies and walls
    |> physics.build()

  // Set initial velocity
  physics.set_velocity(world, "bullet-" <> new_id(), vec3.scale(direction, 50.0))
}
```

### Triggers (no physics response)

For areas that detect entry but don't block movement:

```gleam
// Make a trigger by using collision events without physical response
scene.mesh(
  id: "checkpoint",
  geometry: checkpoint_geo,
  material: transparent_material,
  transform: checkpoint_transform,
  physics: Some(
    physics.new_rigid_body(physics.Fixed)
    |> physics.with_collider(physics.Box(offset: transform.identity, size: vec3.Vec3(2.0, 4.0, 2.0)))
    |> physics.with_collision_events()
    |> physics.with_sensor()  // Detects but doesn't block
    |> physics.build()
  ),
)
```

## Performance tips

### Use Fixed bodies for static geometry

Fixed bodies are highly optimized. Use them for anything that doesn't move.

### Limit collision events

Only enable `with_collision_events()` on bodies that need it. Every collision event has overhead.

### Use collision groups

Reducing the number of potential collisions is faster than resolving them. If enemies don't need to collide with each other, put them in the same layer and exclude self-collision.


## Next steps

You now understand how to add physics to your Tiramisu games. The key insights:

1. Physics world flows through init -> update -> view
2. Three body types: Dynamic, Fixed, Kinematic
3. Collider shapes approximate your geometry
4. Forces are continuous, impulses are instant
5. Collision events drive game logic
6. Character controllers give precise movement

Next, learn about [Lustre Integration](06-lustre-integration.md) to add UI overlays to your game.
