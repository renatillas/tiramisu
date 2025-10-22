/// Advanced Physics Demo
///
/// Demonstrates all advanced physics features:
/// - Collision groups and filtering
/// - Axis locks for 2D-style movement
/// - Forces, impulses, and torques
/// - Raycasting for ground detection
/// - Collision event handling
/// - Different body types (Dynamic, Kinematic, Fixed)
/// - Various collider shapes
import gleam/float
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import tiramisu
import tiramisu/background
import tiramisu/camera
import tiramisu/debug
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/physics
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub fn main() {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

// --- Model ---

pub type Model {
  Model(
    rotation: Float,
    last_collision_message: String,
    is_grounded: Bool,
    show_debug_colliders: Bool,
    cubes: Int,
  )
}

pub type Msg {
  Tick
}

pub type Ids {
  Ground
  MainCamera
  WallRight
  WallLeft
  Floor
  Player
  Ball
  SpinningCube
  Cube
  AmbientLight
  SunLight
  NewCube(Int)
}

// --- Init ---

fn init(
  _ctx: tiramisu.Context(Ids),
) -> #(Model, effect.Effect(Msg), option.Option(physics.PhysicsWorld(Ids))) {
  io.println("🎮 Advanced Physics Demo")
  io.println("=======================")
  io.println("")
  io.println("Loading physics world...")
  io.println("")

  // Initialize physics world with gravity
  let physics_world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -20.0, 0.0)))

  print_instructions()

  #(
    Model(
      rotation: 0.0,
      last_collision_message: "",
      is_grounded: False,
      show_debug_colliders: False,
      cubes: 0,
    ),
    effect.tick(Tick),
    option.Some(physics_world),
  )
}

fn print_instructions() -> Nil {
  io.println("Controls:")
  io.println("  [SPACE] Apply upward impulse to player (jump)")
  io.println("  [Arrow Keys] Apply forces to player")
  io.println("  [R] Apply random impulse to red cube")
  io.println("  [T] Apply torque to spinning cube")
  io.println("  [D] Toggle debug collider visualization")
  io.println("")
  io.println("Features Demonstrated:")
  io.println("  ✓ Collision Groups (player vs ground vs obstacles)")
  io.println("  ✓ Axis Locks (player locked to Z plane for 2D movement)")
  io.println("  ✓ Raycast Ground Detection")
  io.println("  ✓ Collision Events")
  io.println("  ✓ Forces and Impulses")
  io.println("  ✓ Different Collider Shapes")
  io.println("  ✓ Debug Collider Visualization")
  io.println("")
}

// --- Update ---

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context(Ids),
) -> #(Model, effect.Effect(Msg), option.Option(physics.PhysicsWorld(Ids))) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time *. 0.002

      // Get physics world from context (DON'T step yet - do it after queueing commands)
      let assert option.Some(physics_world) = ctx.physics_world

      // Player controls - apply forces
      let physics_world = case
        input.is_key_pressed(ctx.input, input.ArrowUp),
        input.is_key_pressed(ctx.input, input.ArrowDown),
        input.is_key_pressed(ctx.input, input.ArrowLeft),
        input.is_key_pressed(ctx.input, input.ArrowRight)
      {
        True, _, _, _ ->
          physics.apply_impulse(
            physics_world,
            Player,
            vec3.Vec3(0.0, 0.0, -1.0),
          )
        _, True, _, _ ->
          physics.apply_impulse(physics_world, Player, vec3.Vec3(0.0, 0.0, 1.0))
        _, _, True, _ ->
          physics.apply_impulse(
            physics_world,
            Player,
            vec3.Vec3(-1.0, 0.0, 0.0),
          )
        _, _, _, True ->
          physics.apply_impulse(physics_world, Player, vec3.Vec3(1.0, 0.0, 0.0))
        _, _, _, _ -> physics_world
      }

      // Raycast downward from player to detect ground
      // Start raycast from bottom of player (center - half height - small offset)
      let player_pos = case physics.get_transform(physics_world, Player) {
        Ok(t) -> transform.position(t)
        Error(_) -> vec3.Vec3(0.0, 5.0, 0.0)
      }

      // Player collider is Box(1.0, 1.0, 1.0), so half height is 0.5
      // Start ray from slightly below player center and cast a short distance
      let ray_origin =
        vec3.Vec3(player_pos.x, player_pos.y -. 0.55, player_pos.z)

      let is_grounded = case
        physics.raycast(
          physics_world,
          origin: ray_origin,
          direction: vec3.Vec3(0.0, -1.0, 0.0),
          max_distance: 0.1,
        )
      {
        Ok(hit) -> {
          // Ignore self-collision
          case hit.id == Player {
            True -> False
            False -> {
              let _ = case model.is_grounded {
                False ->
                  io.println("Player landed on: " <> string.inspect(hit.id))
                True -> Nil
              }
              True
            }
          }
        }
        Error(Nil) -> {
          let _ = case model.is_grounded {
            True -> io.println("Player left ground")
            False -> Nil
          }
          False
        }
      }

      // Jump - apply impulse only when grounded
      let physics_world = case
        input.is_key_just_pressed(ctx.input, input.Space),
        is_grounded
      {
        True, True -> {
          physics.apply_impulse(
            physics_world,
            Player,
            vec3.Vec3(0.0, 20.0, 0.0),
          )
        }
        True, False -> {
          physics_world
        }
        _, _ -> physics_world
      }

      let physics_world = case
        input.is_key_just_pressed(ctx.input, input.KeyR)
      {
        True -> {
          physics.apply_impulse(
            physics_world,
            Cube,
            vec3.Vec3(
              float.random() *. 5.0 -. 2.5,
              float.random() *. 10.0,
              float.random() *. 5.0 -. 2.5,
            ),
          )
        }
        False -> physics_world
      }

      // Apply torque to spinning cube
      let physics_world = case input.is_key_pressed(ctx.input, input.KeyT) {
        True ->
          physics.apply_torque(
            physics_world,
            SpinningCube,
            vec3.Vec3(0.0, 5.0, 0.0),
          )
        False -> physics_world
      }

      // Toggle debug collider visualization
      let #(show_debug_colliders, effect) = case
        input.is_key_just_pressed(ctx.input, input.KeyD)
      {
        True -> {
          #(
            !model.show_debug_colliders,
            effect.from(fn(_) {
              debug.show_collider_wireframes(
                physics_world,
                model.show_debug_colliders,
              )
            }),
          )
        }
        False -> #(model.show_debug_colliders, effect.none())
      }

      // NOW step the physics simulation AFTER all commands have been queued
      let physics_world = physics.step(physics_world)

      // Get collision events from the updated world
      let collision_events = physics.get_collision_events(physics_world)
      let last_collision_message = case collision_events {
        [] -> model.last_collision_message
        [event, ..] -> collision_event_to_string(event)
      }

      // Log collision events
      list.each(collision_events, fn(event) {
        io.println("Collision: " <> collision_event_to_string(event))
      })

      #(
        Model(
          rotation: new_rotation,
          last_collision_message: last_collision_message,
          is_grounded: is_grounded,
          show_debug_colliders: show_debug_colliders,
          cubes: model.cubes + 1,
        ),
        effect.batch([effect.tick(Tick), effect]),
        option.Some(physics_world),
      )
    }
  }
}

fn collision_event_to_string(event: physics.CollisionEvent) -> String {
  case event {
    physics.CollisionStarted(a, b) -> a <> " started colliding with " <> b
    physics.CollisionEnded(a, b) -> a <> " ended colliding with " <> b
  }
}

// --- View ---

fn view(model: Model, context: tiramisu.Context(Ids)) -> List(scene.Node(Ids)) {
  let assert option.Some(physics_world) = context.physics_world

  let assert Ok(cam) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  // Geometries
  let assert Ok(box_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(large_box_geo) =
    geometry.box(width: 20.0, height: 0.5, depth: 20.0)
  let assert Ok(wall_geo) = geometry.box(width: 0.5, height: 5.0, depth: 20.0)
  let assert Ok(sphere_geo) =
    geometry.sphere(radius: 0.5, width_segments: 32, height_segments: 16)

  // Materials
  let assert Ok(ground_mat) =
    material.new() |> material.with_color(0x808080) |> material.build()

  let assert Ok(player_mat) =
    material.new()
    |> material.with_color(case model.is_grounded {
      True -> 0x44ff44
      False -> 0xffff44
    })
    |> material.build()

  let assert Ok(cube1_mat) =
    material.new() |> material.with_color(0xff4444) |> material.build()

  let assert Ok(cube2_mat) =
    material.new() |> material.with_color(0x4444ff) |> material.build()

  let assert Ok(sphere_mat) =
    material.new() |> material.with_color(0xff44ff) |> material.build()

  let assert Ok(wall_mat) =
    material.new() |> material.with_color(0x666666) |> material.build()

  // Lights
  let assert Ok(ambient_light) = light.ambient(color: 0xffffff, intensity: 0.5)

  let assert Ok(directional_light) =
    light.directional(color: 0xffffff, intensity: 1.5)

  // Ground (Fixed body, collision layer 0)
  let ground =
    scene.mesh(
      id: Ground,
      geometry: large_box_geo,
      material: ground_mat,
      transform: transform.at(position: vec3.Vec3(0.0, -0.25, 0.0)),
      physics: option.Some({
        physics.new_rigid_body(physics.Fixed)
        |> physics.with_collider(physics.Box(transform.identity, 20.0, 0.5, 20.0))
        |> physics.with_friction(1.0)
        |> physics.with_collision_groups(membership: [0], can_collide_with: [
          1,
          2,
        ])
        |> physics.build()
      }),
    )

  // Player (Dynamic body with axis locks, collision layer 1)
  // Locked to Z plane for 2D-style movement
  let player =
    scene.mesh(
      id: Player,
      geometry: box_geo,
      material: player_mat,
      transform: case physics.get_transform(physics_world, Player) {
        Ok(t) -> t
        Error(_) -> transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(2.0)
        |> physics.with_friction(0.5)
        |> physics.with_linear_damping(0.5)
        |> physics.with_lock_rotation_x()
        |> physics.with_lock_rotation_z()
        |> physics.with_collision_groups(membership: [1], can_collide_with: [
          0,
          2,
        ])
        |> physics.build(),
      ),
    )

  // Cube1 - Falls and bounces (Dynamic, collision layer 2)
  let cube1 =
    scene.mesh(
      id: Cube,
      geometry: box_geo,
      material: cube1_mat,
      transform: case physics.get_transform(physics_world, Cube) {
        Ok(t) -> t
        Error(_) -> transform.at(position: vec3.Vec3(-3.0, 8.0, 0.0))
      },
      physics: option.Some({
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(1.0)
        |> physics.with_restitution(0.7)
        |> physics.with_friction(0.3)
        |> physics.with_collision_groups(membership: [2], can_collide_with: [
          0,
          1,
          2,
        ])
        |> physics.build()
      }),
    )

  let new_cubes =
    list.map(list.range(model.cubes, model.cubes + 10), fn(index) {
      scene.mesh(
        id: NewCube(index),
        geometry: box_geo,
        material: cube1_mat,
        transform: case physics.get_transform(physics_world, NewCube(index)) {
          Ok(t) -> t
          Error(_) -> transform.at(position: vec3.Vec3(-3.0, 8.0, 0.0))
        },
        physics: option.Some({
          physics.new_rigid_body(physics.Dynamic)
          |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
          |> physics.with_mass(1.0)
          |> physics.with_restitution(0.7)
          |> physics.with_friction(0.3)
          |> physics.with_collision_groups(membership: [2], can_collide_with: [
            0,
            1,
            2,
          ])
          |> physics.build()
        }),
      )
    })

  // Cube2 - Spinning cube with torque
  let spinning_cube =
    scene.mesh(
      id: SpinningCube,
      geometry: box_geo,
      material: cube2_mat,
      transform: case
        context.physics_world
        |> option.map(fn(physics_world) {
          physics.get_transform(physics_world, SpinningCube)
        })
        |> option.to_result(Nil)
        |> result.flatten
      {
        Ok(t) -> t
        Error(_) ->
          transform.at(position: vec3.Vec3(3.0, 3.0, 0.0))
          |> transform.rotate_y(model.rotation)
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Box(transform.identity, 1.0, 1.0, 1.0))
        |> physics.with_mass(1.5)
        |> physics.with_restitution(0.3)
        |> physics.with_friction(0.5)
        |> physics.with_angular_damping(0.1)
        |> physics.with_collision_groups(membership: [2], can_collide_with: [
          0,
          1,
          2,
        ])
        |> physics.build(),
      ),
    )

  // Sphere - Different collider shape
  let sphere =
    scene.mesh(
      id: Ball,
      geometry: sphere_geo,
      material: sphere_mat,
      transform: case physics.get_transform(physics_world, Ball) {
        Ok(t) -> t
        Error(_) -> transform.at(position: vec3.Vec3(0.0, 10.0, -3.0))
      },
      physics: option.Some(
        physics.new_rigid_body(physics.Dynamic)
        |> physics.with_collider(physics.Sphere(transform.identity, 0.5))
        |> physics.with_mass(0.8)
        |> physics.with_restitution(0.9)
        |> physics.with_friction(0.1)
        |> physics.with_collision_groups(membership: [2], can_collide_with: [
          0,
          1,
          2,
        ])
        |> physics.build(),
      ),
    )

  // Walls (Fixed bodies)
  let wall_left =
    scene.mesh(
      id: WallLeft,
      geometry: wall_geo,
      material: wall_mat,
      transform: transform.at(position: vec3.Vec3(-5.0, 2.5, 0.0)),
      physics: option.Some(
        physics.new_rigid_body(physics.Fixed)
        |> physics.with_collider(physics.Box(transform.identity, 0.5, 5.0, 20.0))
        |> physics.with_friction(0.5)
        |> physics.with_collision_groups(membership: [0], can_collide_with: [
          1,
        ])
        |> physics.build(),
      ),
    )

  let wall_right =
    scene.mesh(
      id: WallRight,
      geometry: wall_geo,
      material: wall_mat,
      transform: transform.at(position: vec3.Vec3(5.0, 2.5, 0.0)),
      physics: option.Some(
        physics.new_rigid_body(physics.Fixed)
        |> physics.with_collider(physics.Box(transform.identity, 0.5, 5.0, 20.0))
        |> physics.with_friction(0.5)
        |> physics.with_collision_groups(membership: [0], can_collide_with: [
          1,
        ])
        |> physics.build(),
      ),
    )

  [
    scene.camera(
      id: MainCamera,
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 8.0, 15.0)),
      look_at: option.Some(vec3.Vec3(0.0, 2.0, 0.0)),
      active: True,
      viewport: option.None,
    ),
    ground,
    player,
    cube1,
    spinning_cube,
    sphere,
    wall_left,
    wall_right,
    scene.light(
      id: AmbientLight,
      light: ambient_light,
      transform: transform.identity,
    ),
    scene.light(
      id: SunLight,
      light: directional_light,
      transform: transform.at(position: vec3.Vec3(5.0, 10.0, 7.5)),
    ),
    ..new_cubes
  ]
}
