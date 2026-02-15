//// Physics Demo — Interactive WASD Movement
////
//// Demonstrates cacao physics integration with tiramisu:
//// - WASD to move the player box
//// - Space to jump
//// - Dynamic objects that can be pushed around
//// - Collision events: sensor trigger zone logs when player enters/exits
//// - Body queries: read player velocity for ground check
//// - Raycasting: downward ray from player for ground distance detection
//// - Collision groups: two sets of boxes that pass through each other

import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/time/duration
import vec/vec3f

import lustre
import lustre/attribute
import lustre/effect

import input.{type Key}
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick.{type TickContext}
import tiramisu/transform

import cacao

import vec/vec3

// TYPES -----------------------------------------------------------------------

pub type Model {
  Model(
    // Keyboard input state
    input: input.InputState,
    // Physics world state
    physics: Option(cacao.PhysicsWorld),
    // Player position from physics
    player_position: vec3.Vec3(Float),
    // Ground distance from raycast
    ground_distance: Float,
    // Whether player is in the trigger zone
    in_trigger: Bool,
  )
}

pub type Msg {
  Tick(TickContext)
  // Physics world ready
  PhysicsReady(cacao.PhysicsWorld)
  // Keyboard input
  KeyDown(Key)
  KeyUp(Key)
  // Raycast callback
  GotGroundRay(Result(cacao.RayHit, Nil))
  // Body query callbacks
  GotPlayerPosition(vec3.Vec3(Float))
  GotPlayerVelocity(vec3.Vec3(Float))
  RendererCreatedSceneId(String)
}

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register()

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  let model =
    Model(
      input: input.new(),
      physics: None,
      player_position: vec3.Vec3(0.0, 2.0, 0.0),
      ground_distance: 0.0,
      in_trigger: False,
    )
  #(model, cacao.init(vec3.Vec3(0.0, -9.81, 0.0), PhysicsReady))
}

// UPDATE ----------------------------------------------------------------------

/// Movement impulse strength (velocity change per second).
const move_impulse = 15.0

/// Upward impulse applied on jump.
const jump_impulse = 10.0

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    PhysicsReady(world) -> {
      #(Model(..model, physics: Some(world)), effect.none())
    }

    RendererCreatedSceneId(scene_id) -> {
      #(model, tick.subscribe(scene_id, Tick))
    }

    KeyDown(key) -> #(
      Model(..model, input: input.key_down(model.input, key)),
      effect.none(),
    )

    KeyUp(key) -> #(
      Model(..model, input: input.key_up(model.input, key)),
      effect.none(),
    )

    Tick(ctx) -> {
      case model.physics {
        None -> #(model, effect.none())
        Some(world) -> {
          // Step physics simulation
          let world = cacao.step(world)

          // Process collision events synchronously (they're on the world)
          let in_trigger = process_trigger_events(model.in_trigger, world)

          // Build effects: movement, raycast, body queries
          let effects = [
            apply_movement(world, model, ctx),
            cacao.get_position(world, "player", GotPlayerPosition),
            cacao.cast_ray(
              world,
              GotGroundRay,
              origin: model.player_position
                |> vec3f.add(vec3.Vec3(0.0, -0.6, 0.0)),
              direction: vec3.Vec3(0.0, -1.0, 0.0),
              max_distance: 200.0,
            ),
            cacao.get_linear_velocity(world, "player", GotPlayerVelocity),
          ]

          #(
            Model(
              ..model,
              physics: Some(world),
              in_trigger:,
              input: input.end_frame(model.input),
            ),
            effect.batch(effects),
          )
        }
      }
    }

    GotPlayerPosition(pos) -> {
      #(Model(..model, player_position: pos), effect.none())
    }

    GotGroundRay(hit) -> {
      let distance = case hit {
        Ok(ray_hit) -> ray_hit.distance
        Error(Nil) -> 99.0
      }
      #(Model(..model, ground_distance: distance), effect.none())
    }

    GotPlayerVelocity(_velocity) -> {
      #(model, effect.none())
    }
  }
}

/// Process trigger zone collision events for the player.
/// Returns the new in_trigger state.
fn process_trigger_events(current: Bool, world: cacao.PhysicsWorld) -> Bool {
  let player_triggers =
    cacao.get_collisions_for(world, "player")
    |> list.filter(fn(info) { info.is_sensor })

  case player_triggers {
    [] -> current
    _ -> {
      let entered =
        list.any(player_triggers, fn(info) {
          case info.collision_type {
            cacao.CollisionStarted -> True
            cacao.CollisionStopped -> False
          }
        })
      let exited =
        list.any(player_triggers, fn(info) {
          case info.collision_type {
            cacao.CollisionStopped -> True
            cacao.CollisionStarted -> False
          }
        })
      case entered, exited {
        True, _ -> {
          io.println("Player entered trigger zone!")
          True
        }
        _, True -> {
          io.println("Player exited trigger zone!")
          False
        }
        _, _ -> current
      }
    }
  }
}

fn apply_movement(
  world: cacao.PhysicsWorld,
  model: Model,
  ctx: TickContext,
) -> effect.Effect(Msg) {
  let dt = duration.to_seconds(ctx.delta_time)

  // Accumulate movement direction from WASD
  let dz = case
    input.is_pressed(model.input, input.W),
    input.is_pressed(model.input, input.S)
  {
    True, False -> -1.0
    False, True -> 1.0
    _, _ -> 0.0
  }

  let dx = case
    input.is_pressed(model.input, input.A),
    input.is_pressed(model.input, input.D)
  {
    True, False -> -1.0
    False, True -> 1.0
    _, _ -> 0.0
  }
  let displacement = vec3.Vec3(dx, 0.0, dz)

  // Jump on space (just pressed, not held)
  let jump = input.is_just_pressed(model.input, input.Space)

  // Apply movement impulse scaled by delta time for frame-rate independence
  let effects = case dx != 0.0 || dz != 0.0 {
    True -> [
      cacao.apply_impulse(
        world,
        "player",
        vec3f.scale(displacement, move_impulse *. dt),
      ),
    ]
    False -> []
  }

  // Apply jump impulse
  let effects = case jump, model.ground_distance <. 0.1 {
    True, True -> [
      cacao.apply_impulse(world, "player", vec3.Vec3(0.0, jump_impulse, 0.0)),
      ..effects
    ]
    _, _ -> effects
  }

  effect.batch(effects)
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) {
  renderer.renderer(
    [
      renderer.width(800),
      renderer.height(600),
      renderer.background("#1a1a2e"),
      // Make the renderer focusable so it can receive keyboard events
      attribute.attribute("tabindex", "0"),
      // Capture keyboard input via event.code (physical key position)
      input.on_keydown(KeyDown),
      input.on_keyup(KeyUp),
      renderer.on_scene_ready(RendererCreatedSceneId),
    ],
    [
      // Camera looking at the scene from above and behind
      camera.camera(
        "main",
        [
          camera.fov(75.0),
          camera.transform(
            transform.at(vec3.Vec3(0.0, 8.0, 14.0))
            |> transform.with_look_at(vec3.Vec3(0.0, 0.0, 0.0)),
          ),
          camera.active(True),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Player — controllable box (belongs to group 4, collides with all)
      // ---------------------------------------------------------------
      mesh.mesh(
        "player",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x00ff88),
          mesh.metalness(0.4),
          mesh.roughness(0.6),
          mesh.transform(transform.at(vec3.Vec3(0.0, 2.0, 0.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.0),
          cacao.friction(0.2),
          cacao.linear_damping(1.5),
          cacao.angular_damping(1.0),
          cacao.lock_rotations(x: True, y: False, z: True),
          cacao.collision_group(membership: 0b0100, filter: 0b0111),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Collision groups demo: Red team (group 1) and Blue team (group 2)
      // pass through each other but both collide with player (group 4)
      // ---------------------------------------------------------------
      // Red team — group 1, collides with group 1 + group 4
      mesh.mesh(
        "red1",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0xff6b6b),
          mesh.metalness(0.3),
          mesh.roughness(0.7),
          mesh.transform(transform.at(vec3.Vec3(3.0, 2.0, -2.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0001, filter: 0b0101),
        ],
        [],
      ),
      mesh.mesh(
        "red2",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0xe05050),
          mesh.metalness(0.3),
          mesh.roughness(0.7),
          mesh.transform(transform.at(vec3.Vec3(4.0, 2.0, -2.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0001, filter: 0b0101),
        ],
        [],
      ),
      // Blue team — group 2, collides with group 2 + group 4
      mesh.mesh(
        "blue1",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x4ecdc4),
          mesh.metalness(0.5),
          mesh.roughness(0.5),
          mesh.transform(transform.at(vec3.Vec3(3.5, 2.0, -2.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0010, filter: 0b0110),
        ],
        [],
      ),
      mesh.mesh(
        "blue2",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x3498db),
          mesh.metalness(0.5),
          mesh.roughness(0.5),
          mesh.transform(transform.at(vec3.Vec3(4.5, 2.0, -2.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0010, filter: 0b0110),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Sensor trigger zone — detects player entry/exit
      // ---------------------------------------------------------------
      mesh.mesh(
        "trigger_zone",
        [
          mesh.geometry_box(vec3.Vec3(3.0, 2.0, 3.0)),
          mesh.color(0xffaa00),
          mesh.metalness(0.0),
          mesh.roughness(1.0),
          mesh.opacity(0.2),
          mesh.transform(transform.at(vec3.Vec3(-3.0, 1.0, 3.0))),
          cacao.body_type(cacao.Fixed),
          cacao.collider(cacao.Cuboid(1.5, 1.0, 1.5)),
          cacao.sensor(True),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Pushable sphere
      // ---------------------------------------------------------------
      mesh.mesh(
        "sphere",
        [
          mesh.geometry_sphere_simple(0.5),
          mesh.color(0xfeca57),
          mesh.metalness(0.8),
          mesh.roughness(0.2),
          mesh.transform(transform.at(vec3.Vec3(1.0, 2.0, 2.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Ball(0.5)),
          cacao.restitution(0.8),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Stacked boxes — a small tower to knock over
      // ---------------------------------------------------------------
      mesh.mesh(
        "stack1",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0xe056a0),
          mesh.metalness(0.3),
          mesh.roughness(0.7),
          mesh.transform(transform.at(vec3.Vec3(-6.0, 1.0, -1.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.1),
        ],
        [],
      ),
      mesh.mesh(
        "stack2",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x9b59b6),
          mesh.metalness(0.3),
          mesh.roughness(0.7),
          mesh.transform(transform.at(vec3.Vec3(-6.0, 2.0, -1.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.1),
        ],
        [],
      ),
      mesh.mesh(
        "stack3",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x3498db),
          mesh.metalness(0.3),
          mesh.roughness(0.7),
          mesh.transform(transform.at(vec3.Vec3(-6.0, 3.0, -1.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.1),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Static ground
      // ---------------------------------------------------------------
      mesh.mesh(
        "ground",
        [
          mesh.geometry_box(vec3.Vec3(30.0, 1.0, 30.0)),
          mesh.color(0x2d3436),
          mesh.metalness(0.1),
          mesh.roughness(0.9),
          mesh.transform(transform.at(vec3.Vec3(0.0, -0.5, 0.0))),
          cacao.body_type(cacao.Fixed),
          cacao.collider(cacao.Cuboid(15.0, 0.5, 15.0)),
          cacao.friction(0.8),
        ],
        [],
      ),
      // Ambient light
      light.light(
        "ambient",
        [
          light.light_type("ambient"),
          light.color(0xffffff),
          light.intensity(0.4),
        ],
        [],
      ),
      // Directional light with shadows
      light.light(
        "sun",
        [
          light.light_type("directional"),
          light.color(0xffffff),
          light.intensity(1.0),
          light.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          light.cast_shadow(True),
        ],
        [],
      ),
    ],
  )
}
