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
import tiramisu/material
import tiramisu/scene
import vec/vec2
import vec/vec3f

import lustre
import lustre/attribute
import lustre/effect

import input.{type Key}
import tiramisu
import tiramisu/camera
import tiramisu/light
import tiramisu/mesh
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
}

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  let cacao_app = cacao.app()
  let assert Ok(_) =
    tiramisu.register([
      cacao.extension(cacao_app),
      ..tiramisu.builtin_extensions()
    ])

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", cacao_app)
  Nil
}

// INIT ------------------------------------------------------------------------

fn init(world) -> #(Model, effect.Effect(Msg)) {
  let model =
    Model(
      input: input.new(),
      physics: None,
      player_position: vec3.Vec3(0.0, 2.0, 0.0),
      ground_distance: 0.0,
      in_trigger: False,
    )
  #(
    model,
    effect.batch([
      cacao.init(vec3.Vec3(0.0, -9.81, 0.0), world, PhysicsReady),
      tick.subscribe("main", Tick),
    ]),
  )
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

          // Read current player position synchronously (avoids one-frame lag)
          let player_position = case cacao.resolve_body(world, "player") {
            Ok(body) -> cacao.get_translation(body)
            Error(_) -> model.player_position
          }

          // Process collision events synchronously (they're on the world)
          let in_trigger = process_trigger_events(world, model.in_trigger)

          // Cast ray from player center downward. With solid=false the
          // player's own collider is skipped (ray origin is inside it).
          // Distance to ground ≈ player half-height (0.5) when standing.
          let effects = [
            apply_movement(world, model, ctx),
            cacao.cast_ray(
              world,
              GotGroundRay,
              origin: player_position,
              direction: vec3.Vec3(0.0, -1.0, 0.0),
              max_distance: 200.0,
            ),
          ]

          #(
            Model(
              ..model,
              physics: Some(world),
              player_position:,
              in_trigger:,
              input: input.end_frame(model.input),
            ),
            effect.batch(effects),
          )
        }
      }
    }

    GotGroundRay(hit) -> {
      let distance = case echo hit {
        Ok(ray_hit) -> ray_hit.distance
        Error(Nil) -> 99.0
      }
      #(Model(..model, ground_distance: distance), effect.none())
    }
  }
}

/// Process trigger zone collision events for the player.
/// Returns the new in_trigger state.
fn process_trigger_events(world: cacao.PhysicsWorld, current: Bool) -> Bool {
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
  // Ray starts from player center, so ground distance ≈ 0.5 (half-height)
  // when standing on the ground
  let effects = case jump, model.ground_distance <. 0.6 {
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
  tiramisu.scene(
    "main",
    [
      attribute.width(800),
      attribute.height(600),
      scene.background_color(0x1a1a2e),
      // Make the renderer focusable so it can receive keyboard events
      attribute.attribute("tabindex", "0"),
      // Capture keyboard input via event.code (physical key position)
      input.on_keydown(KeyDown),
      input.on_keyup(KeyUp),
    ],
    [
      // Camera looking at the scene from above and behind
      tiramisu.camera(
        "main",
        [
          camera.fov(75.0),
          transform.transform(
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
      tiramisu.mesh(
        "player",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x00ff88),
          transform.transform(transform.at(vec3.Vec3(0.0, 2.0, 0.0))),

          material.cast_shadow(True),

          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.0),
          cacao.friction(0.2),
          cacao.linear_damping(1.5),
          cacao.angular_damping(1.0),
          cacao.lock_x(True),
          cacao.lock_y(True),
          cacao.collision_group(membership: 0b0100, filter: 0b0111),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Collision groups demo: Red team (group 1) and Blue team (group 2)
      // pass through each other but both collide with player (group 4)
      // ---------------------------------------------------------------
      // Red team — group 1, collides with group 1 + group 4
      tiramisu.mesh(
        "red1",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0xff6b6b),
          transform.transform(transform.at(vec3.Vec3(3.0, 2.0, -2.0))),

          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0001, filter: 0b0101),
        ],
        [],
      ),
      tiramisu.mesh(
        "red2",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0xe05050),
          transform.transform(transform.at(vec3.Vec3(4.0, 2.0, -2.0))),

          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0001, filter: 0b0101),
        ],
        [],
      ),
      // Blue team — group 2, collides with group 2 + group 4
      tiramisu.mesh(
        "blue1",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x4ecdc4),
          transform.transform(transform.at(vec3.Vec3(3.5, 2.0, -2.0))),

          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.3),
          cacao.collision_group(membership: 0b0010, filter: 0b0110),
        ],
        [],
      ),
      tiramisu.mesh(
        "blue2",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x3498db),
          transform.transform(transform.at(vec3.Vec3(4.5, 2.0, -2.0))),

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
      tiramisu.mesh(
        "trigger_zone",
        [
          mesh.geometry_box(vec3.Vec3(3.0, 2.0, 3.0)),
          mesh.color(0xffaa00),
          transform.transform(transform.at(vec3.Vec3(-3.0, 1.0, 3.0))),

          material.opacity(0.2),

          cacao.body_type(cacao.Fixed),
          cacao.collider(cacao.Cuboid(1.5, 1.0, 1.5)),
          cacao.sensor(True),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Pushable sphere
      // ---------------------------------------------------------------
      tiramisu.mesh(
        "sphere",
        [
          mesh.sphere(radius: 0.5, segments: vec2.Vec2(16, 16)),
          mesh.color(0xfeca57),
          transform.transform(transform.at(vec3.Vec3(1.0, 2.0, 2.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Ball(0.5)),
          cacao.restitution(0.8),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Stacked boxes — a small tower to knock over
      // ---------------------------------------------------------------
      tiramisu.mesh(
        "stack1",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0xe056a0),
          transform.transform(transform.at(vec3.Vec3(-6.0, 1.0, -1.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.1),
        ],
        [],
      ),
      tiramisu.mesh(
        "stack2",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x9b59b6),
          transform.transform(transform.at(vec3.Vec3(-6.0, 2.0, -1.0))),
          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.1),
        ],
        [],
      ),
      tiramisu.mesh(
        "stack3",
        [
          mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
          mesh.color(0x3498db),
          transform.transform(transform.at(vec3.Vec3(-6.0, 3.0, -1.0))),

          cacao.body_type(cacao.Dynamic),
          cacao.collider(cacao.Cuboid(0.5, 0.5, 0.5)),
          cacao.restitution(0.1),
        ],
        [],
      ),
      // ---------------------------------------------------------------
      // Static ground
      // ---------------------------------------------------------------
      tiramisu.mesh(
        "ground",
        [
          mesh.geometry_box(vec3.Vec3(30.0, 1.0, 30.0)),
          mesh.color(0x2d3436),
          material.receive_shadow(True),

          transform.transform(transform.at(vec3.Vec3(0.0, -0.5, 0.0))),
          cacao.body_type(cacao.Fixed),
          cacao.collider(cacao.Cuboid(15.0, 0.5, 15.0)),
          cacao.friction(0.8),
        ],
        [],
      ),
      // Ambient light
      tiramisu.light(
        "ambient",
        [
          light.kind(light.Ambient),
          light.color(0xffffff),
          light.intensity(0.1),
        ],
        [],
      ),
      // Directional light with shadows
      tiramisu.light(
        "sun",
        [
          light.kind(light.Directional),
          light.color(0xffffff),
          light.intensity(5.0),
          transform.transform(transform.at(vec3.Vec3(5.0, 10.0, 7.0))),
          light.cast_shadow(True),
        ],
        [],
      ),
    ],
  )
}
