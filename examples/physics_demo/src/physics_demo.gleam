//// Physics Demo — Interactive WASD Movement
////
//// Demonstrates cocoa physics integration with tiramisu:
//// - WASD to move the player box
//// - Space to jump
//// - Dynamic objects that can be pushed around
//// - Collision events: sensor trigger zone logs when player enters/exits
//// - Body queries: read player velocity for ground check
//// - Raycasting: downward ray from player for ground distance detection
//// - Joints: pendulum using a declarative revolute joint
//// - Collision groups: two sets of boxes that pass through each other

import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/time/duration

import lustre
import lustre/attribute
import lustre/effect

import tiramisu
import tiramisu/camera
import tiramisu/input.{type Key}
import tiramisu/light
import tiramisu/mesh
import tiramisu/renderer
import tiramisu/tick.{type TickContext}
import tiramisu/transform

import cocoa
import cocoa/body
import cocoa/collision.{type CollisionInfo, Started, Stopped}
import cocoa/force
import cocoa/joint
import cocoa/physics_world
import cocoa/raycast.{type RayHit}
import cocoa/rigidbody

import vec/vec3

// TYPES -----------------------------------------------------------------------

pub type Model {
  Model(
    // Keyboard input state
    input: input.InputState,
    // Ground distance from raycast
    ground_distance: Float,
    // Whether player is in the trigger zone
    in_trigger: Bool,
  )
}

pub type Msg {
  Tick(TickContext)
  // Keyboard input
  KeyDown(Key)
  KeyUp(Key)
  // Collision events callback
  GotCollisions(List(CollisionInfo))
  // Raycast callback
  GotGroundRay(Option(RayHit))
  // Body query callback
  GotPlayerVelocity(#(Float, Float, Float))
  RendererCreatedSceneId(String)
}

// MAIN ------------------------------------------------------------------------

pub fn main() -> Nil {
  let assert Ok(_) = tiramisu.register()
  let assert Ok(_) = cocoa.register()

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

// INIT ------------------------------------------------------------------------

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  let model = Model(input: input.new(), ground_distance: 0.0, in_trigger: False)
  #(model, effect.none())
}

// UPDATE ----------------------------------------------------------------------

/// Movement impulse strength (velocity change per second).
const move_impulse = 15.0

/// Upward impulse applied on jump.
const jump_impulse = 5.0

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
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
      let effects = [apply_movement(model, ctx)]

      // Query collision events every frame
      let collision_effects = [collision.get_collisions(GotCollisions)]

      // Raycast downward from player for ground distance
      let raycast_effects = [
        raycast.cast_ray(
          GotGroundRay,
          origin: vec3.Vec3(0.0, 2.0, 0.0),
          direction: vec3.Vec3(0.0, -1.0, 0.0),
          max_distance: 20.0,
        ),
      ]

      // Query player velocity for display/game logic
      let body_effects = [
        body.get_linear_velocity("player", GotPlayerVelocity),
      ]

      // Clear per-frame input state after reading it
      #(
        Model(..model, input: input.end_frame(model.input)),
        effect.batch(
          list.flatten([
            effects,
            collision_effects,
            raycast_effects,
            body_effects,
          ]),
        ),
      )
    }

    GotCollisions(events) -> {
      // Check for trigger zone events involving the player
      let player_triggers =
        list.filter(events, fn(info) {
          info.is_sensor && collision.involves(info, "player")
        })

      let new_model = case player_triggers {
        [] -> model
        _ -> {
          let entered =
            list.any(player_triggers, fn(info) {
              case info.collision_type {
                Started -> True
                Stopped -> False
              }
            })
          let exited =
            list.any(player_triggers, fn(info) {
              case info.collision_type {
                Stopped -> True
                Started -> False
              }
            })
          case entered, exited {
            True, _ -> {
              io.println("Player entered trigger zone!")
              Model(..model, in_trigger: True)
            }
            _, True -> {
              io.println("Player exited trigger zone!")
              Model(..model, in_trigger: False)
            }
            _, _ -> model
          }
        }
      }
      #(new_model, effect.none())
    }

    GotGroundRay(hit) -> {
      let distance = case hit {
        Some(ray_hit) -> ray_hit.distance
        None -> 99.0
      }
      #(Model(..model, ground_distance: distance), effect.none())
    }

    GotPlayerVelocity(#(_vx, _vy, _vz)) -> {
      #(model, effect.none())
    }
  }
}

fn apply_movement(model: Model, ctx: TickContext) -> effect.Effect(Msg) {
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

  // Jump on space (just pressed, not held)
  let jump = input.is_just_pressed(model.input, input.Space)

  // Apply movement impulse scaled by delta time for frame-rate independence
  let effects = case dx != 0.0 || dz != 0.0 {
    True -> [
      force.apply_impulse(
        "player",
        dx *. move_impulse *. dt,
        0.0,
        dz *. move_impulse *. dt,
      ),
    ]
    False -> []
  }

  // Apply jump impulse
  let effects = case jump {
    True -> [force.apply_impulse("player", 0.0, jump_impulse, 0.0), ..effects]
    False -> effects
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
      // Physics world with gravity
      physics_world.physics_world([physics_world.gravity(0.0, -9.81, 0.0)], [
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
          ],
          [
            rigidbody.rigidbody(
              "player_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.0),
                rigidbody.friction(0.2),
                rigidbody.linear_damping(1.5),
                rigidbody.angular_damping(1.0),
                rigidbody.lock_rotations(x: True, y: False, z: True),
                // Group 4 (bit 2), collides with everything (groups 1+2+4 = 0b0111)
                rigidbody.collision_group(membership: 0b0100, filter: 0b0111),
              ],
              [],
            ),
          ],
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
          ],
          [
            rigidbody.rigidbody(
              "red1_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.3),
                // Group 1 (bit 0), collides with group 1 + group 4
                rigidbody.collision_group(0b0001, 0b0101),
              ],
              [],
            ),
          ],
        ),
        mesh.mesh(
          "red2",
          [
            mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
            mesh.color(0xe05050),
            mesh.metalness(0.3),
            mesh.roughness(0.7),
            mesh.transform(transform.at(vec3.Vec3(4.0, 2.0, -2.0))),
          ],
          [
            rigidbody.rigidbody(
              "red2_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.3),
                rigidbody.collision_group(0b0001, 0b0101),
              ],
              [],
            ),
          ],
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
          ],
          [
            rigidbody.rigidbody(
              "blue1_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.3),
                // Group 2 (bit 1), collides with group 2 + group 4
                rigidbody.collision_group(0b0010, 0b0110),
              ],
              [],
            ),
          ],
        ),
        mesh.mesh(
          "blue2",
          [
            mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
            mesh.color(0x3498db),
            mesh.metalness(0.5),
            mesh.roughness(0.5),
            mesh.transform(transform.at(vec3.Vec3(4.5, 2.0, -2.0))),
          ],
          [
            rigidbody.rigidbody(
              "blue2_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.3),
                rigidbody.collision_group(0b0010, 0b0110),
              ],
              [],
            ),
          ],
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
          ],
          [
            rigidbody.rigidbody(
              "trigger_body",
              [
                rigidbody.body_type(rigidbody.Fixed),
                rigidbody.collider_cuboid(vec3.Vec3(1.5, 1.0, 1.5)),
                rigidbody.sensor(True),
              ],
              [],
            ),
          ],
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
          ],
          [
            rigidbody.rigidbody(
              "sphere_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_ball(0.5),
                rigidbody.restitution(0.8),
              ],
              [],
            ),
          ],
        ),
        // ---------------------------------------------------------------
        // Pendulum — declarative revolute joint
        // The anchor is fixed; the bob hangs and swings.
        // ---------------------------------------------------------------
        mesh.mesh(
          "pendulum_anchor",
          [
            mesh.geometry_box(vec3.Vec3(0.5, 0.5, 0.5)),
            mesh.color(0x666666),
            mesh.metalness(0.6),
            mesh.roughness(0.4),
            mesh.transform(transform.at(vec3.Vec3(5.0, 6.0, 0.0))),
          ],
          [
            rigidbody.rigidbody(
              "pendulum_anchor_body",
              [
                rigidbody.body_type(rigidbody.Fixed),
                rigidbody.collider_cuboid(vec3.Vec3(0.25, 0.25, 0.25)),
              ],
              [],
            ),
          ],
        ),
        mesh.mesh(
          "pendulum_bob",
          [
            mesh.geometry_sphere_simple(0.4),
            mesh.color(0xff00ff),
            mesh.metalness(0.7),
            mesh.roughness(0.3),
            mesh.transform(transform.at(vec3.Vec3(5.0, 3.0, 0.0))),
          ],
          [
            rigidbody.rigidbody(
              "pendulum_bob_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_ball(0.4),
                rigidbody.restitution(0.5),
              ],
              [],
            ),
          ],
        ),
        // Declarative joint connecting anchor and bob
        joint.joint("pendulum_joint", [
          joint.joint_type(joint.Revolute),
          joint.body_a("pendulum_anchor"),
          joint.body_b("pendulum_bob"),
          joint.anchor_a(0.0, -0.25, 0.0),
          joint.anchor_b(0.0, 1.5, 0.0),
          joint.axis(0.0, 0.0, 1.0),
        ]),
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
          ],
          [
            rigidbody.rigidbody(
              "stack1_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.1),
              ],
              [],
            ),
          ],
        ),
        mesh.mesh(
          "stack2",
          [
            mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
            mesh.color(0x9b59b6),
            mesh.metalness(0.3),
            mesh.roughness(0.7),
            mesh.transform(transform.at(vec3.Vec3(-6.0, 2.0, -1.0))),
          ],
          [
            rigidbody.rigidbody(
              "stack2_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.1),
              ],
              [],
            ),
          ],
        ),
        mesh.mesh(
          "stack3",
          [
            mesh.geometry_box(vec3.Vec3(1.0, 1.0, 1.0)),
            mesh.color(0x3498db),
            mesh.metalness(0.3),
            mesh.roughness(0.7),
            mesh.transform(transform.at(vec3.Vec3(-6.0, 3.0, -1.0))),
          ],
          [
            rigidbody.rigidbody(
              "stack3_body",
              [
                rigidbody.body_type(rigidbody.Dynamic),
                rigidbody.collider_cuboid(vec3.Vec3(0.5, 0.5, 0.5)),
                rigidbody.restitution(0.1),
              ],
              [],
            ),
          ],
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
          ],
          [
            rigidbody.rigidbody(
              "ground_body",
              [
                rigidbody.body_type(rigidbody.Fixed),
                rigidbody.collider_cuboid(vec3.Vec3(15.0, 0.5, 15.0)),
                rigidbody.friction(0.8),
              ],
              [],
            ),
          ],
        ),
      ]),
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
