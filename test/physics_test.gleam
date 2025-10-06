import gleam/option
import gleeunit/should
import tiramisu/math/vec3
import tiramisu/physics
import tiramisu/transform

// Test: Create physics world
pub fn create_world_test() {
  let world =
    physics.new_world(physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0),
    ))

  // World should be created (opaque type, just verify it doesn't crash)
  should.be_ok(Ok(world))
}

// Test: Create rigid body with default settings
pub fn create_rigid_body_test() {
  let body =
    physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))

  // Verify body is created (opaque, but we can chain methods)
  let body_with_mass = physics.set_mass(body, 2.0)
  should.be_ok(Ok(body_with_mass))
}

// Test: Builder pattern for rigid body
pub fn rigid_body_builder_test() {
  let body =
    physics.rigid_body(physics.Dynamic, physics.Sphere(0.5))
    |> physics.set_mass(1.0)
    |> physics.set_restitution(0.8)
    |> physics.set_friction(0.3)
    |> physics.set_linear_damping(0.1)
    |> physics.set_angular_damping(0.05)
    |> physics.enable_ccd()

  // Verify builder pattern works
  should.be_ok(Ok(body))
}

// Test: Different body types
pub fn body_types_test() {
  let dynamic = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let kinematic =
    physics.rigid_body(physics.Kinematic, physics.Box(1.0, 1.0, 1.0))
  let fixed = physics.rigid_body(physics.Fixed, physics.Box(1.0, 1.0, 1.0))

  should.be_ok(Ok(dynamic))
  should.be_ok(Ok(kinematic))
  should.be_ok(Ok(fixed))
}

// Test: Different collider shapes
pub fn collider_shapes_test() {
  let box_body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 2.0, 3.0))
  let sphere_body = physics.rigid_body(physics.Dynamic, physics.Sphere(1.5))
  let capsule_body =
    physics.rigid_body(physics.Dynamic, physics.Capsule(1.0, 0.5))
  let cylinder_body =
    physics.rigid_body(physics.Dynamic, physics.Cylinder(2.0, 0.75))

  should.be_ok(Ok(box_body))
  should.be_ok(Ok(sphere_body))
  should.be_ok(Ok(capsule_body))
  should.be_ok(Ok(cylinder_body))
}

// Test: Register and get transform
pub fn register_and_get_transform_test() {
  let world =
    physics.new_world(physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0),
    ))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let initial_transform = transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))

  let world =
    physics.register_body(world, "test_body", body, initial_transform)

  // Get transform - should return the initial transform we set
  case physics.get_transform(world, "test_body") {
    option.Some(t) -> {
      t.position.x
      |> should.equal(0.0)
      t.position.y
      |> should.equal(5.0)
      t.position.z
      |> should.equal(0.0)
    }
    option.None -> should.fail()
  }
}

// Test: Get non-existent body transform
pub fn get_nonexistent_transform_test() {
  let world =
    physics.new_world(physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0),
    ))

  case physics.get_transform(world, "nonexistent") {
    option.Some(_) -> should.fail()
    option.None -> should.be_ok(Ok(Nil))
  }
}

// Test: Unregister body
pub fn unregister_body_test() {
  let world =
    physics.new_world(physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0),
    ))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let initial_transform = transform.identity()

  let world =
    physics.register_body(world, "test_body", body, initial_transform)

  // Verify body exists
  physics.get_transform(world, "test_body")
  |> should.be_some()

  // Unregister body
  let world = physics.unregister_body(world, "test_body")

  // Verify body no longer exists
  physics.get_transform(world, "test_body")
  |> should.be_none()
}

// Test: Physics world with multiple bodies
pub fn multiple_bodies_test() {
  let world =
    physics.new_world(physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0),
    ))

  let body1 = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let body2 = physics.rigid_body(physics.Dynamic, physics.Sphere(0.5))
  let body3 = physics.rigid_body(physics.Fixed, physics.Box(10.0, 0.1, 10.0))

  let world =
    world
    |> physics.register_body(
      "cube",
      body1,
      transform.at(position: vec3.Vec3(0.0, 5.0, 0.0)),
    )
    |> physics.register_body(
      "sphere",
      body2,
      transform.at(position: vec3.Vec3(2.0, 3.0, 0.0)),
    )
    |> physics.register_body(
      "ground",
      body3,
      transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
    )

  // Verify all bodies exist
  physics.get_transform(world, "cube")
  |> should.be_some()

  physics.get_transform(world, "sphere")
  |> should.be_some()

  physics.get_transform(world, "ground")
  |> should.be_some()
}

// Test: Physics step (basic - just verify it doesn't crash)
pub fn physics_step_test() {
  let world =
    physics.new_world(physics.WorldConfig(
      gravity: vec3.Vec3(0.0, -9.81, 0.0),
    ))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let world =
    physics.register_body(
      world,
      "test",
      body,
      transform.at(position: vec3.Vec3(0.0, 10.0, 0.0)),
    )

  // Step physics (this will interact with Rapier FFI)
  let world = physics.step(world, 0.016)

  // Verify world is still valid
  should.be_ok(Ok(world))
}
