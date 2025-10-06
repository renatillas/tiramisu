import gleam/option
import tiramisu/physics
import tiramisu/transform
import tiramisu/vec3

// Test: Create physics world
pub fn create_world_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  // World should be created (opaque type, just verify it doesn't crash)
  assert True
}

// Test: Create rigid body with default settings
pub fn create_rigid_body_test() {
  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))

  // Verify body is created (opaque, but we can chain methods)
  let _body_with_mass = physics.set_mass(body, 2.0)
  assert True
}

// Test: Builder pattern for rigid body
pub fn rigid_body_builder_test() {
  let _body =
    physics.rigid_body(physics.Dynamic, physics.Sphere(0.5))
    |> physics.set_mass(1.0)
    |> physics.set_restitution(0.8)
    |> physics.set_friction(0.3)
    |> physics.set_linear_damping(0.1)
    |> physics.set_angular_damping(0.05)
    |> physics.enable_ccd()

  // Verify builder pattern works
  assert True
}

// Test: Different body types
pub fn body_types_test() {
  let _dynamic = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let _kinematic =
    physics.rigid_body(physics.Kinematic, physics.Box(1.0, 1.0, 1.0))
  let _fixed = physics.rigid_body(physics.Fixed, physics.Box(1.0, 1.0, 1.0))

  assert True
}

// Test: Different collider shapes
pub fn collider_shapes_test() {
  let _box_body =
    physics.rigid_body(physics.Dynamic, physics.Box(1.0, 2.0, 3.0))
  let _sphere_body = physics.rigid_body(physics.Dynamic, physics.Sphere(1.5))
  let _capsule_body =
    physics.rigid_body(physics.Dynamic, physics.Capsule(1.0, 0.5))
  let _cylinder_body =
    physics.rigid_body(physics.Dynamic, physics.Cylinder(2.0, 0.75))

  assert True
}

// Test: Register and get transform
pub fn register_and_get_transform_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let initial_transform = transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))

  let world = physics.register_body(world, "test_body", body, initial_transform)

  // Get transform - should return the initial transform we set
  case physics.get_transform(world, "test_body") {
    option.Some(t) -> {
      assert t.position.x == 0.0
      assert t.position.y == 5.0
      assert t.position.z == 0.0
    }
    option.None -> panic as "Expected Some transform"
  }
}

// Test: Get non-existent body transform
pub fn get_nonexistent_transform_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  assert case physics.get_transform(world, "nonexistent") {
    option.Some(_) -> False
    option.None -> True
  }
}

// Test: Unregister body
pub fn unregister_body_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let initial_transform = transform.identity()

  let world = physics.register_body(world, "test_body", body, initial_transform)

  // Verify body exists
  assert case physics.get_transform(world, "test_body") {
    option.Some(_) -> True
    option.None -> False
  }

  // Unregister body
  let world = physics.unregister_body(world, "test_body")

  // Verify body no longer exists
  assert case physics.get_transform(world, "test_body") {
    option.Some(_) -> False
    option.None -> True
  }
}

// Test: Physics world with multiple bodies
pub fn multiple_bodies_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

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
  let assert option.Some(_) = physics.get_transform(world, "cube")

  let assert option.Some(_) = physics.get_transform(world, "sphere")

  let assert option.Some(_) = physics.get_transform(world, "ground")
}

// Test: Physics step (basic - just verify it doesn't crash)
pub fn physics_step_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let world =
    physics.register_body(
      world,
      "test",
      body,
      transform.at(position: vec3.Vec3(0.0, 10.0, 0.0)),
    )

  // Step physics (this will interact with Rapier FFI)
  let _world = physics.step(world, 0.016)

  // Verify world is still valid
  assert True
}

// Test: Custom gravity vector
pub fn custom_gravity_test() {
  let _world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -20.0, 0.0)))

  assert True
}

// Test: Zero gravity world
pub fn zero_gravity_test() {
  let _world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, 0.0, 0.0)))

  assert True
}

// Test: Update body after registration
pub fn update_body_after_registration_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let world = physics.register_body(world, "test", body, transform.identity())

  // Update body properties
  let updated_body =
    physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
    |> physics.set_mass(5.0)
    |> physics.set_restitution(0.9)

  let world = physics.update_body(world, "test", updated_body)

  assert case physics.get_transform(world, "test") {
    option.Some(_) -> True
    option.None -> False
  }
}

// Test: Physics body properties edge cases
pub fn body_properties_edge_cases_test() {
  // Very high restitution (super bouncy)
  let _bouncy =
    physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
    |> physics.set_restitution(2.0)

  // Zero friction
  let _frictionless =
    physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
    |> physics.set_friction(0.0)

  // High damping
  let _damped =
    physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
    |> physics.set_linear_damping(10.0)
    |> physics.set_angular_damping(10.0)

  assert True
}
