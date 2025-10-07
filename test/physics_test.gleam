import tiramisu/physics
import tiramisu/transform
import vec/vec3

// Test: Register and get transform
pub fn register_and_get_transform_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let initial_transform = transform.at(position: vec3.Vec3(0.0, 5.0, 0.0))

  let world = physics.register_body(world, "test_body", body, initial_transform)

  // Get transform - should return the initial transform we set
  let assert Ok(t) = physics.get_transform(world, "test_body")
  assert t.position.x == 0.0
  assert t.position.y == 5.0
  assert t.position.z == 0.0
}

// Test: Get non-existent body transform
pub fn get_nonexistent_transform_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let assert Error(Nil) = physics.get_transform(world, "nonexistent")
}

// Test: Unregister body
pub fn unregister_body_test() {
  let world =
    physics.new_world(physics.WorldConfig(gravity: vec3.Vec3(0.0, -9.81, 0.0)))

  let body = physics.rigid_body(physics.Dynamic, physics.Box(1.0, 1.0, 1.0))
  let initial_transform = transform.identity()

  let world = physics.register_body(world, "test_body", body, initial_transform)

  // Verify body exists
  let assert Ok(_) = physics.get_transform(world, "test_body")

  // Unregister body
  let world = physics.unregister_body(world, "test_body")

  // Verify body no longer exists
  let assert Error(Nil) = physics.get_transform(world, "test_body")
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
  let assert Ok(_) = physics.get_transform(world, "cube")
  let assert Ok(_) = physics.get_transform(world, "sphere")
  let assert Ok(_) = physics.get_transform(world, "ground")
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

  let assert Ok(_) = physics.get_transform(world, "test")
}
