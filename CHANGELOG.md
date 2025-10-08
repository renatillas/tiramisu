# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Types

- The `tiramisu.Dimensions` type has been added with `width: Float` and `height: Float` fields.
- The `tiramisu.Context` type has had `canvas_width: Float` and `canvas_height: Float` fields added.
- The `tiramisu.Camera` type has been simplified to contain only `projection: CameraProjection` (removed `position` and `look_at_target` fields - these are now set via `scene.Camera` node's `transform` and `look_at`).
- The `scene.SceneNode.Camera` variant has had `look_at: Option(Vec3(Float))` field added.
- The `scene.SceneNode.Light` variant has renamed its field from `light_type` to `light`.
- The `scene.SceneNode.Mesh` and `scene.SceneNode.InstancedMesh` variants now use opaque `Geometry` and `Material` types instead of `GeometryType` and `MaterialType`.
- The `scene.ValidationError` type has been renamed to `scene.GeometryError` with more specific error variants.
- The `scene.MaterialError` type has been added for material validation errors.
- The `scene.LightError` type has been added for light validation errors.
- The `state_machine.StateMachine` type has been made generic over state type: `StateMachine(state, ctx)` instead of `StateMachine(ctx)`.
- The `state_machine.State` type has been made generic: `State(state)` instead of `State` with `id: String`.
- The `state_machine.Transition` type has been made generic: `Transition(state, ctx)` instead of `Transition(ctx)`.
- The `state_machine.StateMachineState` type has been made generic: `StateMachineState(state)` instead of `StateMachineState` with string state IDs.
- The `internal.RendererOptions` type has had its `width: Int` and `height: Int` fields replaced with `dimensions: Option(Dimensions)`.
- The `physics.PhysicsBodyState` type has been simplified to only store `body: RigidBody` (removed `last_known_transform` field).

### Functions

- The `tiramisu.run` function signature has changed from `width width: Int, height height: Int` to `dimensions dimensions: Option(Dimensions)`.
- The `tiramisu.get_window_aspect_ratio` function has been added.
- The `camera.perspective` function has had its `aspect aspect: Float` parameter removed.
- The `camera.camera_2d` function has had its `distance distance: Float` parameter removed.
- The `camera.set_position` function has been removed (use `scene.Camera` node's `transform` field instead).
- The `camera.set_look_at` function has been removed (use `scene.Camera` node's `look_at` field instead).
- The `scene.box` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.sphere` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.plane` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.cylinder` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.cone` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.torus` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.capsule` function now returns `Result(Geometry, GeometryError)` instead of `Result(GeometryType, ValidationError)`.
- The `scene.basic_material` function now returns `Result(Material, MaterialError)` instead of `MaterialType`.
- The `scene.standard_material` function now returns `Result(Material, MaterialError)` instead of `MaterialType`.
- The `scene.physical_material` function now returns `Result(Material, MaterialError)` instead of `MaterialType`.
- The `scene.lambert_material` function now returns `Result(Material, MaterialError)` instead of `MaterialType`.
- The `scene.phong_material` function now returns `Result(Material, MaterialError)` instead of `MaterialType`.
- The `scene.toon_material` function now returns `Result(Material, MaterialError)` instead of `MaterialType`.
- The `scene.ambient_light` function now returns `Result(Light, LightError)`.
- The `scene.directional_light` function now returns `Result(Light, LightError)`.
- The `scene.point_light` function now returns `Result(Light, LightError)`.
- The `scene.spot_light` function now returns `Result(Light, LightError)`.
- The `scene.hemisphere_light` function now returns `Result(Light, LightError)`.
- The `physics.get_body_transform` function has been removed (transforms are now queried directly from Rapier via FFI).
- The `state_machine.new` function now takes `initial_state: state` instead of `initial_state: String`.
- The `state_machine.add_state` function now takes `id: state` instead of `id: String`.
- The `state_machine.add_transition` function now takes `from: state` and `to: state` instead of strings.
- The `state_machine.transition_to` function now takes `target: state` instead of `target: String`.
- The `state_machine.current_state` function now returns `state` instead of `String`.
- The `state_machine.get_state` function now takes `id: state` and returns `Result(State(state), Nil)`.
- The `state_machine.state_ids` function now returns `List(state)` instead of `List(String)`.

### Bug Fixes

- Fixed snake game U-turn bug where making rapid directional changes caused game over. Direction validation now checks against current snake position.
- Fixed touch input coordinate displacement in fullscreen mode by using actual canvas dimensions for coordinate conversion.
- Removed default 8px body margin across all 22 projects using idiomatic Lustre `stylesheets` configuration.

### Features

- Canvas dimensions are now available in `tiramisu.Context` for accurate coordinate conversion and responsive layouts.
- Fullscreen mode is now supported via `dimensions: None` in `tiramisu.run()`.
- Canvas and camera aspect ratios now automatically update on window resize in fullscreen mode.
- Snake game now includes Lustre-based UI overlay with score display and game over screen.
- Camera aspect ratio is now calculated automatically at render time based on viewport or window dimensions.
- Camera position and orientation are now set via `scene.Camera` node's `transform` and `look_at` fields for consistency.
- State machine is now generic over state type, allowing use of custom state enums instead of strings.
- All geometry constructors now return proper validation errors with specific error variants.
- All material constructors now validate parameters and return errors.
- All light constructors now validate parameters and return errors.

### Documentation

- Added "Game Context" section explaining canvas dimensions and coordinate conversion.
- Updated camera documentation to reflect automatic aspect ratio calculation.
- Added examples of fullscreen vs fixed-size canvas configuration.
- Updated all 20 examples and 2 games to use new APIs.

## [1.0.0] - 2025-10-08

Initial release of Tiramisu game engine.

### Features

- MVU (Model-View-Update) architecture with effect system
- Three.js integration for 3D/2D rendering via FFI
- Type-safe scene graph with automatic diffing and patching
- Input handling (keyboard, mouse, touch)
- Camera system (perspective and orthographic)
- Lighting system (ambient, directional, point, spot)
- Material system (basic, standard, physical)
- Geometry primitives (box, sphere, plane, cylinder, cone, torus)
- Asset loading (GLTF models, STL, textures)
- Animation system with tweening
- Physics integration with Rapier3D
- Debug visualization tools
- Multi-camera support with viewports
- UI overlay support with Lustre
- 20 examples demonstrating engine features
- 2 example games (Snake, Flight Simulator)
- 215 passing tests

[Unreleased]: https://github.com/renatillas/tiramisu/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/renatillas/tiramisu/releases/tag/v1.0.0
