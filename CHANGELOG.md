# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **OBJ Model Loading**: Full support for loading Wavefront OBJ models with MTL materials
  - Loads diffuse color maps (map_Kd), normal maps (map_bump), and ambient occlusion maps (map_Ka)
  - Automatically centers models at origin
  - Computes vertex normals when missing
  - Parses MTL texture paths with options (e.g., `map_bump -bm 1 texture.jpg`)
  - New `asset.OBJAsset` type for loading OBJ files
  - New `asset.load_obj()` function for direct loading
  - New `asset.get_obj()` function for cache retrieval
  - Example: `examples/20-not-bread` demonstrates OBJ loading with full textures

- **Material Presets**: Convenient preset constructors that reduce material creation from 9 parameters to 1
  - `scene.plastic(color)` - Standard plastic look (roughness: 0.5, metalness: 0.0)
  - `scene.metal(color)` - Shiny metallic finish (roughness: 0.2, metalness: 1.0)
  - `scene.rough_metal(color)` - Brushed/matte metal (roughness: 0.8, metalness: 1.0)
  - `scene.glossy(color)` - Glossy finish (roughness: 0.3, metalness: 0.0)
  - `scene.matte(color)` - Matte/flat surface (roughness: 1.0, metalness: 0.0)

- **Material Builder Pattern**: Fluent API for custom materials with sensible defaults
  - `scene.new_standard_material()` creates a builder with defaults
  - `scene.mat_color()`, `scene.mat_metalness()`, `scene.mat_roughness()` for basic properties
  - `scene.mat_map()`, `scene.mat_normal_map()`, `scene.mat_ao_map()` for texture maps
  - `scene.mat_roughness_map()`, `scene.mat_metalness_map()` for PBR texture maps
  - `scene.build_material()` validates and creates the final material

- **Geometry Builders**: Builder pattern for geometry with intelligent defaults
  - **BoxBuilder**: `scene.new_box()` with defaults to 1×1×1 cube
    - `box_width()`, `box_height()`, `box_depth()` for individual dimensions
    - `box_size(width, height, depth)` for all dimensions at once
    - `box_cube(size)` for uniform sizing
  - **SphereBuilder**: `scene.new_sphere()` with defaults to radius 1.0, 32×16 segments
    - `sphere_radius()` for size
    - `sphere_width_segments()`, `sphere_height_segments()` for detail
    - `sphere_segments()` sets width segments, height to width/2
  - **PlaneBuilder**: `scene.new_plane()` with defaults to 1×1 plane
    - `plane_width()`, `plane_height()` for individual dimensions
    - `plane_size(width, height)` for both dimensions at once
    - `plane_square(size)` for uniform sizing

- **Transform Convenience Methods**: Intuitive methods for common transformations (reduces code by 50-70%)
  - Relative operations: `translate_by()`, `rotate_by()`, `scale_by()`, `scale_uniform()`
  - Per-axis rotation: `rotate_x()`, `rotate_y()`, `rotate_z()`
  - Directional movement: `move_forward()`, `move_right()`, `move_up()`

- **Tetris Game**: Full-featured Tetris implementation demonstrating UI overlays and game logic
  - Seven classic tetromino pieces with rotation
  - Score tracking with combo multipliers
  - Line clearing with visual feedback
  - Game over detection and restart
  - Lustre-based UI with next piece preview

- **Snake Audio**: Added sound effects to Snake game
  - Fruit collection sound effect
  - Game over sound effect
  - Demonstrates `audio` module usage

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
- The `scene.StandardMaterialBuilder` opaque type has been added for material builder pattern.
- The `scene.BoxBuilder` opaque type has been added for box geometry builder pattern.
- The `scene.SphereBuilder` opaque type has been added for sphere geometry builder pattern.
- The `scene.PlaneBuilder` opaque type has been added for plane geometry builder pattern.
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
- Added `scene.plastic()` - Material preset for plastic look.
- Added `scene.metal()` - Material preset for shiny metal.
- Added `scene.rough_metal()` - Material preset for brushed metal.
- Added `scene.glossy()` - Material preset for glossy surfaces.
- Added `scene.matte()` - Material preset for matte surfaces.
- Added `scene.new_standard_material()` - Create material builder with defaults.
- Added `scene.mat_color()` - Set material color on builder.
- Added `scene.mat_metalness()` - Set metalness on builder.
- Added `scene.mat_roughness()` - Set roughness on builder.
- Added `scene.mat_map()` - Set diffuse texture map on builder.
- Added `scene.mat_normal_map()` - Set normal map on builder.
- Added `scene.mat_ao_map()` - Set ambient occlusion map on builder.
- Added `scene.mat_roughness_map()` - Set roughness texture map on builder.
- Added `scene.mat_metalness_map()` - Set metalness texture map on builder.
- Added `scene.build_material()` - Build final material from builder.
- Added `scene.new_box()` - Create box geometry builder.
- Added `scene.box_width()` - Set box width on builder.
- Added `scene.box_height()` - Set box height on builder.
- Added `scene.box_depth()` - Set box depth on builder.
- Added `scene.box_size()` - Set all box dimensions on builder.
- Added `scene.box_cube()` - Create uniform cube on builder.
- Added `scene.build_box()` - Build final box geometry from builder.
- Added `scene.new_sphere()` - Create sphere geometry builder.
- Added `scene.sphere_radius()` - Set sphere radius on builder.
- Added `scene.sphere_width_segments()` - Set sphere width segments on builder.
- Added `scene.sphere_height_segments()` - Set sphere height segments on builder.
- Added `scene.sphere_segments()` - Set sphere segments (width and height/2) on builder.
- Added `scene.build_sphere()` - Build final sphere geometry from builder.
- Added `scene.new_plane()` - Create plane geometry builder.
- Added `scene.plane_width()` - Set plane width on builder.
- Added `scene.plane_height()` - Set plane height on builder.
- Added `scene.plane_size()` - Set plane dimensions on builder.
- Added `scene.plane_square()` - Create uniform square plane on builder.
- Added `scene.build_plane()` - Build final plane geometry from builder.
- Added `transform.translate_by()` - Move transform by offset (relative).
- Added `transform.rotate_by()` - Rotate transform by angle (relative).
- Added `transform.scale_by()` - Scale transform by factor (relative).
- Added `transform.scale_uniform()` - Set uniform scale on all axes.
- Added `transform.rotate_x()` - Rotate around X axis (pitch).
- Added `transform.rotate_y()` - Rotate around Y axis (yaw).
- Added `transform.rotate_z()` - Rotate around Z axis (roll).
- Added `transform.move_forward()` - Move along local Z axis.
- Added `transform.move_right()` - Move along local X axis.
- Added `transform.move_up()` - Move along world Y axis.

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
- **Ergonomic API Improvements**: Material presets, builder patterns, and transform convenience methods reduce typical scene construction code by 50-70%.
- Builder patterns provide fluent APIs with sensible defaults while maintaining full type safety and validation.

### Documentation

- Added "Game Context" section explaining canvas dimensions and coordinate conversion.
- Updated camera documentation to reflect automatic aspect ratio calculation.
- Added examples of fullscreen vs fixed-size canvas configuration.
- Updated all 20 examples and 3 games to use new APIs.
- Added comprehensive test suite for builder patterns and convenience methods (41 new tests, 250 total tests).

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
