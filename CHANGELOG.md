# Changelog


## v4.1.0 - 2025-10-25

### Added
- **Spritesheet Animation System**: Complete texture-based sprite animation support
  - `spritesheet.from_grid()` - Create spritesheets from grid-based texture atlases
  - `spritesheet.animation()` - Define animation sequences with frame indices and timing
  - Three loop modes: `Once`, `Repeat`, and `PingPong` for different animation behaviors
  - `scene.animated_sprite()` - Scene node for displaying animated sprites
  - Frame-by-frame animation with delta time updates for frame-rate independence
  - Animation state control: play, pause, stop, and change animations dynamically
  - Pixel art support with `NearestFilter` for crisp, non-blurred sprites
  - Independent animation per sprite instance (automatic texture cloning)
  - Floating-point precision handling with `float.loosely_equals` for reliable frame timing
  - Example: `examples/27-spritesheet_animation` demonstrates three independently animating coins
- **Texture Manipulation API**: New utilities for advanced texture control
  - `texture.clone()` - Clone textures for independent manipulation
  - `texture.set_offset()` - Control UV offset for texture positioning
  - `texture.set_repeat()` - Control UV repeat/scaling
  - `texture.set_wrap_mode()` - Set texture wrapping behavior (Repeat, Clamp, MirroredRepeat)
  - `texture.set_filter_mode()` - Set texture filtering (Nearest for pixel art, Linear for smooth)
- **3D Sprite Labels with Canvas Rendering**: New sprite system for depth-aware labels that properly occlude behind 3D objects
  - `scene.canvas()` - Create canvas-based sprites with `paint.Picture` for drawing custom graphics (renamed from `scene.sprite()`)
  - Sprites use canvas 2D rendering via the `paint` library for flexible visual customization
  - True 3D depth testing - sprites hide behind other objects in the scene
  - Perfect for health bars, damage numbers, interactive labels, and UI elements in 3D space
  - Example: `examples/26-css2d_labels` demonstrates sprite usage alongside CSS2D labels
- **CSS2D and CSS3D Label Support**: HTML-based overlay labels for UI elements
  - `scene.css2d_label()` - Create screen-space HTML labels that always render on top
  - `scene.css3d_label()` - Create 3D-positioned HTML labels (no depth occlusion with WebGL)
  - Full HTML/CSS support with Tailwind styling
  - Useful for UI overlays, tooltips, and text that should never be occluded
- **Paint Library Integration**: Added `paint` (v0.4.0) dependency for canvas-based drawing
  - Functional canvas 2D API for creating Pictures
  - Combines shapes, text, fills, strokes with composition
  - Pictures are rendered to canvas textures for sprite display

## v4.0.3 - 2025-10-24

### Fixed
- Fixed a bug where effect.interval and effect.delay were running even if the application was paused.

## v4.0.2 - 2025-10-24

### Fixed
- **Orthographic camera resize handling**: Fixed window resize listener to properly update orthographic cameras
  - Resize event now updates orthographic camera frustum (left, right, top, bottom) based on new window dimensions
  - Previously only perspective cameras were updated on resize, causing 2D games to stretch when window was resized
  - `camera_2d()` cameras now maintain correct aspect ratio and scale across all window sizes

## v4.0.1 - 2025-10-24

### Fixed
- **Production build compatibility**: Fixed constructor name encoding issues that could cause failures in minified builds
  - Background type checking now uses Gleam-generated `Background$is*` functions instead of `constructor.name`
  - Asset type checking now uses Gleam-generated `AssetType$is*` functions instead of `constructor.name`
  - ID serialization now uses WeakMap-based stable constructor IDs instead of `constructor.name`
  - All FFI code is now immune to JavaScript minification and build optimization

## v4.0.0 - 2025-10-22

### Added
- **Comprehensive API documentation**: Added extensive module-level and function-level documentation with examples for:
  - `scene` module - Scene node system with constructor functions, node types, and usage patterns
  - `animation` module - Tweening and model animation system
  - `physics` module - Rigid body dynamics, colliders, forces, and collision detection
  - `material` module - PBR materials, builder pattern, texture mapping examples
  - `light` module - Lighting types, shadow configuration, typical setups
  - `debug` module - Debug visualization utilities and performance monitoring
  - `state_machine` module - Type-safe animation state machines with context
  - `particle_emitter` module - GPU-accelerated particle effects with builder API
  - `audio` module - Spatial and global audio with Web Audio API
  - `asset` module - Asset loading, caching, and batch loading
  - `spatial` module - Spatial partitioning and collision detection
- **Scene node constructor functions**: Scene nodes now use opaque types with explicit constructor functions:
  - `scene.mesh()` - Create mesh nodes
  - `scene.instanced_mesh()` - Create instanced mesh nodes
  - `scene.group()` - Create group nodes
  - `scene.light()` - Create light nodes
  - `scene.camera()` - Create camera nodes
  - `scene.lod()` - Create LOD nodes
  - `scene.model3d()` - Create 3D model nodes
  - `scene.instanced_model()` - Create instanced model nodes
  - `scene.audio()` - Create audio nodes
  - `scene.particles()` - Create particle emitter nodes
  - `scene.debug_*()` - Create debug visualization nodes
- **Enhanced Collider API**: Spatial module now uses a unified `Collider` type supporting both boxes and spheres:
  - `spatial.collider_box()` - Create box collider from min/max points
  - `spatial.collider_box_from_center()` - Create box from center and half-extents
  - `spatial.collider_sphere()` - Create sphere collider from center and radius
  - `spatial.collider_contains_point()` - Point-in-collider test (works for both box and sphere)
  - `spatial.collider_intersects()` - Collider intersection test (box-box, sphere-sphere, box-sphere)
  - Box-sphere collision detection for heterogeneous spatial queries
- **Documentation guide**: Added `docs/physics-guide.md` with comprehensive physics system documentation

### Changed
- **Scene module refactoring**: Moved renderer logic from `tiramisu/internal/renderer.gleam` into `tiramisu/scene.gleam` for better organization (removed 1926 lines, added rendering functions directly to scene module)
- **Scene.Node type is now opaque**: Must use constructor functions instead of direct constructors (e.g., `scene.mesh()` instead of `scene.Mesh()`)
- **Spatial API redesign**: Renamed `AABB` type to `Collider` and updated all related functions:
  - `spatial.aabb()` → `spatial.collider_box()`
  - `spatial.aabb_from_center()` → `spatial.collider_box_from_center()`
  - `spatial.aabb_contains_point()` → `spatial.collider_contains_point()`
  - `spatial.aabb_intersects()` → `spatial.collider_intersects()`
  - `spatial.aabb_center()` → `spatial.collider_center()`
  - Octree API updated to use new collider types: `octree_new()`, `octree_insert()`, `octree_query_radius()`, etc.
- **Type reorganization**:
  - Moved `Object3D` type from `tiramisu/object3d` module to `tiramisu/asset` module
  - Moved `AnimationClip` type from `tiramisu/object3d` module to `tiramisu/animation` module
  - Moved `AnimationPlayback` type to `tiramisu/animation` module
- **Context type simplified**: Removed `input_manager` field from `Context` type (internal implementation detail)
- **Main module streamlined**: Simplified `tiramisu.gleam` by removing example code and focusing on core API surface
- **Documentation improvements**: Updated scene graph and getting started guides, removed spatial partitioning guide (content integrated into API docs)

### Removed
- The `tiramisu/internal/renderer.gleam` module (logic moved to `tiramisu/scene.gleam`)
- The `tiramisu/internal/input_manager.gleam` module (no longer needed)
- The `docs/spatial-partitioning-guide.md` file (integrated into module documentation)
- The `examples/README.md` file (examples are self-documenting with inline comments)

### Fixed
- **Fixed GLTF animation playback speed**: Three.js AnimationMixer now correctly receives delta time in seconds (converted from milliseconds in `updateMixer`)
- **Fixed animation state machine blend durations**: State machine now converts delta_time from milliseconds to seconds internally, allowing blend_duration to remain in seconds as documented (e.g., 0.3 for 300ms blend)
- **Fixed particle system physics and spawning**: Particle system now converts delta_time from milliseconds to seconds internally for correct spawn rates, lifetimes, gravity, and velocity calculations
- Fixed documentation to clarify that `Context.delta_time` is in **milliseconds**, not seconds (e.g., 16.0 for 60 FPS)
- Fixed animation speed in multiple examples that were using incorrect multipliers after delta_time changed to milliseconds:
  - Example 14 (gltf_animated_model): Added clarifying comment
  - Example 15 (character_controller): Changed multiplier from 0.3 to 0.0003
  - Example 19 (ui_overlay): Fixed rotation and movement speed calculations
  - Example 20 (not-bread): Changed multiplier from 0.5 to 0.0005
  - Example 22 (audio_demo): Changed multiplier from 0.5 to 0.0005
  - Example 23 (physics_advanced): Changed multiplier from 2.0 to 0.002
  - Example 24 (effects_showcase): Changed multiplier from 0.5 to 0.0005
  - Example 25 (text_geometry): Changed multiplier from 0.3 to 0.0003

## v3.1.2 - 2025-10-22

- Fix ordering of applied physics commands, as they were reversed before.

## v3.1.1 - 2025-10-21

- Fix camera issue with look_at being cached and not using the updated look_at each frame.

## v3.1.0 - 2025-10-20

### Added
- **InstancedModel scene node**: Efficiently render many copies of loaded 3D models (FBX, GLTF, etc.)
  - `scene.InstancedModel(id, object, instances)` - Automatically instances complex models with multiple meshes/materials
  - Internally creates separate `InstancedMesh` for each unique mesh/material combination
  - Dramatically improves performance: 10,000 models = a few draw calls instead of 10,000
  - Works seamlessly with `asset.load_fbx()` and other model loaders
  - Supports dynamic instance updates through the existing diff/patch system
- **FBX model loading support**: Load Autodesk FBX files with animations and materials
  - `asset.FBXAsset(url, texture_path)` - Load FBX files with optional texture directory path
  - `asset.load_fbx(url, texture_path)` - Direct FBX loading function
  - `asset.get_fbx(cache, url)` - Get FBX data from cache
  - `asset.get_fbx_scene(cache, url)` - Get just the scene object from cached FBX
  - `asset.FBXData` type containing scene and animations
  - Automatic model centering at origin
  - Support for both ASCII and binary FBX formats
  - Skeletal and morph target animation support
- **Texture filtering API**: Control texture appearance for pixel-perfect or smooth rendering
  - `asset.TextureFilter` type with `LinearFilter` and `NearestFilter` variants
  - `asset.set_texture_filter(texture, filter)` - Set filtering mode on any texture
  - `asset.apply_texture_to_object(object, texture, filter)` - Apply texture to all materials in a model with specified filtering
  - Perfect for retro/PSX aesthetics with crisp textures (`NearestFilter`)
- **Object cloning for instancing**: Create independent copies of 3D models
  - `asset.clone_object3d(object)` - Clone an Object3D for reuse in multiple scene locations
  - Essential when placing the same model in multiple positions (Three.js limitation)
- **Enhanced FBX loader with automatic texture fixing**:
  - Attempts to automatically fix broken texture references in FBX files
  - Logs detailed texture loading information for debugging
  - Centers models at origin automatically

### Changed
- `apply_texture_to_object()` now requires a `TextureFilter` parameter for explicit control over texture appearance

## v3.0.2 - 2025-10-20

### Added
- **Kinematic body control**: Kinematic physics bodies can now be moved programmatically via scene transforms - updating a mesh's transform will automatically sync to the physics body
- `physics.update_body_transform()` internal function for syncing scene transforms to physics bodies
- Physics sync optimization: Only Dynamic bodies are synced from physics to rendering; Kinematic and Fixed bodies are controlled by scene transforms

## v3.0.1 - 2025-10-20

### Fixed
- **Critical rotation bug**: Fixed `applyTransform` FFI function to correctly use quaternions instead of incorrectly treating quaternion components as Euler angles, which caused objects to rotate on multiple axes when they should only rotate on one
- Euler/Quaternion conversions now use Three.js's built-in conversion functions via FFI for 100% compatibility with Three.js rotation behavior
- Quaternion multiplication now uses Three.js's built-in `multiply()` function via FFI for accurate rotation composition
- Test suite updated to handle Euler angle representation ambiguity by comparing quaternions directly instead of Euler angles

## v3.0.0 - 2025-10-17

- The `WorldConfig`'s parameter `correspondences` has been removed from the physics module.
- The function `step_world` from the module physics has been removed in favour of the `step` function.
- Fixed references to tiramisu.Color and tiramisu.Texture, as they should be background.Color and background.Texture.
- The `RigidBody` type from the physics module is now opaque 
- The `Transform` type is now opaque - fields cannot be accessed directly
- Direct field access (e.g., `transform.position`) must be replaced with accessor functions
- The `Quaternion` type has been moved from the `physics` module to the `transform` module
- Added `transform.position(t)` - Get position from a transform
- Added `transform.rotation(t)` - Get rotation as Euler angles (radians)
- Added `transform.rotation_quaternion(t)` - Get rotation as a quaternion
- Added `transform.scale(t)` - Get scale from a transform
- Added `transform.with_euler_rotation(t, euler)` - Set rotation using Euler angles
- Added `transform.with_quaternion_rotation(t, quat)` - Set rotation using a quaternion
- Added `transform.with_rotation(t, euler)` - Alias for `with_euler_rotation` (backwards compatible)
- Added `transform.euler_to_quaternion(euler)` - Convert Euler angles to quaternion
- Added `transform.quaternion_to_euler(quat)` - Convert quaternion to Euler angles
- Added `transform.identity_quaternion` - The identity quaternion (no rotation)
- Rotations are now stored internally as quaternions to avoid gimbal lock
- `rotate_by()` now uses proper quaternion multiplication for accurate rotation composition
- All quaternion/Euler conversions implemented in pure Gleam (no FFI)
- Smoother rotation interpolation with spherical linear interpolation (slerp)


## v2.1.0 - 2025-10-14

- The `WorldConfig`'s parameter `correspondences` has been deprecated to let users interact with physics objects created dinamically.

## v2.0.0 - 2025-10-13

- The `background` parameter now requires a `Background` type instead of a hex integer.
- The `Background` type has been added with `Color`, `Texture`, and `CubeTexture` constructors.
- The `width` and `height` parameters have been replaced with a single `dimensions` parameter.
- The `Dimensions` type has been added.
- The `aspect` parameter has been removed from `camera.perspective()`.
- The `camera.set_position()` function has been removed.
- The `camera.set_look_at()` function has been removed.
- The `distance` parameter has been removed from `camera.camera_2d()`.
- Camera position is now configured via the `transform` field on `scene.Camera` nodes.
- Camera look-at is now configured via the `look_at` field on `scene.Camera` nodes.
- The `scene.BoxGeometry` constructor has been removed.
- The `scene.SphereGeometry` constructor has been removed.
- The `scene.PlaneGeometry` constructor has been removed.
- The `scene.CylinderGeometry` constructor has been removed.
- The `scene.ConeGeometry` constructor has been removed.
- The `scene.TorusGeometry` constructor has been removed.
- The `scene.TorusKnotGeometry` constructor has been removed.
- The `tiramisu/geometry` module has been added.
- The `geometry.box()` function has been added.
- The `geometry.sphere()` function has been added.
- The `geometry.plane()` function has been added.
- The `geometry.cylinder()` function has been added.
- The `geometry.cone()` function has been added.
- The `geometry.torus()` function has been added.
- The `geometry.torus_knot()` function has been added.
- Geometry constructors now return `Result(Geometry, GeometryError)`.
- The `GeometryError` type has been added.
- The `scene.StandardMaterial` constructor has been removed.
- The `scene.BasicMaterial` constructor has been removed.
- The `tiramisu/material` module has been added.
- The `material.new()` function has been added.
- The `material.standard()` function has been added.
- The `material.basic()` function has been added.
- The `material.with_color()` function has been added.
- The `material.with_metalness()` function has been added.
- The `material.with_roughness()` function has been added.
- The `material.with_map()` function has been added.
- The `material.with_normal_map()` function has been added.
- The `material.with_ao_map()` function has been added.
- The `material.with_roughness_map()` function has been added.
- The `material.with_metalness_map()` function has been added.
- The `material.build()` function has been added.
- Material constructors now return `Result(Material, MaterialError)`.
- The `MaterialError` type has been added.
- The `scene.DirectionalLight` constructor has been removed.
- The `scene.PointLight` constructor has been removed.
- The `scene.AmbientLight` constructor has been removed.
- The `scene.SpotLight` constructor has been removed.
- The `tiramisu/light` module has been added.
- The `light.directional()` function has been added.
- The `light.point()` function has been added.
- The `light.ambient()` function has been added.
- The `light.spot()` function has been added.
- Light constructors now return `Result(Light, LightError)`.
- The `LightError` type has been added.
- The `light_type` field on `scene.Light` nodes has been renamed to `light`.
- State machines are now generic over state type instead of string-based.
- The `state_machine.new()` function now takes a state value instead of a string.
- The `state_machine.add_state()` function now takes a state value instead of a string.
- The `state_machine.add_transition()` function now takes state values instead of strings.
- The `canvas_width` field has been added to the `Context` type.
- The `canvas_height` field has been added to the `Context` type.
- Fullscreen mode is now supported via `dimensions: option.None`.
- Touch coordinate alignment has been fixed.
- The `src/tiramisu/ffi/` directory has been removed.
- The `src/tiramisu/ffi/asset.mjs` file has been removed.
- The `src/tiramisu/ffi/audio.mjs` file has been removed.
- The `src/tiramisu/ffi/camera.mjs` file has been removed.
- The `src/tiramisu/ffi/debug.mjs` file has been removed.
- The `src/tiramisu/ffi/effects.mjs` file has been removed.
- The `src/tiramisu/ffi/gltf.mjs` file has been removed.
- The `src/tiramisu/ffi/input_capture.mjs` file has been removed.
- The `src/tiramisu/ffi/obj.mjs` file has been removed.
- The `src/tiramisu/ffi/object3d.mjs` file has been removed.
- The `src/tiramisu/ffi/physics.mjs` file has been removed.
- The `src/tiramisu/ffi/renderer.mjs` file has been removed.
- The `src/tiramisu/ffi/stl.mjs` file has been removed.
- The `src/tiramisu/ffi/test_helpers.mjs` file has been removed.
- The `src/tiramisu/ffi/texture.mjs` file has been removed.
- The `src/tiramisu/ffi/ui.mjs` file has been removed.
- The `src/threejs.ffi.mjs` file has been added with pure Three.js bindings.
- The `src/rapier.ffi.mjs` file has been added with pure Rapier physics bindings.
- The `src/tiramisu.ffi.mjs` file has been added with consolidated game engine logic.
- The `tiramisu/internal/animation_manager` module has been added.
- The `tiramisu/internal/audio_manager` module has been added.
- The `tiramisu/internal/camera_manager` module has been added.
- The `tiramisu/internal/id` module has been added.
- The `tiramisu/internal/object_cache` module has been added.
- The `tiramisu/internal/particle_manager` module has been added.
- The `tiramisu/internal/physics_manager` module has been removed.

## v1.0.0 - 2025-10-08

- Initial release.
