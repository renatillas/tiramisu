# Changelog

## v2.0.0 - 2025-10-11

This major release brings significant ergonomic improvements, new features, and breaking changes to the API structure.

### Breaking Changes

- The `scene.Node` type is now generic over an ID type (`Node(id)`) enabling type-safe node identification. Define a custom type for your node IDs and use it throughout your game.
- The `tiramisu.run` function now uses `dimensions: Option(Dimensions)` instead of separate `width` and `height` parameters.
- The `tiramisu.Context` type is now generic over ID type (`Context(id)`) to match the scene nodes.
- The `physics.PhysicsWorld` type is now generic over ID type (`PhysicsWorld(id)`) for type-safe body identification.
- The `physics.build_body` function has been renamed to `physics.build` for consistency with other builder patterns.
- The `camera.perspective` function no longer requires an `aspect` parameter; it's calculated automatically.
- The `camera.camera_2d` function no longer accepts a `distance` parameter; use the `transform` field on the `scene.Camera` node instead.
- Camera position and `look_at` are now configured on the `scene.Camera` node rather than on the camera type itself.
- The `Context` type now includes `canvas_width` and `canvas_height` fields.
- Geometry constructors have been moved to the `tiramisu/geometry` module and return `Result` types.
- Material constructors have been moved to the `tiramisu/material` module and return `Result` types.
- Light constructors have been moved to the `tiramisu/light` module and return `Result` types.
- The `scene.Light` node field `light_type` has been renamed to `light`.
- State machines are now generic over state type instead of using string-based states.

### New Modules

- The `tiramisu/geometry` module provides validated geometry constructors.
- The `tiramisu/material` module provides validated material constructors with builder pattern.
- The `tiramisu/light` module provides validated light constructors with shadow configuration.
- The `tiramisu/transform` module provides transform operations and helpers.
- The `tiramisu/particle_emitter` module provides particle system functionality.

### Material System

- The `material.new` function creates a material builder with sensible defaults.
- The `material.with_color` function sets the material color.
- The `material.with_metalness` function sets the metalness value (0.0-1.0).
- The `material.with_roughness` function sets the roughness value (0.0-1.0).
- The `material.with_color_map` function sets the color texture map.
- The `material.with_normal_map` function sets the normal map.
- The `material.with_ambient_oclusion_map` function sets the ambient occlusion map.
- The `material.with_roughness_map` function sets the roughness map.
- The `material.with_metalness_map` function sets the metalness map.
- The `material.build` function validates and builds the material.
- The `material.basic` function creates an unlit basic material.
- The `material.standard` function creates a PBR standard material.
- The `material.lambert` function creates a matte Lambert material.
- The `material.phong` function creates a shiny Phong material.
- The `material.toon` function creates a cartoon-style toon material.
- The `material.line` function creates a line material.
- The `material.sprite` function creates a sprite material for billboards.

### Geometry System

- The `geometry.box` function creates a validated box geometry.
- The `geometry.sphere` function creates a validated sphere geometry.
- The `geometry.cone` function creates a validated cone geometry.
- The `geometry.plane` function creates a validated plane geometry.
- The `geometry.circle` function creates a validated circle geometry.
- The `geometry.cylinder` function creates a validated cylinder geometry.
- The `geometry.torus` function creates a validated torus geometry.
- The `geometry.tetrahedron` function creates a validated tetrahedron geometry.
- The `geometry.icosahedron` function creates a validated icosahedron geometry.
- The `geometry.custom_geometry` function wraps custom buffer geometry.

### Lighting System

- The `light.ambient` function creates an ambient light.
- The `light.directional` function creates a directional light (like the sun).
- The `light.point` function creates a point light (omnidirectional).
- The `light.spot` function creates a spotlight with cone shape.
- The `light.hemisphere` function creates a hemisphere light for outdoor scenes.
- The `light.with_shadows` function enables shadow casting for a light.
- The `light.with_shadow_resolution` function sets shadow map resolution (512, 1024, 2048, 4096).
- The `light.with_shadow_bias` function adjusts shadow bias to reduce artifacts.

### Physics System

- The `physics.new_rigid_body` function creates a physics body builder.
- The `physics.body_collider` function sets the collider shape.
- The `physics.body_mass` function sets the body mass.
- The `physics.body_restitution` function sets bounce coefficient.
- The `physics.body_friction` function sets friction coefficient.
- The `physics.body_linear_damping` function sets linear damping.
- The `physics.body_angular_damping` function sets angular damping.
- The `physics.enable_body_ccd` function enables continuous collision detection.
- The `physics.body_collision_groups` function configures collision filtering.
- The `physics.lock_rotation_x` function locks rotation on the X axis.
- The `physics.lock_rotation_y` function locks rotation on the Y axis.
- The `physics.lock_rotation_z` function locks rotation on the Z axis.
- The `physics.lock_translation_x` function locks translation on the X axis.
- The `physics.lock_translation_y` function locks translation on the Y axis.
- The `physics.lock_translation_z` function locks translation on the Z axis.
- The `physics.build` function validates and builds the physics body.
- The `physics.raycast` function casts a ray and returns the first hit.
- The `physics.raycast_all` function casts a ray and returns all hits.
- The `physics.get_collision_events` function retrieves collision events from the last frame.
- The `physics.are_colliding` function checks if two specific bodies are colliding.
- The `physics.set_angular_velocity` function sets angular velocity directly.
- The `physics.get_angular_velocity` function gets the current angular velocity.
- The `physics.apply_torque` function applies continuous torque.
- The `physics.apply_torque_impulse` function applies an instantaneous torque impulse.

### Input System

- The `input.new_bindings` function creates empty input bindings.
- The `input.bind_key` function binds a keyboard key to an action.
- The `input.bind_mouse_button` function binds a mouse button to an action.
- The `input.bind_gamepad_button` function binds a gamepad button to an action.
- The `input.is_action_pressed` function checks if an action is currently held.
- The `input.is_action_just_pressed` function checks if an action was pressed this frame.
- The `input.is_action_just_released` function checks if an action was released this frame.
- The `input.get_action_value` function gets the analog value (0.0-1.0) for an action.
- The `input.with_buffer` function creates an input buffer with frame window.
- The `input.update_buffer` function updates buffer with current input state.
- The `input.was_action_pressed_buffered` function checks if action is in buffer.
- The `input.consume_buffered_action` function removes an action from buffer.
- The `input.clear_buffer` function clears all buffered actions.

### Audio System

- The `audio.play_music` function plays background music with fade-in.
- The `audio.stop_music` function stops music with fade-out.
- The `audio.crossfade_music` function crossfades between two music tracks.
- The `audio.config_with_group` function creates audio config with a group.
- The `audio.set_group_volume` function sets volume for an entire audio group.
- The `audio.get_group_volume` function gets the current group volume.
- The `audio.mute_group` function mutes an audio group.
- The `audio.unmute_group` function unmutes an audio group.

### Particle System

- The `scene.new_particle_emitter` function creates a particle emitter builder.
- The `scene.emitter_rate` function sets particles per second.
- The `scene.emitter_lifetime` function sets particle lifetime in seconds.
- The `scene.emitter_velocity` function sets base velocity vector.
- The `scene.emitter_velocity_variance` function sets velocity randomness.
- The `scene.emitter_size` function sets particle size.
- The `scene.emitter_size_variance` function sets size randomness.
- The `scene.emitter_color` function sets starting color.
- The `scene.emitter_fade_to` function sets ending color for fade effect.
- The `scene.emitter_gravity` function sets gravity scale.
- The `scene.emitter_max_particles` function sets maximum particle count.
- The `scene.build_emitter` function validates and builds the emitter.

### Transform System

- The `transform.identity` constant provides an identity transform.
- The `transform.at` function creates a transform at a position.
- The `transform.set_position` function sets the position.
- The `transform.set_rotation` function sets the rotation.
- The `transform.set_scale` function sets the scale.
- The `transform.translate_by` function moves by a relative offset.
- The `transform.rotate_by` function rotates by relative amounts.
- The `transform.scale_by` function scales by relative factors.
- The `transform.scale_uniform` function scales uniformly on all axes.
- The `transform.rotate_x` function rotates around X axis.
- The `transform.rotate_y` function rotates around Y axis.
- The `transform.rotate_z` function rotates around Z axis.
- The `transform.move_forward` function moves forward in local space.
- The `transform.move_right` function moves right in local space.
- The `transform.move_up` function moves up in local space.

### Debug System

- The `debug.inspect_scene` function prints hierarchical scene tree to console.
- The `debug.new_profiler` function creates a performance profiler.
- The `debug.start_section` function begins timing a named section.
- The `debug.end_section` function ends current section and records duration.
- The `debug.print_profile` function outputs formatted timing results.
- The `debug.get_profile_results` function returns timing data as list of tuples.
- The `debug.show_colliders` function enables/disables collision visualization.
- The `debug.show_raycast` function visualizes raycasts as colored lines.

### Features

- Type-safe node IDs via generic `Node(id)` type - define custom ID types for compile-time safety.
- Fullscreen mode is now supported via `dimensions: option.None`.
- Canvas dimensions are now accessible in the `Context` type.
- Camera aspect ratio is calculated automatically at render time.
- Camera `look_at` is now supported directly in scene nodes.
- Shadow maps are enabled by default with PCF soft shadows.
- All meshes cast and receive shadows by default.
- Physics world uses type-safe IDs matching your scene node ID type.
- Hot reload protection prevents duplicate event listeners across module reloads.
- Gamepad polling optimization - only polls when gamepads are actually connected (performance improvement).
- Window resize listener now uses hot reload protection to prevent duplicate registrations.

## v1.0.0 - 2024-10-07

Initial release of Tiramisu game engine for Gleam.

### Features

- 3D rendering via Three.js with WebGL acceleration.
- 2D sprite support with orthographic camera.
- Scene graph system with hierarchical transforms.
- Input handling for keyboard, mouse, touch, and gamepad.
- Basic physics support via Rapier physics engine.
- Asset loading for models, textures, and audio.
- Animation system with Three.js AnimationMixer.
- Camera controls (perspective, orthographic).
- Lighting system (ambient, directional, point, spot, hemisphere).
- Material system (basic, standard, lambert, phong, toon).
- Debug visualization tools.
- MVU (Model-View-Update) architecture.
- Effect system for side effects.
