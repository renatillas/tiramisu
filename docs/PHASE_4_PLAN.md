# Phase 4: Advanced Features - Plan

## Overview
Phase 4 builds on the immutable foundation from Phase 3 to add advanced game engine features while maintaining the declarative, functional programming style.

---

## Current Status

### ✅ Already Completed (from recent work)
1. **3D Model Loading**
   - ✅ STL file loading with custom BufferGeometry
   - ✅ GLTF/GLB loading with skeletal animations
   - ✅ Async loading with promises and effects
   - ✅ `object3d` module for animation configuration
   - ✅ `Model3D` scene node with declarative animations
   - ✅ AnimationMixer integration (automatic updates)
   - ✅ Examples: `stl_loader`, `gltf_animated_model`

---

## Phase 4 Goals

### 1. Animation State Machines 🎯
**Objective**: Create a declarative state machine system for complex animation transitions

**Features to Implement**:
- [ ] `tiramisu/animation/state_machine.gleam`
  - Define animation states with transitions
  - Trigger conditions (time, input, custom predicates)
  - Blend between states with configurable duration
  - Cross-fade support

**Example API**:
```gleam
// Define states
let idle = state_machine.state("idle", idle_animation)
let walk = state_machine.state("walk", walk_animation)
let run = state_machine.state("run", run_animation)

// Define transitions
state_machine.new()
|> state_machine.add_transition(
  from: idle,
  to: walk,
  condition: fn(ctx) { input.is_key_pressed(ctx.input, KeyW) },
  blend_time: 0.2,
)
|> state_machine.add_transition(
  from: walk,
  to: run,
  condition: fn(ctx) { input.is_key_pressed(ctx.input, KeyShift) },
  blend_time: 0.15,
)
```

**Deliverables**:
- [ ] `src/tiramisu/animation/state_machine.gleam` - Core state machine
- [ ] `examples/animation_state_machine/` - Character animation example
- [ ] Documentation with state diagrams

---

### 2. Basic Collision Detection 🎯
**Objective**: Provide simple collision detection for common use cases

**Features to Implement**:
- [ ] `tiramisu/physics/collision.gleam`
  - Bounding box (AABB) collision
  - Sphere collision
  - Ray casting
  - Collision groups/layers
  - Query functions (point in box, ray intersects, etc.)

**Example API**:
```gleam
// Define collision shapes
let player_box = collision.aabb(
  position: player.position,
  size: vec3.Vec3(1.0, 2.0, 1.0),
)

let enemy_sphere = collision.sphere(
  position: enemy.position,
  radius: 0.5,
)

// Check collision
case collision.check(player_box, enemy_sphere) {
  collision.Colliding(info) -> handle_collision(info)
  collision.NotColliding -> continue_game()
}

// Ray casting for projectiles
let ray = collision.ray(origin: gun_pos, direction: forward)
case collision.raycast(ray, enemies) {
  Ok(hit) -> damage_enemy(hit.entity, hit.point)
  Error(Nil) -> miss()
}
```

**Deliverables**:
- [ ] `src/tiramisu/physics/collision.gleam` - Core collision detection
- [ ] `src/tiramisu/physics/aabb.gleam` - AABB utilities
- [ ] `src/tiramisu/physics/ray.gleam` - Ray casting
- [ ] `examples/collision_demo/` - Interactive collision example
- [ ] Tests for collision algorithms

---

### 3. Debug Visualization Tools 🎯
**Objective**: Add visual debugging tools to inspect game state

**Features to Implement**:
- [ ] `tiramisu/debug.gleam`
  - Draw bounding boxes/spheres
  - Draw ray casts
  - Draw coordinate axes
  - Draw grid
  - FPS counter
  - Performance stats
  - Scene graph inspector

**Example API**:
```gleam
import tiramisu/debug

fn view(model: Model) -> List(scene.SceneNode) {
  [
    // Regular scene
    player_mesh,
    enemy_mesh,

    // Debug visualization (only in debug mode)
    ..case model.debug_mode {
      True -> [
        debug.bounding_box(player.collision_box, color: 0x00ff00),
        debug.axes(origin: vec3.zero(), size: 2.0),
        debug.grid(size: 20, divisions: 20),
        debug.fps_counter(position: vec2.Vec2(10.0, 10.0)),
        debug.ray(ray: shooting_ray, max_distance: 100.0, color: 0xff0000),
      ]
      False -> []
    }
  ]
}
```

**Deliverables**:
- [ ] `src/tiramisu/debug.gleam` - Debug visualization API
- [ ] `src/tiramisu/ffi/debug.mjs` - Three.js debug rendering
- [ ] `examples/debug_tools/` - Debug visualization showcase
- [ ] Toggle debug mode with keyboard shortcut

---

### 4. Enhanced Asset Loading 🎯
**Objective**: Expand asset loading capabilities

**Features to Implement**:
- [ ] Audio loading (THREE.AudioLoader)
- [ ] Texture loading improvements
  - Normal maps
  - Environment maps
  - HDR textures
- [ ] Asset preloading with progress
- [ ] Asset caching and management

**Example API**:
```gleam
import tiramisu/assets

// Preload assets with progress
let load_effect = assets.preload([
  assets.Model("character.glb"),
  assets.Texture("diffuse.png"),
  assets.Texture("normal.png"),
  assets.Audio("jump.mp3"),
])
|> effect.on_progress(fn(progress) { LoadProgress(progress) })
|> effect.map(fn(results) { AssetsLoaded(results) })

// Use loaded assets
let model_mesh = scene.Model3D(
  id: "player",
  object: assets.get_model(model.assets, "character.glb"),
  material: scene.StandardMaterial(
    map: assets.get_texture(model.assets, "diffuse.png"),
    normal_map: assets.get_texture(model.assets, "normal.png"),
  ),
  ...
)
```

**Deliverables**:
- [ ] `src/tiramisu/assets.gleam` - Asset management system
- [ ] Audio loading support
- [ ] Normal map and environment map support
- [ ] `examples/asset_loading/` - Preloading example
- [ ] Progress bar component

---

## Implementation Order

### Week 1-2: Animation State Machines
1. Design state machine API
2. Implement core state machine logic
3. Add animation blending support
4. Create character animation example
5. Documentation and tests

### Week 3-4: Collision Detection
1. Implement AABB collision
2. Implement sphere collision
3. Add ray casting
4. Create collision demo example
5. Write comprehensive tests

### Week 5-6: Debug Visualization
1. Implement basic debug shapes (boxes, spheres, lines)
2. Add FPS and performance counters
3. Create debug visualization example
4. Add keyboard shortcuts for debug mode

### Week 7-8: Enhanced Asset Loading
1. Add audio loading support
2. Implement normal map support
3. Add asset preloading with progress
4. Create asset loading example
5. Polish and documentation

---

## Success Criteria

- [ ] All planned features implemented and tested
- [ ] At least 4 comprehensive examples
- [ ] Full API documentation
- [ ] All tests passing
- [ ] Performance benchmarks show <16ms frame time
- [ ] Examples run smoothly at 60 FPS

---

## Breaking Changes

Phase 4 should maintain backward compatibility with Phase 3. Any additions should be opt-in features.

**Potential Breaking Changes**:
- None expected (all features are additive)

---

## Dependencies

**New Three.js Features Used**:
- AudioLoader (for audio loading)
- LineSegments (for debug visualization)
- BoxHelper, GridHelper (for debug tools)

**No External Dependencies Required**:
- All features use existing Three.js functionality
- Pure Gleam implementations where possible

---

## Testing Strategy

1. **Unit Tests**: Test individual collision functions, state machine transitions
2. **Integration Tests**: Test full game loop with collisions and animations
3. **Visual Tests**: Manual testing with debug visualization
4. **Performance Tests**: Benchmark collision detection with many objects

---

## Documentation Deliverables

- [ ] `ANIMATION_STATE_MACHINES.md` - State machine guide
- [ ] `COLLISION_DETECTION.md` - Collision system documentation
- [ ] `DEBUG_TOOLS.md` - Debug visualization guide
- [ ] `ASSET_LOADING.md` - Asset management guide
- [ ] Update main `README.md` with Phase 4 features
- [ ] API documentation for all new modules

---

## Future Considerations (Phase 5+)

After Phase 4, we can consider:
- Physics engine integration (Rapier WASM)
- Spatial audio (3D sound positioning)
- Post-processing effects
- Particle systems
- Advanced lighting (PBR, shadows)
- Networking (multiplayer)

---

## Notes

- Keep the declarative, immutable style
- Follow the diff/patch pattern for efficient updates
- Maintain the MVU architecture
- Ensure all features work with the effect system
- Document everything thoroughly
- Create compelling examples for each feature
