# Performance Guide

Tiramisu is optimized for real-time game performance. This guide covers built-in optimizations and best practices for getting maximum performance from your games.

## Built-In Optimizations

Tiramisu includes several automatic performance optimizations:

### 1. Dirty Flagging (Game Loop Level)

**What it does:** Skips scene diffing entirely when the scene hasn't changed.

**How it works:** The game loop checks if `view()` returns the same list reference as the previous frame using JavaScript's `===` operator. If it does, the diff is skipped completely.

**Benefit:** Infinite speedup for static scenes (0ms diff time instead of 3-4ms for 1000 nodes).

**Use cases:**
- Paused games
- Menu screens
- Static backgrounds
- Loading screens

**Example:**
```gleam
// Cache the scene and return the same reference when paused
pub type Model {
  Model(paused: Bool, cached_scene: Option(List(SceneNode)), todo)
}

fn view(model: Model) -> List(SceneNode) {
  case model.paused {
    True -> {
      // Return cached scene - dirty flagging skips diff!
      case model.cached_scene {
        Some(scene) -> scene
        None -> {
          let scene = build_scene(model)
          // Update model to cache this scene
          scene
        }
      }
    }
    False -> build_scene(model)  // Always rebuild when not paused
  }
}
```

### 2. Scene Diff Optimization (7x Faster)

**What it does:** Optimizes the algorithm that finds differences between frames.

**How it works:**
- Uses `Set` for O(log n) lookups instead of `List` O(n) searches
- Early exit for empty scenes
- Fast-path equality checks for unchanged nodes
- Batch patches by type for renderer efficiency

**Performance:**
- Large scenes (1000 nodes): **41 IPS → 286 IPS** (7x faster)
- Medium scenes (100 nodes): **1992 IPS → 3227 IPS** (1.6x faster)
- No changes: **3332 IPS → 7158 IPS** (2.1x faster)

**Benefit:** Handles large, complex scenes at 60 FPS.

### 3. Instanced Rendering

**What it does:** Renders thousands of identical objects with a single draw call.

**Performance:** 20,000 cubes at 60 FPS with **1 draw call** vs 20,000 draw calls!

**Example:**
```gleam
// Instead of creating 1000 separate Mesh nodes:
let many_cubes = list.range(0, 999)
  |> list.map(fn(i) {
    scene.Mesh(
      id: "cube-" <> int.to_string(i),  // 1000 draw calls!
      geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
      material: scene.StandardMaterial(todo),
      transform: todo,
      physics: option.None,
    )
  })

let instance_transforms = list.range(0, 999)
  |> list.map(fn(i) {
    transform.Transform(
      position: vec3.Vec3(fi *. 2.0, 0.0, 0.0),
      rotation: vec3.Vec3(0.0, 0.0, 0.0),
      scale: vec3.Vec3(1.0, 1.0, 1.0),
    )
  })

scene.InstancedMesh(
  id: "cubes",
  geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
  material: scene.StandardMaterial(todo),
  instances: instance_transforms,  // 1 draw call!
)
```

**When to use:**
- Many identical objects (trees, grass, particles, enemies)
- Static or animated instances
- Need 60 FPS with 1000+ objects

**Limitations:**
- All instances share the same geometry and material
- Can't have individual physics bodies (use spatial partitioning instead)

### 4. Level-of-Detail (LOD) System

**What it does:** Automatically switches meshes based on distance from camera.

**Performance:** 5-10x fewer triangles rendered for distant objects.

**Example:**
```gleam
scene.LOD(
  id: "tree",
  transform: transform.identity,
  levels: [
    // High detail (0-10 units from camera)
    spatial.lod_level(distance: 0.0, node: high_poly_tree),
    // Medium detail (10-30 units)
    spatial.lod_level(distance: 10.0, node: medium_poly_tree),
    // Low detail (30-100 units)
    spatial.lod_level(distance: 30.0, node: low_poly_tree),
    // Billboard (100+ units)
    spatial.lod_level(distance: 100.0, node: billboard_sprite),
  ],
)
```

**Best practices:**
- 3-4 LOD levels is optimal
- Distance thresholds should roughly double (0, 10, 30, 100)
- Lowest LOD can be a billboard sprite
- Triangle counts: High (10k), Medium (2k), Low (500), Billboard (2)

### 5. Frustum Culling

**What it does:** Automatically skips rendering objects outside the camera view.

**Performance:** 2-5x improvement for large scenes with off-screen objects.

**How it works:** Built into Three.js - enabled by default for all meshes.

**No API needed** - it just works! But you can optimize further:
- Keep object bounding boxes tight
- Use LOD for distant objects
- Group related objects with `scene.Group()`

### 6. Spatial Partitioning (Octree)

**What it does:** Efficiently finds nearby objects for game logic.

**Use cases:**
- Find all enemies within radius
- Broad-phase collision detection
- AI perception (what can NPC see?)
- Region-based queries (all objects in this zone)

**Example:**
```gleam
import tiramisu/spatial

// Create octree covering your world
let bounds = spatial.aabb(
  min: vec3.Vec3(-1000.0, -1000.0, -1000.0),
  max: vec3.Vec3(1000.0, 1000.0, 1000.0),
)
let tree = spatial.octree_new(bounds, capacity: 8)

// Insert all entities
let tree = list.fold(model.entities, tree, fn(acc, entity) {
  spatial.octree_insert(acc, entity.position, entity)
})

// Find nearby enemies
let nearby_enemies = spatial.octree_query_radius(
  tree,
  player.position,
  radius: 50.0,
)
```

**Performance:**
- Query 1000 objects in region: **O(log n)** instead of O(n)
- 10-100x faster than checking every object

## Best Practices

### Scene Management

**1. Minimize node count**
```gleam
// ❌ Bad: Too many nodes
let stars = list.range(0, 999)
  |> list.map(fn(i) {
    scene.Mesh(id: "star-" <> int.to_string(i), todo)  // 1000 nodes!
  })

// ✅ Good: Use InstancedMesh
scene.InstancedMesh(
  id: "stars",
  geometry: scene.SphereGeometry(0.1, 8, 8),
  material: scene.BasicMaterial(todo)
  instances: star_transforms,  // 1 node!
)
```

**2. Cache expensive view calculations**
```gleam
// ❌ Bad: Rebuilds scene every frame even when unchanged
fn view(model: Model) -> List(SceneNode) {
  build_entire_ui(model)  // Expensive!
}

// ✅ Good: Cache UI when it hasn't changed
pub type Model {
  Model(
    ui_dirty: Bool,
    cached_ui: List(SceneNode),
    todo
  )
}

fn view(model: Model) -> List(SceneNode) {
  let ui = case model.ui_dirty {
    True -> build_entire_ui(model)
    False -> model.cached_ui  // Dirty flagging wins!
  }
  list.append(build_game_scene(model), ui)
}
```

**3. Use scene groups for hierarchies**
```gleam
// ✅ Good: Group related objects
scene.Group(
  id: "player",
  transform: player_transform,
  children: [
    player_mesh,
    weapon_mesh,
    health_bar,
  ],
)
// Moving the group moves all children - one transform update!
```

### Geometry and Materials

**1. Reuse geometries and materials**
```gleam
// ❌ Bad: Creates new geometry for each box
list.map(positions, fn(pos) {
  scene.Mesh(
    geometry: scene.BoxGeometry(1.0, 1.0, 1.0),  // New geometry each time!
    material: scene.StandardMaterial(todo),
    todo
  )
})

// ✅ Good: Use InstancedMesh (shares geometry/material)
scene.InstancedMesh(
  geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
  material: scene.StandardMaterial(todo),
  instances: transforms,
)
```

**2. Use appropriate geometry detail**
```gleam
// ❌ Bad: Unnecessarily high poly count
scene.SphereGeometry(radius: 1.0, width_segments: 64, height_segments: 64)
// 4096 triangles!

// ✅ Good: Lower poly for small/distant objects
scene.SphereGeometry(radius: 1.0, width_segments: 16, height_segments: 16)
// 512 triangles (8x fewer!)
```

### Lighting

**1. Minimize real-time lights**
```gleam
// ❌ Bad: Too many lights (expensive!)
list.repeat(scene.PointLight(todo), 50)  // 50 lights = slow

// ✅ Good: Use ambient + 1-3 directional/point lights
[
  scene.Light(id: "ambient", light_type: scene.AmbientLight(todo)),
  scene.Light(id: "sun", light_type: scene.DirectionalLight(todo)),
  scene.Light(id: "key", light_type: scene.PointLight(todo)),
]
```

**2. Use baked lighting for static scenes**
- Pre-render lighting into textures
- Use `AmbientLight` + texture maps instead of dynamic lights

### Physics

**1. Use appropriate body types**
```gleam
// ❌ Bad: Everything is Dynamic (expensive!)
physics.rigid_body(physics.Dynamic, collider)

// ✅ Good: Use Fixed for static objects
physics.rigid_body(physics.Fixed, collider)  // Walls, floors, props

// ✅ Good: Use Kinematic for player/NPCs
physics.rigid_body(physics.Kinematic, collider)  // Controlled via code
```

**2. Use simple colliders**
```gleam
// ❌ Bad: Complex collider (many triangles)
physics.CustomGeometry(mesh_geometry)

// ✅ Good: Simple collider (box, sphere, capsule)
physics.Box(width: 1.0, height: 2.0, depth: 1.0)
```

## Profiling

### Chrome DevTools Performance Tab

1. Open DevTools (F12)
2. Go to Performance tab
3. Click Record, play your game for 5 seconds, click Stop
4. Look for:
   - **Long frames** (>16.6ms = dropped frames)
   - **JavaScript execution time** (update/view functions)
   - **Rendering time** (Three.js draw calls)

### Tiramisu Debug Visualizations

```gleam
import tiramisu/debug

// Enable performance monitoring
debug.enable_performance_overlay()

// Shows:
// - FPS (frames per second)
// - Frame time (ms per frame)
// - Draw calls
// - Triangle count
```

### Finding Bottlenecks

**If FPS < 60:**

1. **Check draw calls** (target: <100)
   - Use InstancedMesh for repeated objects
   - Combine meshes with Groups
   - Enable frustum culling

2. **Check triangle count** (target: <100k visible)
   - Use LOD system
   - Reduce geometry segments
   - Use simpler models

3. **Check JavaScript time** (target: <8ms)
   - Cache expensive view calculations
   - Use dirty flagging for static scenes
   - Optimize update logic
   - Use spatial partitioning for queries

4. **Check physics time** (target: <5ms)
   - Use simple colliders
   - Fewer Dynamic bodies
   - Lower physics timestep

## Performance Targets

For 60 FPS games (16.6ms frame budget):

| Task | Budget | Optimization |
|------|--------|--------------|
| Scene diff | 1-3ms | Dirty flagging, minimal nodes |
| Update logic | 2-5ms | Efficient algorithms, spatial queries |
| Physics | 2-5ms | Simple colliders, fewer bodies |
| Rendering | 5-8ms | Instancing, LOD, frustum culling |
| **Total** | **10-21ms** | **Aim for <16ms** |

## Benchmarks

Tiramisu's optimizations (run `gleam dev` to see current benchmarks):

- **Scene diff (1000 nodes):** 286 IPS (3.5ms per diff)
- **Scene diff (no changes):** 7158 IPS (0.14ms per diff)
- **Vec3 operations:** 5-38 million ops/sec
- **Tween updates:** 13 million ops/sec

## Summary

**Quick wins:**
1. Use `InstancedMesh` for repeated objects (1000x faster)
2. Use `LOD` for distant objects (5-10x fewer triangles)
3. Cache static scenes in model state (infinite speedup via dirty flagging)
4. Use spatial partitioning for nearby queries (10-100x faster)
5. Keep draw calls <100 and triangles <100k

**Remember:** Profile first, optimize second. Don't optimize prematurely!
