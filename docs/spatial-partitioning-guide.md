# Spatial Partitioning Guide

Spatial partitioning is a technique for efficiently finding objects in 3D space. Instead of checking every object (O(n)), we use a data structure that narrows down the search to nearby candidates (O(log n)).

## Why Use Spatial Partitioning?

### The Problem

Imagine you have 1000 enemies in your game. You want to find all enemies within 10 units of the player:

```gleam
// ❌ Naive approach: Check every enemy (slow!)
let nearby_enemies = list.filter(model.enemies, fn(enemy) {
  vec3f.distance(player.position, enemy.position) <=. 10.0
})
// Performance: O(n) = 1000 distance calculations
```

This works for small numbers, but with 1000+ objects it becomes a bottleneck.

### The Solution: Octree

An **octree** divides 3D space into regions (octants). Objects are stored based on their position:

```gleam
// ✅ Optimized: Use octree (fast!)
let nearby_enemies = spatial.octree_query_radius(
  model.enemy_tree,
  player.position,
  10.0,
)
// Performance: O(log n) = ~10 checks (100x faster!)
```

## Octree Basics

### What is an Octree?

An octree recursively divides 3D space into 8 octants (like a 3D binary tree):

```
         +--------+
        /|       /|
       / |      / |
      +--------+  |     Top 4 octants (y+)
      |  +-----|--+     - Top NW, NE, SW, SE
      | /      | /
      |/       |/       Bottom 4 octants (y-)
      +--------+        - Bottom NW, NE, SW, SE
```

When an octant exceeds its capacity (e.g., 8 items), it subdivides into 8 smaller octants.

### AABB (Axis-Aligned Bounding Box)

AABBs define rectangular regions in 3D space:

```gleam
import tiramisu/spatial
import vec/vec3

// Create AABB from min/max points
let bounds = spatial.aabb(
  min: vec3.Vec3(-100.0, -100.0, -100.0),
  max: vec3.Vec3(100.0, 100.0, 100.0),
)

// Create AABB from center and half-extents
let bounds = spatial.aabb_from_center(
  center: vec3.Vec3(0.0, 0.0, 0.0),
  half_extents: vec3.Vec3(50.0, 50.0, 50.0),
)

// Check if point is inside
spatial.aabb_contains_point(bounds, vec3.Vec3(10.0, 20.0, 30.0))
// -> True

// Check if two AABBs intersect
spatial.aabb_intersects(aabb1, aabb2)
// -> True/False
```

## Creating an Octree

### Step 1: Define World Bounds

```gleam
import tiramisu/spatial

// Define the region your octree covers
let world_bounds = spatial.aabb(
  min: vec3.Vec3(-1000.0, -100.0, -1000.0),  // Bottom-left-back corner
  max: vec3.Vec3(1000.0, 100.0, 1000.0),     // Top-right-front corner
)

// Create octree with capacity of 8 items per node
let tree = spatial.octree_new(world_bounds, capacity: 8)
```

**Capacity** determines when subdivision occurs:
- **Low capacity (4-8)**: More subdivisions, faster queries, more memory
- **High capacity (16-32)**: Fewer subdivisions, slower queries, less memory
- **Recommended**: 8 for most games

### Step 2: Insert Objects

```gleam
// Insert individual object
let tree = spatial.octree_insert(
  tree,
  position: vec3.Vec3(10.0, 0.0, 5.0),
  item: enemy,
)

// Insert many objects with fold
let tree = list.fold(model.enemies, tree, fn(acc_tree, enemy) {
  spatial.octree_insert(acc_tree, enemy.position, enemy)
})
```

**Important:** Objects outside the octree bounds are ignored (not inserted).

### Step 3: Query Objects

```gleam
// Query by bounding box region
let query_bounds = spatial.aabb(
  min: vec3.Vec3(-10.0, -10.0, -10.0),
  max: vec3.Vec3(10.0, 10.0, 10.0),
)
let objects_in_region = spatial.octree_query(tree, query_bounds)
// Returns: List(#(Vec3(Float), a))

// Query by radius (sphere)
let nearby = spatial.octree_query_radius(
  tree,
  center: vec3.Vec3(0.0, 0.0, 0.0),
  radius: 50.0,
)

// Query all objects (for iteration)
let all_objects = spatial.octree_query_all(tree)
```

## Use Cases

### 1. Find Nearby Enemies

```gleam
pub type Enemy {
  Enemy(id: Int, position: vec3.Vec3(Float), health: Float)
}

pub type Model {
  Model(
    player: Player,
    enemies: List(Enemy),
    enemy_tree: spatial.Octree(Enemy),
    todo
  )
}

// In init or update, rebuild octree
fn rebuild_enemy_tree(enemies: List(Enemy)) -> spatial.Octree(Enemy) {
  let bounds = spatial.aabb(
    min: vec3.Vec3(-500.0, 0.0, -500.0),
    max: vec3.Vec3(500.0, 100.0, 500.0),
  )
  let tree = spatial.octree_new(bounds, 8)

  list.fold(enemies, tree, fn(acc, enemy) {
    spatial.octree_insert(acc, enemy.position, enemy)
  })
}

// Find enemies in attack range
fn find_targets(model: Model, attack_range: Float) -> List(Enemy) {
  spatial.octree_query_radius(
    model.enemy_tree,
    model.player.position,
    attack_range,
  )
  |> list.map(fn(result) {
    let #(_position, enemy) = result
    enemy
  })
}
```

### 2. Collision Detection (Broad Phase)

```gleam
pub type Collidable {
  Collidable(id: String, position: vec3.Vec3(Float), radius: Float)
}

// Find potential collisions
fn find_collision_candidates(
  tree: spatial.Octree(Collidable),
  object: Collidable,
) -> List(Collidable) {
  // Query objects within object's radius + safety margin
  spatial.octree_query_radius(
    tree,
    object.position,
    object.radius +. 2.0,  // Safety margin
  )
  |> list.map(fn(result) { result.1 })  // Extract Collidable
  |> list.filter(fn(other) { other.id != object.id })  // Exclude self
}

// Then do narrow-phase collision detection on candidates
fn check_collision(a: Collidable, b: Collidable) -> Bool {
  let distance = vec3f.distance(a.position, b.position)
  distance <=. a.radius +. b.radius
}
```

### 3. AI Perception

```gleam
// NPC can "see" objects within view distance
fn get_visible_objects(
  npc: NPC,
  tree: spatial.Octree(GameObject),
  view_distance: Float,
) -> List(GameObject) {
  spatial.octree_query_radius(tree, npc.position, view_distance)
  |> list.map(fn(result) { result.1 })
  |> list.filter(fn(obj) {
    // Additional checks: line of sight, field of view, etc.
    is_in_view_cone(npc, obj) && has_line_of_sight(npc, obj)
  })
}
```

### 4. Region-Based Queries

```gleam
// Find all objects in a zone (trigger, etc.)
let zone_bounds = spatial.aabb(
  min: vec3.Vec3(-10.0, 0.0, -10.0),
  max: vec3.Vec3(10.0, 5.0, 10.0),
)

let objects_in_zone = spatial.octree_query(tree, zone_bounds)
```

### 5. Click Selection (Raycasting)

```gleam
// Find objects near where player clicked
fn get_clickable_objects(
  tree: spatial.Octree(GameObject),
  click_ray_origin: vec3.Vec3(Float),
  max_distance: Float,
) -> List(GameObject) {
  // Create bounding box around ray
  let query_bounds = spatial.aabb(
    min: vec3.Vec3(
      click_ray_origin.x -. 1.0,
      click_ray_origin.y -. 1.0,
      click_ray_origin.z -. 1.0,
    ),
    max: vec3.Vec3(
      click_ray_origin.x +. 1.0 +. max_distance,
      click_ray_origin.y +. 1.0,
      click_ray_origin.z +. 1.0 +. max_distance,
    ),
  )

  spatial.octree_query(tree, query_bounds)
  |> list.map(fn(result) { result.1 })
  // Then do accurate ray-object intersection test
}
```

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Example (1000 objects) |
|-----------|------------|------------------------|
| Insert | O(log n) | ~10 comparisons |
| Query (radius) | O(log n + k) | ~10 + k results |
| Query (all) | O(n) | 1000 items |
| Build tree | O(n log n) | ~10,000 operations |

**k** = number of results returned

### Space Complexity

- **Memory usage**: O(n) where n = number of objects
- **Tree nodes**: Depends on object distribution
  - Uniform distribution: fewer nodes
  - Clustered distribution: more nodes (more subdivisions)

### When to Rebuild

Octrees are **immutable** in Tiramisu. Rebuild when:
- Objects move significantly
- Objects are added/removed
- Every frame (for dynamic scenes)
- Every few frames (for mostly-static scenes)

```gleam
pub type Model {
  Model(
    objects: List(GameObject),
    spatial_tree: spatial.Octree(GameObject),
    frames_since_rebuild: Int,
    todo
  )
}

fn update(model: Model, msg: Msg, ctx: Context) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      // Rebuild every 5 frames (optimization for mostly-static scenes)
      let should_rebuild = model.frames_since_rebuild >= 5

      let #(tree, frames) = case should_rebuild {
        True -> #(rebuild_tree(model.objects), 0)
        False -> #(model.spatial_tree, model.frames_since_rebuild + 1)
      }

      #(
        Model(..model, spatial_tree: tree, frames_since_rebuild: frames),
        effect.tick(Tick),
      )
    }
  }
}
```

## Best Practices

### 1. Choose Appropriate Bounds

```gleam
// ❌ Bad: Bounds too large (wasted space)
let bounds = spatial.aabb(
  min: vec3.Vec3(-10000.0, -10000.0, -10000.0),
  max: vec3.Vec3(10000.0, 10000.0, 10000.0),
)

// ✅ Good: Bounds match your actual world
let bounds = spatial.aabb(
  min: vec3.Vec3(-500.0, 0.0, -500.0),   // Ground level minimum
  max: vec3.Vec3(500.0, 100.0, 500.0),   // Max building height
)
```

### 2. Choose Appropriate Capacity

```gleam
// ❌ Bad: Capacity too low (excessive subdivisions)
spatial.octree_new(bounds, 1)

// ❌ Bad: Capacity too high (slow queries)
spatial.octree_new(bounds, 100)

// ✅ Good: Balanced capacity
spatial.octree_new(bounds, 8)  // For most games
spatial.octree_new(bounds, 16) // For very large scenes
```

### 3. Batch Insertions

```gleam
// ❌ Bad: Insert one at a time in loop
let tree = spatial.octree_new(bounds, 8)
let tree = spatial.octree_insert(tree, pos1, item1)
let tree = spatial.octree_insert(tree, pos2, item2)
// todo

// ✅ Good: Use fold for batch insert
let tree = list.fold(items, spatial.octree_new(bounds, 8), fn(acc, item) {
  spatial.octree_insert(acc, item.position, item)
})
```

### 4. Reuse Queries

```gleam
// ❌ Bad: Query multiple times
let nearby1 = spatial.octree_query_radius(tree, pos, 10.0)
let nearby2 = spatial.octree_query_radius(tree, pos, 10.0)  // Duplicate!

// ✅ Good: Query once, reuse result
let nearby = spatial.octree_query_radius(tree, pos, 10.0)
// Use 'nearby' for multiple purposes
```

### 5. Combine with Other Techniques

```gleam
// Use octree for broad-phase, then accurate checks
let candidates = spatial.octree_query_radius(tree, player.pos, 50.0)

let actual_collisions = list.filter(candidates, fn(candidate) {
  let #(_, obj) = candidate
  // Narrow-phase: accurate collision detection
  check_detailed_collision(player, obj)
})
```

## Comparison: Octree vs Other Methods

### Naive Linear Search

```gleam
// O(n) - Check every object
list.filter(objects, fn(obj) {
  vec3f.distance(player.position, obj.position) <=. radius
})
```

**Pros:** Simple, no memory overhead
**Cons:** Slow for >100 objects
**Use case:** Very small scenes (<50 objects)

### Grid (Spatial Hash)

```gleam
// Divide space into uniform grid cells
// O(1) insertion, O(1) query (on average)
```

**Pros:** Faster than octree for uniform distribution
**Cons:** Bad for clustered objects, wastes memory
**Use case:** Objects evenly distributed, 2D games

### Octree

```gleam
// Hierarchical subdivision
// O(log n) insertion, O(log n + k) query
```

**Pros:** Good for any distribution, 3D-friendly
**Cons:** More complex, rebuilding cost
**Use case:** 3D games, non-uniform distribution

### When to Use What?

| Objects | Distribution | Recommended |
|---------|--------------|-------------|
| <50 | Any | Naive search |
| 50-500 | Uniform | Grid or octree |
| 500-5000 | Clustered | Octree |
| 5000+ | Any | Octree + LOD |

## Example: Complete Enemy System

```gleam
import gleam/list
import tiramisu/spatial
import vec/vec3

pub type Enemy {
  Enemy(id: Int, position: vec3.Vec3(Float), health: Float, speed: Float)
}

pub type Model {
  Model(
    player: vec3.Vec3(Float),
    enemies: List(Enemy),
    enemy_tree: spatial.Octree(Enemy),
  )
}

// Rebuild octree when enemies move
fn rebuild_enemy_tree(enemies: List(Enemy)) -> spatial.Octree(Enemy) {
  let world_bounds = spatial.aabb(
    min: vec3.Vec3(-1000.0, 0.0, -1000.0),
    max: vec3.Vec3(1000.0, 100.0, 1000.0),
  )

  list.fold(enemies, spatial.octree_new(world_bounds, 8), fn(tree, enemy) {
    spatial.octree_insert(tree, enemy.position, enemy)
  })
}

// Find enemies to attack
fn find_attack_targets(model: Model, range: Float) -> List(Enemy) {
  spatial.octree_query_radius(model.enemy_tree, model.player, range)
  |> list.map(fn(result) { result.1 })
}

// Find enemies for AI awareness
fn enemies_in_awareness_zone(
  model: Model,
  npc_position: vec3.Vec3(Float),
  awareness_radius: Float,
) -> List(Enemy) {
  spatial.octree_query_radius(model.enemy_tree, npc_position, awareness_radius)
  |> list.map(fn(result) { result.1 })
  |> list.filter(fn(enemy) { enemy.health >. 0.0 })  // Only living enemies
}
```

## Summary

**Key concepts:**
- Octrees divide 3D space hierarchically for fast spatial queries
- AABBs define rectangular regions for collision and containment checks
- Use `octree_query_radius()` for "nearby object" queries
- Use `octree_query()` for region-based queries
- Rebuild octrees when objects move (every frame or few frames)
- Choose capacity = 8-16 for most games

**Performance:**
- O(log n) insert and query (vs O(n) naive search)
- 10-100x faster for 1000+ objects
- Essential for large, dynamic scenes

**Next steps:**
- [Performance Guide](./performance-guide.md) - Full optimization strategies
- [Scene Graph Guide](./scene-graph-guide.md) - Understanding the scene structure
- [Physics Guide](./physics-guide.md) - Collision detection and response
