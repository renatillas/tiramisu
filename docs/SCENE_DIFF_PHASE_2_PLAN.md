# Scene Diff Optimization - Phase 2 Plan

## Phase 1 Summary

**Achievement**: **7x performance improvement** for large scenes (41 → 286 IPS)
**Key Learning**: Algorithmic improvements (set-based lookups) >> Micro-optimizations

## Phase 2 Goals

Focus on **skipping work entirely** rather than doing work faster:

1. **Dirty flagging** - Don't diff scenes that haven't changed
2. **Structural hashing** - O(1) change detection instead of O(n) comparison
3. **Real-world optimization** - Optimize for actual game patterns

**Target**: **10-100x improvement** for static/mostly-static scenes

---

## Strategy 1: Scene-Level Dirty Flagging

### Concept

Track whether a scene has been modified since last diff. If clean, skip diff entirely.

```gleam
pub opaque type Scene {
  Scene(nodes: List(SceneNode), version: Int, dirty: Bool)
}

pub fn new_scene(nodes: List(SceneNode)) -> Scene {
  Scene(nodes: nodes, version: 0, dirty: True)
}

pub fn mark_dirty(scene: Scene) -> Scene {
  Scene(..scene, version: scene.version + 1, dirty: True)
}

pub fn mark_clean(scene: Scene) -> Scene {
  Scene(..scene, dirty: False)
}

pub fn diff_scenes(prev: Scene, curr: Scene) -> #(Scene, List(Patch)) {
  case prev.version == curr.version && !curr.dirty {
    True -> #(Scene(..curr, dirty: False), [])  // No changes!
    False -> {
      let patches = diff(prev.nodes, curr.nodes)
      #(Scene(..curr, dirty: False), patches)
    }
  }
}
```

### Benefits

- **Static scenes**: Skip diff entirely (**infinite speedup**)
- **Unchanged frames**: Early exit with version check (O(1) vs O(n))
- **Simple API**: User calls `mark_dirty()` when scene changes

### Challenges

- **API change**: Requires wrapping scene list in opaque type
- **User burden**: Users must remember to mark dirty
- **False negatives**: Forgetting mark_dirty = visual bugs

### Decision

⚠️ **Skip for now** - Requires significant API change. Better as Phase 3 after validation.

---

## Strategy 2: Structural Hashing

### Concept

Hash each node's visual properties. Compare hashes before doing full comparison.

```gleam
pub opaque type SceneNode {
  // ... existing node types
  // Internal: cache hash for quick comparison
}

// Hash a node's visual properties
fn hash_node(node: SceneNode) -> Int {
  case node {
    Mesh(id, geom, mat, trans, phys) ->
      hash_combine([
        hash_string(id),
        hash_geometry(geom),
        hash_material(mat),
        hash_transform(trans),
        hash_option_physics(phys),
      ])
    // ... other node types
  }
}

// Compare using hashes first
fn compare_nodes_with_hash(id: String, prev: SceneNode, curr: SceneNode) -> List(Patch) {
  let prev_hash = hash_node(prev)
  let curr_hash = hash_node(curr)

  case prev_hash == curr_hash {
    True -> []  // Likely unchanged (handle collisions)
    False -> compare_nodes_detailed(id, prev, curr)  // Definitely changed
  }
}
```

### Benefits

- **O(1) comparison**: Hash check vs full node comparison
- **No API change**: Internal optimization
- **Works with existing code**: Drop-in replacement

### Challenges

- **Hash collisions**: Must handle false positives (rare)
- **Hash computation cost**: Must be cheaper than comparison
- **Cache invalidation**: When to recompute hashes?

### Hash Function Design

Need fast, collision-resistant hash for Gleam types:

```gleam
// Simple FNV-1a hash
fn hash_combine(hashes: List(Int)) -> Int {
  list.fold(hashes, 2166136261, fn(hash, h) {
    // XOR then multiply by prime
    let hash = int.bitwise_exclusive_or(hash, h)
    int.bitwise_and(hash * 16777619, 0xFFFFFFFF)
  })
}

fn hash_string(s: String) -> Int {
  // Gleam stdlib doesn't have string hashing
  // Use JS FFI: s.split('').reduce((h, c) => h * 31 + c.charCodeAt(0), 0)
  hash_string_ffi(s)
}

fn hash_float(f: Float) -> Int {
  // Convert to bits for hashing
  float.to_string(f) |> hash_string
}

fn hash_transform(t: transform.Transform) -> Int {
  hash_combine([
    hash_vec3(t.position),
    hash_vec3(t.rotation),
    hash_vec3(t.scale),
  ])
}
```

### Implementation Plan

1. Add FFI hash functions for primitives (string, float)
2. Implement hash_node for each SceneNode variant
3. Add hash-based fast path in compare_nodes
4. Benchmark: verify hashing is faster than comparison

### Expected Impact

- **Mostly unchanged scenes**: 2-3x improvement
- **Fully unchanged scenes**: Already handled by prev == curr
- **Changed scenes**: Small overhead from hashing

---

## Strategy 3: Partial Scene Updates (Real-World Pattern)

### Concept

Games typically update **only a few nodes** per frame (character position, animations).
Optimize for this common case.

```gleam
// Track which nodes changed since last frame
pub opaque type Scene {
  Scene(nodes: List(SceneNode), changed_ids: Set(String))
}

pub fn update_node(scene: Scene, id: String, node: SceneNode) -> Scene {
  let new_nodes = list.map(scene.nodes, fn(n) {
    case n.id == id {
      True -> node
      False -> n
    }
  })
  Scene(nodes: new_nodes, changed_ids: set.insert(scene.changed_ids, id))
}

pub fn diff_partial(prev: Scene, curr: Scene) -> List(Patch) {
  // Only compare nodes in changed_ids
  list.filter_map(set.to_list(curr.changed_ids), fn(id) {
    case get_node(prev.nodes, id), get_node(curr.nodes, id) {
      Some(prev_node), Some(curr_node) ->
        Ok(compare_nodes(id, prev_node, curr_node))
      _, _ -> Error(Nil)
    }
  })
  |> list.flatten
}
```

### Benefits

- **Animation frames**: Only diff animated nodes
- **Partial updates**: Skip unchanged subtrees
- **Real-world optimization**: Matches actual usage patterns

### Challenges

- **API change**: Users must track changes
- **Complexity**: More API surface
- **Hierarchy changes**: What if parent changes affect children?

### Decision

⚠️ **Not for Phase 2** - Requires API redesign. Better as separate "incremental diff" API.

---

## Phase 2 Implementation Plan

### Focus: Structural Hashing (Most Practical)

**Goal**: Add hash-based fast path for node comparison

### Step 1: Add Hash FFI Functions

Create `/src/tiramisu/ffi/hash.mjs`:

```javascript
// FNV-1a hash for strings
export function hashString(str) {
  let hash = 2166136261;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0; // Convert to unsigned 32-bit
}

// Hash float by converting to string
export function hashFloat(f) {
  return hashString(f.toString());
}

// Combine multiple hashes
export function combineHashes(hashes) {
  let hash = 2166136261;
  for (const h of hashes) {
    hash ^= h;
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0;
}
```

### Step 2: Add Gleam Hash Module

Create `/src/tiramisu/internal/hash.gleam`:

```gleam
import gleam/javascript/array

@external(javascript, "../ffi/hash.mjs", "hashString")
pub fn hash_string(s: String) -> Int

@external(javascript, "../ffi/hash.mjs", "hashFloat")
pub fn hash_float(f: Float) -> Int

@external(javascript, "../ffi/hash.mjs", "combineHashes")
fn combine_hashes_ffi(hashes: array.Array(Int)) -> Int

pub fn combine_hashes(hashes: List(Int)) -> Int {
  array.from_list(hashes)
  |> combine_hashes_ffi
}

pub fn hash_bool(b: Bool) -> Int {
  case b {
    True -> 1
    False -> 0
  }
}

pub fn hash_int(i: Int) -> Int {
  i
}
```

### Step 3: Add Hash Functions in scene.gleam

```gleam
import tiramisu/internal/hash

fn hash_vec3(v: Vec3) -> Int {
  hash.combine_hashes([
    hash.hash_float(v.x),
    hash.hash_float(v.y),
    hash.hash_float(v.z),
  ])
}

fn hash_transform(t: transform.Transform) -> Int {
  hash.combine_hashes([
    hash_vec3(t.position),
    hash_vec3(t.rotation),
    hash_vec3(t.scale),
  ])
}

fn hash_geometry(geom: GeometryType) -> Int {
  case geom {
    BoxGeometry(w, h, d) ->
      hash.combine_hashes([
        1, // variant discriminator
        hash.hash_float(w),
        hash.hash_float(h),
        hash.hash_float(d),
      ])
    SphereGeometry(r, ws, hs) ->
      hash.combine_hashes([
        2,
        hash.hash_float(r),
        hash.hash_int(ws),
        hash.hash_int(hs),
      ])
    // ... other geometry types
    _ -> 0  // Fallback for complex types
  }
}

fn hash_material(mat: MaterialType) -> Int {
  case mat {
    BasicMaterial(color, transparent, opacity, _) ->
      hash.combine_hashes([
        1,
        hash.hash_int(color),
        hash.hash_bool(transparent),
        hash.hash_float(opacity),
        // Skip texture map (opaque type, expensive to hash)
      ])
    // ... other material types
    _ -> 0
  }
}

fn hash_node(node: SceneNode) -> Int {
  case node {
    Mesh(id, geom, mat, trans, _) ->
      hash.combine_hashes([
        hash.hash_string(id),
        hash_geometry(geom),
        hash_material(mat),
        hash_transform(trans),
        // Skip physics (optional, complex)
      ])
    Light(id, light, trans) ->
      hash.combine_hashes([
        hash.hash_string(id),
        hash_transform(trans),
        // Simplified: skip light type details
      ])
    Group(id, trans, _) ->
      hash.combine_hashes([
        hash.hash_string(id),
        hash_transform(trans),
        // Children handled by diff algorithm
      ])
    // ... other node types
    _ -> hash.hash_string(node.id)
  }
}
```

### Step 4: Add Hash-Based Fast Path

Update `compare_nodes_detailed`:

```gleam
fn compare_nodes_detailed(
  id: String,
  prev: SceneNode,
  curr: SceneNode,
) -> List(Patch) {
  // Hash-based fast path
  let prev_hash = hash_node(prev)
  let curr_hash = hash_node(curr)

  case prev_hash == curr_hash {
    True -> []  // Likely unchanged (99.99% accurate)
    False -> {
      // Definitely changed - do detailed comparison
      case prev, curr {
        Mesh(..), Mesh(..) -> compare_mesh_fields(...)
        Light(..), Light(..) -> compare_light_fields(...)
        // ... rest of comparison
      }
    }
  }
}
```

### Step 5: Benchmark New Scenarios

Add to `/dev/tiramisu_dev.gleam`:

```gleam
// Animation frame: only transforms change (common pattern)
bench.Input("Animation (100 nodes, 10 transforms)", {
  let prev = create_flat_scene(100)
  let curr = update_transforms(prev, 10) // Update 10 node transforms
  ScenePair(prev, curr)
})

// Partial update: 1 node material changes
bench.Input("Partial (100 nodes, 1 material)", {
  let prev = create_flat_scene(100)
  let curr = update_material(prev, 0)  // Update 1 material
  ScenePair(prev, curr)
})
```

---

## Expected Results

### Before Hashing (Current)

- Large (1000 nodes): 277 IPS
- No changes (100 nodes): 7,611 IPS
- All changed (100 nodes): 2,299 IPS

### After Hashing (Target)

- Large (1000 nodes): **500+ IPS** (1.8x improvement)
- No changes (100 nodes): **20,000+ IPS** (2.6x improvement)
- All changed (100 nodes): **2,000 IPS** (small overhead acceptable)
- **NEW** Animation (100 nodes, 10 changes): **15,000+ IPS**
- **NEW** Partial (100 nodes, 1 change): **50,000+ IPS**

### Key Metrics

- **Hash computation cost**: Must be < 50% of comparison cost
- **Collision rate**: < 0.01% (practically zero)
- **Real-world impact**: 2-3x for typical game patterns

---

## Implementation Checklist

### Phase 2A: Foundation
- [ ] Create `/src/tiramisu/ffi/hash.mjs` with hash functions
- [ ] Create `/src/tiramisu/internal/hash.gleam` module
- [ ] Add hash functions for primitives (string, float, bool, int)
- [ ] Test hash functions (consistency, distribution)

### Phase 2B: Node Hashing
- [ ] Implement `hash_vec3`, `hash_transform`
- [ ] Implement `hash_geometry` for all geometry types
- [ ] Implement `hash_material` for all material types
- [ ] Implement `hash_node` for all node types
- [ ] Test: verify same node produces same hash

### Phase 2C: Integration
- [ ] Add hash-based fast path in `compare_nodes_detailed`
- [ ] Update tests to ensure correctness
- [ ] Verify no regressions in existing benchmarks

### Phase 2D: New Benchmarks
- [ ] Add "Animation frame" benchmark (10% nodes change transforms)
- [ ] Add "Partial update" benchmark (1% nodes change materials)
- [ ] Add "Static scene" benchmark (100 frames, no changes)

### Phase 2E: Validation
- [ ] Run all benchmarks, verify improvements
- [ ] Profile hash computation cost
- [ ] Test for hash collisions (run 1M+ hashes)
- [ ] Document results in optimization plan

---

## Success Criteria

✅ **Must Have**:
- Hash-based fast path working correctly
- No regressions in existing benchmarks
- All tests passing

✅ **Good to Have**:
- 2x improvement for mostly-unchanged scenes
- <5% overhead for fully-changed scenes

✅ **Excellent**:
- 3x+ improvement for typical game patterns
- Real-world game scenarios validated

---

## Rollback Plan

If hashing adds overhead without sufficient benefit:

1. Keep Phase 1 optimizations (proven 7x improvement)
2. Revert hash-based comparison
3. Document findings: when hashing helps vs hurts
4. Consider alternative: dirty flagging (Phase 3)

---

## Timeline

- **Phase 2A-B** (Foundation + Node Hashing): 1-2 hours
- **Phase 2C-D** (Integration + Benchmarks): 1 hour
- **Phase 2E** (Validation): 30 minutes

**Total**: 2-3 hours for Phase 2 implementation + testing

---

## Phase 2 Implementation Results

### What We Built

Implemented comprehensive structural hashing system:
- ✅ FNV-1a hash functions in `/src/tiramisu/ffi/hash.mjs`
- ✅ Gleam hash module in `/src/tiramisu/internal/hash.gleam`
- ✅ Hash functions for all node types (Mesh, Light, Group, Model3D, Debug nodes)
- ✅ Hash-based fast path in `compare_nodes_detailed`

### Benchmark Results

**Before Hashing (Phase 1)**:
- Large (1000 nodes): 277 IPS
- All changed (100 nodes): **2,299 IPS**
- No changes (100 nodes): 7,611 IPS

**With Hashing (Phase 2)**:
- Large (1000 nodes): 274 IPS (no change)
- All changed (100 nodes): **1,133 IPS** (⚠️ **50% slower!**)
- No changes (100 nodes): 7,542 IPS (no change)

### Analysis: Why Hashing Failed

**Hash Computation Overhead > Comparison Savings**

In game loops, most nodes change every frame:
- Character animations (transform updates)
- Camera movement (transform updates)
- Dynamic objects (position/rotation updates)

For changing nodes, we pay **double cost**:
1. Hash computation (prev + curr)
2. Full comparison (when hashes differ)

**The Problem**:
```gleam
// With hashing
let prev_hash = hash_node(prev)  // Cost: ~50 operations
let curr_hash = hash_node(curr)  // Cost: ~50 operations
case prev_hash == curr_hash {    // Cost: 1 comparison
  True -> []
  False -> compare_fields(...)   // Cost: ~20 operations
}
// Total for changed node: 121 operations

// Without hashing
compare_fields(...)  // Cost: ~20 operations
// Total for changed node: 20 operations
```

**Hashing is 6x slower for the common case (nodes that change)!**

### Key Learnings

1. **Optimize for the common case**
   - Game loops: nodes change every frame (animations, movement)
   - Static scenes are rare in games
   - Hashing helps caching, not rendering

2. **Beware of double work**
   - Hash computation + comparison = wasted effort when nodes differ
   - Fast-path must be *truly* fast (near-zero cost)

3. **Structural equality is expensive in Gleam**
   - `prev == curr` compares entire data structures
   - Not a simple pointer comparison (like Rust/C++)
   - Gleam's immutability makes equality checks non-trivial

4. **Profile real workloads**
   - "No changes" benchmark is misleading
   - Real games have mostly changing nodes
   - Static scenes are edge cases

### When Would Hashing Help?

**Caching scenarios**:
- UI elements that rarely change
- Static environment meshes (buildings, terrain)
- Cached render layers

**But**: Those scenarios benefit more from:
- Don't diff at all (separate static/dynamic layers)
- Dirty flagging (mark subtrees as unchanged)
- Spatial culling (skip off-screen objects)

### Decision: Revert Phase 2

**Reverted**:
- ❌ Removed `/src/tiramisu/internal/hash.gleam`
- ❌ Removed `/src/tiramisu/ffi/hash.mjs`
- ❌ Removed hash-based fast path from scene.gleam

**Kept**:
- ✅ Phase 1 optimizations (7x improvement)
- ✅ Set-based lookups
- ✅ Patch batching
- ✅ Pre-computed hierarchy depths

**Final Performance** (reverted to Phase 1):
- Large (1000 nodes): **266 IPS** (7x from original 41 IPS)
- All changed (100 nodes): **2,256 IPS** (restored from 1,133 IPS)
- No changes (100 nodes): **7,548 IPS**

### Conclusion

Phase 2 taught us valuable lessons about optimization:

1. ✅ **Algorithmic improvements win**: Set-based lookups (O(log n) vs O(n)) = 7x gain
2. ❌ **Micro-optimizations can backfire**: Hashing added 50% overhead
3. ✅ **Measure real workloads**: "All changed" is more realistic than "no changes"
4. ✅ **Simple is fast**: Direct comparison beats hash + comparison

**Final Recommendation**: Stop here. **7x improvement achieved**. Further optimization should focus on higher-level strategies (dirty flagging, spatial culling, render batching) rather than diff algorithm internals.
