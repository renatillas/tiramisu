# List Performance Analysis & Optimization Plan

## The Core Problem

Gleam lists are **immutable linked lists**. Every operation is O(n):
- `list.append([1, 2], [3, 4])` - O(n) traversal to copy first list
- `list.flatten([[1], [2], [3]])` - O(n * m) traverse all sub-lists
- `array.from_list([1, 2, 3])` - O(n) conversion for FFI

**In the game loop, this happens every frame (60 FPS)!**

## Current Bottlenecks

### 1. **Scene Diffing** (CRITICAL - Every Frame)

```gleam
// scene.gleam - diff()
list.flatten([removals, parent_change_removals, additions, updates])
// O(4n) - traverses 4 separate lists to build result
```

**Every frame**:
1. Build 4 separate patch lists
2. `list.flatten()` traverses all 4 lists
3. Result passed to JS via FFI (implicit conversion)

### 2. **Batch Patch Application** (CRITICAL - Every Frame)

```gleam
// batch_patches()
list.partition(updates, fn(patch) { ... })  // O(n) traverse
list.partition(other_updates, fn(patch) { ... })  // O(n) traverse
list.partition(remaining_updates, fn(patch) { ... })  // O(n) traverse

list.flatten([
  removals,                // O(n)
  parent_change_removals,  // O(n)
  transform_updates,       // O(n)
  material_updates,        // O(n)
  geometry_updates,        // O(n)
  misc_updates,            // O(n)
  additions,               // O(n)
])  // Total: O(7n) - 7x list traversal!
```

### 3. **Scene Flattening** (Every Frame)

```gleam
// flatten_scene()
list.fold(nodes, acc, fn(acc, node) { ... })  // O(n) for each level
// For nested scenes: O(n * depth)
```

### 4. **Asset Batch Loading** (Startup)

```gleam
// assets.gleam
array.from_list(assets)  // O(n) conversion
// Later:
toList(errors)  // O(n) conversion back
```

## Performance Impact Analysis

**Scene with 1000 nodes, 100 patches:**

```
Without optimizations:
- flatten_scene (prev):    1000 iterations
- flatten_scene (curr):    1000 iterations
- Partition updates (3x):  300 iterations
- Flatten patches (7 lists): 700 iterations
- Total: ~3000 list traversals/frame

At 60 FPS: 180,000 list traversals/second!
```

## Optimization Strategies

### Strategy 1: Use Accumulators Instead of list.append/flatten

**Current (BAD)**:
```gleam
[]
|> list.append(removals)
|> list.append(additions)
|> list.append(updates)
// O(n + m + k) - traverses each list
```

**Optimized (GOOD)**:
```gleam
// Build in reverse with prepend (O(1)), then reverse once at end
let patches = []
let patches = list.append(patches, removals)     // Still bad!

// BETTER: Prepend everything, reverse once
let patches = []
// Prepend (O(1)) instead of append (O(n))
let patches = prepend_all(patches, removals)
let patches = prepend_all(patches, additions)
let patches = prepend_all(patches, updates)
list.reverse(patches)  // Single O(n) reverse at end
```

### Strategy 2: Eliminate Redundant Partitioning

**Current**:
```gleam
list.partition(updates, is_transform)  // O(n) - creates 2 lists
list.partition(other, is_material)     // O(m) - creates 2 lists
list.partition(remaining, is_geometry) // O(k) - creates 2 lists
```

**Optimized**:
```gleam
// Single pass with fold, accumulate into tuple of lists
list.fold(updates, #([], [], [], []), fn(acc, patch) {
  let #(transforms, materials, geometries, misc) = acc
  case patch {
    UpdateTransform(..) -> #([patch, ..transforms], materials, geometries, misc)
    UpdateMaterial(..) -> #(transforms, [patch, ..materials], geometries, misc)
    UpdateGeometry(..) -> #(transforms, materials, [patch, ..geometries], misc)
    _ -> #(transforms, materials, geometries, [patch, ..misc])
  }
})
// O(n) once instead of O(n + m + k)
```

### Strategy 3: Avoid list.flatten Entirely

**Current**:
```gleam
list.flatten([
  removals,
  parent_change_removals,
  transform_updates,
  material_updates,
  geometry_updates,
  misc_updates,
  additions,
])
// O(7n) - traverses 7 lists
```

**Optimized**:
```gleam
// Manual concatenation with prepend (reverse order)
let result = []
let result = prepend_all(result, list.reverse(additions))
let result = prepend_all(result, list.reverse(misc_updates))
let result = prepend_all(result, list.reverse(geometry_updates))
let result = prepend_all(result, list.reverse(material_updates))
let result = prepend_all(result, list.reverse(transform_updates))
let result = prepend_all(result, list.reverse(parent_change_removals))
let result = prepend_all(result, list.reverse(removals))
result
// O(7n) still, but no intermediate allocations

// OR: Return tuple instead of flattened list
#(removals, parent_change_removals, transform_updates, ..., additions)
// Let FFI handle concatenation in O(1) array operations
```

### Strategy 4: FFI-Side List Handling (BEST FOR HOT PATHS)

**Current**:
```javascript
// game.mjs
const patches = diff(currentNodes, newNodes);  // Returns Gleam list
applyPatches(scene, patches);  // Traverses Gleam list in JS
```

**Problem**: Gleam list → JS requires traversal:
```javascript
function gleamListToArray(gleamList) {
  const result = [];
  let current = gleamList;
  while (current && current.head !== undefined) {
    result.push(current.head);
    current = current.tail;
  }
  return result;  // O(n) traversal!
}
```

**Optimized**: Return **patches as tuple of arrays**:

```gleam
// scene.gleam
pub type PatchGroups {
  PatchGroups(
    removals: List(String),  // Just IDs
    updates: List(Patch),
    additions: List(Patch)
  )
}

@internal
pub fn diff_optimized(prev: List(SceneNode), curr: List(SceneNode)) -> PatchGroups {
  // ... diffing logic ...
  PatchGroups(removals: removal_ids, updates: updates, additions: additions)
}
```

```javascript
// game.mjs
const patchGroups = diff_optimized(currentNodes, newNodes);
// PatchGroups is a JS object: { removals: GleamList, updates: GleamList, additions: GleamList }

// Process each group separately (better cache locality)
applyRemovals(scene, patchGroups.removals);
applyUpdates(scene, patchGroups.updates);
applyAdditions(scene, patchGroups.additions);
```

### Strategy 5: Use dict.fold Instead of list Operations

**Current**:
```gleam
let prev_ids = dict.keys(prev_dict)  // O(n) - creates list
let curr_ids = dict.keys(curr_dict)  // O(n) - creates list
let prev_id_set = set.from_list(prev_ids)  // O(n log n)
let curr_id_set = set.from_list(curr_ids)  // O(n log n)
```

**Optimized**:
```gleam
// Work directly with dict - no intermediate lists
dict.fold(curr_dict, #([], [], []), fn(acc, id, node) {
  let #(removals, updates, additions) = acc
  case dict.get(prev_dict, id) {
    Ok(prev_node) -> {
      // Node exists in both - check for updates
      let patches = compare_nodes(id, prev_node, node)
      #(removals, list.append(updates, patches), additions)
    }
    Error(_) -> {
      // New node - addition
      #(removals, updates, [AddNode(id, node), ..additions])
    }
  }
})
```

## Implementation Priority

### Phase 1: Low-Hanging Fruit (30 min)
1. ✅ **Replace list.flatten with manual prepend** in `batch_patches`
2. ✅ **Single-pass partitioning** in `batch_patches`
3. ✅ **Remove redundant list.append** in compare_nodes (already done with accumulator)

**Expected**: 20-30% improvement

### Phase 2: FFI Optimization (1 hour)
1. **Return PatchGroups tuple** instead of flattened list
2. **Update FFI to handle grouped patches**
3. **Benchmark with grouped patches**

**Expected**: 30-40% improvement

### Phase 3: Dict-First Approach (2 hours)
1. **Rewrite diff to use dict.fold** instead of list operations
2. **Eliminate intermediate list creations**
3. **Work with dicts until final patch generation**

**Expected**: 50-100% improvement (2x faster)

## Benchmark Targets

**Current** (Phase 1 complete):
- Large (1000 nodes): 266 IPS

**Phase 1 Target** (better list operations):
- Large (1000 nodes): **350 IPS** (+30%)

**Phase 2 Target** (FFI optimization):
- Large (1000 nodes): **450 IPS** (+70%)

**Phase 3 Target** (dict-first):
- Large (1000 nodes): **500+ IPS** (2x original)

## Next Steps

1. Start with Phase 1 (replace list.flatten, single-pass partition)
2. Benchmark after each change
3. Move to Phase 2 if gains are significant
4. Consider Phase 3 only if Phase 2 shows promise

**Key Principle**: Minimize list traversals, work with dicts/sets longer, flatten only at FFI boundary.
