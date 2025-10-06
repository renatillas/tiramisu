# Level of Detail (LOD) Demo

Demonstrates **automatic Level of Detail (LOD) switching** based on camera distance.

## What It Does

- Renders **11 objects** at different distances (150 units apart)
- Each object automatically switches between 4 detail levels:
  - **High detail** (0-20 units): Icosahedron with 3 subdivisions (2562 triangles)
  - **Medium detail** (20-50 units): Icosahedron with 2 subdivisions (642 triangles)
  - **Low detail** (50-100 units): Icosahedron with 1 subdivision (162 triangles)
  - **Billboard** (100+ units): Simple plane (2 triangles)
- Real-time FPS and triangle count monitoring

## What is Level of Detail (LOD)?

LOD is a rendering optimization that:
1. Switches between different meshes based on distance from the camera
2. Uses high-poly models when close, low-poly models when far
3. Reduces GPU workload by rendering fewer triangles for distant objects

## Performance Impact

**Without LOD:**
- All objects at max detail: 11 × 2562 tris = **28,182 triangles**
- Significant GPU overhead for distant, barely-visible objects

**With LOD (this demo):**
- Close objects: High detail (2562 tris each)
- Distant objects: Low detail or billboards (162-2 tris each)
- **Result**: ~5-10x fewer triangles rendered depending on camera position

## Controls

- **W/S** - Move camera forward/backward to see LOD transitions
- **P** - Toggle performance stats (FPS, triangle count, camera position)

**Watch the triangle count drop dramatically as you move away!**

## Color Coding

- **Green**: High detail level (close)
- **Yellow**: Medium detail level (mid-range)
- **Orange**: Low detail level (far)
- **Red**: Billboard level (very far)

## Running

```bash
cd examples/lod_demo
gleam run -t javascript
# Open http://localhost:1234 (or your dev server)
```

## Expected Results

- **FPS**: 60 (smooth even with many objects)
- **Triangles**: Varies dramatically based on camera position
  - Camera at Z=30: ~20,000 tris (mostly high/medium detail)
  - Camera at Z=100: ~5,000 tris (mix of medium/low detail)
  - Camera at Z=200: ~500 tris (mostly billboards)

## When to Use LOD

✅ **Open world games** with view distances >100 units
✅ **Many similar objects** (trees, rocks, buildings)
✅ **Performance-critical** scenes where every triangle counts
✅ **Mobile games** where GPU power is limited

❌ **Small scenes** where all objects are always close
❌ **Unique objects** that don't have simpler versions
❌ **Indoor scenes** with limited view distances

## Implementation Details

```gleam
scene.LOD(
  id: "tree",
  levels: [
    scene.LODLevel(distance: 0.0, node: high_poly_tree),    // 0-20 units
    scene.LODLevel(distance: 20.0, node: medium_poly_tree), // 20-50 units
    scene.LODLevel(distance: 50.0, node: low_poly_tree),    // 50-100 units
    scene.LODLevel(distance: 100.0, node: billboard),       // 100+ units
  ],
  transform: transform.Transform(...),
)
```

**Key points:**
- Levels sorted by distance (closest first)
- Three.js automatically picks the appropriate level based on camera distance
- Each level can be any `SceneNode` (Mesh, Group, Model3D)
- Transitions are instant (no cross-fading by default)

## Best Practices

### Distance Thresholds

Choose distances based on screen size:
- **High → Medium**: When object becomes <200 pixels
- **Medium → Low**: When object becomes <100 pixels
- **Low → Billboard**: When object becomes <50 pixels

### Model Preparation

1. **High detail**: Original model (as designed)
2. **Medium detail**: ~50% polygon reduction
3. **Low detail**: ~80% polygon reduction
4. **Billboard**: Single quad with texture

### Common Pitfalls

❌ **Too few levels**: Noticeable pop-in when switching
❌ **Too many levels**: Unnecessary complexity
❌ **Wrong distances**: High-poly models used too far away
✅ **3-4 levels** is usually optimal

## Advanced: Hysteresis

To prevent "flickering" when an object is exactly at a threshold distance, Three.js supports hysteresis (a buffer zone). Currently not exposed in Tiramisu API, but could be added if needed.

## See Also

- [frustum_culling_demo](/examples/frustum_culling_demo) - Automatic culling of off-screen objects
- [stress_test_1000](/examples/stress_test_1000) - Instanced rendering for many identical objects
- [DRAW_CALL_OPTIMIZATION.md](/docs/DRAW_CALL_OPTIMIZATION.md) - How to reduce draw calls
