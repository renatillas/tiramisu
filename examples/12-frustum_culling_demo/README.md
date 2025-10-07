# Frustum Culling Demo

Demonstrates **automatic frustum culling** in Tiramisu - only rendering objects visible to the camera.

## What It Does

- Renders **1000 cubes** in a large 3D grid (10×10×10)
- Cubes orbit around the center, moving in and out of the camera's view
- **Frustum culling automatically skips rendering off-screen cubes**
- Real-time FPS and draw call monitoring

## What is Frustum Culling?

Frustum culling is a rendering optimization that:
1. Tests each object against the camera's **view frustum** (the pyramidal volume the camera can see)
2. **Skips rendering** objects entirely outside the frustum
3. Saves GPU time by reducing draw calls and vertex processing

Three.js (and Tiramisu) enable frustum culling **automatically** by default!

## Performance Impact

**Without Frustum Culling:**
- All 1000 cubes rendered every frame
- ~1000 draw calls
- Significant GPU overhead for off-screen objects

**With Frustum Culling (automatic):**
- Only visible cubes rendered (~200-400 depending on camera angle)
- ~200-400 draw calls
- **2-5x performance improvement** depending on scene

## How It Works in Tiramisu

Frustum culling is **enabled automatically** for all scene nodes:
- Each `Mesh`, `InstancedMesh`, `Model3D`, and `Group` is tested against the camera frustum
- Three.js uses bounding volumes (bounding boxes/spheres) for fast intersection tests
- No configuration needed - it just works!

## Controls

- **P** - Toggle performance stats (FPS, draw calls, triangles)

Watch the **draw call count** change as cubes orbit in and out of view!

## Running

```bash
cd examples/frustum_culling_demo
gleam run -t javascript
# Open http://localhost:1234 (or your dev server)
```

## Expected Results

- **FPS**: 60 (smooth performance with 1000 cubes)
- **Draw Calls**: Varies from ~200-600 as cubes move in/out of view
- **Triangles**: Proportional to visible cubes (each cube = 12 triangles)

## When Frustum Culling Helps

✅ **Large scenes** with many objects spread out
✅ **Open world games** where most objects are off-screen
✅ **Procedurally generated environments** with distant objects

❌ **Small scenes** where everything is always visible
❌ **Top-down 2D games** with orthographic cameras showing full scene

## Implementation Details

- **Automatic**: Three.js handles frustum culling by default
- **Bounding Volumes**: Uses axis-aligned bounding boxes (AABB) for fast tests
- **Per-Object**: Each mesh/group tested individually
- **GPU-Optimized**: Culling happens before vertex processing

## Advanced: Disabling Frustum Culling

In rare cases, you may want to disable frustum culling:
- Objects with vertex shaders that extend beyond bounding box
- Skyboxes or full-screen effects
- Shadows that need to render off-screen objects

**Note**: Tiramisu doesn't currently expose a way to disable frustum culling per-node, as it's rarely needed. If you need this, please open an issue!

## See Also

- [DRAW_CALL_OPTIMIZATION.md](/docs/DRAW_CALL_OPTIMIZATION.md) - How instanced rendering reduces draw calls
- [stress_test_1000](/examples/stress_test_1000) - Performance benchmark with 20,000 instanced cubes
