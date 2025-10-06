# Debug Visualization Example

This example demonstrates all debug visualization features in Tiramisu.

## Features Demonstrated

### Visual Debug Tools
- **Bounding Boxes** - Wireframe boxes for collision visualization
- **Debug Spheres** - Wireframe spheres for radius visualization
- **Lines** - Simple lines between points
- **Rays** - Directional lines with origin and direction
- **Coordinate Axes** - RGB axes (X=red, Y=green, Z=blue)
- **Grid** - Ground plane grid for spatial reference
- **Points** - Small spheres marking positions
- **Paths** - Connected lines forming trails
- **Crosses** - 3-axis markers at positions

### Performance Monitoring
- **FPS** - Frames per second
- **Frame Time** - Time per frame in milliseconds
- **Draw Calls** - Number of render calls
- **Triangles** - Number of triangles rendered
- **Memory Usage** - JavaScript heap usage

## Controls

- **D** - Toggle debug visualizations on/off
- **P** - Toggle performance stats output to console

## Running the Example

```bash
cd examples/debug_visualization
gleam run -m lustre/dev start
```

Then open your browser to the displayed URL.

## Code Highlights

### Declarative Debug Nodes

All debug visualizations are regular `SceneNode` values that can be conditionally included in your view:

```gleam
let debug_nodes = case model.debug_enabled {
  True -> [
    debug.grid("grid", 20.0, 20, debug.white),
    debug.axes("axes", vec3.zero(), 2.0),
    debug.bounding_box("bbox", min, max, debug.green),
    debug.sphere("sphere", center, radius, debug.red),
  ]
  False -> []
}
```

### Performance Stats

Access performance statistics programmatically:

```gleam
let stats = debug.get_performance_stats()
io.println("FPS: " <> float.to_string(stats.fps))
```

Stats are automatically tracked by the engine and updated every frame.

## Implementation Notes

- All debug nodes use the same diff/patch system as regular scene nodes
- Debug visualizations have no performance impact when not included in the scene
- Performance monitoring is always active but lightweight
- Colors can be specified using hex values or convenience constants

## See Also

- `src/tiramisu/debug.gleam` - Debug API documentation
- `test/debug_test.gleam` - Unit tests for all debug features
