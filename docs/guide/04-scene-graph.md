# The Scene Graph

Your `view` function returns a tree of scene nodes. This tree—the **scene graph**—describes everything visible in your game: meshes, lights, cameras, and their spatial relationships.

If you're coming from Unity or Godot, think of it as the hierarchy panel, but defined in code. If you're coming from React, think of it as JSX but for 3D scenes.

## How rendering works

Every frame, after all messages are processed, Tiramisu calls your `view` function:

```gleam
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  // Return a tree of nodes
}
```

The scene graph you return is **declarative**—it describes what should exist, not how to create it. Tiramisu compares your new tree to the previous frame's tree and applies only the changes. This diffing approach means:

- Adding a node? Tiramisu creates it.
- Removing a node? Tiramisu destroys it.
- Changing a transform? Tiramisu updates it.
- Node unchanged? Tiramisu does nothing.

You never manually create or destroy Three.js objects. You describe the scene, and Tiramisu makes it real.

## Node types

### scene.mesh

The workhorse of 3D graphics. A mesh combines geometry (shape) with material (appearance):

```gleam
let assert Ok(cube_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
let assert Ok(cube_mat) =
  material.new()
  |> material.with_color(0x4ecdc4)
  |> material.build()

scene.mesh(
  id: "player",
  geometry: cube_geo,
  material: cube_mat,
  transform: transform.at(position: vec3.Vec3(0.0, 1.0, 0.0)),
  physics: None,
)
```

The `id` must be unique within your scene. Tiramisu uses it to track nodes across frames for efficient updates.

### scene.empty

A container node with no visual representation. Use it to:

- Group related nodes
- Create a root for your scene
- Establish parent-child transform hierarchies
- Act as pivot points for rotation

```gleam
scene.empty(
  id: "root",
  transform: transform.identity,
  children: [camera_node, player_node, ground_node],
)
```

### Adding children to any node

You don't need empty nodes just to create hierarchy. The `with_children` function adds children to any node type:

```gleam
scene.mesh(
  id: "player",
  geometry: player_geo,
  material: player_mat,
  transform: player_transform,
  physics: None,
)
|> scene.with_children([
  weapon_mesh,
  health_bar,
  name_label,
])
```

This is the idiomatic way to build hierarchies. The mesh becomes the parent, and its children inherit its transform. When the player moves, the weapon, health bar, and name tag move with it.

### scene.camera

Defines how the scene is viewed:

```gleam
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  near: 0.1,
  far: 1000.0,
)

scene.camera(
  id: "main-camera",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
  look_at: Some(vec3.Vec3(0.0, 0.0, 0.0)),
  active: True,
  viewport: None,
  postprocessing: None,
)
```

The `active: True` flag means this camera is used for rendering. You can have multiple cameras (for minimaps, picture-in-picture) by using viewports.

### scene.light

Lights illuminate your scene. Without them, everything is black:

```gleam
let assert Ok(sun) = light.directional(intensity: 0.8, color: 0xffffff)

scene.light(
  id: "sun",
  light: sun,
  transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
)
```

See [Lighting](#lighting) below for all light types.

### scene.instanced_mesh

Render thousands of identical objects efficiently:

```gleam
let tree_positions = list.range(0, 999)
  |> list.map(fn(i) {
    let x = int.to_float(i % 32) *. 2.0
    let z = int.to_float(i / 32) *. 2.0
    transform.at(position: vec3.Vec3(x, 0.0, z))
  })

scene.instanced_mesh(
  id: "forest",
  geometry: tree_geo,
  material: tree_mat,
  instances: tree_positions,
)
```

One draw call renders all 1000 trees. Use this for foliage, particles, bullets, coins—anything that appears many times with the same geometry and material.

### scene.lod

Level-of-detail for performance optimization:

```gleam
scene.lod(
  id: "distant-building",
  transform: building_transform,
  levels: [
    scene.lod_level(distance: 0.0, node: high_detail_mesh),
    scene.lod_level(distance: 50.0, node: medium_detail_mesh),
    scene.lod_level(distance: 150.0, node: low_detail_mesh),
  ],
)
```

Tiramisu automatically switches between levels based on camera distance.

### scene.audio

Attach audio sources to the scene:

```gleam
scene.audio(
  id: "engine-sound",
  audio: audio.PositionalAudio(
    buffer: engine_buffer,
    position: car_position,
    config: audio.config()
      |> audio.with_state(audio.Playing)
      |> audio.with_loop(True),
  ),
)
```

### scene.object_3d

Load external 3D models (GLTF/GLB/FBX/OBJ/STL):

```gleam
scene.object_3d(
  id: "character",
  object: loaded_object,
  transform: character_transform,
  animation: option.Some(animation_state),
  physics: option.None,
  transparent: False,
)
```

## Building your scene tree

Your `view` function must return a single root node. The most common patterns:

### Simple scene with empty root

```gleam
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [camera_node, player_mesh, ground_mesh],
  )
}
```

### Using with_children for hierarchy

```gleam
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.empty(id: "root", transform: transform.identity, children: [])
  |> scene.with_children([
    camera_node,
    player_mesh
      |> scene.with_children([weapon, shield]),
    ground_mesh,
  ])
}
```

### Combining multiple lists

When building children from multiple sources, use `list.concat`:

```gleam
fn view(model: Model, ctx: tiramisu.Context) -> scene.Node {
  scene.empty(id: "root", transform: transform.identity, children: [])
  |> scene.with_children(list.concat([
    [camera_node],
    [player_mesh],
    enemy_meshes,
    light_nodes,
  ]))
}
```

## Transforms

Every node has a transform defining its position, rotation, and scale. Transforms are immutable values built with helper functions:

```gleam
import tiramisu/transform
import vec/vec3

// Identity: origin, no rotation, scale 1
transform.identity

// Position only
transform.at(position: vec3.Vec3(5.0, 0.0, -3.0))

// Full builder pattern
transform.identity
  |> transform.with_position(vec3.Vec3(5.0, 2.0, -3.0))
  |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))
  |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))
```

### Rotation

Rotations use Euler angles in radians. The order is X -> Y -> Z:

```gleam
// Common rotations (in radians)
let quarter_turn = 1.5708   // 90 degrees (pi/2)
let half_turn = 3.1416      // 180 degrees (pi)
let full_turn = 6.2832      // 360 degrees (2*pi)

// Rotate 90 degrees around Y axis
transform.with_euler_rotation(vec3.Vec3(0.0, quarter_turn, 0.0))

// Face downward (rotate -90 degrees around X)
transform.with_euler_rotation(vec3.Vec3(-quarter_turn, 0.0, 0.0))
```

### Parent-child transforms

Child nodes inherit their parent's transform. A child at position (1, 0, 0) under a parent at (5, 0, 0) appears at world position (6, 0, 0):

```gleam
scene.mesh(
  id: "car",
  geometry: car_geo,
  material: car_mat,
  transform: transform.at(position: vec3.Vec3(10.0, 0.0, 0.0)),
  physics: None,
)
|> scene.with_children([
  // Wheel is at (1, -0.5, 1) relative to car
  // World position: (11, -0.5, 1)
  scene.mesh(
    id: "front-left-wheel",
    geometry: wheel_geo,
    material: wheel_mat,
    transform: transform.at(position: vec3.Vec3(1.0, -0.5, 1.0)),
    physics: None,
  ),
  scene.mesh(
    id: "front-right-wheel",
    geometry: wheel_geo,
    material: wheel_mat,
    transform: transform.at(position: vec3.Vec3(-1.0, -0.5, 1.0)),
    physics: None,
  ),
])
```

Moving the car moves all wheels automatically. Rotating the car rotates all wheels around the car's origin.

## Geometry

Geometry defines the shape of a mesh. All geometry constructors return `Result(Geometry, GeometryError)`:

```gleam
import tiramisu/geometry
import vec/vec2
import vec/vec3

// Box - width, height, depth
let assert Ok(box) = geometry.box(size: vec3.Vec3(2.0, 1.0, 3.0))

// Sphere - radius and segment counts
let assert Ok(sphere) = geometry.sphere(
  radius: 1.0,
  segments: vec2.Vec2(32, 16),
)

// Plane - width, height
let assert Ok(plane) = geometry.plane(size: vec2.Vec2(10.0, 10.0))

// Cylinder
let assert Ok(cylinder) = geometry.cylinder(
  radius_top: 1.0,
  radius_bottom: 1.0,
  height: 2.0,
  radial_segments: 32,
)

// Cone (cylinder with zero top radius)
let assert Ok(cone) = geometry.cone(
  radius: 1.0,
  height: 2.0,
  segments: 32,
)

// Circle
let assert Ok(circle) = geometry.circle(radius: 1.0, segments: 32)

// Torus (donut)
let assert Ok(torus) = geometry.torus(
  radius: 1.0,
  tube: 0.4,
  radial_segments: 16,
  tubular_segments: 32,
)

// Polyhedra
let assert Ok(tetra) = geometry.tetrahedron(radius: 1.0, detail: 0)
let assert Ok(icosa) = geometry.icosahedron(radius: 1.0, detail: 0)
```

### Segments and performance

Higher segment counts produce smoother curves but use more vertices. For a sphere:

- `segments: vec2.Vec2(8, 4)` - Blocky, 32 faces (good for far objects)
- `segments: vec2.Vec2(32, 16)` - Smooth, 512 faces (good default)
- `segments: vec2.Vec2(64, 32)` - Very smooth, 2048 faces (close-ups only)

## Materials

Materials define how geometry is shaded. Tiramisu supports several material types, each with different performance and visual characteristics.

### Standard material (PBR)

The recommended material for realistic lighting. Uses physically-based metalness/roughness:

```gleam
let assert Ok(mat) =
  material.new()
  |> material.with_color(0x4ecdc4)
  |> material.with_metalness(0.8)  // 0 = plastic, 1 = metal
  |> material.with_roughness(0.2)  // 0 = mirror, 1 = matte
  |> material.build()
```

Add textures for detail:

```gleam
material.new()
  |> material.with_color(0xffffff)
  |> material.with_color_map(albedo_texture)
  |> material.with_normal_map(normal_texture)
  |> material.with_roughness_map(roughness_texture)
  |> material.build()
```

### Basic material

Unlit, flat color. Ignores all lighting:

```gleam
let assert Ok(mat) = material.basic(
  color: 0xff0000,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
  map: None,
)
```

Use for: UI elements, debug visualization, stylized games that don't need lighting.

### Phong material

Classic Blinn-Phong shading with specular highlights:

```gleam
let assert Ok(mat) = material.phong(
  color: 0xffe66d,
  shininess: 100.0,
  map: None,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
)
```

Use for: Stylized games, when you want a specific retro look.

### Lambert material

Diffuse-only shading, no specular highlights:

```gleam
let assert Ok(mat) = material.lambert(
  color: 0x95e1d3,
  map: None,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
)
```

Use for: Matte objects, stylized games, better performance than Phong.

### Toon material

Cel-shading for cartoon-style graphics:

```gleam
let assert Ok(mat) = material.toon(
  color: 0xf38181,
  map: None,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
)
```

### Transparency

Any material can be transparent:

```gleam
material.new()
  |> material.with_color(0x00ff00)
  |> material.with_transparent(True)
  |> material.with_opacity(0.5)  // 50% transparent
  |> material.build()
```

## Lighting

Without lights, your scene is completely black (except for basic materials). Tiramisu provides several light types:

### Ambient light

Uniform lighting from all directions. Fills in shadows:

```gleam
let assert Ok(ambient) = light.ambient(intensity: 0.3, color: 0xffffff)

scene.light(id: "ambient", light: ambient, transform: transform.identity)
```

Every scene should have some ambient light, or shadowed areas will be pure black.

### Directional light

Parallel rays, like the sun. Position sets the direction (the light shines from that position toward the origin):

```gleam
let assert Ok(sun) = light.directional(intensity: 0.8, color: 0xffffff)

scene.light(
  id: "sun",
  light: sun,
  transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
)
```

### Point light

Radiates from a point in all directions:

```gleam
let assert Ok(lamp) = light.point(
  intensity: 1.0,
  color: 0xff6b6b,
  distance: 50.0,  // Light reaches 50 units, then fades to zero
)

scene.light(
  id: "lamp",
  light: lamp,
  transform: transform.at(position: vec3.Vec3(0.0, 5.0, 0.0)),
)
```

### Spot light

A cone of light, like a flashlight:

```gleam
let assert Ok(spot) = light.spot(
  intensity: 1.0,
  color: 0xffffff,
  distance: 100.0,
  angle: 0.5,      // Cone angle in radians
  penumbra: 0.2,   // Soft edge (0 = hard, 1 = fully soft)
)

scene.light(
  id: "flashlight",
  light: spot,
  transform: transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
    |> transform.with_euler_rotation(vec3.Vec3(-1.57, 0.0, 0.0)),  // Point down
)
```

### Hemisphere light

Two-color ambient, simulating sky above and ground below:

```gleam
let assert Ok(hemi) = light.hemisphere(
  intensity: 0.6,
  sky_color: 0x0077ff,     // Blue from above
  ground_color: 0x553311,  // Brown from below
)

scene.light(id: "sky", light: hemi, transform: transform.identity)
```

## Cameras

### Perspective camera

Standard 3D camera with depth:

```gleam
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,  // Degrees, 60-90 is typical
  near: 0.1,            // Objects closer than this are clipped
  far: 1000.0,          // Objects farther than this are clipped
)

scene.camera(
  id: "main",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
  look_at: Some(vec3.Vec3(0.0, 0.0, 0.0)),  // Point at origin
  active: True,
  viewport: None,
  postprocessing: None,
)
```

### Orthographic camera

No perspective distortion. Objects appear the same size regardless of distance:

```gleam
let assert Ok(cam) = camera.orthographic(
  left: -10.0,
  right: 10.0,
  top: 10.0,
  bottom: -10.0,
  near: 0.1,
  far: 1000.0,
)
```

Use for: 2D games, isometric views, UI rendering.

### 2D helper

Simplified orthographic camera for pixel-perfect 2D:

```gleam
let cam = camera.camera_2d(width: 800, height: 600)
```

### Multiple cameras

Use viewports for picture-in-picture effects:

```gleam
scene.empty(id: "cameras", transform: transform.identity, children: [])
|> scene.with_children([
  // Main camera (full screen, active)
  scene.camera(
    id: "main",
    camera: main_cam,
    transform: transform.identity,
    look_at: None,
    active: True,
    viewport: None,
    postprocessing: None,
  ),
  // Minimap camera (top-right corner)
  scene.camera(
    id: "minimap",
    camera: minimap_cam,
    transform: transform.at(position: vec3.Vec3(0.0, 100.0, 0.0)),
    look_at: Some(vec3.Vec3(0.0, 0.0, 0.0)),
    active: False,  // Rendered via viewport, not as main camera
    viewport: Some(camera.viewport(
      x: 800, y: 450,
      width: 200, height: 150,
    )),
    postprocessing: None,
  ),
])
```

## Scene diffing

Tiramisu compares each frame's scene graph to the previous frame's. Understanding this helps you write efficient views.

### What triggers updates?

**Node added** (ID didn't exist before):
```gleam
// Frame 1: [mesh_a, mesh_b]
// Frame 2: [mesh_a, mesh_b, mesh_c]
// Result: mesh_c created
```

**Node removed** (ID no longer exists):
```gleam
// Frame 1: [mesh_a, mesh_b, mesh_c]
// Frame 2: [mesh_a, mesh_c]
// Result: mesh_b destroyed
```

**Transform changed** (same ID, different transform):
```gleam
// Frame 1: mesh at (0, 0, 0)
// Frame 2: mesh at (1, 0, 0)
// Result: mesh position updated
```

**Node unchanged** (same ID, same properties):
```gleam
// No update needed, very efficient
```

### IDs matter

The `id` field is how Tiramisu tracks nodes. Always use stable, unique IDs:

```gleam
// Good: Stable IDs based on entity's actual identifier
list.map(enemies, fn(enemy) {
  scene.mesh(
    id: "enemy-" <> enemy.id,
    geometry: geo,
    material: mat,
    transform: enemy.transform,
    physics: None,
  )
})

// Bad: Index-based IDs that shift when list changes
list.index_map(enemies, fn(enemy, idx) {
  scene.mesh(
    id: "enemy-" <> int.to_string(idx),
    geometry: geo,
    material: mat,
    transform: enemy.transform,
    physics: None,
  )
})
```

If enemy 0 dies and enemy 1 becomes enemy 0, the engine sees "enemy-0 changed" instead of "enemy-0 removed, enemy-1 moved." Use stable identifiers.

## Performance tips

### Use instanced meshes for repeated objects

```gleam
// Bad: 1000 draw calls
list.map(trees, fn(t) {
  scene.mesh(id: t.id, geometry: geo, material: mat, transform: t.transform, physics: None)
})

// Good: 1 draw call
scene.instanced_mesh(
  id: "trees",
  geometry: tree_geo,
  material: tree_mat,
  instances: tree_transforms,
)
```

### Share geometry and materials

Create geometry and materials once, reuse them:

```gleam
// At module level or in init
let assert Ok(enemy_geo) = geometry.box(size: vec3.Vec3(1.0, 1.0, 1.0))
let assert Ok(enemy_mat) =
  material.new()
  |> material.with_color(0xff0000)
  |> material.build()

// In view, reuse for all enemies
list.map(enemies, fn(e) {
  scene.mesh(
    id: e.id,
    geometry: enemy_geo,  // Same geometry
    material: enemy_mat,  // Same material
    transform: e.transform,
    physics: None,
  )
})
```

### Keep hierarchies shallow

Deep nesting slows down transform calculations:

```gleam
// Avoid: Deep nesting for no reason
scene.empty(id: "a", transform: transform.identity, children: [])
|> scene.with_children([
  scene.empty(id: "b", transform: transform.identity, children: [])
  |> scene.with_children([
    scene.empty(id: "c", transform: transform.identity, children: [])
    |> scene.with_children([actual_mesh])
  ])
])

// Prefer: Flat or 2-3 levels max
scene.empty(id: "root", transform: transform.identity, children: [])
|> scene.with_children(list.concat([
  [actual_mesh],
  other_meshes,
]))
```

### Use LOD for distant objects

```gleam
scene.lod(
  id: "building",
  transform: building_transform,
  levels: [
    scene.lod_level(distance: 0.0, node: high_detail),    // Close
    scene.lod_level(distance: 50.0, node: medium_detail), // Medium
    scene.lod_level(distance: 150.0, node: billboard),    // Far: simple quad
  ],
)
```

## Debug visualization

Tiramisu provides helpers for visual debugging:

```gleam
import tiramisu/debug

// Bounding box (AABB)
debug.bounding_box(
  id: "player-bounds",
  min: vec3.Vec3(-1.0, 0.0, -1.0),
  max: vec3.Vec3(1.0, 2.0, 1.0),
  color: debug.color_green,
)

// Sphere
debug.sphere(
  id: "trigger",
  center: vec3.Vec3(0.0, 0.0, 0.0),
  radius: 5.0,
  color: debug.color_red,
)

// Line
debug.line(id: "ray", from: start, to: end, color: debug.color_blue)

// Coordinate axes
debug.axes(id: "origin", origin: vec3.Vec3(0.0, 0.0, 0.0), size: 5.0)

// Grid
debug.grid(id: "ground", size: 100.0, divisions: 10, color: debug.color_white)
```

Add these to your scene during development, remove before shipping.

## Next steps

You now understand how Tiramisu renders scenes. The key insights:

1. `view` returns a declarative scene graph
2. Use `with_children` to build hierarchies on any node type
3. Tiramisu diffs the graph and updates efficiently
4. Use stable IDs to help the differ
5. Transforms are hierarchical
6. Share geometry and materials
7. Use instancing for many identical objects

Next, learn about [Physics](05-physics.md) to add realistic simulation to your game.
