# Scene Graph Guide

A **scene graph** is a tree structure representing all objects in your game world. Tiramisu uses a declarative scene graph that you define in your `view()` function.

## Core Concepts

### Scene Nodes

Every object in your game is a `scene.Node`. Nodes can be:

- **Mesh** - 3D object with geometry and material
- **InstancedMesh** - Many identical objects (efficient!)
- **Light** - Light source (ambient, directional, point, spot)
- **Camera** - Viewpoint for rendering
- **Group** - Container for child nodes
- **LOD** - Level-of-detail system
- **Model3D** - Loaded 3D model with animations
- **Audio** - 2D or 3D audio source
- **Debug nodes** - Visualization helpers (box, sphere, line, etc.)

### The View Function

Your `view()` function returns a single root `scene.Node` every frame. If you have multiple top-level nodes, wrap them in `scene.empty()`:

```gleam
fn view(model: Model) -> scene.Node {
  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [
      camera_node,
      player_mesh,
      enemy_mesh,
      light_node,
    ],
  )
}
```

Tiramisu automatically **diffs** the scene tree against the previous frame and applies only the changes.

## Creating Scene Nodes

### Meshes

Basic 3D objects with geometry and material:

```gleam
import gleam/option
import tiramisu/geometry
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

let assert Ok(cube_geometry) = geometry.box(
  width: 1.0,
  height: 1.0,
  depth: 1.0,
)

let assert Ok(cube_material) =
  material.new()
  |> material.with_color(0x4ecdc4)
  |> material.with_metalness(0.5)
  |> material.with_roughness(0.5)
  |> material.build()

scene.mesh(
  id: "cube",  // Unique identifier
  geometry: cube_geometry,
  material: cube_material,
  transform: transform.identity,  // Position (0,0,0), no rotation, scale (1,1,1)
  physics: option.None,
)
```

### Geometry Types

Built-in geometries (all return `Result(Geometry, GeometryError)`):

```gleam
import tiramisu/geometry

// Box
let assert Ok(box_geo) = geometry.box(width: 2.0, height: 1.0, depth: 1.0)

// Sphere
let assert Ok(sphere_geo) = geometry.sphere(
  radius: 1.0,
  width_segments: 32,
  height_segments: 16,
)

// Cylinder
let assert Ok(cylinder_geo) = geometry.cylinder(
  radius_top: 1.0,
  radius_bottom: 1.0,
  height: 2.0,
  radial_segments: 32,
)

// Plane
let assert Ok(plane_geo) = geometry.plane(width: 10.0, height: 10.0)

// Circle
let assert Ok(circle_geo) = geometry.circle(radius: 1.0, segments: 32)

// Cone
let assert Ok(cone_geo) = geometry.cone(radius: 1.0, height: 2.0, segments: 32)

// Torus
let assert Ok(torus_geo) = geometry.torus(
  radius: 1.0,
  tube: 0.4,
  radial_segments: 16,
  tubular_segments: 32,
)

// Polyhedra
let assert Ok(tetra_geo) = geometry.tetrahedron(radius: 1.0, detail: 0)
let assert Ok(icosa_geo) = geometry.icosahedron(radius: 1.0, detail: 0)

// Custom (loaded from file)
geometry.custom(buffer_geometry)
```

### Material Types

All materials are created using the `material` module and return `Result(Material, MaterialError)`.

**StandardMaterial** - PBR (physically-based) with metalness/roughness (recommended):
```gleam
import tiramisu/material

// Builder pattern (recommended)
let assert Ok(mat) =
  material.new()
  |> material.with_color(0x4ecdc4)
  |> material.with_metalness(0.8)  // 0.0 = non-metal, 1.0 = metal
  |> material.with_roughness(0.2)  // 0.0 = smooth, 1.0 = rough
  |> material.with_color_map(texture)
  |> material.with_normal_map(normal_texture)
  |> material.with_emissive(0x000000)  // No glow by default
  |> material.with_emissive_intensity(0.0)
  |> material.build()

// Or direct constructor with all parameters
let assert Ok(mat2) = material.standard(
  color: 0x4ecdc4,
  metalness: 0.8,
  roughness: 0.2,
  map: option.Some(texture),
  normal_map: option.Some(normal_texture),
  ambient_oclusion_map: option.None,
  roughness_map: option.None,
  metalness_map: option.None,
  displacement_map: option.None,
  displacement_scale: 1.0,
  displacement_bias: 0.0,
  emissive: 0x000000,
  emissive_intensity: 0.0,
  transparent: False,
  opacity: 1.0,
)
```

**BasicMaterial** - Unlit, flat color:
```gleam
let assert Ok(basic_mat) = material.basic(
  color: 0xff0000,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
  map: option.None,
)
```

**PhongMaterial** - Classic Phong shading:
```gleam
let assert Ok(phong_mat) = material.phong(
  color: 0xffe66d,
  shininess: 100.0,
  map: option.None,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
)
```

**LambertMaterial** - Matte, diffuse-only:
```gleam
let assert Ok(lambert_mat) = material.lambert(
  color: 0x95e1d3,
  map: option.None,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
)
```

**ToonMaterial** - Cel-shaded:
```gleam
let assert Ok(toon_mat) = material.toon(
  color: 0xf38181,
  map: option.None,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
)
```

### Lights

All lights are created using the `light` module and return `Result(Light, LightError)`.

**AmbientLight** - Uniform lighting from all directions:
```gleam
import tiramisu/light

let assert Ok(ambient) = light.ambient(intensity: 0.5, color: 0xffffff)

scene.light(
  id: "ambient",
  light: ambient,
  transform: transform.identity,
)
```

**DirectionalLight** - Parallel rays (like the sun):
```gleam
let assert Ok(sun) = light.directional(intensity: 0.8, color: 0xffffff)

scene.light(
  id: "sun",
  light: sun,
  transform: transform.at(position: vec3.Vec3(10.0, 10.0, 10.0)),
)
```

**PointLight** - Radiates from a point:
```gleam
let assert Ok(lamp) = light.point(
  intensity: 1.0,
  color: 0xff6b6b,
  distance: 50.0,
)

scene.light(
  id: "lamp",
  light: lamp,
  transform: transform.at(position: vec3.Vec3(0.0, 5.0, 0.0)),
)
```

**SpotLight** - Cone of light:
```gleam
let assert Ok(spotlight) = light.spot(
  intensity: 1.0,
  color: 0xffffff,
  distance: 100.0,
  angle: 0.5,      // Radians
  penumbra: 0.2,   // Soft edge
)

scene.light(
  id: "spotlight",
  light: spotlight,
  transform: transform.at(position: vec3.Vec3(0.0, 10.0, 0.0))
    |> transform.with_euler_rotation(vec3.Vec3(-1.57, 0.0, 0.0)),  // Point down
)
```

**HemisphereLight** - Sky + ground colors:
```gleam
let assert Ok(hemi) = light.hemisphere(
  intensity: 0.6,
  sky_color: 0x0077ff,
  ground_color: 0x553311,
)

scene.light(
  id: "hemi",
  light: hemi,
  transform: transform.identity,
)
```

### Cameras

**Perspective Camera** (3D games):
```gleam
import tiramisu/camera

// Aspect ratio is calculated automatically from viewport/window dimensions
let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  near: 0.1,
  far: 1000.0,
)

scene.camera(
  id: "main",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(0.0, 5.0, 10.0)),
  look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),  // Point camera at origin
  active: True,  // This camera is used for rendering
  viewport: option.None,
  children: [],
  postprocessing: option.None,
)
```

**Orthographic Camera** (2D games, isometric):
```gleam
let assert Ok(cam) = camera.orthographic(
  left: -10.0,
  right: 10.0,
  top: 10.0,
  bottom: -10.0,
  near: 0.1,
  far: 1000.0,
)

scene.camera(
  id: "2d_cam",
  camera: cam,
  transform: transform.identity,
  active: True,
  viewport: option.None,
  children: [],
  postprocessing: option.None,
)
```

**2D Helper** (orthographic camera for pixel-perfect 2D):
```gleam
// Creates orthographic camera with automatic aspect ratio
let cam = camera.camera_2d(width: 800, height: 600)

scene.camera(
  id: "2d",
  camera: cam,
  transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),  // Distance from scene
  active: True,
  viewport: option.None,
  children: [],
  postprocessing: option.None,
)
```

**Multiple Cameras** (picture-in-picture):
```gleam
let assert Ok(main_cam) = camera.perspective(
  field_of_view: 75.0,
  near: 0.1,
  far: 1000.0,
)

let assert Ok(minimap_cam) = camera.perspective(
  field_of_view: 60.0,
  near: 0.1,
  far: 1000.0,
)

scene.empty(
  id: "root",
  transform: transform.identity,
  children: [
    // Main camera (full screen)
    scene.camera(
      id: "main",
      camera: main_cam,
      transform: transform.identity,
      look_at: option.None,
      active: True,
      viewport: option.None,
      children: [],
      postprocessing: option.None,
    ),
    // Mini-map camera (top-right corner)
    scene.camera(
      id: "minimap",
      camera: minimap_cam,
      transform: transform.at(position: vec3.Vec3(0.0, 100.0, 0.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: False,
      viewport: option.Some(scene.Viewport(x: 800, y: 450, width: 200, height: 150)),
      children: [],
      postprocessing: option.None,
    ),
  ],
)
```

## Hierarchy with Empty Nodes

Empty nodes act as containers for children, creating parent-child relationships:

```gleam
let assert Ok(body_geo) = geometry.box(width: 1.0, height: 2.0, depth: 1.0)
let assert Ok(body_mat) =
  material.new()
  |> material.with_color(0x4ecdc4)
  |> material.build()

let assert Ok(weapon_geo) = geometry.box(width: 0.2, height: 0.2, depth: 1.5)
let assert Ok(weapon_mat) =
  material.new()
  |> material.with_color(0x888888)
  |> material.build()

let assert Ok(healthbar_geo) = geometry.plane(width: 1.0, height: 0.1)
let assert Ok(healthbar_mat) = material.basic(
  color: 0x00ff00,
  transparent: False,
  opacity: 1.0,
  alpha_test: 0.0,
  map: option.None,
)

scene.empty(
  id: "player",
  transform: player_transform,
  children: [
    // Body mesh
    scene.mesh(
      id: "player_body",
      geometry: body_geo,
      material: body_mat,
      transform: transform.identity,  // Relative to parent
      physics: option.None,
    ),
    // Weapon (attached to player)
    scene.mesh(
      id: "weapon",
      geometry: weapon_geo,
      material: weapon_mat,
      transform: transform.at(position: vec3.Vec3(0.5, 0.5, 0.0)),  // Offset from player center
      physics: option.None,
    ),
    // Health bar (UI element)
    scene.mesh(
      id: "health_bar",
      geometry: healthbar_geo,
      material: healthbar_mat,
      transform: transform.at(position: vec3.Vec3(0.0, 1.5, 0.0))  // Above player
        |> transform.with_scale(vec3.Vec3(model.player_health /. 100.0, 1.0, 1.0)),
      physics: option.None,
    ),
  ],
)
```

**Benefits of hierarchy:**
- Moving parent moves all children
- Rotating parent rotates all children around parent origin
- Scaling parent scales all children
- Easy to manage complex objects

## Transforms

Every node has a transform (position, rotation, scale). The Transform type is opaque, so you use builder functions:

```gleam
import tiramisu/transform

// Identity (default: origin, no rotation, scale 1)
transform.identity

// Position only
transform.at(position: vec3.Vec3(10.0, 0.0, 5.0))

// Build with multiple properties
transform.identity
  |> transform.with_position(vec3.Vec3(5.0, 2.0, -3.0))
  |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // Radians (90° on Y axis)
  |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))  // 2x larger

// Or start from a position and build on it
transform.at(position: vec3.Vec3(5.0, 2.0, -3.0))
  |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))
  |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))
```

**Rotation:**
- Uses Euler angles in radians
- Order: X → Y → Z
- π radians = 180°, π/2 = 90°

**Common rotations:**
```gleam
// Quarter turn clockwise (90°)
vec3.Vec3(0.0, 1.5708, 0.0)

// Half turn (180°)
vec3.Vec3(0.0, 3.1416, 0.0)

// Face down
vec3.Vec3(-1.5708, 0.0, 0.0)
```

## Instanced Rendering

For rendering many identical objects efficiently:

```gleam
// Create 1000 transform instances
let transforms = list.range(0, 999)
  |> list.map(fn(i) {
    let fi = int.to_float(i)
    transform.identity
      |> transform.with_position(vec3.Vec3(fi *. 2.0, 0.0, 0.0))
      |> transform.with_euler_rotation(vec3.Vec3(0.0, fi *. 0.1, 0.0))
  })

let assert Ok(tree_geo) = geometry.cylinder(
  radius_top: 0.5,
  radius_bottom: 0.5,
  height: 3.0,
  radial_segments: 8,
)

let assert Ok(tree_mat) =
  material.new()
  |> material.with_color(0x8b4513)
  |> material.build()

scene.instanced_mesh(
  id: "trees",
  geometry: tree_geo,
  material: tree_mat,
  instances: transforms,  // All instances in 1 draw call!
)
```

**Performance:**
- 1 draw call instead of 1000
- Can render 10,000+ instances at 60 FPS
- All instances share geometry/material

## Level-of-Detail (LOD)

Automatically switch detail based on distance:

```gleam
// Assume these are loaded or created elsewhere
let complex_geometry = // 10,000 triangles
let medium_geometry = // 2,000 triangles
let low_geometry =  // 500 triangles

let assert Ok(detailed_material) =
  material.new()
  |> material.with_color(0x808080)
  |> material.build()

let assert Ok(simple_material) =
  material.new()
  |> material.with_color(0x808080)
  |> material.build()

let assert Ok(billboard_geo) = geometry.plane(width: 10.0, height: 15.0)
let assert Ok(billboard_mat) = material.basic(
  color: 0x808080,
  transparent: True,
  opacity: 0.8,
  alpha_test: 0.0,
  map: option.Some(building_texture),
)

scene.lod(
  id: "building",
  transform: transform.at(position: vec3.Vec3(100.0, 0.0, 50.0)),
  levels: [
    // High detail (0-50 units)
    scene.lod_level(
      distance: 0.0,
      node: scene.mesh(
        id: "building_high",
        geometry: complex_geometry,
        material: detailed_material,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
    // Medium detail (50-150 units)
    scene.lod_level(
      distance: 50.0,
      node: scene.mesh(
        id: "building_medium",
        geometry: medium_geometry,
        material: simple_material,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
    // Low detail (150-500 units)
    scene.lod_level(
      distance: 150.0,
      node: scene.mesh(
        id: "building_low",
        geometry: low_geometry,
        material: simple_material,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
    // Billboard (500+ units)
    scene.lod_level(
      distance: 500.0,
      node: scene.mesh(
        id: "building_billboard",
        geometry: billboard_geo,
        material: billboard_mat,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
  ],
)
```

## Scene Diffing

Tiramisu automatically detects changes between frames:

### What Triggers Updates?

**Node removed:**
```gleam
// Frame 1
[mesh1, mesh2, mesh3]

// Frame 2 (mesh2 removed)
[mesh1, mesh3]

// Patch: RemoveNode("mesh2")
```

**Node added:**
```gleam
// Frame 1
[mesh1, mesh2]

// Frame 2 (mesh3 added)
[mesh1, mesh2, mesh3]

// Patch: AddNode("mesh3", ...)
```

**Transform changed:**
```gleam
// Frame 1
scene.mesh(
  id: "player",
  transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
  ...
)

// Frame 2
scene.mesh(
  id: "player",
  transform: transform.at(position: vec3.Vec3(1.0, 0.0, 0.0)),  // Moved!
  ...
)

// Patch: UpdateTransform("player", new_transform)
```

**Material changed:**
```gleam
// Patch: UpdateMaterial("cube", new_material)
```

**Geometry changed:**
```gleam
// Patch: UpdateGeometry("shape", new_geometry)
```

### Optimization: Unchanged Nodes

If a node is **identical** between frames, no patch is generated:

```gleam
// Both frames have identical mesh
let assert Ok(wall_geo) = geometry.box(width: 10.0, height: 5.0, depth: 1.0)
let assert Ok(wall_mat) = material.new()
  |> material.with_color(0x808080)
  |> material.build()

let static_mesh = scene.mesh(
  id: "wall",
  geometry: wall_geo,
  material: wall_mat,
  transform: transform.identity,
  physics: option.None,
)

// Frame 1
[static_mesh, dynamic_mesh1]

// Frame 2
[static_mesh, dynamic_mesh2]

// Only dynamic_mesh changed - static_mesh generates no patch!
```

## Best Practices

### 1. Use Meaningful IDs

```gleam
// ❌ Bad: String ids with ambiguous ids
id: "mesh1"

// ✅ Good: Typed Ids with descriptive names
pub type Id {
  PlayerBody
  EnemyGoblin(goblin_index: Int)
  Ground
}
id: PlayerBody
id: EnemyGoblin(10)
id: Ground
```

### 2. Keep Hierarchies Shallow

```gleam
// ❌ Bad: Deep nesting (slow)
scene.empty(
  id: "root",
  transform: transform.identity,
  children: [
    scene.empty(
      id: "level1",
      transform: transform.identity,
      children: [
        scene.empty(
          id: "level2",
          transform: transform.identity,
          children: [
            scene.empty(id: "level3", transform: transform.identity, children: [mesh])
          ]
        )
      ]
    )
  ]
)

// ✅ Good: Flat or 2-3 levels max
scene.empty(id: "player", transform: transform.identity, children: [body, weapon, ui])
```

### 3. Use instanced_mesh() for Repeated Objects

```gleam
// ❌ Bad: 100 separate meshes
list.range(0, 99)
  |> list.map(fn(i) {
    scene.mesh(id: "coin_" <> int.to_string(i), ...)
  })

// ✅ Good: 1 instanced mesh
scene.instanced_mesh(id: "coins", instances: coin_transforms)
```

### 4. Minimize Material/Geometry Variety

```gleam
// ❌ Bad: Different material for each enemy
list.map(enemies, fn(e) {
  let assert Ok(mat) = material.new()
    |> material.with_color(e.color)
    |> material.build()
  scene.Mesh(id: e.id, geometry: enemy_geo, material: mat, ...)
})

// ✅ Good: Same material, use instanced mesh
let assert Ok(enemy_mat) = material.new()
  |> material.with_color(0xff0000)
  |> material.build()

scene.instanced_mesh(
  id: "enemies",
  geometry: enemy_geo,
  material: enemy_mat,
  instances: enemy_transforms,
)
```

## Debug Visualization

Tiramisu provides debug helper functions for visualization:

```gleam
import tiramisu/debug

// Box (AABB)
debug.bounding_box(
  id: "collision_box",
  min: vec3.Vec3(-1.0, 0.0, -1.0),
  max: vec3.Vec3(1.0, 2.0, 1.0),
  color: debug.color_green,
)

// Sphere
debug.sphere(
  id: "trigger_zone",
  center: vec3.Vec3(0.0, 0.0, 0.0),
  radius: 5.0,
  color: debug.color_red,
)

// Line
debug.line(
  id: "raycast",
  from: vec3.Vec3(0.0, 1.0, 0.0),
  to: vec3.Vec3(0.0, 1.0, 10.0),
  color: debug.color_blue,
)

// Axes (X=red, Y=green, Z=blue)
debug.axes(
  id: "world_origin",
  origin: vec3.Vec3(0.0, 0.0, 0.0),
  size: 5.0,
)

// Grid
debug.grid(
  id: "ground_grid",
  size: 100.0,
  divisions: 10,
  color: debug.color_white,
)

// Point
debug.point(
  id: "spawn_point",
  position: vec3.Vec3(10.0, 0.0, 5.0),
  size: 0.5,
  color: debug.color_yellow,
)
```
