# Scene Graph Guide

A **scene graph** is a tree structure representing all objects in your game world. Tiramisu uses a declarative scene graph that you define in your `view()` function.

## Core Concepts

### Scene Nodes

Every object in your game is a `SceneNode`. Nodes can be:

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

Your `view()` function returns a `List(SceneNode)` every frame:

```gleam
fn view(model: Model) -> List(SceneNode) {
  [
    camera_node,
    player_mesh,
    enemy_mesh,
    light_node,
  ]
}
```

Tiramisu automatically **diffs** this list against the previous frame and applies only the changes.

## Creating Scene Nodes

### Meshes

Basic 3D objects with geometry and material:

```gleam
import gleam/option
import tiramisu/scene
import tiramisu/transform
import vec/vec3

scene.Mesh(
  id: "cube",  // Unique identifier
  geometry: scene.BoxGeometry(1.0, 1.0, 1.0),
  material: scene.StandardMaterial(
    color: 0x4ecdc4,
    metalness: 0.5,
    roughness: 0.5,
    map: option.None,
    normal_map: option.None,
  ),
  transform: transform.Transform(
    position: vec3.Vec3(0.0, 0.0, 0.0),
    rotation: vec3.Vec3(0.0, 0.0, 0.0),
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  ),
  physics: option.None,
)
```

### Geometry Types

Built-in geometries:

```gleam
// Box
scene.BoxGeometry(width: 2.0, height: 1.0, depth: 1.0)

// Sphere
scene.SphereGeometry(radius: 1.0, width_segments: 32, height_segments: 16)

// Cylinder
scene.CylinderGeometry(
  radius_top: 1.0,
  radius_bottom: 1.0,
  height: 2.0,
  radial_segments: 32,
)

// Plane
scene.PlaneGeometry(width: 10.0, height: 10.0)

// Circle
scene.CircleGeometry(radius: 1.0, segments: 32)

// Cone
scene.ConeGeometry(radius: 1.0, height: 2.0, segments: 32)

// Torus
scene.TorusGeometry(
  radius: 1.0,
  tube: 0.4,
  radial_segments: 16,
  tubular_segments: 32,
)

// Polyhedra
scene.TetrahedronGeometry(radius: 1.0, detail: 0)
scene.IcosahedronGeometry(radius: 1.0, detail: 0)

// Custom (loaded from file)
scene.CustomGeometry(buffer_geometry)
```

### Material Types

**BasicMaterial** - Unlit, flat color:
```gleam
scene.BasicMaterial(
  color: 0xff0000,
  transparent: False,
  opacity: 1.0,
  map: option.None,
)
```

**StandardMaterial** - PBR (physically-based) with metalness/roughness:
```gleam
scene.StandardMaterial(
  color: 0x4ecdc4,
  metalness: 0.8,  // 0.0 = non-metal, 1.0 = metal
  roughness: 0.2,  // 0.0 = smooth, 1.0 = rough
  map: option.Some(texture),
  normal_map: option.Some(normal_texture),
)
```

**PhongMaterial** - Classic Phong shading:
```gleam
scene.PhongMaterial(
  color: 0xffe66d,
  shininess: 100.0,
  map: option.None,
)
```

**LambertMaterial** - Matte, diffuse-only:
```gleam
scene.LambertMaterial(
  color: 0x95e1d3,
  map: option.None,
)
```

**ToonMaterial** - Cel-shaded:
```gleam
scene.ToonMaterial(
  color: 0xf38181,
  map: option.None,
)
```

### Lights

**AmbientLight** - Uniform lighting from all directions:
```gleam
scene.Light(
  id: "ambient",
  light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.5),
  transform: transform.identity,
)
```

**DirectionalLight** - Parallel rays (like the sun):
```gleam
scene.Light(
  id: "sun",
  light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
  transform: transform.Transform(
    position: vec3.Vec3(10.0, 10.0, 10.0),
    rotation: vec3.Vec3(0.0, 0.0, 0.0),
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  ),
)
```

**PointLight** - Radiates from a point:
```gleam
scene.Light(
  id: "lamp",
  light_type: scene.PointLight(
    color: 0xff6b6b,
    intensity: 1.0,
    distance: 50.0,
  ),
  transform: transform.Transform(
    position: vec3.Vec3(0.0, 5.0, 0.0),
    rotation: vec3.Vec3(0.0, 0.0, 0.0),
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  ),
)
```

**SpotLight** - Cone of light:
```gleam
scene.Light(
  id: "spotlight",
  light_type: scene.SpotLight(
    color: 0xffffff,
    intensity: 1.0,
    distance: 100.0,
    angle: 0.5,      // Radians
    penumbra: 0.2,   // Soft edge
  ),
  transform: transform.Transform(
    position: vec3.Vec3(0.0, 10.0, 0.0),
    rotation: vec3.Vec3(-1.57, 0.0, 0.0),  // Point down
    scale: vec3.Vec3(1.0, 1.0, 1.0),
  ),
)
```

**HemisphereLight** - Sky + ground colors:
```gleam
scene.Light(
  id: "hemi",
  light_type: scene.HemisphereLight(
    sky_color: 0x0077ff,
    ground_color: 0x553311,
    intensity: 0.6,
  ),
  transform: transform.identity,
)
```

### Cameras

**Perspective Camera** (3D games):
```gleam
import tiramisu/camera

let assert Ok(cam) = camera.perspective(
  field_of_view: 75.0,
  aspect: 16.0 /. 9.0,
  near: 0.1,
  far: 1000.0,
)

let cam = cam
  |> camera.set_position(vec3.Vec3(0.0, 5.0, 10.0))
  |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

scene.Camera(
  id: "main",
  camera: cam,
  transform: transform.identity,
  active: True,  // This camera is used for rendering
  viewport: option.None,
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

scene.Camera(
  id: "2d_cam",
  camera: cam,
  transform: transform.identity,
  active: True,
  viewport: option.None,
)
```

**Multiple Cameras** (picture-in-picture):
```gleam
[
  // Main camera (full screen)
  scene.Camera(
    id: "main",
    camera: main_cam,
    transform: transform.identity,
    active: True,
    viewport: option.None,
  ),
  // Mini-map camera (top-right corner)
  scene.Camera(
    id: "minimap",
    camera: minimap_cam,
    transform: transform.at(position: vec3.Vec3(0.0, 100.0, 0.0)),
    active: False,
    viewport: option.Some(#(800, 450, 200, 150)),  // x, y, width, height
  ),
]
```

## Hierarchy with Groups

Group nodes contain children, creating parent-child relationships:

```gleam
scene.Group(
  id: "player",
  transform: player_transform,
  children: [
    // Body mesh
    scene.Mesh(
      id: "player_body",
      geometry: scene.BoxGeometry(1.0, 2.0, 1.0),
      material: scene.StandardMaterial(...),
      transform: transform.identity,  // Relative to parent
      physics: option.None,
    ),
    // Weapon (attached to player)
    scene.Mesh(
      id: "weapon",
      geometry: scene.BoxGeometry(0.2, 0.2, 1.5),
      material: scene.StandardMaterial(...),
      transform: transform.Transform(
        position: vec3.Vec3(0.5, 0.5, 0.0),  // Offset from player center
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    // Health bar (UI element)
    scene.Mesh(
      id: "health_bar",
      geometry: scene.PlaneGeometry(1.0, 0.1),
      material: scene.BasicMaterial(color: 0x00ff00, ...),
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 1.5, 0.0),  // Above player
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(model.player_health /. 100.0, 1.0, 1.0),
      ),
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

Every node has a transform (position, rotation, scale):

```gleam
import tiramisu/transform

// Identity (default: origin, no rotation, scale 1)
transform.identity

// Position only
transform.at(position: vec3.Vec3(10.0, 0.0, 5.0))

// Full transform
transform.Transform(
  position: vec3.Vec3(5.0, 2.0, -3.0),
  rotation: vec3.Vec3(0.0, 1.57, 0.0),  // Radians (90° on Y axis)
  scale: vec3.Vec3(2.0, 2.0, 2.0),      // 2x larger
)
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
    transform.Transform(
      position: vec3.Vec3(fi *. 2.0, 0.0, 0.0),
      rotation: vec3.Vec3(0.0, fi *. 0.1, 0.0),
      scale: vec3.Vec3(1.0, 1.0, 1.0),
    )
  })

scene.InstancedMesh(
  id: "trees",
  geometry: scene.CylinderGeometry(0.5, 0.5, 3.0, 8),
  material: scene.StandardMaterial(color: 0x8b4513, ...),
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
scene.LOD(
  id: "building",
  transform: transform.at(position: vec3.Vec3(100.0, 0.0, 50.0)),
  levels: [
    // High detail (0-50 units)
    scene.lod_level(
      distance: 0.0,
      node: scene.Mesh(
        id: "building_high",
        geometry: complex_geometry,  // 10,000 triangles
        material: detailed_material,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
    // Medium detail (50-150 units)
    scene.lod_level(
      distance: 50.0,
      node: scene.Mesh(
        id: "building_medium",
        geometry: medium_geometry,  // 2,000 triangles
        material: simple_material,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
    // Low detail (150-500 units)
    scene.lod_level(
      distance: 150.0,
      node: scene.Mesh(
        id: "building_low",
        geometry: low_geometry,  // 500 triangles
        material: simple_material,
        transform: transform.identity,
        physics: option.None,
      ),
    ),
    // Billboard (500+ units)
    scene.lod_level(
      distance: 500.0,
      node: scene.Mesh(
        id: "building_billboard",
        geometry: scene.PlaneGeometry(10.0, 15.0),
        material: scene.BasicMaterial(
          color: 0x808080,
          transparent: True,
          opacity: 0.8,
          map: option.Some(building_texture),
        ),
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
scene.Mesh(
  id: "player",
  transform: transform.at(position: vec3.Vec3(0.0, 0.0, 0.0)),
  ...
)

// Frame 2
scene.Mesh(
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
let static_mesh = scene.Mesh(
  id: "wall",
  geometry: scene.BoxGeometry(10.0, 5.0, 1.0),
  material: scene.StandardMaterial(color: 0x808080, ...),
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
// ❌ Bad: Ambiguous
id: "mesh1"

// ✅ Good: Descriptive
id: "player_body"
id: "enemy_goblin_1"
id: "ui_health_bar"
```

### 2. Cache Static Scenes

```gleam
pub type Model {
  Model(
    scene_dirty: Bool,
    cached_scene: List(SceneNode),
    ...
  )
}

fn view(model: Model) -> List(SceneNode) {
  case model.scene_dirty {
    True -> {
      let scene = build_scene(model)
      // Update model.cached_scene = scene in update function
      scene
    }
    False -> model.cached_scene  // Reuse! Dirty flagging optimization
  }
}
```

### 3. Keep Hierarchies Shallow

```gleam
// ❌ Bad: Deep nesting (slow)
Group(
  "root",
  children: [
    Group("level1", children: [
      Group("level2", children: [
        Group("level3", children: [mesh])
      ])
    ])
  ]
)

// ✅ Good: Flat or 2-3 levels max
Group("player", children: [body, weapon, ui])
```

### 4. Use InstancedMesh for Repeated Objects

```gleam
// ❌ Bad: 100 separate meshes
list.range(0, 99)
  |> list.map(fn(i) {
    scene.Mesh(id: "coin_" <> int.to_string(i), ...)
  })

// ✅ Good: 1 instanced mesh
scene.InstancedMesh(id: "coins", instances: coin_transforms)
```

### 5. Minimize Material/Geometry Variety

```gleam
// ❌ Bad: Different material for each
list.map(enemies, fn(e) {
  scene.Mesh(material: scene.StandardMaterial(color: e.color, ...), ...)
})

// ✅ Good: Same material, use instances
scene.InstancedMesh(
  material: scene.StandardMaterial(color: 0xff0000, ...),
  instances: enemy_transforms,
)
```

## Debug Visualization

Tiramisu provides debug nodes for visualization:

```gleam
import tiramisu/debug

// Box (AABB)
scene.DebugBox(
  id: "collision_box",
  min: vec3.Vec3(-1.0, 0.0, -1.0),
  max: vec3.Vec3(1.0, 2.0, 1.0),
  color: 0x00ff00,
)

// Sphere
scene.DebugSphere(
  id: "trigger_zone",
  center: vec3.Vec3(0.0, 0.0, 0.0),
  radius: 5.0,
  color: 0xff0000,
)

// Line
scene.DebugLine(
  id: "raycast",
  from: vec3.Vec3(0.0, 1.0, 0.0),
  to: vec3.Vec3(0.0, 1.0, 10.0),
  color: 0x0000ff,
)

// Axes (X=red, Y=green, Z=blue)
scene.DebugAxes(
  id: "world_origin",
  origin: vec3.Vec3(0.0, 0.0, 0.0),
  size: 5.0,
)

// Grid
scene.DebugGrid(
  id: "ground_grid",
  size: 100.0,
  divisions: 10,
  color: 0x444444,
)

// Point
scene.DebugPoint(
  id: "spawn_point",
  position: vec3.Vec3(10.0, 0.0, 5.0),
  size: 0.5,
  color: 0xffff00,
)
```

## Summary

**Key concepts:**
- Scene graph is a tree of nodes
- `view()` returns `List(SceneNode)` every frame
- Tiramisu diffs and applies only changes
- Use Groups for hierarchy
- Use InstancedMesh for many identical objects
- Use LOD for distant objects
- Cache static scenes for performance

**Next steps:**
- [Performance Guide](./performance-guide.md) - Optimization techniques
- [Animation Guide](./animation-guide.md) - Tweens and state machines
- [Physics Guide](./physics-guide.md) - Rigid bodies and collisions
