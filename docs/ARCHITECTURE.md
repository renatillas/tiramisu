# Tiramisu Architecture: Declarative Scene Management

## Overview

Tiramisu uses a declarative, immutable approach to 3D scene management, inspired by Lustre and react-three-fiber. Instead of mutating Three.js objects directly, users declare the desired scene state, and Tiramisu efficiently diffs and applies changes.

## Core Concepts

### 1. Scene Nodes (Virtual DOM for 3D)

Scene nodes are immutable data structures that describe the 3D scene:

```gleam
pub type SceneNode {
  Mesh(
    id: String,
    geometry: GeometryType,
    material: MaterialType,
    transform: Transform,
  )
  Group(
    id: String,
    transform: Transform,
    children: List(SceneNode),
  )
  Light(
    id: String,
    light_type: LightType,
    transform: Transform,
  )
}

pub type Transform {
  Transform(
    position: Vec3,
    rotation: Vec3,
    scale: Vec3,
  )
}
```

### 2. Render Function

Users return a `List(SceneNode)` from their render function:

```gleam
fn render(state: GameState) -> List(SceneNode) {
  [
    Mesh(
      id: "ship",
      geometry: Cone(radius: 1.0, height: 2.0, segments: 4),
      material: Basic(color: 0x00ff99),
      transform: Transform(
        position: state.ship.position,
        rotation: state.ship.rotation,
        scale: Vec3(1.0, 1.0, 1.0),
      ),
    ),
    Light(
      id: "ambient",
      light_type: Ambient(color: 0xffffff, intensity: 0.5),
      transform: Transform(...),
    ),
  ]
}
```

### 3. Scene Diffing

The renderer maintains:
- **Previous scene tree**: Last rendered scene nodes
- **Current scene tree**: New scene nodes from render function
- **Three.js scene**: Actual Three.js objects

Diffing algorithm:
1. Compare previous and current trees by node ID
2. Detect additions, removals, and updates
3. Generate minimal set of patches
4. Apply patches to Three.js scene

```gleam
pub type Patch {
  AddNode(id: String, node: SceneNode, parent_id: Option(String))
  RemoveNode(id: String)
  UpdateTransform(id: String, transform: Transform)
  UpdateMaterial(id: String, material: MaterialType)
  // etc.
}
```

### 4. Game Loop Flow

```
User State → Update → New State → Render → Scene Nodes
                                              ↓
                                           Diff Engine
                                              ↓
                                          Patches
                                              ↓
                                      Apply to Three.js
```

## Benefits

1. **Immutability**: No mutation, pure functional code
2. **Declarative**: Describe what you want, not how to get there
3. **Performance**: Only update what changed
4. **Debugging**: Full scene state is serializable
5. **Time travel**: Can replay scene history
6. **Testability**: Render functions are pure

## Implementation Plan

1. Define scene node types and transform types
2. Implement scene diffing algorithm
3. Create patch application system
4. Update game loop to orchestrate diff/patch
5. Refactor existing examples to use new API
