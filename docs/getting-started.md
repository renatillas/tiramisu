# Getting Started with Tiramisu

Tiramisu is a type-safe 3D game engine for Gleam, built on Three.js. It follows the **Model-View-Update (MVU)** architecture, making game state management predictable and testable.

## Installation

### Create a New Project

```bash
gleam new my_game
cd my_game
```

### Add Tiramisu Dependency

```bash
gleam add tiramisu
```

Then install dependencies:

```bash
gleam deps download
```

### Add additional configuration to your gleam.toml

```toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
```

## Your First Game: Hello Cube

Let's create a spinning 3D cube in under 50 lines of code.

### Create the Game File

Replace the contents of `src/my_game.gleam` with:

```gleam
import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/scene
import tiramisu/transform
import tiramisu/vec3

// Model: Game state
pub type Model {
  Model(rotation: Float)
}

// Msg: Events that can happen
pub type Msg {
  Tick
}

pub fn main() {
  tiramisu.run(
    width: 800,
    height: 600,
    background: 0x1a1a2e,  // Dark blue background
    init: init,
    update: update,
    view: view,
  )
}

// Initialize the game state
fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg)) {
  #(Model(rotation: 0.0), effect.tick(Tick))
}

// Update game state based on events
fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation), effect.tick(Tick))
    }
  }
}

// Render the scene
fn view(model: Model) -> List(scene.SceneNode) {
  // Create camera
  let assert Ok(camera) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 800.0 /. 600.0,
      near: 0.1,
      far: 1000.0,
    )
    |> result.map(fn(camera) {
      camera
      |> camera.set_position(vec3.Vec3(0.0, 0.0, 5.0))
      |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))
      |> scene.Camera(
        id: "main",
        camera: _,
        transform: transform.identity,
        active: True,
        viewport: option.None,
      )
  })

  // Create rotating cube
  let cube =
    scene.Mesh(
      id: "cube",
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
        rotation: vec3.Vec3(model.rotation, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    )

  // Add lights
  let ambient =
    scene.Light(
      id: "ambient",
      light_type: scene.AmbientLight(color: 0xffffff, intensity: 0.5),
      transform: transform.identity,
    )

  let directional =
    scene.Light(
      id: "sun",
      light_type: scene.DirectionalLight(color: 0xffffff, intensity: 0.8),
      transform: transform.Transform(
        position: vec3.Vec3(5.0, 5.0, 5.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    )

  [camera_node, cube, ambient, directional]
}
```

### Run Your Game

```bash
gleam run -m lustre/dev start
```


Open your browser and navigate to the displayed URL. You should see a spinning teal cube!

## Understanding the Code

### Model-View-Update Architecture

Tiramisu follows the **MVU pattern** (also called The Elm Architecture):

```
┌─────────────────────────────────────────┐
│                                         │
│  ┌──────┐    ┌────────┐    ┌──────┐   │
│  │ Init │ -> │ Model  │ -> │ View │   │
│  └──────┘    └────────┘    └──────┘   │
│                   ▲            │        │
│                   │            ▼        │
│              ┌────────┐    ┌──────┐   │
│              │ Update │ <- │  Msg │   │
│              └────────┘    └──────┘   │
│                                         │
└─────────────────────────────────────────┘
```

1. **init**: Create initial game state and effects
2. **view**: Render the current state as a scene
3. **update**: Handle events and produce new state
4. **Model**: Immutable game state
5. **Msg**: Events that trigger updates

### Key Concepts

#### 1. Context

The `Context` type provides timing and input information:

```gleam
pub type Context {
  Context(
    delta_time: Float,  // Time since last frame (seconds)
    input: InputState,  // Keyboard, mouse, touch state
  )
}
```

#### 2. Effects

Effects are side effects that return messages:

```gleam
effect.tick(Tick)                    // Run on every frame
effect.from(fn(dispatch) { ... })    // Custom effect
effect.batch([effect1, effect2])     // Run multiple effects
effect.none()                         // No effects
```

#### 3. Scene Nodes

Your `view` function returns a list of `SceneNode`:

```gleam
pub type SceneNode {
  Camera(...)       // Camera (one must be active: True)
  Mesh(...)         // 3D object with geometry and material
  Light(...)        // Light source
  Group(...)        // Container for child nodes
  Sprite(...)       // 2D sprite (billboarded quad)
  InstancedMesh(...)  // Many identical objects (efficient)
  LOD(...)          // Level-of-detail system
}
```

#### 4. Transforms

Every scene node has a transform (position, rotation, scale):

```gleam
transform.Transform(
  position: vec3.Vec3(x, y, z),
  rotation: vec3.Vec3(rx, ry, rz),  // Radians
  scale: vec3.Vec3(sx, sy, sz),
)

// Or use the identity transform
transform.identity  // Position (0,0,0), no rotation, scale (1,1,1)
```

