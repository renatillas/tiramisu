# Getting Started with Tiramisu

Tiramisu is a type-safe 3D game engine for Gleam, built on Three.js. It follows the **Model-View-Update (MVU)** architecture, making game state management predictable and testable.

## Installation

### Prerequisites

- [Gleam](https://gleam.run/) 1.0.0 or later
- Node.js 18 or later
- A modern web browser

### Create a New Project

```bash
gleam new my_game
cd my_game
```

### Add Tiramisu Dependency

Add Tiramisu to your `gleam.toml`:

```toml
[dependencies]
gleam_stdlib = ">= 0.44.0 and < 2.0.0"
tiramisu = ">= 0.0.3 and < 1.0.0"
```

Then install dependencies:

```bash
gleam deps download
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
  let assert Ok(cam) =
    camera.perspective(
      field_of_view: 75.0,
      aspect: 800.0 /. 600.0,
      near: 0.1,
      far: 1000.0,
    )
  let cam =
    cam
    |> camera.set_position(vec3.Vec3(0.0, 0.0, 5.0))
    |> camera.look(at: vec3.Vec3(0.0, 0.0, 0.0))

  let camera_node =
    scene.Camera(
      id: "main",
      camera: cam,
      transform: transform.identity(),
      active: True,
      viewport: option.None,
    )

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
      transform: transform.identity(),
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
gleam run -t javascript
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
transform.identity()  // Position (0,0,0), no rotation, scale (1,1,1)
```

## Project Structure

A typical Tiramisu project looks like:

```
my_game/
├── gleam.toml           # Project configuration
├── src/
│   ├── my_game.gleam    # Main game file
│   └── my_game/         # Game modules
│       ├── entities.gleam
│       ├── levels.gleam
│       └── physics.gleam
├── assets/              # Game assets
│   ├── models/
│   ├── textures/
│   └── audio/
└── build/               # Compiled output
```

## Core Modules

Tiramisu provides these core modules:

- **tiramisu**: Game loop and initialization
- **tiramisu/scene**: Scene nodes (meshes, lights, cameras)
- **tiramisu/camera**: Camera types and configuration
- **tiramisu/transform**: Position, rotation, scale
- **tiramisu/vec3**: 3D vector math
- **tiramisu/effect**: Side effect system
- **tiramisu/input**: Keyboard, mouse, touch input
- **tiramisu/assets**: Asset loading and caching
- **tiramisu/physics**: Physics simulation (rigid bodies, colliders)
- **tiramisu/audio**: 2D and 3D audio
- **tiramisu/animation**: Tweens and animation state machines
- **tiramisu/debug**: Debug visualization tools

## What's Next?

Now that you have a basic game running, explore these tutorials:

1. **[Tutorial 1: Your First Game](./tutorial-1-first-game.md)** - Add keyboard input and interactivity
2. **[Tutorial 2: Adding Physics](./tutorial-2-physics.md)** - Bouncing balls with gravity
3. **[Tutorial 3: Loading Assets](./tutorial-3-assets.md)** - Load 3D models and textures
4. **[Tutorial 4: Character Controller](./tutorial-4-character.md)** - WASD movement + animations
5. **[Tutorial 5: Audio and Effects](./tutorial-5-audio.md)** - Sound effects + background music
6. **[Tutorial 6: Building a Complete Game](./tutorial-6-complete-game.md)** - Simple 3D platformer

## Examples

Check out the `/examples` directory for working examples:

- `0-effects_system` - Effect system with dynamic cube spawning
- `1-keyboard_mouse_input` - Input handling
- `3-materials_and_lights` - Material types and lighting
- `4-geometry_showcase` - Built-in geometry types
- `14-gltf_animated_model` - Loading animated 3D models
- `17-physics_demo` - Physics simulation
- `18-stress_test` - 20,000 instances at 60 FPS

## Getting Help

- **Examples**: See `/examples` for working code
- **Guides**: Check `/docs` for in-depth guides
- **API Docs**: Run `gleam docs build` to generate API documentation

Happy game development! 🎮
