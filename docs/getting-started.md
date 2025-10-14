# Getting Started with Tiramisu

Tiramisu is a type-safe 3D game engine for Gleam, built on Three.js. It follows the **Model-View-Update (MVU)** architecture, making game state management predictable and testable.

## Quick Start with Mascarpone 🧀

**The easiest way to start** is using [Mascarpone](https://hexdocs.pm/mascarpone/), an interactive CLI tool that scaffolds complete Tiramisu projects.

### Create a New Project

```bash
gleam new my_game
cd my_game
```

### Run Mascarpone

```bash
gleam add mascarpone
gleam run -m mascarpone
```

The interactive TUI will guide you through:

1. **Lustre Integration** - Choose whether to include Lustre for UI overlays (menus, HUDs)
2. **Project Template** - Select from:
   - **2D Game** - Orthographic camera and sprite setup
   - **3D Game** - Perspective camera with lighting
   - **Physics Demo** - Physics-enabled objects

Mascarpone automatically:
- ✅ Configures `gleam.toml` with all dependencies
- ✅ Sets up Three.js and Rapier3D CDN imports
- ✅ Creates a working game example
- ✅ Adds necessary stylesheets for fullscreen games
- ✅ Generates `.gitignore` for Gleam projects

### Run Your Game

```bash
gleam run -m lustre/dev start
```

Open your browser and navigate to http://localhost:1234 - you should see your game running!

---

## Manual Installation (Alternative)

If you prefer to set up your project manually or add Tiramisu to an existing project:

### Add Tiramisu Dependency

```bash
gleam add tiramisu
```

### Configure your gleam.toml

Add Three.js, Rapier, and styling configuration:

```toml
[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
stylesheets = [
  { content = "body { margin: 0; padding: 0; overflow: hidden; } canvas { display: block; }" }
]
```

## Your First Game: Hello Cube (Manual Setup)

If you used Mascarpone, you already have a working game! This section shows how to create a spinning 3D cube manually if you chose the manual installation path.

### Create the Game File

Replace the contents of `src/my_game.gleam` with:

```gleam
import gleam/option
import tiramisu
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

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
    dimensions: option.Some(tiramisu.Dimensions(width: 800.0, height: 600.0)),
    background: tiramisu.Color(0x1a1a2e),  // Dark blue background
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
  let assert Ok(cam) = camera.perspective(
    field_of_view: 75.0,
    near: 0.1,
    far: 1000.0,
  )

  let camera_node = scene.Camera(
    id: "main",
    camera: cam,
    transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
    look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
    active: True,
    viewport: option.None,
  )

  // Create rotating cube
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

  let cube = scene.Mesh(
    id: "cube",
    geometry: cube_geometry,
    material: cube_material,
    transform: transform.Transform(
      position: vec3.Vec3(0.0, 0.0, 0.0),
      rotation: vec3.Vec3(model.rotation, model.rotation, 0.0),
      scale: vec3.Vec3(1.0, 1.0, 1.0),
    ),
    physics: option.None,
  )

  // Add lights
  let assert Ok(ambient_light) = light.ambient(
    intensity: 0.5,
    color: 0xffffff,
  )

  let ambient = scene.Light(
    id: "ambient",
    light: ambient_light,
    transform: transform.identity,
  )

  let assert Ok(directional_light) = light.directional(
    intensity: 0.8,
    color: 0xffffff,
  )

  let directional = scene.Light(
    id: "sun",
    light: directional_light,
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

Open your browser and navigate to http://localhost:1234. You should see a spinning teal cube!

## Understanding the Code

### Model-View-Update Architecture

Tiramisu follows the **MVU pattern** (also called The Elm Architecture):

1. **init**: Create initial game state and effects
2. **view**: Render the current state as a scene
3. **update**: Handle events and produce new state
4. **Model**: Immutable game state
5. **Msg**: Events that trigger updates

### Key Concepts

#### 1. Context

The `Context` type provides timing, input, and canvas dimension information:

```gleam
pub type Context {
  Context(
    delta_time: Float,      // Time since last frame (seconds)
    input: InputState,      // Keyboard, mouse, touch state
    canvas_width: Float,    // Current canvas width in pixels
    canvas_height: Float,   // Current canvas height in pixels
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

