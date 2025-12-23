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
gleam add --dev mascarpone 
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
import tiramisu/physics
import gleam/option
import gleam/time/duration
import tiramisu
import tiramisu/background
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
    background: background.Color(0x1a1a2e),  // Dark blue background
    init: init,
    update: update,
    view: view,
  )
}

// Initialize the game state
fn init(_ctx: tiramisu.Context) -> #(Model, effect.Effect(Msg), option.Option(physics.PhysicsWorld)) {
  #(Model(rotation: 0.0), effect.tick(Tick), option.None)
}

// Update game state based on events
fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(physics.PhysicsWorld)) {
  case msg {
    Tick -> {
      // ctx.delta_time is a Duration type - convert to seconds for rotation
      let delta_seconds = duration.to_seconds(ctx.delta_time)
      let new_rotation = model.rotation +. delta_seconds
      #(Model(rotation: new_rotation), effect.tick(Tick), option.None)
    }
  }
}

// Render the scene
fn view(model: Model, _ctx: tiramisu.Context) -> scene.Node {
  // Create camera
  let assert Ok(cam) = camera.perspective(
    field_of_view: 75.0,
    near: 0.1,
    far: 1000.0,
  )

  let camera_node = scene.camera(
    id: "main",
    camera: cam,
    transform: transform.at(position: vec3.Vec3(0.0, 0.0, 5.0)),
    look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
    active: True,
    viewport: option.None,
    children: [],
    postprocessing: option.None,
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

  let cube = scene.mesh(
    id: "cube",
    geometry: cube_geometry,
    material: cube_material,
    transform: transform.identity
      |> transform.with_position(vec3.Vec3(0.0, 0.0, 0.0))
      |> transform.with_euler_rotation(vec3.Vec3(model.rotation, model.rotation, 0.0)),
    physics: option.None,
  )

  // Add lights
  let assert Ok(ambient_light) = light.ambient(
    intensity: 0.5,
    color: 0xffffff,
  )

  let ambient = scene.light(
    id: "ambient",
    light: ambient_light,
    transform: transform.identity,
  )

  let assert Ok(directional_light) = light.directional(
    intensity: 0.8,
    color: 0xffffff,
  )

  let directional = scene.light(
    id: "sun",
    light: directional_light,
    transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
  )

  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [camera_node, cube, ambient, directional],
  )
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
    delta_time: Float,      // Time since last frame in milliseconds (e.g., 16.0 for 60 FPS)
    input: InputState,      // Keyboard, mouse, touch state
    canvas_width: Float,    // Current canvas width in pixels
    canvas_height: Float,   // Current canvas height in pixels
    ...
  )
}
```

**Delta Time**: Always use `delta_time` for movement and animations to ensure frame-rate independence. `delta_time` is in **milliseconds**, so convert to seconds by multiplying by `0.001`. For example, if you want an object to move at 5 units per second: `5.0 *. ctx.delta_time *. 0.001` each frame.

#### 2. Effects

Effects are side effects that may return messages:

```gleam
effect.tick(Tick)                    // Run on every frame
effect.from(fn(dispatch) { ... })    // Custom effect
effect.batch([effect1, effect2])     // Run multiple effects
effect.none()                         // No effects
```

#### 3. Scene Nodes

Your `view` function returns a single root `scene.Node`. If you have multiple top-level nodes, wrap them in `scene.empty()`:

```gleam
fn view(model: Model, _ctx: Context) -> scene.Node {
  scene.empty(
    id: "root",
    transform: transform.identity,
    children: [camera_node, player_mesh, enemy_mesh, light_node],
  )
}
```


#### 4. Transforms

Every scene node has a transform (position, rotation, scale). The Transform type is opaque, so you use builder functions:

```gleam
// Identity transform (origin, no rotation, scale 1)
transform.identity

// Position only
transform.at(position: vec3.Vec3(5.0, 0.0, -3.0))

// Build with multiple properties
transform.identity
  |> transform.with_position(vec3.Vec3(5.0, 2.0, -3.0))
  |> transform.with_euler_rotation(vec3.Vec3(0.0, 1.57, 0.0))  // Radians (90° on Y)
  |> transform.with_scale(vec3.Vec3(2.0, 2.0, 2.0))  // 2x larger
```

**Rotation:** Uses Euler angles in radians (π radians = 180°, π/2 = 90°)

