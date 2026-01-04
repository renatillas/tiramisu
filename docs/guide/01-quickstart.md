# Quickstart

This guide will get you up and running with Tiramisu in just a few minutes. By the end, you'll have a spinning 3D cube on screen and understand the basic structure of a Tiramisu game.

## What is Tiramisu?

Tiramisu is a **type-safe 3D game engine** for Gleam. If you're coming from other game engines like Unity, Godot, or even web frameworks like React, you might be wondering what makes Tiramisu different.

The short answer: **everything is a function**.

There's no scene editor, no drag-and-drop components, no hidden mutable state. Instead, you describe your game as a pure function from state to scene. The engine handles the messy details of rendering, input, and timing. You focus on the logic.

This approach comes from the Elm Architecture (also called Model-View-Update or MVU), which Lustre uses for web UIs. Tiramisu adapts this pattern for games, giving you:

- **Predictable state** - All changes happen in one place, making debugging trivial
- **Testable logic** - Pure functions are easy to unit test
- **Time-travel debugging** - You can replay and inspect any state
- **No hidden bugs** - The type system catches errors at compile time

If you're initially put off by the lack of a visual editor, we encourage you to stick with it. The benefits compound as your game grows in complexity.

## Prerequisites

Before we begin, make sure you have:

- [Gleam](https://gleam.run/getting-started/installing/)
- A modern web browser (Chrome, Firefox, Safari, or Edge)
- A text editor of your choice

Tiramisu runs in the browser using JavaScript, so there's no native compilation step to worry about.

## Creating a new project

The easiest way to start is with **Mascarpone**, an interactive CLI tool that scaffolds complete Tiramisu projects:

```bash
# Create a new Gleam project
gleam new my_game
cd my_game

# Add Mascarpone as a dev dependency
gleam add --dev mascarpone

# Run the interactive setup
gleam run -m mascarpone
```

Mascarpone will guide you through a few questions:

1. **Lustre Integration** - Do you want a UI layer for menus and HUDs? If you're unsure, say yes. It's easier to remove later than to add.

2. **Project Template** - Choose from:
   - **2D Game** - Orthographic camera, sprite-friendly setup
   - **3D Game** - Perspective camera with lighting (recommended for this guide)
   - **Physics Demo** - Pre-configured with Rapier physics

Behind the scenes, Mascarpone configures everything: dependencies, CDN imports for Three.js and Rapier, stylesheets for fullscreen rendering, and a working example to start from.

## Running your game

Once Mascarpone finishes, start the development server:

```bash
gleam run -m lustre/dev start
```

Open your browser to [http://localhost:1234](http://localhost:1234). You should see your game running!

The dev server watches for changes and automatically recompiles. Edit your code, save, and the browser refreshes.

## Understanding the code

Let's start with the simplest possible Tiramisu app. Open `src/my_game.gleam`:

```gleam
import gleam/option
import tiramisu
import tiramisu/scene
import tiramisu/geometry
import tiramisu/material
import tiramisu/transform

pub fn main() {
  tiramisu.element(my_scene())
  |> tiramisu.start("#app", tiramisu.FullScreen, option.None)
}

fn my_scene() -> scene.Node {
  let cube_geo = geometry.box(1.0, 1.0, 1.0)
  let cube_mat = material.basic(color: 0x4ecdc4)

  scene.group(id: "root", transform: transform.identity, children: [
    scene.mesh(id: "cube", geometry: cube_geo, material: cube_mat, ...),
    // camera, lights, etc.
  ])
}
```

That's it! `tiramisu.element` takes a scene node and renders it. No state, no messages, no update loop—just a static 3D scene.

### How it works

1. **`tiramisu.element(scene)`** - Creates an app from a static scene
2. **`tiramisu.start(app, selector, dimensions, bridge)`** - Runs the app in the DOM element matching the selector

The `element` function is perfect for:
- Learning scene construction
- Static visualizations
- Demos and prototypes

### Adding animation

A static cube is nice, but games need movement! For that, you need **state** and a **game loop**. That's where `tiramisu.application` comes in:

```gleam
import gleam/option
import gleam/time/duration
import tiramisu
import tiramisu/effect
import tiramisu/scene

pub type Model {
  Model(rotation: Float)
}

pub type Msg {
  Tick
}

pub fn main() {
  tiramisu.application(init, update, view)
  |> tiramisu.start("#app", tiramisu.FullScreen, option.None)
}

fn init(_ctx) {
  #(Model(rotation: 0.0), effect.dispatch(Tick), option.None)
}

fn update(model, msg, ctx) {
  case msg {
    Tick -> {
      let delta = duration.to_seconds(ctx.delta_time)
      #(Model(rotation: model.rotation +. delta), effect.dispatch(Tick), option.None)
    }
  }
}

fn view(model, _ctx) {
  scene.mesh(
    id: "cube",
    transform: transform.rotate_y(model.rotation),
    // ... geometry, material, etc.
  )
}
```

This introduces the **Model-View-Update** pattern:

- **Model** - Your game state (here, just a rotation angle)
- **Msg** - Events that can happen (here, just `Tick` for each frame)
- **init** - Creates initial state and dispatches the first `Tick`
- **update** - Handles messages and returns new state + next effect
- **view** - Renders state as a scene

The key insight: `effect.dispatch(Tick)` queues a message for the next frame, creating the game loop.

This pattern is covered in depth in [State Management](02-state-management.md) and [Side Effects](03-side-effects.md).

## The game loop

Let's trace what happens each frame:

```text
+---------------------------------------------------------+
|                    TIRAMISU RUNTIME                     |
+---------------------------------------------------------+
|                                                         |
|   +----------+                                          |
|   |   init   | ---> Initial Model + Effects             |
|   +----------+                                          |
|        |                                                |
|        v                                                |
|   +----------+      +----------+      +----------+      |
|   |  update  | <--> |  Model   | ---> |   view   |      |
|   +----------+      +----------+      +----------+      |
|        ^                                    |           |
|        |                                    v           |
|   +----------+                        +----------+      |
|   | Messages | <--------------------- |  Render  |      |
|   +----------+                        +----------+      |
|                                                         |
+---------------------------------------------------------+
```

1. **init** runs once at startup, creating the initial Model
2. **update** processes any queued messages, producing a new Model
3. **view** converts the Model into a scene tree
4. The runtime renders the scene and processes effects
5. Effects may dispatch new messages, which queue for the next frame
6. Repeat from step 2

This is a **closed loop**. Data flows in one direction: Model -> view -> render -> messages -> update -> Model. There's no way for state to change except through update, which makes reasoning about your game much simpler.

## Making changes

Try modifying the cube's color. Find the material definition:

```gleam
let assert Ok(cube_material) =
  material.new()
  |> material.with_color(0x4ecdc4)  // Change this!
  |> material.with_metalness(0.5)
  |> material.with_roughness(0.5)
  |> material.build()
```

Change `0x4ecdc4` to `0xff6b6b` for a coral red. Save, and watch the browser update.

Now try changing the rotation speed:

```gleam
let new_rotation = model.rotation +. delta *. 2.0  // Twice as fast!
```

Experiment! The type system will catch most mistakes before you even run the code.

## Where to go from here

You now understand the core pattern: Model holds state, Messages describe events, update changes state, view renders it. Everything else in Tiramisu builds on this foundation.

**If you prefer learning by reading**, continue to the next guide on [State Management](02-state-management.md), which dives deeper into structuring your Model and Messages.

**If you prefer learning by example**, check out the `examples/` folder in the Tiramisu repository. Start with the simpler ones and work your way up.

**If you want to build something now**, try these modifications:

1. Add keyboard controls to rotate the cube manually
2. Add a second cube that rotates in the opposite direction
3. Change the camera position based on mouse movement

The [Input guide](04-input.md) covers keyboard and mouse handling in detail.

## Getting help

If you get stuck:

- Check the [API documentation](https://hexdocs.pm/tiramisu/) for function signatures
- Browse the examples for working code
- Open an issue on GitHub if something seems broken

Welcome to Tiramisu. Let's make some games.
