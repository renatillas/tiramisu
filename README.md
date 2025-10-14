# 🍰 Tiramisu

<img width="720" height="720" alt="Tiramisu-logo-03" src="https://github.com/user-attachments/assets/e4dff9c1-e132-4caa-82fc-37220990857b" />


**A type-safe 3D game engine for Gleam**

Tiramisu brings the power of functional programming and static type safety to game development, leveraging Three.js for professional-grade 3D rendering while maintaining Gleam's elegant, immutable design principles.

[![Package Version](https://img.shields.io/hexpm/v/tiramisu)](https://hex.pm/packages/tiramisu)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tiramisu/)

---

## ✨ Features

- **🔒 Type-Safe**: Catch bugs at compile time with Gleam's expressive type system
- **🎮 3D & 2D**: Full 3D capabilities powered by Three.js, with excellent 2D support
- **⚡ Immutable**: Predictable game state management through functional updates
- **🎬 Effect System**: MVU architecture inspired by Lustre for clean game loops
- **📦 Rich API**: Scene graphs, materials, lighting, animations, physics, and more
- **🚀 Production Ready**: Built on battle-tested Three.js with WebGL acceleration
- **🎯 Zero Runtime Overhead**: Compiles directly to readable JavaScript

---

## 🚀 Quick Start

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

type Model {
  Model(rotation: Float)
}

type Msg {
  Tick
}

type Ids {
  Cube
  MainCamera
  Sun
}

pub fn main() {
  tiramisu.run(
    dimensions: option.None,  // Fullscreen
    background: tiramisu.Color(0x1a1a2e),
    init: init,
    update: update,
    view: view,
  )
}

fn init(_ctx: tiramisu.Context(Ids)) {
  #(Model(rotation: 0.0), effect.tick(Tick), option.None)
}

fn update(model: Model, msg: Msg, ctx: tiramisu.Context(Ids)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(rotation: new_rotation), effect.tick(Tick), option.None)
    }
  }
}

fn view(model: Model, _ctx: tiramisu.Context(Ids)) {
  let assert Ok(cam) = camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)
  let assert Ok(cube_geo) = geometry.box(width: 1.0, height: 1.0, depth: 1.0)
  let assert Ok(cube_mat) =
    material.new()
    |> material.with_color(0x00ff00)
    |> material.with_metalness(1.0)
    |> material.with_roughness(0.3)
    |> material.build()
  let assert Ok(sun) = light.directional(intensity: 1.0, color: 0xffffff)

  [
    scene.Camera(
      id: MainCamera,
      camera: cam,
      transform: transform.at(position: vec3.Vec3(0.0, 2.0, 5.0)),
      look_at: option.Some(vec3.Vec3(0.0, 0.0, 0.0)),
      active: True,
      viewport: option.None,
    ),
    scene.Mesh(
      id: Cube,
      geometry: cube_geo,
      material: cube_mat,
      transform: transform.identity
        |> transform.rotate_y(model.rotation),
      physics: option.None,
    ),
    scene.Light(
      id: Sun,
      light: sun,
      transform: transform.at(position: vec3.Vec3(5.0, 5.0, 5.0)),
    ),
  ]
}
```

---

## 🧀 Quick Project Setup with Mascarpone

**The fastest way to start a new Tiramisu project:**

```sh
gleam add --dev mascarpone
gleam run -m mascarpone
```

[Mascarpone](https://hexdocs.pm/mascarpone/) is an interactive CLI tool that scaffolds complete Tiramisu projects with:

- 🎨 **Beautiful TUI** - Interactive project configuration
- 🎮 **Multiple Templates** - Choose from 2D games, 3D games, or physics demos
- 📦 **Automatic Setup** - Configured `gleam.toml`, dependencies, and CDN imports
- 🖥️ **Lustre Integration** - Optional UI overlays for menus and HUDs
- ⚡ **Working Examples** - Start with a functional game, not an empty file

After creating your project, just run:

```sh
gleam run -m lustre/dev start
```

Then open http://localhost:1234 to see your game!

---

## 📦 Manual Installation

Alternatively, add Tiramisu to an existing Gleam project:

```sh
gleam add tiramisu@2
```

### Configure your gleam.toml

Add Three.js, Rapier, and styling configuration:

```toml
name = "my_game"
version = "0.1.0"

# ...

[tools.lustre.html]
scripts = [
  { type = "importmap", content = "{ \"imports\": { \"three\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js\", \"three/addons/\": \"https://cdn.jsdelivr.net/npm/three@0.180.0/examples/jsm/\", \"@dimforge/rapier3d-compat\": \"https://cdn.jsdelivr.net/npm/@dimforge/rapier3d-compat@0.11.2/+esm\" } }" }
]
stylesheets = [
  { content = "body { margin: 0; padding: 0; overflow: hidden; }" }
]
```

**Note**: The `stylesheets` configuration removes default body margins and prevents scrollbars, which is essential for fullscreen games.

---

## 🏗️ Core Concepts

### MVU Architecture

Tiramisu follows the Model-View-Update pattern:

- **Model**: Your immutable game state
- **Update**: Pure function that transforms state based on messages
- **View**: Declarative scene description from current state

### Effect System

Side effects (network, timers, audio) are handled through a composable effect system, keeping your update logic pure and testable.

### Scene Graph

Hierarchical node system powered by Three.js, with functional updates and transforms. Support for meshes, lights, cameras, groups, and more.

---

## 🎮 What Can You Build?

- 3D games and simulations
- 2D platformers and top-down games
- Interactive visualizations
- Architectural walkthroughs
- Educational simulations
- Physics-based experiences
- Anything that runs in a browser!

---

## 🛠️ Built With

- **[Gleam](https://gleam.run)** - Type-safe functional language
- **[Three.js](https://threejs.org)** - 3D graphics library
- **[Lustre](https://lustre.build)** - Elm-inspired web framework

---

## 🤝 Contributing

Contributions are welcome! Whether it's bug reports, feature requests, or code contributions, please feel free to open an issue or pull request.

---

## 📄 License

MIT License - see [LICENSE.md](./LICENSE.md) for details.

---

## 🌟 Why Tiramisu?

> "Like the dessert, Tiramisu is made of layers - each one adding richness and depth. From the functional purity of Gleam to the rendering power of Three.js, every layer works together to create something delightful."

**Start building type-safe games today.** ✨

---

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
